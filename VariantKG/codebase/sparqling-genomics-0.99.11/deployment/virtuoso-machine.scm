;;; Copyright © 2019, 2020 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (gnu))
(use-service-modules base networking ssh shepherd)
(use-package-modules ssh databases linux gawk curl nss certs)

;; Define the IPTables rules
(define %iptables-rules
  "*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
-A INPUT -p icmp -j ACCEPT
-A INPUT -i lo -j ACCEPT
-A INPUT -p tcp --dport 22 -j ACCEPT
-A INPUT -p tcp --dport 8890 -j ACCEPT
-A INPUT -p tcp --dport 1111 -j ACCEPT
-A INPUT -j DROP
-A FORWARD -j DROP
COMMIT
")

(define (start-virtuoso)
  #~(begin
      (use-modules (ice-9 popen)
                   (ice-9 rdelim)
                   (ice-9 format))
      (unless (file-exists? "/db") (mkdir "/db"))
      (let* ((grep #$(file-append grep "/bin/grep"))
             (awk  #$(file-append gawk "/bin/awk"))
             (df   #$(file-append coreutils "/bin/df"))
             (head #$(file-append coreutils "/bin/head"))
             (tail #$(file-append coreutils "/bin/tail"))
             (wc   #$(file-append coreutils "/bin/wc"))
             (threads (let* ((port (open-input-pipe (string-append
                                                     grep " processor /proc/cpuinfo | "
                                                     wc " -l")))
                             (str  (read-line port)))
                        (close-pipe port)
                        (inexact->exact (string->number str))))
             (memory  (let* ((port (open-input-pipe (string-append
                                                     head " -n 1 /proc/meminfo | "
                                                     awk " '{ print $2 }'")))
                             (str  (read-line port)))
                        (close-pipe port)
                        (inexact->exact (floor (/ (string->number str) 1024.0 1024.0)))))
             (expected-database-size
                      (let* ((port (open-input-pipe (string-append
                                                     df " /db | "
                                                     tail " -n 1 | "
                                                     awk " '{ print $2 }'")))
                             (str  (read-line port)))
                        (close-pipe port)
                        (inexact->exact (floor (/ (string->number str) 1024.0 1024.0)))))
             (number-of-buffers (/ (* (* memory 1000 1000 1024) 0.66) 8000))
             (virtuoso
              (lambda (str) (zero? (apply system*
                                          #$(file-append virtuoso-ose "/bin/virtuoso-t")
                                          (string-tokenize str))))))
        (format #t "Starting Virtuoso...~%")
        (call-with-output-file "/etc/virtuoso.ini"
          (lambda (port)
            (for-each
             (lambda (pair) (if (pair? pair)
                                (format port "~a = ~a~%" (car pair) (cdr pair))
                                (format port "~a~%" pair)))
             `("[Database]"
               (DatabaseFile                     . "/db/virtuoso.db")
               (ErrorLogFile                     . "/db/virtuoso.log")
               (LockFile                         . "/db/virtuoso.lck")
               (TransactionFile                  . "/db/virtuoso.trx")
               (xa_persistent_file               . "/db/virtuoso.pxa")
               (FileExtend                       . "200")
               (MaxCheckpointRemap               . ,(string-drop-right (format #f "~,0f" (/ (* expected-database-size 1000 1000 1024) 4)) 1))

               "[Parameters]"
               (MaxClientConnection              . ,(number->string (* threads 10)))
               (DirsAllowed                      . "/")
               (VADInstallDir                    . "/share/virtuoso/vad/")
               (RdfFreeTextRulesSize             . "1000")
               (IndexTreeMaps                    . "1024")
               (MaxQueryMem                      . ,(string-append (string-drop-right (format #f "~,0f" (/ memory 2.0)) 1) "G"))
               (AdjustVectorSize                 . "1")
               (ThreadsPerQuery                  . ,(number->string threads))
               (AsyncQueueMaxThreads             . ,(number->string threads))
               (NumberOfBuffers                  . ,(string-drop-right (format #f "~,0f" (round number-of-buffers)) 1))
               (MaxDirtyBuffers                  . ,(string-drop-right (format #f "~,0f" (round (* number-of-buffers 0.25))) 1))
               ""
               "[HTTPServer]"
               (ServerPort                       . "8890")
               (ServerRoot                       . "/var/lib/virtuoso/vsp")
               (MaxClientConnections             . ,(number->string (* threads 10)))
               (ServerThreads                    . ,(number->string (* threads 10)))
               (HttpPrintWarningsInOutput        . "0")
               (Charset                          . "UTF-8")
               (MaintenancePage                  . "atomic.html")
               (EnabledGzipContent               . "1")
               ""
               "[VDB]"
               (ArrayOptimization                . "1")
               ""
               "[Replication]"
               (ServerEnable                     . "0")
               ""
               "[Zero Config]"
               (ServerName                       . "virtuoso")
               ""
               "[URIQA]"
               (DefaultHost                      . "localhost:8890")
               ""
               "[SPARQL]"
               (ExternalQuerySource              . "1")
               (ResultSetMaxRows                 . "1000000000000")
               (MaxQueryCostEstimationTime       . "9000000000000")
               (MaxQueryExecutionTime            . "9000000000000")
               (DefaultQuery                     . "")
               (DeferInferenceRulesInit          . "0")))))
        (virtuoso "+configfile /etc/virtuoso.ini"))))

(define (virtuoso-service)
  (let [(start-db (start-virtuoso))]
    (simple-service 'virtuoso shepherd-root-service-type
                    (list (shepherd-service
                           (provision '(virtuoso))
                           (requirement '())
                           (start #~(lambda () #$start-db))
                           (respawn? #t))))))

(operating-system
 (host-name "virtuoso")
 (timezone "Europe/Amsterdam")
 (locale "en_US.utf8")

 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (target "/dev/sda")))

 (file-systems (cons (file-system
                      (device (file-system-label "root"))
                      (mount-point "/")
                      (type "xfs"))
                     %base-file-systems))

 (users (cons (user-account
               (name "virtuoso")
               (group "users")
               (supplementary-groups '("wheel")))
              %base-user-accounts))

 (packages (cons* procps util-linux curl nss nss-certs %base-packages))

 (services
  (append
   (cons* (service dhcp-client-service-type)
          (service openssh-service-type (openssh-configuration (port-number 22)))
          (service iptables-service-type
                   (iptables-configuration
                    (ipv4-rules (plain-file "iptables.rules" %iptables-rules))
                    (ipv6-rules (plain-file "ip6tables.rules" %iptables-rules))))
          (virtuoso-service)
          %base-services))))
