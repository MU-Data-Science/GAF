dnl Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program. If not, see <http://www.gnu.org/licenses/>.
dnl -*- Autoconf -*-

AC_INIT(sparqling-genomics, 0.99.11)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_PROG_CC
AM_PROG_CC_C_O
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h stdio.h])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

dnl Installation directories for .scm and .go files.
moddir="${prefix}/share/guile/site/$GUILE_EFFECTIVE_VERSION"
godir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([moddir])
AC_SUBST([godir])

if $GUILE_TOOLS | grep -q compile; then
   true
else
   AC_MSG_ERROR([Guile 3.0, 2.2, or 2.0 required.])
fi

dnl This macro is used to resolve the absolute path to @datarootdir@ so that
dnl the absolute path can be used in the configuration file for the web
dnl interface.
dnl ---------------------------------------------------------------------------
AC_DEFUN([AC_DEFINE_DIR], [
  prefix_NONE=
  exec_prefix_NONE=
  test "x$prefix" = xNONE && prefix_NONE=yes && prefix=$ac_default_prefix
  test "x$exec_prefix" = xNONE && exec_prefix_NONE=yes && exec_prefix=$prefix
dnl In Autoconf 2.60, ${datadir} refers to ${datarootdir}, which in turn
dnl refers to ${prefix}.  Thus we have to use `eval' twice.
  eval ac_define_dir="\"[$]$2\""
  eval ac_define_dir="\"$ac_define_dir\""
  AC_SUBST($1, "$ac_define_dir")
  AC_DEFINE_UNQUOTED($1, "$ac_define_dir", [$3])
  test "$prefix_NONE" && prefix=NONE
  test "$exec_prefix_NONE" && exec_prefix=NONE
])

dnl The SYSCONFDIR is left configurable on 'make'.  However, the 'sg-web'
dnl script needs no 'make' step, so we expand the variable here.
SYSCONFDIR=`test "$prefix" = NONE && prefix=$ac_default_prefix; eval echo "${sysconfdir}"`
AC_SUBST([sysconfdir], [$SYSCONFDIR])

webconfdir=$SYSCONFDIR/sparqling-genomics
AC_SUBST([webconfdir])

dnl Use the above macro to define a resolved path for the web root.
dnl ---------------------------------------------------------------------------
AC_DEFINE_DIR([WEB_ROOT], [datarootdir], [Data directory for web resources.])

AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([web/sg-web], [chmod +x web/sg-web])
AC_CONFIG_FILES([web/sg-auth-manager], [chmod +x web/sg-auth-manager])
AC_CONFIG_FILES([web/sg-web-test], [chmod +x web/sg-web-test])
AC_CONFIG_FILES([tools/folder2rdf/folder2rdf],
                [chmod +x tools/folder2rdf/folder2rdf])
AC_CONFIG_FILES([tools/virtuoso-config/virtuoso-config],
                [chmod +x tools/virtuoso-config/virtuoso-config])
AC_CONFIG_FILES([tools/ega2rdf/ega2rdf],
                [chmod +x tools/ega2rdf/ega2rdf])
AC_CONFIG_FILES([
  Makefile
  doc/Makefile
  doc/sparqling-genomics.sty
  tools/Makefile
  tools/bam2rdf/Makefile
  tools/ega2rdf/Makefile
  tools/folder2rdf/Makefile
  tools/json2rdf/Makefile
  tools/table2rdf/Makefile
  tools/virtuoso-config/Makefile
  tools/vcf2rdf/Makefile
  tools/xml2rdf/Makefile
  web/Makefile
  web/ldap/authenticate.scm
  web/sg-web.service
  web/www/config.scm
])

PKG_PROG_PKG_CONFIG
PKG_CHECK_MODULES([htslib], [htslib])
PKG_CHECK_MODULES([raptor2], [raptor2])
PKG_CHECK_MODULES([libxml2], [libxml-2.0])
PKG_CHECK_MODULES([gnutls], [gnutls])
PKG_CHECK_MODULES([zlib], [zlib])


dnl Let users provide a path to the libldap library because there is
dnl no portable way to find the path to libldap.
dnl ---------------------------------------------------------------------------
AC_ARG_WITH([libldap-prefix],
  [AS_HELP_STRING([--with-libldap-prefix=DIR], [search for libldap in DIR])],
  [case "$withval" in
    yes|no)
      ;;
    *)
      LIBLDAP="$withval/lib/libldap"
      LIBLDAP_PREFIX="$withval"
      LIBLDAP_LIBDIR="$withval/lib"
      ;;
   esac])

dnl Library name suitable for `dynamic-link'.
AC_MSG_CHECKING([for libldap shared library name])
AC_MSG_RESULT([$LIBLDAP])
AC_SUBST([LIBLDAP])
AC_SUBST([LIBLDAP_PREFIX])
AC_SUBST([LIBLDAP_LIBDIR])

dnl Check for presence of 'pdflatex', 'bibtex', and 'htlatex'.
AC_CHECK_PROG(PDFLATEX, pdflatex, pdflatex)
AC_CHECK_PROG(BIBTEX, bibtex, bibtex)
AC_CHECK_PROG(HTLATEX, htlatex, htlatex)

if test "x$BIBTEX" = "x"; then
  AC_MSG_WARN([Unable to create the user manual.])
else
  if test "x$PDFLATEX" = "x"; then
    AC_MSG_WARN([Unable to create PDF version of the user manual.])
  fi
  if test "x$HTLATEX" = "x"; then
    AC_MSG_WARN([Unable to create HTML version of the user manual.])
  fi
fi

AC_SUBST(ENABLE_MTRACE_OPTION)
AC_ARG_ENABLE([mtrace],
  AC_HELP_STRING([--enable-mtrace],
  [enable mtrace support @<:@default=no@:>@]),
  AM_CONDITIONAL([ENABLE_MTRACE_OPTION], [true]),
  AM_CONDITIONAL([ENABLE_MTRACE_OPTION], [false]))

AM_CONDITIONAL([HAVE_PDFLATEX], test -n "$PDFLATEX")
AM_CONDITIONAL([HAVE_HTLATEX], test -n "$HTLATEX")
AM_CONDITIONAL([HAVE_BIBTEX], test -n "$BIBTEX")

AC_OUTPUT
