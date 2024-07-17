# Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

AUTOMAKE_OPTIONS 	= subdir-objects
SUBDIRS                 = tools web

# Only attempt to build the documentation when PDFLATEX is available.
if HAVE_PDFLATEX
SUBDIRS                += doc
endif

deploymentdir           = ${pkgdatadir}
ontologydir             = ${pkgdatadir}
nobase_ontology_DATA    = ontologies/sparqling-genomics.ttl
nobase_deployment_DATA  = deployment/virtuoso-machine.scm
EXTRA_DIST              = $(nobase_ontology_DATA)   \
                          $(nobase_deployment_DATA) \
                          environment.scm

# Build docker image with Guix
docker: dist
	guix pack -C none -e '(load "environment.scm")' \
             --with-source=sparqling-genomics-$(VERSION).tar.gz \
             --format=docker \
             -S /bin=bin -S /lib=lib -S /share=share -S /etc=etc -S /var=var \
             glibc-utf8-locales tzdata coreutils bash virtuoso-ose
