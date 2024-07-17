# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS     = subdir-objects
SUBDIRS              = .
xml2rdf_CFLAGS       = -I$(srcdir)/include -I$(srcdir)/../common/include      \
                       $(gnutls_CFLAGS) $(libxml2_CFLAGS) $(raptor2_CFLAGS)   \
                       $(zlib_CFLAGS)

if ENABLE_MTRACE_OPTION
xml2rdf_CFLAGS      += -DENABLE_MTRACE
endif

bin_PROGRAMS         = xml2rdf
xml2rdf_SOURCES      = ../common/src/helper.c ../common/include/helper.h      \
                       ../common/include/master-ontology.h                    \
                       src/main.c include/runtime_configuration.h             \
                       ../common/src/list.c ../common/include/list.h          \
                       src/id.c include/id.h                                  \
                       src/runtime_configuration.c                            \
                       src/ui.c include/ui.h                                  \
                       src/ontology.c include/ontology.h                      \
                       src/xml.c include/xml.h

xml2rdf_LDADD        = $(gnutls_LIBS) $(libxml2_LIBS) $(raptor2_LIBS)         \
                       $(zlib_LIBS)

EXTRA_DIST           = tests/input.xml
