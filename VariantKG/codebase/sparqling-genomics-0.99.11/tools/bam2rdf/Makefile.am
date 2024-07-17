# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS     = subdir-objects
SUBDIRS              = .
bam2rdf_CFLAGS       = -I$(srcdir)/include -I$(srcdir)/../common/include      \
                       $(gnutls_CFLAGS) $(htslib_CFLAGS) $(raptor2_CFLAGS)

if ENABLE_MTRACE_OPTION
bam2rdf_CFLAGS      += -DENABLE_MTRACE
endif

bin_PROGRAMS         = bam2rdf
bam2rdf_SOURCES      = ../common/src/helper.c ../common/include/helper.h      \
                       ../common/include/master-ontology.h                    \
                       src/main.c include/runtime_configuration.h             \
                       src/runtime_configuration.c                            \
                       src/ui.c include/ui.h                                  \
                       src/ontology.c include/ontology.h                      \
                       src/bam_header.c include/bam_header.h                  \
                       src/bam_reads.c include/bam_reads.h

bam2rdf_LDFLAGS      = # -pthread
bam2rdf_LDADD        = $(gnutls_LIBS) $(htslib_LIBS) $(raptor2_LIBS)
