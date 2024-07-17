# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS     = subdir-objects
SUBDIRS              = .
json2rdf_CFLAGS      = -I$(srcdir)/include -I$(srcdir)/../common/include            \
                       $(gnutls_CFLAGS) $(raptor2_CFLAGS) $(zlib_CFLAGS)

if ENABLE_MTRACE_OPTION
json2rdf_CFLAGS     += -DENABLE_MTRACE
endif

bin_PROGRAMS         = json2rdf
json2rdf_SOURCES     = ../common/src/helper.c ../common/include/helper.h            \
                       ../common/include/master-ontology.h                          \
                       src/main.c include/runtime_configuration.h                   \
                       ../common/src/list.c ../common/include/list.h                \
                       src/runtime_configuration.c                                  \
                       src/ui.c include/ui.h                                        \
                       src/ontology.c include/ontology.h                            \
                       src/yajl_alloc.c include/yajl_alloc.h                        \
                       src/yajl_buf.c include/yajl_buf.h                            \
                       src/yajl.c include/yajl_bytestack.h include/yajl_common.h    \
                       src/yajl_encode.c include/yajl_encode.h                      \
                       src/yajl_gen.c include/yajl_gen.h                            \
                       src/yajl_lex.c include/yajl_lex.h                            \
                       src/yajl_parser.c include/yajl_parse.h include/yajl_parser.h \
                       src/json.c include/json.h

json2rdf_LDADD       = $(gnutls_LIBS) $(raptor2_LIBS) $(zlib_LIBS)

EXTRA_DIST           = tests/input.json
