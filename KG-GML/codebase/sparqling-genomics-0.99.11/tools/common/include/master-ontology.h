/* Copyright (C) 2019  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MASTER_ONTOLOGY_H
#define MASTER_ONTOLOGY_H

/* These string constants can be used to concatenate strings at compile-time. */
#define URI_W3            "http://www.w3.org"
#define URI_MASTER        "https://sparqling-genomics.org/" VERSION

#define STR_PREFIX_MASTER              URI_MASTER "/"
#define STR_PREFIX_ORIGIN              "origin://"
#define STR_PREFIX_RDF                 URI_W3 "/1999/02/22-rdf-syntax-ns#"
#define STR_PREFIX_RDFS                URI_W3 "/2000/01/rdf-schema#"
#define STR_PREFIX_OWL                 URI_W3 "/2002/07/owl#"
#define STR_PREFIX_XSD                 URI_W3 "/2001/XMLSchema#"

/* The following marcros can be used to construct terms (nodes) and URIs.
 * These assume 'config.raptor_world', 'config.uris', 'config.ontology',
 * and 'config.raptor_serializer' exist and have been initialized.
 */
#define uri(index, suffix)                                      \
  raptor_new_uri_relative_to_base (config.raptor_world,         \
                                   config.uris[index],          \
                                   str)

#define class(index)      config.ontology->classes[index]
#define predicate(index)  config.ontology->predicates[index]

#define literal(str, datatype)                                  \
  raptor_new_term_from_literal                                  \
  (config.raptor_world, (unsigned char *)str,                   \
   config.ontology->xsds[datatype],                             \
   NULL)

#define register_statement(stmt)                                \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  raptor_free_statement (stmt)

#define register_statement_reuse_subject(stmt)                  \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  stmt->subject = NULL;                                         \
  raptor_free_statement (stmt)

#define register_statement_reuse_predicate(stmt)                \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  stmt->predicate = NULL;                                       \
  raptor_free_statement (stmt)

#define register_statement_reuse_object(stmt)                   \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  stmt->object = NULL;                                          \
  raptor_free_statement (stmt)

#define register_statement_reuse_subject_predicate(stmt)        \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  stmt->subject = NULL;                                         \
  stmt->predicate = NULL;                                       \
  raptor_free_statement (stmt)

#define register_statement_reuse_subject_object(stmt)           \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  stmt->subject = NULL;                                         \
  stmt->object = NULL;                                          \
  raptor_free_statement (stmt)

#define register_statement_reuse_predicate_object(stmt)         \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  stmt->predicate = NULL;                                       \
  stmt->object = NULL;                                          \
  raptor_free_statement (stmt)

#define register_statement_reuse_all(stmt)                      \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  stmt->subject = NULL;                                         \
  stmt->predicate = NULL;                                       \
  stmt->object = NULL;                                          \
  raptor_free_statement (stmt)

#endif  /* MASTER_ONTOLOGY_H */
