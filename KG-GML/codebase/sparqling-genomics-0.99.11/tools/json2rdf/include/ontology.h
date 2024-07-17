/* 
 * Copyright (C) 2019  Roel Janssen <roel@gnu.org>
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

#ifndef ONTOLOGY_H
#define ONTOLOGY_H

#include <stdbool.h>
#include <stdint.h>
#include <raptor2.h>

#include "master-ontology.h"

/* These string constants can be used to concatenate strings at compile-time. */
#define URI_ONTOLOGY      "sg://" VERSION "/json2rdf"

#define STR_PREFIX_BASE                URI_ONTOLOGY "/"
#define STR_PREFIX_DYNAMIC_TYPE        URI_ONTOLOGY "/DynamicType/"

typedef enum
{
  PREFIX_BASE = 0,
  PREFIX_MASTER,
  PREFIX_ORIGIN,
  PREFIX_DYNAMIC_TYPE,
  PREFIX_RDF,
  PREFIX_RDFS,
  PREFIX_OWL,
  PREFIX_XSD
} ontology_prefix;

typedef enum
{
  CLASS_RDF_TYPE = 0,
  CLASS_ORIGIN,
} ontology_class;

typedef struct
{
  raptor_term **classes;
  raptor_uri  **prefixes;
  raptor_uri **xsds;
  int32_t     classes_length;
  int32_t     prefixes_length;
  int32_t     xsds_length;
} ontology_t;

#define XSD_STRING              0
#define XSD_INTEGER             1
#define XSD_FLOAT               2
#define XSD_BOOLEAN             3

bool ontology_init (ontology_t **ontology_ptr);
void ontology_free (ontology_t *ontology);

raptor_term* term (int32_t index, char *suffix);

#endif  /* ONTOLOGY_H */
