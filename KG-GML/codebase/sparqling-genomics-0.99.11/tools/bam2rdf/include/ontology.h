/* Copyright (C) 2018  Roel Janssen <roel@gnu.org>
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
#include <htslib/sam.h>
#include <htslib/vcf.h>

#include "master-ontology.h"

#define URI_ONTOLOGY      "sg://" VERSION "/bam2rdf"

/* These string constants can be used to concatenate strings at compile-time. */
#define STR_PREFIX_BASE                URI_ONTOLOGY "/"
#define STR_PREFIX_BAM_HEADER          URI_ONTOLOGY "/header/"
#define STR_PREFIX_BAM_REFERENCE_SEQ   URI_ONTOLOGY "/refseq/"
#define STR_PREFIX_BAM_READ_GROUP      URI_ONTOLOGY "/readgroup/"
#define STR_PREFIX_BAM_PROGRAM         URI_ONTOLOGY "/program/"
#define STR_PREFIX_BAM_COMMENT         URI_ONTOLOGY "/comment/"
#define STR_PREFIX_BAM_READ            URI_ONTOLOGY "/read/"
#define STR_PREFIX_SEQUENCE            URI_ONTOLOGY "/sequence/"

typedef enum
{
  PREFIX_BASE = 0,
  PREFIX_MASTER,
  PREFIX_BAM_HEADER,
  PREFIX_BAM_REFERENCE_SEQ,
  PREFIX_BAM_READ_GROUP,
  PREFIX_BAM_PROGRAM,
  PREFIX_BAM_COMMENT,
  PREFIX_BAM_READ,
  PREFIX_SEQUENCE,
  PREFIX_ORIGIN,
  PREFIX_RDF,
  PREFIX_OWL,
  PREFIX_XSD,
  PREFIX_UNKNOWN
} ontology_prefix;

typedef enum
{
  CLASS_RDF_TYPE = 0,
  CLASS_ORIGIN,
  CLASS_BAM_HEADER,
  CLASS_BAM_REFERENCE_SEQ,
  CLASS_BAM_READ_GROUP,
  CLASS_BAM_PROGRAM,
  CLASS_BAM_COMMENT,
  CLASS_SAMPLE,
  CLASS_BAM_READ,
  CLASS_UNKNOWN
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

#define XSD_STRING              BCF_HT_STR
#define XSD_INTEGER             BCF_HT_INT
#define XSD_FLOAT               BCF_HT_REAL
#define XSD_BOOLEAN             BCF_HT_FLAG

bool ontology_init (ontology_t **ontology_ptr);
void ontology_free (ontology_t *ontology);

raptor_term* term (int32_t index, char *suffix);

#endif  /* ONTOLOGY_H */
