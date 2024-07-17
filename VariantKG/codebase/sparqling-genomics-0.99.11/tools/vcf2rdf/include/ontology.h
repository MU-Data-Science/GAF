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
#include <htslib/vcf.h>

#include "master-ontology.h"

/* These string constants can be used to concatenate strings at compile-time. */
#define URI_ONTOLOGY                    "sg://" VERSION "/vcf2rdf"
#define STR_PREFIX_BASE                 URI_ONTOLOGY "/"
#define STR_PREFIX_VCF_HEADER           URI_ONTOLOGY "/header/"
#define STR_PREFIX_VCF_HEADER_INFO      URI_ONTOLOGY "/info/"
#define STR_PREFIX_VCF_HEADER_FORMAT    URI_ONTOLOGY "/format/"
#define STR_PREFIX_VCF_HEADER_FORMAT_GT URI_ONTOLOGY "/format/GT/"
#define STR_PREFIX_VCF_HEADER_FILTER    URI_ONTOLOGY "/filter/"
#define STR_PREFIX_VCF_HEADER_ALT       URI_ONTOLOGY "/alt/"
#define STR_PREFIX_VCF_HEADER_CONTIG    URI_ONTOLOGY "/contig/"
#define STR_PREFIX_VARIANT_CALL         URI_ONTOLOGY "/variant/"
#define STR_PREFIX_SEQUENCE             URI_ONTOLOGY "/sequence/"
#define STR_PREFIX_FALDO                "http://biohackathon.org/resource/faldo#"
#define STR_PREFIX_REFERENCE            "http://www.ncbi.nlm.nih.gov/nuccore/"

typedef enum
{
  PREFIX_BASE = 0,
  PREFIX_MASTER,
  PREFIX_VCF_HEADER,
  PREFIX_VCF_HEADER_INFO,
  PREFIX_VCF_HEADER_FORMAT,
  PREFIX_VCF_HEADER_FORMAT_GT,
  PREFIX_VCF_HEADER_FILTER,
  PREFIX_VCF_HEADER_ALT,
  PREFIX_VCF_HEADER_CONTIG,
  PREFIX_VARIANT_CALL,
  PREFIX_SEQUENCE,
  PREFIX_ORIGIN,
  PREFIX_RDF,
  PREFIX_RDFS,
  PREFIX_OWL,
  PREFIX_XSD,
  PREFIX_FALDO,
  PREFIX_REFERENCE
} ontology_prefix;

typedef enum
{
  CLASS_ORIGIN = 0,
  CLASS_VCF_HEADER,
  CLASS_VCF_HEADER_INFO,
  CLASS_VCF_HEADER_FORMAT,
  CLASS_VCF_HEADER_FILTER,
  CLASS_VCF_HEADER_ALT,
  CLASS_VCF_HEADER_CONTIG,
  CLASS_SAMPLE,
  CLASS_VARIANT_CALL,
  CLASS_HETEROZYGOUS,
  CLASS_MULTIZYGOUS,
  CLASS_NULLIZYGOUS,
  CLASS_HOMOZYGOUS,
  CLASS_HOMOZYGOUS_REFERENCE,
  CLASS_HOMOZYGOUS_ALTERNATIVE
} ontology_class;

typedef enum
{
  PREDICATE_RDF_TYPE = 0,
  PREDICATE_RDFS_LABEL,
  PREDICATE_CONVERTED_BY,
  PREDICATE_VERSION_INFO,
  PREDICATE_FILENAME,
  PREDICATE_ORIGINATED_FROM,
  PREDICATE_SAMPLE,
  PREDICATE_VARIANT_ID,
  PREDICATE_CHROMOSOME,
  PREDICATE_POSITION,
  PREDICATE_REF,
  PREDICATE_ALT,
  PREDICATE_QUAL,
  PREDICATE_FILTER,
  PREDICATE_PLOIDY,
  PREDICATE_FOUND_IN
} ontology_predicate;

typedef struct
{
  raptor_term **classes;
  raptor_term **predicates;
  raptor_uri  **prefixes;
  raptor_uri **xsds;
  int32_t     classes_length;
  int32_t     predicates_length;
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
