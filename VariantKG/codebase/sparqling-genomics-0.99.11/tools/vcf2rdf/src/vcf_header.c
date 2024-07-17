/*
 * Copyright (C) 2018  Roel Janssen <roel@gnu.org>
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

#include "vcf_header.h"
#include "runtime_configuration.h"
#include "helper.h"
#include "ui.h"

#include <stdio.h>
#include <raptor2.h>

extern RuntimeConfiguration config;

void
process_header_item (bcf_hdr_t *vcf_header,
                     char      *identifier,
                     int32_t   prefix,
                     int32_t   index)
{
  int32_t j = 0;
  for (; j < vcf_header->hrec[index]->nkeys; j++)
    {
      char *key   = vcf_header->hrec[index]->keys[j];
      char *value = vcf_header->hrec[index]->vals[j];

      if (!key || !value ||
          /* It seems that htslib adds an “IDX” key/value pair at the end
           * to keep track of the order of the header items.  We don't
           * want that property to leak into the RDF. */
          !strcmp (key, "IDX"))
        continue;

      if (strcmp (key, "ID"))
        {
          raptor_statement *stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (prefix, (char *)identifier);
          stmt->predicate = term (prefix, key);
          stmt->object    = literal (value, XSD_STRING);

          register_statement (stmt);
          stmt = NULL;
        }
    }
}

void
process_header (bcf_hdr_t *vcf_header, raptor_term *origin,
                const unsigned char *origin_str)
{
  if (!vcf_header || !origin || !origin_str) return;

  /* GENERAL NODES
   * --------------------------------------------------------------------------
   * The following nodes are used multiple times in the processing loop.  Once
   * a node is added to the model (via a statement), its memory is managed by
   * the model.  This means that we can't add the node to more than one
   * statement.  Instead we have to clone the node first, so that they live in
   * two disjoint memory locations.
   *
   * If we are looking for performance enhancements, we could change this in
   * librdf.
   */

  raptor_statement *stmt = NULL;
  int32_t prefix         = -1;
  int32_t type           = -1;

  /* Register samples.
   * ----------------------------------------------------------------------- */
  int32_t number_of_samples = bcf_hdr_nsamples (vcf_header);
  int32_t index             = 0;

  for (; index < number_of_samples; index++)
    {
      /* Handle the --sample command-line option. */
      if (config.sample && strcmp(vcf_header->samples[index], config.sample))
        continue;

      /* Resize block when needed. */
      if ((config.sample_ids_blocks * INDEX_BLOCK_SIZE) <= config.sample_ids_len)
        {
          config.sample_ids_blocks++;
          config.sample_ids = realloc (config.sample_ids,
                                       config.sample_ids_blocks *
                                       INDEX_BLOCK_SIZE *
                                       sizeof (*(config.sample_ids)));
        }

      /* Store the index in the cache. */
      if (! generate_sample_id (origin_str, index,
                                config.sample_ids[index]))
        {
          ui_print_general_memory_error ();
          return;
        }
      else
        config.sample_ids_len++;

      raptor_term *subject = term (PREFIX_ORIGIN, config.sample_ids[index]);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = subject;
      stmt->predicate = predicate (PREDICATE_RDF_TYPE);
      stmt->object    = class (CLASS_SAMPLE);
      register_statement_reuse_all (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = subject;
      stmt->predicate = predicate (PREDICATE_FOUND_IN);
      stmt->object    = origin;
      register_statement_reuse_all (stmt);
      stmt = NULL;

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = subject;
      stmt->predicate = predicate (PREDICATE_RDFS_LABEL);
      stmt->object    = literal (vcf_header->samples[index], XSD_STRING);
      register_statement_reuse_predicate (stmt);
    }

  /* Skip the rest of the triplets when metadata-only mode is enabled. */
  if (config.metadata_only)
    return;

  /* Process header fields.
   * ----------------------------------------------------------------------- */

  char *identifier    = NULL;
  char *key           = NULL;
  char *value         = NULL;
  for (index = 0; index < vcf_header->nhrec; index++)
    {
      if (vcf_header->hrec[index]->nkeys > 0)
        {
          key   = vcf_header->hrec[index]->keys[0];
          value = vcf_header->hrec[index]->vals[0];
          if (key && value)
            identifier = (!strcmp (key, "ID")) ? value : key;
        }
      else
        identifier = vcf_header->hrec[index]->key;

      if (!identifier)
        continue;

      /* Handle simple key-value fields.
       * ------------------------------------------------------------------- */
      if (vcf_header->hrec[index]->value)
        {
          raptor_term* subject = term (PREFIX_VCF_HEADER, identifier);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = subject;
          stmt->predicate = predicate (PREDICATE_RDF_TYPE);
          stmt->object    = class (CLASS_VCF_HEADER);
          register_statement_reuse_all (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = subject;
          stmt->predicate = term (PREFIX_VCF_HEADER, vcf_header->hrec[index]->key);
          stmt->object    = literal (vcf_header->hrec[index]->value, XSD_STRING);
          register_statement_reuse_subject (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = subject;
          stmt->predicate = predicate (PREDICATE_ORIGINATED_FROM);
          stmt->object    = origin;
          register_statement_reuse_predicate_object (stmt);
        }

      /* Handle other fields.
       * ------------------------------------------------------------------- */
      else if (vcf_header->hrec[index]->type == BCF_HL_INFO)
        {
          if (!config.process_info_fields)
            continue;

          prefix = PREFIX_VCF_HEADER_INFO;
          type  = CLASS_VCF_HEADER_INFO;

          /* Cache the indexes of INFO fields.
           * ---------------------------------------------------------------- */

          /* Resize block when needed. */
          if ((config.info_field_indexes_blocks * INDEX_BLOCK_SIZE) <=
              config.info_field_indexes_len)
            {
              config.info_field_indexes_blocks++;
              config.info_field_indexes = realloc (config.info_field_indexes,
                                                   config.info_field_indexes_blocks *
                                                   INDEX_BLOCK_SIZE * sizeof (int *));
            }

          /* Store the index in the cache. */
          config.info_field_indexes[config.info_field_indexes_len] = index;
          config.info_field_indexes_len++;
        }

      else if (vcf_header->hrec[index]->type == BCF_HL_FLT)
        {
          prefix = PREFIX_VCF_HEADER_FILTER;
          type  = CLASS_VCF_HEADER_FILTER;
        }
      else if (vcf_header->hrec[index]->type == BCF_HL_FMT)
        {
          if (!config.process_format_fields)
            continue;

          prefix = PREFIX_VCF_HEADER_FORMAT;
          type  = CLASS_VCF_HEADER_FORMAT;

          /* Cache the indexes of FORMAT fields.
           * ---------------------------------------------------------------- */

          /* Resize block when needed. */
          if ((config.format_field_indexes_blocks * INDEX_BLOCK_SIZE) <=
              config.format_field_indexes_len)
            {
              config.format_field_indexes_blocks++;
              config.format_field_indexes = realloc (config.format_field_indexes,
                                                     config.format_field_indexes_blocks
                                                     * INDEX_BLOCK_SIZE * sizeof (int *));
            }

          /* Store the index in the cache. */
          config.format_field_indexes[config.format_field_indexes_len] = index;
          config.format_field_indexes_len++;
        }
      else if (vcf_header->hrec[index]->type == BCF_HL_CTG)
        {
          prefix = PREFIX_VCF_HEADER_CONTIG;
          type  = CLASS_VCF_HEADER_CONTIG;
        }
      else if (!strcmp (vcf_header->hrec[index]->key, "ALT"))
        {
          prefix = PREFIX_VCF_HEADER_ALT;
          type  = CLASS_VCF_HEADER_ALT;
        }
      else
        {
          prefix = PREFIX_VCF_HEADER;
          type  = CLASS_VCF_HEADER;
        }

      if (prefix >= 0 && type >= 0)
        {
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (prefix, identifier);
          stmt->predicate = predicate (PREDICATE_RDF_TYPE);
          stmt->object    = config.ontology->classes[type];
          register_statement_reuse_predicate_object (stmt);

          process_header_item (vcf_header, identifier, prefix, index);
        }

      if (prefix >= 0)
        {
          /* Add a reference to the 'origin'. */
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (prefix, identifier);
          stmt->predicate = predicate (PREDICATE_ORIGINATED_FROM);
          stmt->object    = origin;
          register_statement_reuse_predicate_object (stmt);
        }

      prefix = -1;
      type   = -1;
    }
}
