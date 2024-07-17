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

#include "vcf_variants.h"
#include "runtime_configuration.h"
#include "helper.h"
#include "ui.h"

#include <stdio.h>
#include <htslib/vcf.h>
#include <raptor2.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

extern RuntimeConfiguration config;

void
build_field_identities (bcf_hdr_t *header)
{
  config.field_identities = malloc (header->nhrec * sizeof (field_identity_t));
  if (config.field_identities == NULL)
    {
      ui_print_general_memory_error ();
      return;
    }

  int32_t index = 0, j;
  for (; index < header->nhrec; index++)
    {
      config.field_identities[index].id = NULL;
      config.field_identities[index].type = -1;
      config.field_identities[index].number = -1;

      for (j = 0; j < header->hrec[index]->nkeys; j++)
        {
          char *key   = header->hrec[index]->keys[j];
          char *value = header->hrec[index]->vals[j];
          if (!key || !value) continue;
          if (!strcmp (key, "ID"))
            config.field_identities[index].id = value;
          else if (!strcmp (key, "Number"))
            config.field_identities[index].number =
              (!strcmp (value, ".")) ? -2
              : (!strcmp (value, "A")) ? -3
              : (!strcmp (value, "R")) ? -4
              : (!strcmp (value, "G")) ? -5
              : atoi (value);
          else if (!strcmp (key, "Type"))
            config.field_identities[index].type =
              (!strcmp (value, "Integer")) ? XSD_INTEGER
              : (!strcmp (value, "Float"))   ? XSD_FLOAT
              : (!strcmp (value, "String"))  ? XSD_STRING
              : (!strcmp (value, "Flag"))    ? XSD_BOOLEAN
              : -1;

          if (config.field_identities[index].id != NULL &&
              config.field_identities[index].type != -1 &&
              config.field_identities[index].number != -1)
            break;
        }
    }
}

static void
get_field_identity (int32_t index, char **id, int32_t *type, int32_t *number)
{
  *id     = config.field_identities[index].id;
  *type   = config.field_identities[index].type;
  *number = config.field_identities[index].number;
}

static void
process_variant_for_sample (bcf_hdr_t *header,
                            bcf1_t *buffer,
                            raptor_term *origin,
                            const unsigned char *origin_str,
                            int32_t sample_index,
                            int32_t number_of_samples)
{

  /* Skip variant when not applicable to the sample.
   * --------------------------------------------------------------------
   *
   * In multi-sample VCF files, a variant that occurs in one sample may
   * not occur in another.  Here we filter the variants that are not
   * applicable to the current sample.  We do this by looking at the
   * genotype (GT) format field.  When all genotypes are the same as the
   * reference allele, we drop the variant.
   */

  if (config.process_format_fields
      && number_of_samples > 0)
    {
      char **dst = NULL;
      int32_t ndst = 0;
      int32_t gt = bcf_get_genotypes (header, buffer, &dst, &ndst);
      int32_t ploidy = gt / number_of_samples;
      int32_t *ptr = (int32_t *)dst + sample_index * ploidy;
      int32_t k;
      uint8_t skip = 1;

      for (k = 0; k < ploidy; k++)
        {
          if (bcf_gt_is_missing (ptr[k]))
            continue;

          if (bcf_gt_allele (ptr[k]) != 0)
            {
              skip = 0;
              break;
            }
        }

      free (dst);
      if (skip == 1)
        {
          config.non_unique_variant_counter++;
          return;
        }
    }

  /* Create 'generic' nodes and URIs.
   * -------------------------------------------------------------------- */
  raptor_term *self        = NULL;
  raptor_statement *stmt   = NULL;
  char *variant_id         = NULL;

  if (! generate_variant_id (origin_str, config.variant_id_buf))
    ui_print_general_memory_error ();
  else
    variant_id = config.variant_id_buf;

  self = term (PREFIX_ORIGIN, variant_id);
  if (!self)
    {
      ui_print_redland_error ();
      return;
    }

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = self;
  stmt->predicate = predicate (PREDICATE_ORIGINATED_FROM);
  stmt->object    = origin;
  register_statement_reuse_all (stmt);

  if (number_of_samples > 0)
    {
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = self;
      stmt->predicate = predicate (PREDICATE_SAMPLE);
      stmt->object    = term (PREFIX_ORIGIN, config.sample_ids[sample_index]);
      register_statement_reuse_subject_predicate (stmt);
    }

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = self;
  stmt->predicate = predicate (PREDICATE_RDF_TYPE);
  stmt->object    = class (CLASS_VARIANT_CALL);
  register_statement_reuse_all (stmt);

  /* The original variant ID should be preserved.  Unfortunately, it is
   * not guaranteed to be unique, so we can't use it as an identifier.
   *
   * Furthermore, we noticed that many variant IDs are simply a single dot,
   * so to avoid duplicated entries for dots, we filter those out. */
  if (buffer->d.id[0] != '.')
    {
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = self;
      stmt->predicate = predicate (PREDICATE_VARIANT_ID);
      stmt->object    = literal (buffer->d.id, XSD_STRING);
      register_statement_reuse_subject_predicate (stmt);
    }

  /* Add position information
   * -------------------------------------------------------------------- */

  /* We would usually do 'chromosome = bcf_seqname (header, buffer)', but
   * the function call overhead is worth avoiding.  So we just directly access
   * the field containing the chromosome identifier instead. */
  char *chromosome = (char *)header->id[BCF_DT_CTG][buffer->rid].key;

  /* HTSlib uses 0-based positions, while in the VCF 1-based position are used.
   * Therefore we need to add one to the position here. */
  uint32_t position = buffer->pos + 1;

  /* Add the standard fields.
   * Default fields: ID, CHROM, POS, REF, ALT, QUAL, FILTER, INFO, FORMAT.
   * -------------------------------------------------------------------- */
  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = self;
  stmt->predicate = predicate (PREDICATE_CHROMOSOME);

  if (config.reference != NULL &&
      config.reference[config.reference_len - 1] == '#')
    {
      size_t chromosome_len = strlen (chromosome);
      char chr_buffer[chromosome_len + 2];
      snprintf (chr_buffer, chromosome_len + 2, "#%s", chromosome);
      stmt->object = term (PREFIX_REFERENCE, chr_buffer);
    }
  else
    stmt->object = term (PREFIX_REFERENCE, chromosome);

  register_statement_reuse_subject_predicate (stmt);
  snprintf (config.number_buffer, 32, "%u", position);

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = self;
  stmt->predicate = predicate (PREDICATE_POSITION);
  stmt->object    = literal (config.number_buffer, XSD_INTEGER);
  register_statement_reuse_subject_predicate (stmt);

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = self;
  stmt->predicate = predicate (PREDICATE_REF);
  stmt->object    = term (PREFIX_SEQUENCE, buffer->d.allele[0]);
  register_statement_reuse_subject_predicate (stmt);

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = self;
  stmt->predicate = predicate (PREDICATE_ALT);
  stmt->object    = term (PREFIX_SEQUENCE, buffer->d.allele[1]);
  register_statement_reuse_subject_predicate (stmt);

  /* The QUAL indicator "." means that the QUAL value is missing or unknown.
   * In such a case we skip the entire triplet.  This behavior needs to be
   * documented as such, so that users don't forget to treat it as an optional
   * field. */
  if (isfinite (buffer->qual))
    {
      snprintf (config.number_buffer, 32, "%4.6f", buffer->qual);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = self;
      stmt->predicate = predicate (PREDICATE_QUAL);
      stmt->object    = literal (config.number_buffer, XSD_FLOAT);

      register_statement_reuse_subject_predicate (stmt);
    }

  /* Process filter fields.
   * -------------------------------------------------------------------- */
  bcf_unpack (buffer, BCF_UN_FLT);
  int filter_index = 0;
  for (; filter_index < buffer->d.n_flt; filter_index++)
    {
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = self;
      stmt->predicate = predicate (PREDICATE_FILTER);
      stmt->object    = term (PREFIX_BASE,
                              (char *)(header->id[BCF_DT_ID][buffer->d.flt[filter_index]].key));
      register_statement_reuse_subject_predicate (stmt);
    }

  char *id_str           = NULL;
  void *value            = NULL;
  int32_t state          = 0;
  int32_t type;
  int32_t value_len;
  int32_t index;
  int32_t number;
  uint32_t i;

  /* Process INFO fields.
   * -------------------------------------------------------------------- */
  if (config.process_info_fields)
    {
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = self;

      for (i = 0; i < config.info_field_indexes_len; i++)
        {
          id_str    = NULL;
          type      = -1;
          value     = NULL;
          value_len = 0;
          index     = config.info_field_indexes[i];
          number    = -1;

          get_field_identity (index, &id_str, &type, &number);

          if (!id_str || type == -1)
            goto clean_up_iteration;

          state = bcf_get_info_values (header, buffer, id_str, &value, &value_len, type);
          if (!value || state < 0)
            goto clean_up_iteration;

          /* Each value can be a list of values.  Therefore, we must take the 'number'
           * of items into account. In the code below, 'k' is used as list index.
           */
          if (number < 0 && number_of_samples > 0)
            number = state / number_of_samples;

          if (type == XSD_INTEGER || type == XSD_FLOAT)
            {
              int32_t k;
              for (k = 0; k < number; k++)
                {
                  if (type == XSD_INTEGER)
                    {
                      snprintf (config.number_buffer, 32, "%d", (((int32_t *)value)[k]));
                      stmt->object = literal (config.number_buffer, XSD_INTEGER);
                    }
                  else if (type == XSD_FLOAT)
                    {
                      snprintf (config.number_buffer, 32, "%f", (((float *)value)[k]));
                      stmt->object = literal (config.number_buffer, XSD_FLOAT);
                    }

                  if (number == 1)
                    stmt->predicate = term (PREFIX_VCF_HEADER_INFO, id_str);
                  else
                    {
                      snprintf (config.number_buffer, 32, "%s%d", id_str, (k + 1));
                      stmt->predicate = term (PREFIX_VCF_HEADER_INFO, config.number_buffer);
                    }

                  register_statement_reuse_subject (stmt);
                  stmt = raptor_new_statement (config.raptor_world);
                  stmt->subject   = self;
                }
            }
          else
            {
              stmt->predicate = term (PREFIX_VCF_HEADER_INFO, id_str);
              if (type == XSD_STRING)
                stmt->object = literal (((char *)value), XSD_STRING);
              else if (type == XSD_BOOLEAN && state == 1)
                stmt->object = literal ("true", XSD_BOOLEAN);

              register_statement_reuse_subject (stmt);
              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = self;
            }

        clean_up_iteration:
          free (value);
          value = NULL;
        }

      stmt->subject = NULL;
      raptor_free_statement (stmt);
    }

  /* Process FORMAT fields.
   * -------------------------------------------------------------------- */
  if (config.process_format_fields && number_of_samples > 0)
    {
      for (i = 0; i < config.format_field_indexes_len; i++)
        {
          id_str    = NULL;
          type      = -1;
          value     = NULL;
          value_len = 0;
          index     = config.format_field_indexes[i];
          number    = -1;

          get_field_identity (index, &id_str, &type, &number);

          if (!id_str || type == -1)
            continue;

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = self;

          if (!strcmp (id_str, "GT"))
            {
              char **dst = NULL;
              int32_t ndst = 0;
              int32_t gt = bcf_get_genotypes (header, buffer, &dst, &ndst);
              int32_t ploidy = gt / number_of_samples;
              int32_t *ptr = (int32_t *)dst + sample_index * ploidy;
              int32_t genotypes[ploidy];

              int32_t k;
              for (k = 0; k < ploidy; k++)
                {
                  genotypes[k] = (bcf_gt_is_missing (ptr[k]))
                    ? -1
                    : bcf_gt_allele (ptr[k]);

                  snprintf (config.number_buffer, 32, "allele_%d", (k + 1));
                  stmt->predicate = term (PREFIX_VCF_HEADER_FORMAT_GT,
                                          config.number_buffer);

                  snprintf (config.number_buffer, 32, "%d", genotypes[k]);
                  stmt->object    = literal (config.number_buffer, XSD_INTEGER);
                  register_statement_reuse_subject (stmt);

                  stmt = raptor_new_statement (config.raptor_world);
                  stmt->subject   = self;
                }

              stmt->predicate = term (PREFIX_VCF_HEADER_FORMAT, id_str);
              int32_t genotype_class = -1;

              if (ploidy == 2)
                genotype_class = (genotypes[0] == 0 && genotypes[1] == 0)
                  ? CLASS_HOMOZYGOUS_REFERENCE
                  : (genotypes[0] == 1 && genotypes[1] == 1)
                  ? CLASS_HOMOZYGOUS_ALTERNATIVE
                  : CLASS_HETEROZYGOUS;

              else if (ploidy == 1)
                genotype_class = CLASS_HOMOZYGOUS;
              else if (ploidy > 2)
                genotype_class = CLASS_MULTIZYGOUS;
              else if (ploidy == 0)
                genotype_class = CLASS_NULLIZYGOUS;

              stmt->object    = config.ontology->classes[genotype_class];
              register_statement_reuse_subject_object (stmt);

              /* Also output the “raw” genotype information. */
              snprintf (config.number_buffer, 32, "%d", ploidy);
              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = self;
              stmt->predicate = predicate (PREDICATE_PLOIDY);
              stmt->object    = literal (config.number_buffer, XSD_INTEGER);
              register_statement_reuse_subject_predicate (stmt);

              free (dst);
            }
          else
            {
              state = bcf_get_format_values (header, buffer, id_str, &value, &value_len, type);
              if (!value || state < 0)
                goto clean_format_iteration;

              /* Each value can be a list of values.  Therefore, we must take the 'number'
               * of items into account. In the code below, 'k' is used as list index.
               *
               * Furthermore, for an unspecified number of values, we can get actual
               * number by dividing the 'number_of_samples' by the 'state'
               * (number of values).
               */
              if (number < 0)
                number = state / number_of_samples;

              int32_t value_offset = sample_index * number;
              if (type == XSD_INTEGER || type == XSD_FLOAT)
                {
                  int32_t k;
                  for (k = 0; k < number; k++)
                    {
                      if (number == 1)
                        stmt->predicate = term (PREFIX_VCF_HEADER_FORMAT, id_str);
                      else
                        {
                          snprintf (config.number_buffer, 32, "%s%d", id_str, (k + 1));
                          stmt->predicate = term (PREFIX_VCF_HEADER_FORMAT, config.number_buffer);
                        }

                      if (type == XSD_INTEGER)
                        {
                          snprintf (config.number_buffer, 32, "%d", (((int32_t *)value)[value_offset + k]));
                          stmt->object = literal (config.number_buffer, XSD_INTEGER);
                        }
                      else if (type == XSD_FLOAT)
                        {
                          snprintf (config.number_buffer, 32, "%f", (((float *)value)[value_offset + k]));
                          stmt->object = literal (config.number_buffer, XSD_FLOAT);
                        }

                      register_statement_reuse_subject (stmt);
                      stmt = raptor_new_statement (config.raptor_world);
                      stmt->subject   = self;
                    }
                }
              else
                {
                  stmt->predicate = term (PREFIX_VCF_HEADER_FORMAT, id_str);

                  if (type == XSD_STRING)
                    {
                      char *content = calloc (sizeof (char), (number + 1));
                      if (content == NULL)
                        goto clean_format_iteration;

                      strncpy (content, (char *)value + value_offset, number);
                      content[number] = '\0';

                      stmt->object = literal (content, XSD_STRING);
                      free (content);
                    }
                  else if (type == XSD_BOOLEAN && state == 1)
                    stmt->object = literal ("true", XSD_BOOLEAN);

                  register_statement_reuse_subject (stmt);
                  stmt = raptor_new_statement (config.raptor_world);
                  stmt->subject   = self;
                }

            clean_format_iteration:
              stmt->subject = NULL;
              raptor_free_statement (stmt);
              free (value);
              value = NULL;
            }
        }
    }

  raptor_free_term (self);
}

void
process_variant (bcf_hdr_t *header, bcf1_t *buffer, raptor_term *origin,
                 const unsigned char *origin_str)
{
  if (!header || !buffer || !origin || !origin_str) return;
  int32_t number_of_samples = bcf_hdr_nsamples (header);

  /* Handle the program options for leaving out FILTER fields.
   * ------------------------------------------------------------------------ */
  if ((config.filter && bcf_has_filter (header, buffer, config.filter) == 1)
      || (config.keep && bcf_has_filter (header, buffer, config.keep) != 1))
    {
      /* Up the variant ID because we might want to add this variant
       * at a later time.  When processing the same file, it will keep the
       * variant IDs in sync. */
      config.non_unique_variant_counter +=
        (number_of_samples > 0) ? number_of_samples : 1;
      return;
    }

  /* Unpack up and including the ALT field.
   * ------------------------------------------------------------------------ */
  bcf_unpack (buffer, BCF_UN_STR);

  /* If the allele information is still missing after unpacking the buffer,
   * we will end up without REF information.  Skip these records. */
  if (buffer->d.allele == NULL)
    return;

  /* Some reference datasets like dbSNP don't define samples.
   * Accomodating for this use-case is a bit special, so let's deal with
   * it here. */
  if (number_of_samples == 0 && !(config.sample))
    process_variant_for_sample (header, buffer, origin, origin_str, -1,
                                number_of_samples);

  /* When samples are defined (as usual), we should treat each variant call
   * for a given sample as a unique call.  This makes sure multi-sample VCFs
   * are handled correctly automatically. */
  int32_t sample_index = 0;
  for (; sample_index < number_of_samples; sample_index++)
    {
      /* Handle the --sample command-line option. */
      if (config.sample && strcmp(header->samples[sample_index], config.sample))
        continue;

      process_variant_for_sample (header, buffer, origin, origin_str,
                                  sample_index, number_of_samples);
    }
}
