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

#ifndef RUNTIMECONFIGURATION_H
#define RUNTIMECONFIGURATION_H

/*
 * This object provides the basic infrastructure to make the rest of the
 * program more efficient or more convenient to write.
 */

#include "ontology.h"
#include "helper.h"
#include <stdbool.h>
#include <stdint.h>
#include <raptor2.h>

/* To reduce the number of small allocations, we use a bulk-allocate mechanism
 * for resources that remain alive during most of program's execution.  This
 * variable controls the size of the items to allocate in bulk. */
#define INDEX_BLOCK_SIZE 32

typedef struct
{
  char *id;
  int32_t type;
  int32_t number;
} field_identity_t;

/* This struct can be used to make program options available throughout the
 * entire code without needing to pass them around as parameters.  Do not write
 * to these values, other than in the runtime_configuration_init() and
 * ui_process_command_line() functions. */
typedef struct
{
  /* Command-line configurable options. */
  char              *filter;
  char              *keep;
  char              *input_file;
  char              *reference;
  char              *caller;
  char              *output_format;
  char              *sample;
  char              (*sample_ids)[HASH_ALGORITHM_PRINT_LENGTH + 16];
  char              *user_hash;
  uint32_t          non_unique_variant_counter;
  int32_t           reference_len;
  bool              header_only;
  bool              metadata_only;
  bool              show_progress_info;
  bool              process_info_fields;
  bool              process_format_fields;
  bool              input_from_stdin;

  /* Raptor-specifics */
  raptor_world      *raptor_world;
  raptor_serializer *raptor_serializer;

  /* Application-specific ontology. */
  ontology_t        *ontology;

  /* Caching and internal performance optimizing structures. */
  int32_t           *info_field_indexes;
  size_t            info_field_indexes_len;
  size_t            info_field_indexes_blocks;
  int32_t           *format_field_indexes;
  size_t            format_field_indexes_len;
  size_t            format_field_indexes_blocks;
  size_t            sample_ids_len;
  size_t            sample_ids_blocks;
  field_identity_t  *field_identities;

  /* Shared buffers. */
  char variant_id_buf[HASH_ALGORITHM_PRINT_LENGTH + 16];
  char number_buffer[32];
} RuntimeConfiguration;

bool runtime_configuration_init (void);
bool runtime_configuration_redland_init (void);
void runtime_configuration_free (void);
void runtime_configuration_redland_free (void);

bool generate_variant_id (const unsigned char *origin, char *variant_id);
bool generate_sample_id (const unsigned char *origin, int32_t sample_index,
                         char *sample_id);


#endif  /* RUNTIMECONFIGURATION_H */
