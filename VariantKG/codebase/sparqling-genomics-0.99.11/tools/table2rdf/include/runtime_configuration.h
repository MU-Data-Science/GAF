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

/* This struct can be used to make program options available throughout the
 * entire code without needing to pass them around as parameters.  Do not write
 * to these values, other than in the runtime_configuration_init() and
 * ui_process_command_line() functions. */
typedef struct
{
  /* Command-line configurable options. */
  char              *input_file;
  char              *caller;
  char              *output_format;
  char              *delimiter;
  char              *secondary_delimiter;
  char              *header_line;
  char              *ignore_lines_with;
  char              **predicate_transformers_buffer;
  char              **predicate_transformer_keys;
  char              **predicate_transformer_values;
  char              **object_transformers_buffer;
  char              **object_transformer_keys;
  char              **object_transformer_values;
  uint32_t          predicate_transformers_buffer_len;
  uint32_t          predicate_transformers_buffer_alloc_len;
  uint32_t          predicate_transformer_alloc_len;
  uint32_t          predicate_transformer_len;
  uint32_t          object_transformers_buffer_len;
  uint32_t          object_transformers_buffer_alloc_len;
  uint32_t          object_transformer_alloc_len;
  uint32_t          object_transformer_len;
  bool              show_progress_info;
  bool              skip_first_line;
  bool              input_from_stdin;

  /* Raptor-specifics */
  raptor_world      *raptor_world;
  raptor_serializer *raptor_serializer;
  raptor_uri        **prefix;

  /* Application-specific ontology. */
  ontology_t        *ontology;

  /* Shared buffers. */
  uint32_t          column_counter;
  uint32_t          row_counter;
  uint32_t          prefix_name_counter;
  char              id_buf[HASH_ALGORITHM_PRINT_LENGTH + 16];
  char              number_buffer[32];
} RuntimeConfiguration;

bool runtime_configuration_init (void);
bool runtime_configuration_redland_init (void);
void runtime_configuration_free (void);
void runtime_configuration_redland_free (void);

bool generate_column_id (const unsigned char *origin, char *column_id);
bool generate_row_id (const unsigned char *origin, char *row_id);
bool generate_prefix_name (unsigned char *prefix_name);
bool preregister_predicate_transformer (const char *pair);
bool preregister_object_transformer (const char *pair);
bool register_predicate_transformers ();
bool register_object_transformers ();

#endif  /* RUNTIMECONFIGURATION_H */
