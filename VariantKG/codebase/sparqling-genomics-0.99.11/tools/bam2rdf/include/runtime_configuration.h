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
  char              *reference;
  char              *mapper;
  char              *output_format;
  uint32_t          non_unique_read_counter;
  uint32_t          header_counter;
  bool              header_only;
  bool              metadata_only;
  bool              show_progress_info;

  /* Raptor-specifics */
  raptor_world      *raptor_world;
  raptor_serializer *raptor_serializer;

  /* Application-specific ontology. */
  ontology_t        *ontology;

  /* Shared buffers. */
  char read_id_buf[HASH_ALGORITHM_PRINT_LENGTH + 16];
  char header_id_buf[HASH_ALGORITHM_PRINT_LENGTH + 16];
  char number_buffer[32];
} RuntimeConfiguration;

bool runtime_configuration_init (void);
bool runtime_configuration_redland_init (void);
void runtime_configuration_free (void);
void runtime_configuration_redland_free (void);

bool generate_read_id (const unsigned char *origin, char *read_id);
bool generate_header_id (const unsigned char *origin, char *header_id);

#endif  /* RUNTIMECONFIGURATION_H */
