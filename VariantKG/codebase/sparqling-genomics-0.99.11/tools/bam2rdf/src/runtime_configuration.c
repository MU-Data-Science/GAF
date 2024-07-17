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

#include "runtime_configuration.h"
#include "ui.h"
#include "helper.h"
#include "ontology.h"
#include <stdio.h>
#include <stdlib.h>

/* This is where we can set default values for the program's options. */
RuntimeConfiguration config;

bool
runtime_configuration_init (void)
{
  config.input_file = NULL;
  config.reference = NULL;
  config.mapper = NULL;
  config.output_format = NULL;
  config.non_unique_read_counter = 0;
  config.header_counter = 0;
  config.header_only = false;
  config.metadata_only = false;
  config.show_progress_info = false;

  return true;
}

bool
runtime_configuration_redland_init (void)
{
  if (!config.output_format)
    config.output_format = "ntriples";

  config.raptor_world      = raptor_new_world();
  config.raptor_serializer = raptor_new_serializer (config.raptor_world,
                                                    config.output_format);

  if (!config.raptor_world || !config.raptor_serializer)
    return (ui_print_redland_error () == 0);

  raptor_serializer_start_to_file_handle (config.raptor_serializer, NULL, stdout);
  if (!ontology_init (&(config.ontology)))
    return (ui_print_redland_error () == 0);

  return true;
}

void
runtime_configuration_redland_free (void)
{
  /* Free the Redland-allocated memory. */
  ontology_free (config.ontology);
}

void
runtime_configuration_free (void)
{
  runtime_configuration_redland_free ();

  raptor_serializer_serialize_end (config.raptor_serializer);
  raptor_free_serializer (config.raptor_serializer);
  raptor_free_world (config.raptor_world);
}

bool
generate_read_id (const unsigned char *origin, char *read_id)
{
  int32_t bytes_written;
  bytes_written = snprintf (read_id,
                            HASH_ALGORITHM_PRINT_LENGTH + 16,
                            "%s@R%u",
                            origin,
                            config.non_unique_read_counter);

  config.non_unique_read_counter++;
  return (bytes_written > 0);
}

bool
generate_header_id (const unsigned char *origin, char *header_id)
{
  int32_t bytes_written;
  bytes_written = snprintf (header_id,
                            HASH_ALGORITHM_PRINT_LENGTH + 16,
                            "%s@H%u",
                            origin,
                            config.header_counter);

  config.header_counter++;
  return (bytes_written > 0);
}
