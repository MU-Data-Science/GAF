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

#include "ui.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <getopt.h>

#include "runtime_configuration.h"

extern RuntimeConfiguration config;

void
ui_show_help (void)
{
  puts ("\nAvailable options:\n"
	"  --header-only,           -o  Only process the BAM header.\n"
	"  --metadata-only          -m  Output only metadata.  This mode "
                                       "can be used to find samples.\n"
	"  --help,                  -h  Show this message.\n"
	"  --progress-info,         -p  Show progress information.\n"
	"  --version,               -v  Show versioning information.\n"
        "  --mapper=ARG,            -c  The mapper used to produce the BAM "
                                       "file.\n"
        "  --input-file=ARG,        -i  The input file to process.\n"
        "  --output-format          -O  The output format to serialize to.\n"
        "  --reference=ARG,         -r  The reference genome the reads "
                                       "are mapped to.  GRCh37 is "
                                       "assumed.\n");
}

void
ui_show_version (void)
{
  /* The VERSION variable is defined by the build system. */
  puts ("Version: " VERSION "\n");
}

void
ui_process_command_line (int argc, char **argv)
{
  int arg = 0;
  int index = 0;

  /* Program options
   * ------------------------------------------------------------------- */
  static struct option options[] =
    {
      { "header-only",           no_argument,       0, 'o' },
      { "input-file",            required_argument, 0, 'i' },
      { "mapper",                required_argument, 0, 'M' },
      { "metadata-only",         no_argument,       0, 'm' },
      { "output-format",         required_argument, 0, 'O' },
      { "progress-info",         no_argument,       0, 'p' },
      { "reference",             required_argument, 0, 'r' },
      { "help",                  no_argument,       0, 'h' },
      { "version",               no_argument,       0, 'v' },
      { 0,                       0,                 0, 0   }
    };

  while ( arg != -1 )
    {
      /* Make sure to list all short options in the string below. */
      arg = getopt_long (argc, argv, "i:M:r:O:omphv", options, &index);
      switch (arg)
        {
        case 'i': config.input_file = optarg;                    break;
        case 'M': config.mapper = optarg;                        break;
        case 'r': config.reference = optarg;                     break;
        case 'O': config.output_format = optarg;                 break;
        case 'o': config.header_only = true;                     break;
        case 'm': config.metadata_only = true;                   break;
        case 'p': config.show_progress_info = true;              break;
        case 'h': ui_show_help ();                               break;
        case 'v': ui_show_version ();                            break;
        }

      /* When a required argument is missing, quit the program.
       * An error message will be displayed by getopt. */
      if (arg == '?') exit (1);
    }
}

int32_t
ui_print_bam_file_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot open '%s'.\n", file_name);
  return 1;
}

int32_t
ui_print_bam_header_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot read header of '%s'.\n", file_name);
  return 1;
}

void
ui_print_bam_unknown_header (const char *header_item)
{
  fprintf (stderr, "WARNING: Skipped header '%s'.\n", header_item);
}

int32_t
ui_print_memory_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Not enough memory available for processing '%s'.\n", file_name);
  return 1;
}

int32_t
ui_print_general_memory_error (void)
{
  fputs ("ERROR: Not enough memory available.\n", stderr);
  return 1;
}

int32_t
ui_print_redland_error (void)
{
  fputs ("ERROR: Couldn't initialize Redland.\n", stderr);
  return 1;
}

int32_t
ui_print_query_error (const char *query)
{
  fprintf (stderr, "ERROR: Could not execute query:\n%s\n", query);
  return 1;
}

int32_t
ui_print_file_format_error (void)
{
  fprintf (stderr, "ERROR: This program only handles \".sam\", "
                   "\".bam\", and \".cram\" files.\n");
  return 1;
}

void
ui_show_missing_options_warning (void)
{
  if (config.input_file)
    {
      if (!config.reference)
        fputs ("Warning: No --reference has been specified.  "
               "This may lead to incomplete and/or ambiguous information "
               "in the database.\n", stderr);

      if (!config.mapper)
        fputs ("Warning: No --mapper has been specified.  "
               "This may lead to incomplete and/or ambiguous information "
               "in the database.\n", stderr);
    }
}
