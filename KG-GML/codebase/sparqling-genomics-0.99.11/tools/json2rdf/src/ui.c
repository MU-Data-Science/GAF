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
        "  --hash=ARG,              -H  Use ARG as file identification hash.\n"
	"  --help,                  -h  Show this message.\n"
	"  --version,               -v  Show versioning information.\n"
        "  --input-file=ARG,        -i  The input file to process.\n"
        "  --stdin                  -I  Read input from a pipe instead of a "
                                       "file.\n");
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
      { "input-file",            required_argument, 0, 'i' },
      { "stdin",                 no_argument,       0, 'I' },
      { "output-format",         required_argument, 0, 'O' },
      { "hash",                  required_argument, 0, 'H' },
      { "help",                  no_argument,       0, 'h' },
      { "version",               no_argument,       0, 'v' },
      { 0,                       0,                 0, 0   }
    };

  while ( arg != -1 )
    {
      /* Make sure to list all short options in the string below. */
      arg = getopt_long (argc, argv, "i:O:H:Ihv", options, &index);
      switch (arg)
        {
        case 'i': config.input_file = optarg;                    break;
        case 'I': config.input_from_stdin = true;                break;
        case 'O': config.output_format = optarg;                 break;
        case 'H': config.user_hash = optarg;                     break;
        case 'h': ui_show_help ();                               break;
        case 'v': ui_show_version ();                            break;
        }

      /* When a required argument is missing, quit the program.
       * An error message will be displayed by getopt. */
      if (arg == '?') exit (1);
    }
}

int32_t
ui_print_file_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot read '%s'.\n", file_name);
  return 1;
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
  fprintf (stderr, "ERROR: This program only handles \".json\", "
                   "\".json.gz\" files.\n");
  return 1;
}

void
ui_show_missing_options_warning (void)
{
}
