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
#include <string.h>
#include <stdbool.h>
#include <getopt.h>

#include "runtime_configuration.h"

extern RuntimeConfiguration config;

void
ui_show_help (void)
{
  puts ("\nAvailable options:\n"
	"  --help,                   -h  Show this message.\n"
	"  --progress-info,          -p  Show progress information.\n"
	"  --version,                -v  Show versioning information.\n"
        "  --caller=ARG,             -c  The program used to produce the input "
                                        "file.\n"
	"  --delimiter,              -d  The delimiter to distinguish fields "
                                        "in the file.\n"
	"  --secondary-delimiter,    -D  A secondary delimiter to distinguish "
                                        "fields in a field.\n"
	"  --header-line,            -H  When the input file does not contain a "
                                        "header\n"
        "                                line, provide it here. When using this "
                                        "argument,\n"
        "                                the header line must use ';' as the "
                                        "delimiter.\n"
        "  --skip-first-line=ARG,    -S  Ignore the first line in the file.\n"
        "  --transform-object=ARG    -t  A pair in the form colname=uri-prefix "
                                        "where the value\n"
        "                                for the column identified by colname "
                                        "is transformed\n"
        "                                into a URI for "
                                        "which uri-prefix is the prefix.  This"
        "\n                                argument may be repeated.\n"
        "  --transform-predicate=ARG -T  Same as -t, except this operates on the "
                                        "column names\n"
        "                                instead of the values.\n"
        "  --input-file=ARG,         -i  The input file to process.\n"
        "  --stdin,                  -I  Read input from a pipe instead of a "
                                        "file.\n"
        "  --ignore-lines-with=ARG   -j  Ignore lines starting with ARG.\n"
        "  --output-format           -O  The output format to serialize to.\n");
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
      { "caller",                required_argument, 0, 'c' },
      { "delimiter",             required_argument, 0, 'd' },
      { "secondary-delimiter",   required_argument, 0, 'D' },
      { "header-line",           required_argument, 0, 'H' },
      { "help",                  no_argument,       0, 'h' },
      { "input-file",            required_argument, 0, 'i' },
      { "stdin",                 no_argument,       0, 'I' },
      { "ignore-lines-with",     required_argument, 0, 'j' },
      { "output-format",         required_argument, 0, 'O' },
      { "progress-info",         no_argument,       0, 'p' },
      { "skip-first-line",       no_argument,       0, 'S' },
      { "transform-object",      required_argument, 0, 't' },
      { "transform-predicate",   required_argument, 0, 'T' },
      { "version",               no_argument,       0, 'v' },
      { 0,                       0,                 0, 0   }
    };

  while ( arg != -1 )
    {
      /* Make sure to list all short options in the string below. */
      arg = getopt_long (argc, argv, "c:d:D:i:O:H:St:T:Ij:ophv", options, &index);
      switch (arg)
        {
        case 'c': config.caller = optarg;                        break;
        case 'd': config.delimiter = optarg;                     break;
        case 'D': config.secondary_delimiter = optarg;           break;
        case 'i': config.input_file = optarg;                    break;
        case 'I': config.input_from_stdin = true;                break;
        case 'j': config.ignore_lines_with = optarg;             break;
        case 'O': config.output_format = optarg;                 break;
        case 'p': config.show_progress_info = true;              break;
        case 'H': config.header_line = optarg;                   break;
        case 'S': config.skip_first_line = true;                 break;
        case 't': preregister_object_transformer (optarg);       break;
        case 'T': preregister_predicate_transformer (optarg);    break;
        case 'h': ui_show_help ();                               break;
        case 'v': ui_show_version ();                            break;
        }
    }

  /* Passing '\t' on the command-line can be parsed as if it were '\\t'.
   * Let's fix that here. */
  if (config.delimiter && !strcmp(config.delimiter, "\\t"))
    config.delimiter = "\t";

  if (config.header_line != NULL)
    {
      if (strchr (config.header_line, ';') == NULL)
        {
          fprintf (stderr, "When using --header-line, use ';' as the delimiter "
                           "for the header string.\n");
          exit (0);
        }
    }
}

int32_t
ui_print_file_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot open '%s'.\n", file_name);
  return 1;
}

int32_t
ui_print_file_read_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot read from '%s'.\n", file_name);
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
  fprintf (stderr, "ERROR: This program only handles \".vcf\", "
                   "\".vcf.gz\", \".bcf\", and \".bcf.gz\" files.\n");
  return 1;
}
