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
	"  --header-only,           -o  Only process the VCF header.\n"
	"  --metadata-only,         -m  Output only metadata.  This mode "
                                       "can be used to find samples.\n"
        "  --hash=ARG,              -H  Use ARG as file identification hash.\n"
	"  --help,                  -h  Show this message.\n"
	"  --progress-info,         -p  Show progress information.\n"
	"  --version,               -v  Show versioning information.\n"
        "  --caller=ARG,            -c  The caller used to produce the VCF "
                                       "file.\n"
        "  --filter=ARG,            -f  Omit calls with FILTER=ARG from the "
                                       "output.\n"
        "  --sample=ARG,            -s  Only process variant calls for ARG.\n"
        "  --without-info-fields,   -x  Do not process INFO fields.\n"
        "  --without-format-fields, -y  Do not process FORMAT fields.\n"
        "  --input-file=ARG,        -i  The input file to process.\n"
        "  --stdin,                 -I  Read input from a pipe instead of a "
                                       "file.\n"
        "  --keep=ARG,              -k  Omit calls without FILTER=ARG from the "
                                       "output.\n"
        "  --output-format,         -O  The output format to serialize to.\n"
        "  --reference=ARG,         -r  Prefix for the chromosome names.  "
                                       "Defaults to:\n"
        "                               \"https://www.ncbi.nlm.nih.gov/nuccore/\".\n");
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
      { "filter",                required_argument, 0, 'f' },
      { "input-file",            required_argument, 0, 'i' },
      { "stdin",                 no_argument,       0, 'I' },
      { "keep",                  required_argument, 0, 'k' },
      { "reference",             required_argument, 0, 'r' },
      { "header-only",           no_argument,       0, 'o' },
      { "metadata-only",         no_argument,       0, 'm' },
      { "output-format",         required_argument, 0, 'O' },
      { "progress-info",         no_argument,       0, 'p' },
      { "sample",                required_argument, 0, 's' },
      { "without-info-fields",   no_argument,       0, 'x' },
      { "without-format-fields", no_argument,       0, 'y' },
      { "progress-info",         no_argument,       0, 'p' },
      { "hash",                  required_argument, 0, 'H' },
      { "help",                  no_argument,       0, 'h' },
      { "version",               no_argument,       0, 'v' },
      { 0,                       0,                 0, 0   }
    };

  while ( arg != -1 )
    {
      /* Make sure to list all short options in the string below. */
      arg = getopt_long (argc, argv, "c:f:i:k:r:O:s:H:Iomxyphv", options, &index);
      switch (arg)
        {
        case 'c': config.caller = optarg;                        break;
        case 'f': config.filter = optarg;                        break;
        case 'i': config.input_file = optarg;                    break;
        case 'I': config.input_from_stdin = true;                break;
        case 'k': config.keep = optarg;                          break;
        case 'r': config.reference = optarg;                     break;
        case 'o': config.header_only = true;                     break;
        case 'm': config.metadata_only = true;                   break;
        case 'O': config.output_format = optarg;                 break;
        case 'p': config.show_progress_info = true;              break;
        case 's': config.sample = optarg;                        break;
        case 'x': config.process_info_fields = false;            break;
        case 'y': config.process_format_fields = false;          break;
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
ui_print_vcf_file_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot open '%s'.\n", file_name);
  return 1;
}

int32_t
ui_print_vcf_header_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot read header of '%s'.\n", file_name);
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
ui_print_file_format_error (void)
{
  fprintf (stderr, "ERROR: This program only handles \".vcf\", "
                   "\".vcf.gz\", \".vcf.bgz\", \".bcf\", \".bcf.gz\", and "
                   "\".bcf.bgz\" files.\n");
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

      if (!config.caller)
        fputs ("Warning: No --caller has been specified.  "
               "This may lead to incomplete and/or ambiguous information "
               "in the database.\n", stderr);
    }
}
