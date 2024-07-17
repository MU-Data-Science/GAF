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

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <raptor2.h>
#include <htslib/sam.h>
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>

#ifdef ENABLE_MTRACE
#include <mcheck.h>
#endif

#include "ui.h"
#include "helper.h"
#include "runtime_configuration.h"
#include "bam_header.h"
#include "ontology.h"

extern RuntimeConfiguration config;

int
main (int argc, char **argv)
{
#ifdef ENABLE_MTRACE
  mtrace ();
#endif

  /* Initialize the run-time configuration.
   * ------------------------------------------------------------------------ */
  if (!runtime_configuration_init ()) return 1;

  /* Process command-line arguments.
   * ------------------------------------------------------------------------ */
  if (argc > 1)
    ui_process_command_line (argc, argv);
  else
    ui_show_help ();

  /* Read the input file.
   * ------------------------------------------------------------------------ */
  if (config.input_file)
    {
      /* Initialize the Redland run-time configuration.
       * -------------------------------------------------------------------- */
      if (!runtime_configuration_redland_init ()) return 1;

      ui_show_missing_options_warning ();
      hts_verbose = 0;

      /* Determine whether the input file is a SAM, BAM, or CRAM.
       * -------------------------------------------------------------------- */
      int32_t input_file_len = strlen (config.input_file);
      bool is_sam = false;
      bool is_bam = false;
      bool is_cram = false;

      if (input_file_len > 3)
        {
          is_sam = !strcmp (config.input_file + input_file_len - 3, "sam");
          is_bam = !strcmp (config.input_file + input_file_len - 3, "bam");
        }

      if (!is_sam && !is_bam && input_file_len > 4)
        is_cram = !strcmp (config.input_file + input_file_len - 4, "cram");

      if (!(is_sam || is_bam || is_cram))
        return ui_print_file_format_error ();

      /* Prepare the buffers needed to read the BAM file.
       * -------------------------------------------------------------------- */
      bam_hdr_t *bam_header = NULL;
      htsFile   *bam_stream = NULL;

      /* The format is determined automatically by checking the first few bytes
       * by the hts_open function.  We can therefore use the same function call
       * regardless of the actual input file's format.  Neat stuff! */
      bam_stream = hts_open (config.input_file, "r");
      
      if (!bam_stream)
        return ui_print_bam_file_error (config.input_file);

      /* Read the SAB/BAM/CRAM header.
       * -------------------------------------------------------------------- */
      bam_header = sam_hdr_read (bam_stream);
      if (!bam_header)
        {
          hts_close (bam_stream);
          return ui_print_bam_header_error (config.input_file);
        }

      unsigned char *file_hash = helper_get_hash_from_file (config.input_file);
      if (!file_hash) return 1;

      raptor_statement *stmt;
      raptor_term *node_filename;

      node_filename = term (PREFIX_ORIGIN, (char *)file_hash);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_RDF, "#type");
      stmt->object    = term (PREFIX_MASTER, "Origin");
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, "convertedBy");
      stmt->object    = term (PREFIX_MASTER, "bam2rdf-" VERSION);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_MASTER, "bam2rdf-" VERSION);
      stmt->predicate = term (PREFIX_OWL, "#versionInfo");
      stmt->object    = literal (VERSION, XSD_STRING);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, "filename");
      stmt->object    = literal (config.input_file, XSD_STRING);
      register_statement (stmt);
      stmt = NULL;

      /* Process the header. */
      process_header (bam_header, file_hash);

      /* Process the reads/alignments. */
      if (!config.metadata_only)
        fputs ("Warning: Processing reads hasn't been implemented.\n", stderr);

      /* Clean up. */
      raptor_free_term (node_filename);
      runtime_configuration_free ();

      free (file_hash);
      bam_hdr_destroy (bam_header);
      hts_close (bam_stream);
    }

#ifdef ENABLE_MTRACE
  muntrace ();
#endif

  return 0;
}
