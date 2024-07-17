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

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <raptor2.h>
#include <zlib.h>
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>

#ifdef ENABLE_MTRACE
#include <mcheck.h>
#endif

#include "ui.h"
#include "helper.h"
#include "runtime_configuration.h"
#include "xml.h"
#include "ontology.h"
#include "id.h"
#include "list.h"

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
  if (config.input_file || config.input_from_stdin)
    {
      /* Initialize the Redland run-time configuration.
       * -------------------------------------------------------------------- */
      if (!runtime_configuration_redland_init ()) return 1;

      ui_show_missing_options_warning ();

      /* Open a file stream.
       * -------------------------------------------------------------------- */

      int32_t input_file_len = 0;
      if (!(config.input_from_stdin || config.input_file == NULL))
        input_file_len = strlen (config.input_file);

      if (input_file_len == 0)
        config.input_from_stdin = true;

      gzFile stream;
      if (config.input_from_stdin)
        stream = gzdopen (fileno(stdin), "r");
      else
        stream = gzopen (config.input_file, "r");

      if (!stream)
        return ui_print_file_error (config.input_file);

      /* Get the file hash.
       * -------------------------------------------------------------------- */

      unsigned char *file_hash = NULL;
      if (!config.user_hash && !config.input_from_stdin)
        file_hash = helper_get_hash_from_file (config.input_file);
      else if (!config.user_hash && config.input_from_stdin)
        {
          const int buf_len = gnutls_hash_get_len (HASH_ALGORITHM);
          unsigned char buf[buf_len];
          memset (buf, '\0', buf_len);
          file_hash = calloc (buf_len * 2 + 1, sizeof (unsigned char));
          if (!file_hash) return 1;

          int status = gnutls_rnd (GNUTLS_RND_KEY, buf, buf_len);
          if (status || (! get_pretty_hash (buf, buf_len, file_hash)))
            return 1;
        }
      else
        file_hash = (unsigned char *)config.user_hash;

      if (!file_hash) return 1;

      raptor_statement *stmt;
      raptor_term *node_filename;

      config.origin_hash = (char *)file_hash;
      node_filename = term (PREFIX_ORIGIN, (char *)file_hash);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = node_filename;
      stmt->predicate = predicate (PREDICATE_RDF_TYPE);
      stmt->object    = class (CLASS_ORIGIN);
      register_statement_reuse_all (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = node_filename;
      stmt->predicate = term (PREFIX_MASTER, HASH_ALGORITHM_NAME);
      stmt->object    = literal ((char *)file_hash, XSD_STRING);
      register_statement_reuse_subject (stmt);

      raptor_term *xml2rdf = term (PREFIX_MASTER, "xml2rdf-" VERSION);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = node_filename;
      stmt->predicate = predicate (PREDICATE_CONVERTED_BY);
      stmt->object    = xml2rdf;
      register_statement_reuse_all (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = xml2rdf;
      stmt->predicate = predicate (PREDICATE_VERSION_INFO);
      stmt->object    = literal (VERSION, XSD_STRING);
      register_statement_reuse_predicate (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = node_filename;
      stmt->predicate = predicate (PREDICATE_FILENAME);

      stmt->object    = (config.input_from_stdin)
        ? literal ("stdin", XSD_STRING)
        : literal (config.input_file, XSD_STRING);

      register_statement_reuse_subject_predicate (stmt);
      stmt = NULL;

      /* Setup and invoke the XML parser. 
       * -------------------------------------------------------------------
       *
       * We read the data in chunks so that we can process large files.
       * The ‘buffer’ determines the chunk size.
       */
      char buffer[4096];
      int bytes_read = 0;
      xmlSAXHandler handler;
      xmlParserCtxtPtr ctx;

      handler = make_sax_handler ();
      ctx = xmlCreatePushParserCtxt (&handler, NULL, buffer, bytes_read, NULL);

      while ((bytes_read = gzfread (buffer, 1, sizeof (buffer), stream)) > 0)
        {
          if (xmlParseChunk (ctx, buffer, bytes_read, 0))
            {
              xmlParserError(ctx, "xmlParseChunk");
              break;
            }
        }

      xmlParseChunk (ctx, buffer, 0, 1);
      xmlFreeParserCtxt (ctx);

      gzclose (stream);

      /* Clean up. */
      raptor_free_term (node_filename);
      runtime_configuration_free ();

      if (!config.user_hash) free (file_hash);
    }

#ifdef ENABLE_MTRACE
  muntrace ();
#endif

  return 0;
}
