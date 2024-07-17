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
#include "json.h"
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
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_RDF, "#type");
      stmt->object    = term (PREFIX_MASTER, "Origin");
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, HASH_ALGORITHM_NAME);
      stmt->object    = literal ((char *)file_hash, XSD_STRING);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, "convertedBy");
      stmt->object    = term (PREFIX_MASTER, "json2rdf-" VERSION);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_MASTER, "json2rdf-" VERSION);
      stmt->predicate = term (PREFIX_OWL, "#versionInfo");
      stmt->object    = literal (VERSION, XSD_STRING);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, "filename");
      stmt->object    = literal (config.input_file, XSD_STRING);
      register_statement (stmt);
      stmt = NULL;

      /* Setup and invoke the JSON parser.
       * ------------------------------------------------------------------- */

      unsigned char buffer[4096];
      int bytes_read = 0;

      /* Set the parser callbacks. */
      static yajl_callbacks callbacks = {
        on_null_value,
        on_bool_value,
        NULL,
        NULL,
        on_numeric_value,
        on_string_value,
        on_map_start,
        on_map_key,
        on_map_end,
        on_array_start,
        on_array_end
      };

      yajl_alloc_funcs allocation_functions;
      yajl_handle handle;
      yajl_status status;
      json_state_t state;

      json_state_initialize (&state);
      yajl_set_default_alloc_funcs (&allocation_functions);
      handle = yajl_alloc (&callbacks, &allocation_functions, &state);

      while ((bytes_read = gzfread (buffer, 1, sizeof (buffer), stream)) > 0)
        if (yajl_parse (handle, buffer, bytes_read) != yajl_status_ok)
          break;

      status = yajl_complete_parse (handle);
      if (status != yajl_status_ok)
        {
          unsigned char * error_message;
          error_message = yajl_get_error (handle, 0, buffer, bytes_read);
          fflush (stdout);
          fprintf (stderr, "%s", (char *)error_message);
          yajl_free_error (handle, error_message);
        }

      json_state_free (&state);
      yajl_free (handle);
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
