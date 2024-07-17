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

#include "table.h"
#include "ui.h"
#include "runtime_configuration.h"
#include "helper.h"
#include "tools.h"

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>

extern RuntimeConfiguration config;

bool
is_integer (const char *input, uint32_t length)
{
  uint32_t index = 0;
    for (; index < length; index++)
      if (!isdigit (input[index]))
        return false;

    return true;
}

bool
is_float (const char *input, uint32_t length)
{
  uint32_t has_dot = 0;
  uint32_t has_digits = 0;
  uint32_t index = 0;
  for (; index < length; index++)
    {
      if (input[index] == '.') has_dot += 1;
      else if (isdigit (input[index])) has_digits += 1;
      else return false;
    }

  return (has_dot == 1 && has_digits > 0);
}

bool
is_flag (const char *input, uint32_t length)
{
  char buf[length];
  strncpy (buf, input, length);

  uint32_t index = 0;
  for (; index < length; index++)
    buf[index] = toupper(buf[index]);

  return ((!strcmp (buf, "T")) ||
          (!strcmp (buf, "F")) ||
          (!strcmp (buf, "TRUE")) ||
          (!strcmp (buf, "FALSE")) ||
          (!strcmp (buf, "YES")) ||
          (!strcmp (buf, "NO")));
}

table_hdr_t *
process_header (gzFile stream, raptor_term *origin, const char *filename)
{
  table_hdr_t *header = calloc (1, sizeof (table_hdr_t));
  if (!header)
    {
      ui_print_general_memory_error();
      return NULL;
    }

  header->keys = calloc (64, sizeof (char *));
  header->column_ids = calloc (64, sizeof (char *));
  header->object_transformer_ids = calloc (64, sizeof (int32_t *));
  header->predicate_transformer_ids = calloc (64, sizeof (int32_t *));

  header->keys_alloc_len = 64;
  if (header->keys == NULL
      || header->column_ids == NULL
      || header->object_transformer_ids == NULL
      || header->predicate_transformer_ids == NULL)
    {
      ui_print_general_memory_error();
      free (header->keys);
      free (header->column_ids);
      free (header->object_transformer_ids);
      free (header->predicate_transformer_ids);
      free (header);
      return NULL;
    }

  int32_t index;
  for (index = 0; index < 64; index++)
    {
      header->object_transformer_ids[index] = TRANSFORMER_INDEX_UNKNOWN;
      header->predicate_transformer_ids[index] = TRANSFORMER_INDEX_UNKNOWN;
    }

  char *line = NULL;
  size_t line_len = 0;
  ssize_t result = 0;

  if (config.header_line == NULL)
    {
      result = gzgetdelim (&line, &line_len, '\n', stream);
      bool ignore_line = (config.ignore_lines_with != NULL);
      while (ignore_line)
        {
          size_t ignore_length = strlen (config.ignore_lines_with);
          size_t i = 0;
          for (; i < ignore_length; i++)
            if (line[i] != config.ignore_lines_with[i])
              break;

          ignore_line = (i == ignore_length);
          if (ignore_line)
            {
              free (line);
              line = NULL;
              line_len = 0;

              result = gzgetdelim (&line, &line_len, '\n', stream);
            }
        }
    }
  else
    line = config.header_line;

  if (!(result == -1 && !config.header_line))
    {
      /* The 'gzgetdelim' function does not remove the delimiter, so let's do
       * that here. */
      size_t line_strlen = strlen (line);
      if (line[line_strlen - 1] == '\n')
        line[line_strlen - 1] = '\0';

      header->keys_len = 0;
      char *token = NULL;

      if (config.header_line)
        token = strtok (line, ";");
      else
        token = strtok (line, config.delimiter);

      raptor_statement *stmt;
      while (token != NULL)
        {
          /* Dynamically grow the number of keys, but keep it as an
           * array so that lookups remain constant time. */
          if (header->keys_len >= header->keys_alloc_len)
            {
              header->keys_alloc_len = header->keys_alloc_len + 64;
              header->keys = realloc (header->keys,
                                      header->keys_alloc_len * sizeof (char *));
              header->column_ids = realloc (header->column_ids,
                                            header->keys_alloc_len * sizeof (char *));
              header->object_transformer_ids = realloc (header->object_transformer_ids,
                                                        header->keys_alloc_len *
                                                        sizeof (int32_t *));
              header->predicate_transformer_ids = realloc (header->predicate_transformer_ids,
                                                           header->keys_alloc_len *
                                                           sizeof (int32_t *));

              if (header->keys == NULL
                  || header->column_ids == NULL
                  || header->object_transformer_ids == NULL
                  || header->predicate_transformer_ids == NULL)
                {
                  ui_print_general_memory_error();
                  return NULL;
                }

              for (index = header->keys_alloc_len - 64; // The old length
                   index < header->keys_alloc_len;
                   index++)
                {
                  header->object_transformer_ids[index] = TRANSFORMER_INDEX_UNKNOWN;
                  header->predicate_transformer_ids[index] = TRANSFORMER_INDEX_UNKNOWN;
                }
            }

          header->keys[header->keys_len] = trim_quotes (token, strlen (token));
          char *column_id = sanitize_string (header->keys[header->keys_len],
                                             strlen (header->keys[header->keys_len]));

          if (! column_id)
            {
              ui_print_general_memory_error();
              return NULL;
            }

          header->column_ids[header->keys_len] = column_id;
          raptor_term *subject = term (PREFIX_COLUMN, column_id);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = subject;
          stmt->predicate = predicate (PREDICATE_RDF_TYPE);
          stmt->object    = class (CLASS_COLUMN);
          register_statement_reuse_all (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = subject;
          stmt->predicate = predicate (PREDICATE_FOUND_IN);
          stmt->object    = origin;
          register_statement_reuse_all (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = subject;
          stmt->predicate = predicate (PREDICATE_LABEL);
          stmt->object    = literal (header->keys[header->keys_len],
                                     XSD_STRING);
          register_statement_reuse_subject_predicate (stmt);

          snprintf (config.number_buffer, 32, "%u", header->keys_len);
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = subject;
          stmt->predicate = predicate (PREDICATE_POSITION);
          stmt->object    = literal (config.number_buffer, XSD_INTEGER);
          register_statement_reuse_predicate (stmt);

          header->keys_len += 1;

          if (config.header_line)
            token = strtok (NULL, ";");
          else
            token = strtok (NULL, config.delimiter);
        }
    }
  else
    {
      ui_print_file_read_error ((char *)filename);
    }

  if (config.header_line == NULL)
    free (line);

  return header;
}

void
process_column (table_hdr_t* hdr, char *token, uint32_t column_index)
{
  /* When a column is empty, don't add any triples. */
  if (token == NULL) return;

  uint32_t trimmed_length = 0;
  char *trimmed_token     = NULL;
  raptor_statement *stmt  = NULL;
  int32_t trans_index     = 0;

  trimmed_token = trim_quotes (token, strlen (token));
  if (trimmed_token == NULL) return;
  trimmed_length = strlen (trimmed_token);

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = term (PREFIX_ORIGIN, config.id_buf);

  /* ------------------------------------------------------------------------
   * PREDICATE TRANSFORMATION
   * ------------------------------------------------------------------------ */

  trans_index = hdr->predicate_transformer_ids[column_index];
  if (trans_index == TRANSFORMER_INDEX_UNKNOWN)
    {
      trans_index = 0;
      for (; trans_index < config.predicate_transformer_len; trans_index++)
        if (!strcmp (hdr->column_ids[column_index],
                     config.predicate_transformer_keys[trans_index]))
          break;

      if (trans_index >= config.predicate_transformer_len)
        trans_index = TRANSFORMER_INDEX_UNAVAILABLE;

      hdr->predicate_transformer_ids[column_index] = trans_index;
    }

  if (trans_index >= 0 && trans_index < config.predicate_transformer_len)
    stmt->predicate = raptor_new_term_from_uri_string (config.raptor_world,
                                                       ((unsigned char *)
                                                        config.predicate_transformer_values[trans_index]));
  else
    stmt->predicate = term (PREFIX_COLUMN, hdr->column_ids[column_index]);

  /* ------------------------------------------------------------------------
   * OBJECT TRANSFORMATION
   * ------------------------------------------------------------------------ */

  trans_index = hdr->object_transformer_ids[column_index];
  if (trans_index == TRANSFORMER_INDEX_UNKNOWN)
    {
      trans_index = 0;
      for (; trans_index < config.object_transformer_len; trans_index++)
        if (!strcmp (hdr->column_ids[column_index],
                     config.object_transformer_keys[trans_index]))
          break;

      if (trans_index >= config.object_transformer_len)
        trans_index = TRANSFORMER_INDEX_UNAVAILABLE;

      hdr->object_transformer_ids[column_index] = trans_index;
    }

  /* When a transformer is available, we treat the value as a URI. */
  if (trans_index >= 0 && trans_index < config.object_transformer_len)
    {
      /* The ontology can either use a '/' or a '#' as separator.
       * In Redland, an '#' behaves different than a '/'.  We have
       * to deal with that here. */
      char *end_token = trimmed_token;
      bool end_token_allocated = false;
      uint32_t uri_len = strlen (config.object_transformer_values[trans_index]);
      if (config.object_transformer_values[trans_index][uri_len - 1] == '#')
        {
          uint32_t token_len = strlen (trimmed_token);
          end_token = calloc (token_len + 2, sizeof (char));
          end_token_allocated = true;
          if (end_token == NULL)
            {
              ui_print_general_memory_error ();
              return;
            }

          snprintf (end_token, (token_len + 2) * sizeof (char),
                    "#%s", trimmed_token);
        }

      stmt->object = term (trans_index +
                           config.ontology->prefixes_static_length,
                           end_token);

      if (end_token_allocated)
        free (end_token);
    }

  /* Without a transformer, the value will be treated as a "literal" instead
   * of a URI. */
  else
    {
      /* Determine the actual type this data represents.
       * TODO: Also detect booleans. */
      int32_t data_type;
      if (is_integer (trimmed_token, trimmed_length))
        data_type = XSD_INTEGER;
      else if (is_float (trimmed_token, trimmed_length))
        data_type = XSD_FLOAT;
      else
        data_type = XSD_STRING;

      stmt->object    = literal (trimmed_token, data_type);
    }

  register_statement (stmt);
  free (trimmed_token);
  trimmed_token = NULL;
}

void
process_row (table_hdr_t* hdr, gzFile stream, raptor_term *origin,
             const unsigned char *origin_str, const char *filename)
{
  char *line_orig = NULL;
  char *line      = NULL;
  size_t line_len = 0;

  if (gzgetdelim (&line, &line_len, '\n', stream) != -1)
    {
      /* We use 'strsep' later on, which will eventually set line to NULL.
       * To be able to free the memory allocated by 'gzgetdelim', we must store
       * the memory address.  */
      line_orig = line;

      /* The 'gzgetdelim' function does not remove the delimiter, so let's do
       * that here. */
      size_t line_strlen = strlen (line);
      if (line[line_strlen - 1] == '\n')
        line[line_strlen - 1] = '\0';

      char *token            = NULL;
      raptor_statement *stmt = NULL;

      if (! generate_row_id (origin_str, config.id_buf))
        {
          ui_print_general_memory_error();
          return;
        }

      raptor_term *subject = term (PREFIX_ORIGIN, config.id_buf);
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = subject;
      stmt->predicate = predicate (PREDICATE_RDF_TYPE);
      stmt->object    = class (CLASS_ROW);
      register_statement_reuse_all (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = subject;
      stmt->predicate = predicate (PREDICATE_ORIGINATED_FROM);
      stmt->object    = origin;
      register_statement_reuse_predicate_object (stmt);

      token = strsep (&line, config.delimiter);
      uint32_t column_index = 0;
      for (; column_index < hdr->keys_len; column_index++)
        {
          if (token != NULL)
            {
              char *secondary_delim = (config.secondary_delimiter)
                ? strstr (token, config.secondary_delimiter)
                : NULL;

              if (config.secondary_delimiter && secondary_delim)
                {
                  char *previous_position = token;
                  uint32_t delimiter_length = strlen (config.secondary_delimiter);
                  while (secondary_delim != NULL)
                    {
                      *secondary_delim = '\0';
                      process_column (hdr, previous_position, column_index);

                      /*  Move on to the next token. */
                      previous_position = secondary_delim + (delimiter_length * sizeof (char));
                      secondary_delim = strstr (previous_position,
                                                config.secondary_delimiter);
                    }

                  /* Also process the last column that doesn't have the
                   * secondary delimiter at its end. */
                  process_column (hdr, previous_position, column_index);
                  previous_position = NULL;
                }
              else
                process_column (hdr, token, column_index);
            }

          token = strsep (&line, config.delimiter);
        }
    }
  else if (!gzeof (stream))
    ui_print_file_read_error ((char *)filename);
  else
    line_orig = line;

  free (line_orig);
}
