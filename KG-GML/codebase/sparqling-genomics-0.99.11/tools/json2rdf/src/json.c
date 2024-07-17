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

#include "json.h"
#include "helper.h"
#include "ui.h"
#include "ontology.h"
#include "runtime_configuration.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

extern RuntimeConfiguration config;

void
json_state_initialize (json_state_t *state)
{
  if (state == NULL)
    return;

  state->unnamed_map_id = 0;
  state->subjects = NULL;
  state->predicates = NULL;
  state->last_event = EVENT_UNKNOWN;
}

void
json_state_free (json_state_t *state)
{
  if (state->subjects)
    list_free_all (state->subjects, free);

  if (state->predicates)
    list_free_all (state->predicates, free);

  json_state_initialize (state);
}

bool
is_integer (const char *input, uint32_t length)
{
  uint32_t index = 0;
    for (; index < length; index++)
      if (!isdigit (input[index]))
        return false;

    return true;
}

int
on_any_value (json_state_t *ctx, void *value, size_t value_length, int32_t xsd_type)
{
  if (! context_is_available (ctx)) return 0;

  raptor_statement *stmt;
  list_t *subjects = list_nth (ctx->subjects, 1);
  list_t *predicates = list_nth (ctx->predicates, 1);
  char *subject = (char *)subjects->data;
  char *predicate = (char *)predicates->data;

  char *buffer = malloc (value_length + 1);
  if (buffer == NULL)
    {
      ui_print_general_memory_error ();
      return 0;
    }

  if (xsd_type == XSD_STRING)
    snprintf (buffer, value_length + 1, "%s", (unsigned char *)value);
  else
    snprintf (buffer, value_length + 1, "%s", (char *)value);

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = term (PREFIX_BASE, subject);
  stmt->predicate = term (PREFIX_DYNAMIC_TYPE, predicate);
  stmt->object = literal (buffer, xsd_type);
  register_statement (stmt);

  if (ctx->last_event == EVENT_ON_MAP_KEY
      && ctx->predicates != NULL)
    {
      list_t *first = list_nth (ctx->predicates, 1);
      free (first->data);
      ctx->predicates = list_remove (first);
    }

  ctx->last_event = EVENT_ON_VALUE;
  free (buffer);
  return 1;
}

int
on_null_value (void *ctx_ptr)
{
  if (! context_is_available (ctx_ptr)) return 0;
  json_state_t *ctx = ctx_ptr;
  ctx->last_event = EVENT_ON_VALUE;

  return 1;
}

int
on_bool_value (void *ctx_ptr, int value)
{
  if (! context_is_available (ctx_ptr)) return 0;
  json_state_t *ctx = ctx_ptr;
  ctx->last_event = EVENT_ON_VALUE;

  raptor_statement *stmt;
  list_t *subjects = list_nth (ctx->subjects, 1);
  list_t *predicates = list_nth (ctx->predicates, 1);
  char *subject = (char *)subjects->data;
  char *predicate = (char *)predicates->data;

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = term (PREFIX_BASE, subject);
  stmt->predicate = term (PREFIX_DYNAMIC_TYPE, predicate);

  if (value)
    stmt->object = literal ("true", XSD_BOOLEAN);
  else
    stmt->object = literal ("false", XSD_BOOLEAN);

  register_statement (stmt);
  return 1;
}

int
on_numeric_value (void *ctx, const char *value, size_t value_length)
{
  return on_any_value ((json_state_t *)ctx,
                       (char *)value,
                       value_length,
                       (is_integer (value, value_length))
                         ? XSD_INTEGER
                         : XSD_FLOAT);
}

int
on_string_value (void *ctx, const unsigned char *value, size_t value_length)
{
  return on_any_value ((json_state_t *)ctx,
                       (unsigned char *)value,
                       value_length,
                       XSD_STRING);
}

int
on_map_start (void *ctx_ptr)
{
  if (! context_is_available (ctx_ptr)) return 0;
  json_state_t *ctx = ctx_ptr;

  char *buffer = malloc (MAP_ID_BUFFER_LENGTH);
  if (buffer == NULL)  
    {
      ui_print_general_memory_error ();
      return 0;
    }

  unnamed_map_id (ctx, buffer, NULL);

  raptor_statement *stmt;
  if ((ctx->last_event == EVENT_ON_MAP_KEY
       || ctx->last_event == EVENT_ON_MAP_END
       || ctx->last_event == EVENT_ON_ARRAY_START)
      && ctx->predicates != NULL
      && ctx->subjects != NULL)
    {
      list_t *parent = ctx->subjects;
      char *parent_name = parent->data;

      list_t *predicates = ctx->predicates;
      char *predicate_name = (char *)predicates->data;

      if (parent_name != NULL)
        {
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_BASE, parent_name);
          stmt->predicate = term (PREFIX_DYNAMIC_TYPE, predicate_name);
          stmt->object    = term (PREFIX_BASE, buffer);
          register_statement (stmt);
        }
    }

  ctx->last_event = EVENT_ON_MAP_START;
  ctx->subjects = list_prepend (ctx->subjects, buffer);
  if (ctx->subjects == NULL)
    {
      ui_print_general_memory_error ();
      return 0;
    }

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = term (PREFIX_BASE, buffer);
  stmt->predicate = term (PREFIX_RDF, "#type");
  stmt->object    = term (PREFIX_BASE, "JsonObject");
  register_statement (stmt);

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = term (PREFIX_BASE, buffer);
  stmt->predicate = term (PREFIX_MASTER, "originatedFrom");
  stmt->object    = term (PREFIX_ORIGIN, config.origin_hash);
  register_statement (stmt);

  return 1;
}

int
on_map_key (void *ctx_ptr, const unsigned char *value, size_t value_length)
{
  if (! context_is_available (ctx_ptr)) return 0;
  json_state_t *ctx = ctx_ptr;
  ctx->last_event = EVENT_ON_MAP_KEY;

  char *buffer = malloc (value_length + 1);
  if (buffer == NULL)
    {
      ui_print_general_memory_error ();
      return 0;
    }

  snprintf (buffer, value_length + 1, "%s", (char *)value);
  ctx->predicates = list_prepend (ctx->predicates, buffer);
  if (ctx->predicates == NULL)
    {
      ui_print_general_memory_error ();
      return 0;
    }

  return 1;
}

int
on_map_end (void *ctx_ptr)
{
  if (! context_is_available (ctx_ptr)) return 0;
  json_state_t *ctx = ctx_ptr;
  ctx->last_event = EVENT_ON_MAP_END;

  if (ctx->subjects != NULL)
    {
      list_t *first = list_nth (ctx->subjects, 1);
      free (first->data);
      ctx->subjects = list_remove (first);
    }

  if (ctx->predicates != NULL)
    {
      list_t *first = list_nth (ctx->predicates, 1);
      free (first->data);
      ctx->predicates = list_remove (first);
    }

  return 1;
}

int
on_array_start (void *ctx_ptr)
{
  if (! context_is_available (ctx_ptr)) return 0;
  json_state_t *ctx = ctx_ptr;
  ctx->last_event = EVENT_ON_ARRAY_START;

  if (ctx->predicates != NULL)
    {
      list_t *predicates = list_nth (ctx->predicates, 1);
      char *predicate = strdup ((char *)predicates->data);
      ctx->predicates = list_prepend (predicates, predicate);
      if (ctx->predicates == NULL)
        {
          ui_print_general_memory_error ();
          return 0;
        }
    }

  return 1;
}

int
on_array_end (void *ctx_ptr)
{
  if (! context_is_available (ctx_ptr)) return 0;
  json_state_t *ctx = ctx_ptr;
  ctx->last_event = EVENT_ON_ARRAY_END;

  if (ctx->predicates != NULL)
    {
      list_t *first = list_nth (ctx->predicates, 1);
      free (first->data);
      ctx->predicates = list_remove (first);
    }

  return 1;
}

void
unnamed_map_id (json_state_t *ctx, char *buffer, int32_t *length)
{
  if (ctx == NULL)
    return;

  int32_t len;
  len = snprintf (buffer, MAP_ID_BUFFER_LENGTH,
                  "Object%u", ctx->unnamed_map_id);

  if (length != NULL)
    *length = len;

  ctx->unnamed_map_id += 1;
}

bool
context_is_available (void *ctx)
{
  if (ctx == NULL)
    {
      fprintf (stderr, "The parser needs context.\n");
      return false;
    }

  return true;
}
