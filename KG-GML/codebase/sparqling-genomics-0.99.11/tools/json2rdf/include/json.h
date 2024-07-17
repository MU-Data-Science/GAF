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

#ifndef JSON_H
#define JSON_H

#include "yajl_parse.h"
#include "yajl_gen.h"
#include "yajl_alloc.h"
#include "list.h"
#include <stdbool.h>
#include <stdint.h>

typedef struct
{
  char *key;
  size_t length;
} json_key_t;

typedef enum
{
 EVENT_UNKNOWN = 0,
 EVENT_ON_VALUE,
 EVENT_ON_MAP_START,
 EVENT_ON_MAP_KEY,
 EVENT_ON_MAP_END,
 EVENT_ON_ARRAY_START,
 EVENT_ON_ARRAY_END
} EventType;

/* State object.
 * -------------------------------------------------------------------------
 * We use a streaming parser that allows passing a single state object
 * around.  This is the definition of the state object expected to be
 * available to the implemented callback functions.
 */

typedef struct
{
  uint32_t unnamed_map_id;

  list_t *subjects;
  list_t *predicates;

  EventType last_event;
} json_state_t;

#define MAP_ID_BUFFER_LENGTH 32
void unnamed_map_id (json_state_t *ctx, char *buffer, int32_t *length);
bool context_is_available (void *ctx);

void json_state_initialize (json_state_t *state);
void json_state_free (json_state_t *state);

/* Callback functions for YAJL.
 * ------------------------------------------------------------------------- */

int on_null_value (void *ctx);
int on_bool_value (void *ctx, int value);
int on_numeric_value (void *ctx, const char *value, size_t value_length);
int on_string_value (void *ctx, const unsigned char *value, size_t value_length);
int on_map_start (void *ctx);
int on_map_key (void *ctx, const unsigned char *value, size_t value_length);
int on_map_end (void *ctx);
int on_array_start (void *ctx);
int on_array_end (void *ctx);

#endif /* JSON_H */
