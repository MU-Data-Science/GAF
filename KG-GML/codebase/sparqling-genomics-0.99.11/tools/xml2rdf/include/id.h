/*
 * Copyright (C) 2019 Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef ID_H
#define ID_H

#include "list.h"
#include <stdint.h>

typedef struct
{
  int32_t number;
  char *identifier;
} id_tracker_t;

list_t* id_tracker_init (void);
list_t* id_remove (list_t *list, char *identifier);
int32_t id_next (list_t **list, char *identifier);
int32_t id_current (list_t *list, char *identifier);
void id_tracker_free (list_t *list);
bool id_find (void *item, void *needle);

#endif
