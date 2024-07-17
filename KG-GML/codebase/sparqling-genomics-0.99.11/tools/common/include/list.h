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

#ifndef LIST_H
#define LIST_H

#include <stdbool.h>
#include <stdint.h>

typedef struct _list_t
{
  void* data;
  struct _list_t* previous;
  struct _list_t* next;
} list_t;

list_t* list_append (list_t* list, void* data);
list_t* list_prepend (list_t* list, void* data);
list_t* list_remove (list_t* list);
list_t* list_remove_remainder (list_t* list);
void list_free (list_t* list);
void list_free_all (list_t* list, void (*callback) (void *));
int list_length (list_t* list);
list_t* list_next (list_t* list);
list_t* list_previous (list_t* list);
list_t* list_nth (list_t* list, unsigned int nth);
list_t* list_last (list_t* list);
void list_print (list_t *list);
char* list_to_uri_path (list_t *list, int32_t identifier);
list_t* list_find (list_t *list, void *item,
                   bool (*true_function)(void *, void *));

#endif
