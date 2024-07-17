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

#include "list.h"

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#define STRING_BUFFER_SIZE 4096

list_t*
list_append (list_t* list, void* data)
{
  /* Create the first element if the list is empty. */
  if (list == NULL)
  {
    list = calloc (1, sizeof (list_t));
    assert (list != NULL);
    
    list->data = data;
  }

  /* Traverse to the last element and add a new one to it. */
  else
  {
    list_t* last = list_last (list);
    list_t *next = calloc (1, sizeof (list_t));
    assert (next != NULL);

    next->previous = last;
    next->data = data;
    last->next = next;
    list = next;
  }

  return list;
}

list_t*
list_prepend (list_t* list, void* data)
{
  list_t* new_list = calloc (1, sizeof (list_t));
  assert (new_list != NULL);

  list = list_nth (list, 1);

  new_list->next = list;
  new_list->data = data;

  if (list != NULL)
    list->previous = new_list;
  
  return new_list;
}


list_t*
list_remove (list_t* list)
{
  if (list == NULL) return NULL;

  list_t *next = list->next;
  list_t *prev = list->previous;

  free (list), list = NULL;

  if (next != NULL) next->previous = prev;
  if (prev != NULL) prev->next = next;

  return (next != NULL) ? next : prev;
}


void
list_free (list_t* list)
{
  if (list == NULL) return;

  // Rewind the list.
  list = list_nth (list, 1);

  // Clean up.
  while (list != NULL)
    list = list_remove (list);

}

list_t*
list_remove_remainder (list_t* list)
{
  if (list == NULL) return NULL;

  // Clean up.
  while (list != NULL)
    list = list_remove (list);

  return list;
}

void
list_free_all (list_t* list, void (*callback) (void *))
{
  if (list == NULL) return;

  // Rewind the list.
  list = list_nth (list, 1);

  // Clean up.
  while (list != NULL)
  {
    if (callback != NULL) callback (list->data);
    list = list_remove (list);
  }
}


int
list_length (list_t* list)
{
  // Return zero when the list is unallocated.
  if (list == NULL) return 0;

  list = list_nth (list, 1);
  
  int length = 1;

  while (list->next != NULL)
  {
    length++;
    list = list->next;
  }

  return length;
}


list_t*
list_next (list_t* list)
{
  if (list == NULL) return NULL;
  return list->next;
}

list_t*
list_previous (list_t* list)
{
  if (list == NULL) return NULL;
  return list->previous;
}

list_t*
list_nth (list_t* list, unsigned int nth)
{
  if (list == NULL || nth < 1) return NULL;

  // Rewind the list.
  while (list->previous != NULL)
    list = list->previous;

  unsigned int counter;
  for (counter = 1; counter < nth; counter++)
  {
    if (list->next == NULL) break;
    list = list->next;
  }

  return list;
}


list_t*
list_last (list_t* list)
{
  if (list == NULL) return NULL;

  while (list->next != NULL)
    list = list->next;

  return list;
}

void
list_print (list_t *list)
{
  if (list == NULL) return;

  // Rewind the list.
  list = list_nth (list, 1);

  // Clean up.
  while (list != NULL)
  {
    if (list->next == NULL)
      printf ("%s.\n", (char *)list->data);
    else
      printf ("%s -> ", (char *)list->data); 

    list = list->next;
  }
}

char *
list_to_uri_path (list_t *list, int32_t identifier)
{
  if (list == NULL)
    return NULL;

  // Rewind the list.
  list = list_nth (list, 1);

  char *buffer;
  buffer = calloc (1, STRING_BUFFER_SIZE);

  char *state = buffer;
  if (buffer == NULL)
    return NULL;

  // Clean up.
  while (list != NULL)
  {
    if (list->next == NULL)
      state += snprintf (state, (STRING_BUFFER_SIZE - (state - buffer)),
                         "%d/%s.\n", identifier, (char *)list->data);
    else
      state += snprintf (state, (STRING_BUFFER_SIZE - (state - buffer)),
                         "/%s", (char *)list->data);

    list = list->next;
  }

  return buffer;
}

list_t*
list_find (list_t *list, void *item, bool (*true_function)(void *, void *))
{
  if (list == NULL || item == NULL || true_function == NULL)
    return NULL;

  list = list_nth (list, 1);
  while (list != NULL)
    {
      if (true_function (list->data, item))
        return list;

      list = list->next;
    }

  return NULL;
}
