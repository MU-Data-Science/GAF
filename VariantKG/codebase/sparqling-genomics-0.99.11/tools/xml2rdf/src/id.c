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

#include "id.h"
#include "list.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

list_t*
id_tracker_init (void)
{
  /* This is the same as an empty list. */
  return NULL;
}

list_t*
id_tracker_add (list_t *list, char *identifier)
{
  id_tracker_t *id = calloc (1, sizeof (id_tracker_t));
  if (id == NULL)
    return NULL;

  id->identifier = strdup (identifier);
  return list_prepend (list, id);
}

list_t*
id_remove (list_t *list, char *identifier)
{
  if (list == NULL)
    return list;

  list_t* item = list_find (list, identifier, id_find);
  if (item == NULL)
    return list;

  id_tracker_t *id = (id_tracker_t *)item->data;
  if (id == NULL)
    return list;

  free (((id_tracker_t *)item->data)->identifier);
  free (item->data);
  item->data = NULL;

  return list_remove (item);
}

bool
id_find (void *item, void *needle)
{
  if (item == NULL || needle == NULL)
    return false;

  id_tracker_t *id = (id_tracker_t *)item;

  if (id->identifier == NULL)
    return false;

  return (!strcmp (id->identifier, (char *)needle));
}


int32_t
id_next (list_t **list, char *identifier)
{
  if (list == NULL)
    return -1;

  list_t* item = list_find (*list, identifier, id_find);
  if (item == NULL)
    item = id_tracker_add (*list, identifier);

  if (item == NULL)
    return -1;
  else
    *list = item;

  id_tracker_t *id = (id_tracker_t *)item->data;
  if (id == NULL)
    return -1;

  id->number = id->number + 1;

  return id->number;
}

int32_t
id_current (list_t *list, char *identifier)
{
  if (list == NULL)
    return -1;

  list_t* item = list_find (list, identifier, id_find);
  if (item == NULL)
    return -1;

  id_tracker_t *id = (id_tracker_t *)item->data;
  if (id == NULL)
    return -1;

  return id->number;
}

void
id_free (void *item)
{
  if (item == NULL)
    return;

  id_tracker_t *id = (id_tracker_t *)item;
  free (id->identifier);
  id->identifier = NULL;
  free (id);
}

void
id_tracker_free (list_t *list)
{
  list_free_all (list, id_free);
}
