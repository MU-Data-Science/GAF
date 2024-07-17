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

#include "xml.h"
#include "helper.h"
#include "id.h"
#include "ontology.h"
#include "runtime_configuration.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>

extern RuntimeConfiguration config;

/*
 * We implement a streaming XML parser using SAX.  The libxml2 parser calls
 * functions upon encountering a certain event.
 */

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

  return ((!strcmp (buf, "TRUE"))  ||
          (!strcmp (buf, "FALSE")) ||
          (!strcmp (buf, "true"))  ||
          (!strcmp (buf, "false")) ||
          (!strcmp (buf, "YES"))   ||
          (!strcmp (buf, "NO"))    ||
          (!strcmp (buf, "yes"))   ||
          (!strcmp (buf, "no")));
}

int32_t
xsd_type (const char *input, int32_t length)
{
  int32_t type = XSD_STRING;
  if (is_integer (input, length))
    type = XSD_INTEGER;
  else if (is_float (input, length))
    type = XSD_FLOAT;

  return type;
}


/* This function is called to report the start of an element.
 * ------------------------------------------------------------------------- */
static void
on_start_element (void *ctx, 
                  const xmlChar *name,
                  const xmlChar **attributes)
{
  char *element_name = (char *)name;
  config.xml_path = list_append (config.xml_path, element_name);

  /* There are two ways to convey information in XML:
   * by using attributes, or by using child-elements.  Child elements
   * are handled automatically because they will trigger another
   * ‘on_start_element’ call.  Attributes do not.  So we must handle
   * attributes immediately.
   */
  if (attributes)
    {
      int32_t id = id_next (&(config.id_tracker), element_name);
      raptor_statement *stmt;

      int32_t subject_name_length = strlen (element_name) + 76;
      char subject_name[subject_name_length];

      int32_t written;
      written = snprintf (subject_name, subject_name_length, "%s/%s/%d",
                          config.origin_hash, element_name, id);

      if (written < 0 || written > subject_name_length)
        return;

      int32_t attribute_index = 0;
      char *key, *value;
      while (attributes[attribute_index] != NULL)
        {
          key = (char *)attributes[attribute_index];
          value = (char *)attributes[attribute_index + 1];

          if (key == NULL || value == NULL)
            break;

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_BASE, subject_name);
          stmt->predicate = term (PREFIX_DYNAMIC_TYPE, key);
          stmt->object    = literal (value, xsd_type (value, strlen (value)));
          register_statement (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_BASE, subject_name);
          stmt->predicate = predicate (PREDICATE_RDF_TYPE);
          stmt->object    = term (PREFIX_BASE, "XmlAttribute");
          register_statement_reuse_predicate (stmt);

          attribute_index += 2;
        }
    }
  else
    id_next (&(config.id_tracker), element_name);
}


/* This function is called to report the end has been reached of an element.
 * ------------------------------------------------------------------------- */
static void
on_end_element (void *ctx, const xmlChar *name)
{
  raptor_statement *stmt;
  int32_t identifier = -1;
  int32_t written;
  int32_t subject_name_length;
  char *element_name = (char *)name;

  /* The ‘subject_name_length’ is the sum of the hash (64), two
   * slashes (2), the element_name length and the maximum
   * number of characters in a positive int32_t (10). */
  if (config.value_buffer == NULL)
    {
      identifier = id_current (config.id_tracker, element_name);
      subject_name_length = strlen (element_name) + 75;
    }
  else
    {
      list_t *last = list_last (config.xml_path);
      if (last == NULL) return;

      list_t *subject = last->previous;
      if (subject == NULL || subject->data == NULL) return;

      char *item_name = (char *)subject->data;
      identifier = id_current (config.id_tracker, item_name);
      subject_name_length = strlen (item_name) + 76;
    }

  if (identifier < 0)
    return;

  char subject_name[subject_name_length + 1];
  written = snprintf (subject_name, subject_name_length, "%s/%s/%d",
                      config.origin_hash,
                      (config.value_buffer == NULL)
                        ? element_name
                        : ((char *)config.xml_path->previous->data),
                      identifier);

  if (written < 0 || written > subject_name_length)
    return;

  if (config.value_buffer != NULL)
    {
      int32_t type = xsd_type (config.value_buffer, config.value_buffer_len);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_BASE, subject_name);
      stmt->predicate = term (PREFIX_DYNAMIC_TYPE, element_name);
      stmt->object    = literal (config.value_buffer, type);
      register_statement (stmt);

      free (config.value_buffer);
      config.value_buffer = NULL;
      config.value_buffer_len = 0;
    }
  else
    {
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_BASE, subject_name);
      stmt->predicate = predicate (PREDICATE_RDF_TYPE);
      stmt->object    = term (PREFIX_DYNAMIC_TYPE, element_name);
      register_statement_reuse_predicate (stmt);

      /* When the subject is nested inside a parent element,
       * describe its direct relationship. 
       * -------------------------------------------------------------------- */

      list_t *last = list_last (config.xml_path);
      if (last == NULL) return;

      list_t *object = last->previous;
      if (! (object == NULL || object->data == NULL))
        {
          char *original_name = (char *)object->data;
          int32_t object_id = id_current (config.id_tracker, original_name);
          if (object_id < 0)
            return;

          int32_t object_name_length = strlen (original_name) + 76;
          char object_name[object_name_length + 1];

          int32_t written = snprintf (object_name, object_name_length,
                                      "%s/%s/%d", config.origin_hash,
                                      original_name, object_id);

          if (written > 0 && written < object_name_length)
            {
              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = term (PREFIX_BASE, subject_name);
              stmt->predicate = predicate (PREDICATE_ISPARTOF);
              stmt->object    = term (PREFIX_BASE, object_name);
              register_statement_reuse_predicate (stmt);
            }
        }

      /* When the subject is the absolute parent element, describe its
       * relationship to the Origin. 
       * -------------------------------------------------------------------- */
      else
        {
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_BASE, subject_name);
          stmt->predicate = predicate (PREDICATE_ORIGINATED_FROM);
          stmt->object    = term (PREFIX_ORIGIN, config.origin_hash);
          register_statement_reuse_predicate (stmt);
        }
    }

  config.xml_path->data = NULL;
  config.xml_path = list_remove (config.xml_path);
}


/* This function is called to catch the value of an element.
 * ------------------------------------------------------------------------- */
static void
on_value (void *ctx, const xmlChar *value, int len)
{
  if (config.id_tracker == NULL)
    return;

  if (! only_contains_whitespace ((char *)value, len))
    {
      int32_t value_buffer_len = config.value_buffer_len + len;
      char *value_buffer = realloc (config.value_buffer, value_buffer_len + 1);
      if (value_buffer == NULL)
        {
          fprintf (stderr,
                   "Error: Due to not having enough memory, some values "
                   "may be incomplete or missing.");
          return;
        }

      strncpy (value_buffer + config.value_buffer_len, (char *)value, len);
      value_buffer[value_buffer_len] = '\0';

      config.value_buffer = value_buffer;
      config.value_buffer_len = value_buffer_len;
    }
}

static xmlEntityPtr
get_entity (void *ctx, const xmlChar *name)
{
  return xmlGetPredefinedEntity (name);
}

/* This function creates the parser and sets the callback functions.
 * ------------------------------------------------------------------------- */
xmlSAXHandler make_sax_handler (void)
{
  xmlSAXHandler handler;
  memset (&handler, 0, sizeof (xmlSAXHandler));

  handler.initialized = XML_SAX2_MAGIC;
  handler.startElement = on_start_element;
  handler.endElement = on_end_element;
  handler.characters = on_value;
  handler.getEntity = get_entity;

  return handler;
}
