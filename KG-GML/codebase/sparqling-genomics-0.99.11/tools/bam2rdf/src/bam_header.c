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

#include "bam_header.h"
#include "runtime_configuration.h"
#include "helper.h"
#include "ui.h"

#include <stdio.h>
#include <raptor2.h>

extern RuntimeConfiguration config;

void
process_header_line (char *line, const unsigned char *origin)
{
  if (line == NULL || line[0] != '@') return;

  char type[3]               = { line[1], line[2], 0 };

  /* Determine header type.
   * ----------------------------------------------------------------------- */

  ontology_prefix ont_prefix = PREFIX_UNKNOWN;
  ontology_class  ont_class  = CLASS_UNKNOWN;

  if (!strcmp ("SQ", type))
    {
      ont_prefix = PREFIX_BAM_REFERENCE_SEQ;
      ont_class  = CLASS_BAM_REFERENCE_SEQ;
    }
  else if (!strcmp("RG", type))
    {
      ont_prefix = PREFIX_BAM_READ_GROUP;
      ont_class  = CLASS_BAM_READ_GROUP;
    }
  else if (!strcmp("PG", type))
    {
      ont_prefix = PREFIX_BAM_PROGRAM;
      ont_class  = CLASS_BAM_PROGRAM;
    }
  else if (!strcmp ("CO", type))
    {
      ont_prefix = PREFIX_BAM_COMMENT;
      ont_class  = CLASS_BAM_COMMENT;
    }
  else if (!strcmp ("HD", type))
    {
      ont_prefix = PREFIX_BAM_HEADER;
      ont_class  = CLASS_BAM_HEADER;
    }
  else
    {
      ui_print_bam_unknown_header (line);
      return;
    }

  char *tab              = strchr (line + 3, '\t');
  char *colon            = NULL;
  char *header_id        = NULL;
  raptor_statement *stmt = NULL;

  /* Generate a unique header identifier.
   * ----------------------------------------------------------------------- */

  if (! generate_header_id (origin, config.header_id_buf))
    ui_print_general_memory_error ();
  else
    header_id = config.header_id_buf;

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = term (PREFIX_ORIGIN, (char *)header_id);
  stmt->predicate = term (PREFIX_RDF, "#type");
  stmt->object    = class (ont_class);
  register_statement_reuse_object (stmt);

  stmt = raptor_new_statement (config.raptor_world);
  stmt->subject   = term (PREFIX_ORIGIN, (char *)header_id);
  stmt->predicate = term (PREFIX_MASTER, "originatedFrom");
  stmt->object    = term (PREFIX_ORIGIN, (char *)origin);
  register_statement (stmt);

  if (ont_class != CLASS_BAM_COMMENT)
    {
      while (tab != NULL)
        {
          /* Instead of string-copying, we can gather the key/value pair
           * by terminating the strings at the delimiters. */
          char *key = tab + 1;
          colon = strchr (key, ':');

          /* Every field must have a KEY:VALUE syntax.
           * So when the colon is missing, we cannot parse this field. */
          if (colon == NULL)
            continue;

          *colon = '\0';
          char *value = colon + 1;
          tab = strchr (value, '\t');

          /* For the last key/value pair, tab will be NULL at this point. */
          if (tab != NULL) *tab = '\0';

          /* Add the RDF statements. */
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_ORIGIN, (char *)header_id);
          stmt->predicate = term (ont_prefix, key);

          /* Most values should be interpreted as strings, but the following
           * "whitelisted" entries are interpreted as integers. */
          if (ont_class == CLASS_BAM_REFERENCE_SEQ && !strcmp (key, "LN"))
            stmt->object = literal (value, XSD_INTEGER);
          else
            stmt->object    = literal (value, XSD_STRING);

          register_statement (stmt);
        }
    }
  else if (tab != NULL)
    {
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_ORIGIN, (char *)header_id);
      stmt->predicate = term (PREFIX_BAM_COMMENT, "text");
      stmt->object    = literal (tab + 1, XSD_STRING);
    }
}

void
process_header (bam_hdr_t *bam_header, const unsigned char *origin)
{
  if (!bam_header || !origin) return;

  /* Process header lines.
   * ----------------------------------------------------------------------- */

  char *line = bam_header->text;
  while (line != NULL)
    {
      char *delimiter = strchr (line, '\n');
      if (delimiter != NULL)
        *delimiter = '\0';

      process_header_line (line, origin);

      if (delimiter != NULL)
        line = delimiter + 1;
      else
        line = NULL;
    }

  /* Skip the rest of the triplets when metadata-only mode is enabled. */
  if (config.metadata_only)
    return;

}
