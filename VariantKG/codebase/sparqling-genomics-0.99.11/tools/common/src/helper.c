/*
 * Copyright (C) 2018, 2019  Roel Janssen <roel@gnu.org>
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

#include "helper.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gnutls/crypto.h>

#define HASH_ERROR_MESSAGE "Hashing failed.\n"

bool
get_pretty_hash (unsigned char *hash, uint32_t length, unsigned char *output)
{
  if (output == NULL) return false;

  const char *numbers[] = {
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
    "0a", "0b", "0c", "0d", "0e", "0f", "10", "11", "12", "13",
    "14", "15", "16", "17", "18", "19", "1a", "1b", "1c", "1d",
    "1e", "1f", "20", "21", "22", "23", "24", "25", "26", "27",
    "28", "29", "2a", "2b", "2c", "2d", "2e", "2f", "30", "31",
    "32", "33", "34", "35", "36", "37", "38", "39", "3a", "3b",
    "3c", "3d", "3e", "3f", "40", "41", "42", "43", "44", "45",
    "46", "47", "48", "49", "4a", "4b", "4c", "4d", "4e", "4f",
    "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
    "5a", "5b", "5c", "5d", "5e", "5f", "60", "61", "62", "63",
    "64", "65", "66", "67", "68", "69", "6a", "6b", "6c", "6d",
    "6e", "6f", "70", "71", "72", "73", "74", "75", "76", "77",
    "78", "79", "7a", "7b", "7c", "7d", "7e", "7f", "80", "81",
    "82", "83", "84", "85", "86", "87", "88", "89", "8a", "8b",
    "8c", "8d", "8e", "8f", "90", "91", "92", "93", "94", "95",
    "96", "97", "98", "99", "9a", "9b", "9c", "9d", "9e", "9f",
    "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9",
    "aa", "ab", "ac", "ad", "ae", "af", "b0", "b1", "b2", "b3",
    "b4", "b5", "b6", "b7", "b8", "b9", "ba", "bb", "bc", "bd",
    "be", "bf", "c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7",
    "c8", "c9", "ca", "cb", "cc", "cd", "ce", "cf", "d0", "d1",
    "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "da", "db",
    "dc", "dd", "de", "df", "e0", "e1", "e2", "e3", "e4", "e5",
    "e6", "e7", "e8", "e9", "ea", "eb", "ec", "ed", "ee", "ef",
    "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9",
    "fa", "fb", "fc", "fd", "fe", "ff"
  };

  uint32_t i = 0;
  for (; i < length; i++)
    memcpy (output + (i * 2), numbers[hash[i]], 2);

  output[length * 2] = '\0';

  return true;
}

unsigned char *
helper_get_hash_from_file (const char *filename)
{
  const size_t READ_BUFFER_MAX_LENGTH = 4000000;
  const int HASH_LENGTH = gnutls_hash_get_len (HASH_ALGORITHM);

  /* Initialize GnuTLS.
   * ------------------------------------------------------------------------ */
  int error;
  gnutls_hash_hd_t handler;
  unsigned char binary_digest[HASH_LENGTH];

  error = gnutls_hash_init (&handler, HASH_ALGORITHM);
  if (error < 0)
    {
      fprintf (stderr, "ERROR: Cannot initialize GnuTLS hash function.\n");
      return NULL;
    }

  /* Open the file.
   * ------------------------------------------------------------------------ */
  FILE *file = fopen (filename, "rb");
  char *buffer = NULL;

  if (file == NULL)
    {
      fprintf (stderr, "ERROR: Cannot open '%s'.\n", filename);
      gnutls_hash_deinit (handler, NULL);
      return NULL;
    }

  buffer = calloc (sizeof (char), READ_BUFFER_MAX_LENGTH);

  if (buffer == NULL)
    {
      fprintf (stderr, "ERROR: Not enough memory available for processing '%s'.\n", filename);
      gnutls_hash_deinit (handler, NULL);
      fclose (file);
      return NULL;
    }

  /* Process the file.
   * ------------------------------------------------------------------------ */
  size_t bytes_read = 0;
  bytes_read = fread (buffer, sizeof (char), READ_BUFFER_MAX_LENGTH, file);
  while (bytes_read == READ_BUFFER_MAX_LENGTH)
    {
      error = gnutls_hash (handler, buffer, bytes_read);
      if (error < 0)
        {
          fprintf (stderr, HASH_ERROR_MESSAGE);
          return NULL;
        }

      bytes_read = fread (buffer, sizeof (char), READ_BUFFER_MAX_LENGTH, file);
    }

  error = gnutls_hash (handler, buffer, bytes_read);
  if (error < 0)
    {
      fprintf (stderr, HASH_ERROR_MESSAGE);
      return NULL;
    }

  fclose (file);
  memset (buffer, 0, READ_BUFFER_MAX_LENGTH * sizeof (char));
  bytes_read = 0;
  free (buffer);
  buffer = NULL;

  unsigned char *pretty_digest = calloc (sizeof (char), (HASH_LENGTH * 2) + 1);
  if (!pretty_digest)
    {
      gnutls_hash_deinit (handler, NULL);
      return NULL;
    }

  gnutls_hash_output (handler, binary_digest);
  if (!get_pretty_hash (binary_digest, HASH_LENGTH, pretty_digest))
    {
      fprintf (stderr, "ERROR: Couldn't print a hash.\n");
      gnutls_hash_deinit (handler, NULL);
      return NULL;
    }

  gnutls_hash_deinit (handler, NULL);
  return pretty_digest;
}

/* This function replaces non-alphanumeric characters with
 * underscores, and all uppercase characters with their lowercase
 * equivalent. */
char *
sanitize_string (const char *string, uint32_t length)
{
  if (string == NULL || length < 1)
    return NULL;

  char *output = strdup (string);
  if (output == NULL)
    return NULL;

  uint32_t index = 0;
  for (; index < length; index++)
    {
      if (! isalnum(output[index]))
        output[index] = '_';
      else
        output[index] = tolower (output[index]);
    }

  return output;
}

/* This function removes single quotes and double quotes
 * in the first, and last position of a string. */
char *
trim_quotes (const char *string, uint32_t length)
{
  if (string == NULL || length < 1)
    return NULL;

  char *output = strdup (string);
  if (output == NULL)
    return NULL;

  if (output[length - 1] == '"' || output[length - 1] == '\'')
    output[length - 1] = '\0';

  if (output[0] == '"' || output[0] == '\'')
    {
      uint32_t index = 1;
      for (; index < length; index++)
        output[index - 1] = output[index];

      output[length - 1] = '\0';
    }

  return output;
}

bool
only_contains_whitespace (const char *input, int32_t length)
{
  if (input == NULL)
    return false;

  int position = 0;
  for (; position < length; position++)
    if (! isspace (input[position]))
      return false;

  return true;
}
