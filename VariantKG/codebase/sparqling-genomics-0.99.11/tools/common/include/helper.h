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

#ifndef HELPER_H
#define HELPER_H

#include <stdbool.h>
#include <stdint.h>

#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>

#define HASH_ALGORITHM GNUTLS_DIG_MD5
#define HASH_ALGORITHM_NAME "md5"
#define HASH_ALGORITHM_PRINT_LENGTH 32

bool get_pretty_hash (unsigned char *hash,
                      uint32_t length,
                      unsigned char *output);

unsigned char *helper_get_hash_from_file (const char *filename);
bool only_contains_whitespace (const char *input, int32_t length);
char *trim_quotes (const char *string, uint32_t length);
char *sanitize_string (const char *string, uint32_t length);

#endif /* HELPER_H */
