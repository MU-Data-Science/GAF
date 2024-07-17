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

#ifndef UI_H
#define UI_H

#include <stdint.h>
#include <stdbool.h>

/*----------------------------------------------------------------------------.
 | GENERAL UI STUFF                                                           |
 '----------------------------------------------------------------------------*/

void ui_show_help (void);
void ui_show_version (void);
void ui_process_command_line (int argc, char **argv);

/*----------------------------------------------------------------------------.
 | ERROR HANDLING                                                             |
 '----------------------------------------------------------------------------*/

int32_t ui_print_vcf_file_error (const char *file_name);
int32_t ui_print_vcf_header_error (const char *file_name);
int32_t ui_print_general_memory_error (void);
int32_t ui_print_file_format_error (void);
int32_t ui_print_redland_error (void);

/*----------------------------------------------------------------------------.
 | WARNING HANDLING                                                           |
 '----------------------------------------------------------------------------*/

void ui_show_missing_options_warning (void);

#endif /* UI_H */
