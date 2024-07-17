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

#ifndef VCF_VARIANTS_H
#define VCF_VARIANTS_H

#include <raptor2.h>
#include <htslib/vcf.h>

void process_variant (bcf_hdr_t *header, bcf1_t *buffer, raptor_term *origin,
                      const unsigned char *origin_str);
void build_field_identities (bcf_hdr_t *header);

#endif /* VCF_VARIANTS_H */

