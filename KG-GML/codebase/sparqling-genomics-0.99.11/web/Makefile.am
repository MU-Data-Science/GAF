# Copyright © 2017, 2018, 2019 Roel Janssen <roel@gnu.org>
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Affero General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

include guile.am

SOURCES =                                               \
  auth-manager/api.scm                                  \
  auth-manager/config-reader.scm                        \
  auth-manager/config.scm                               \
  auth-manager/permission-check.scm                     \
  auth-manager/proxy.scm                                \
  auth-manager/virtuoso.scm                             \
  json.scm                                              \
  json/builder.scm                                      \
  json/parser.scm                                       \
  json/syntax.scm                                       \
  ldap/authenticate.scm                                 \
  logger.scm                                            \
  sparql/driver.scm                                     \
  sparql/lang.scm                                       \
  sparql/md5.scm                                        \
  sparql/parser.scm                                     \
  sparql/stream.scm                                     \
  sparql/util.scm                                       \
  www/base64.scm                                        \
  www/components/project-graphs.scm                     \
  www/components/query-history.scm                      \
  www/components/rdf-stores.scm                         \
  www/components/sessions.scm                           \
  www/config-reader.scm                                 \
  www/config.scm                                        \
  www/db/api.scm                                        \
  www/db/cache.scm                                      \
  www/db/connections.scm                                \
  www/db/exploratory.scm                                \
  www/db/graphs.scm                                     \
  www/db/projects.scm                                   \
  www/db/prompt.scm                                     \
  www/db/queries.scm                                    \
  www/db/sessions.scm                                   \
  www/html.scm                                          \
  www/pages.scm                                         \
  www/pages/automate.scm                                \
  www/pages/collect.scm                                 \
  www/pages/connection-failure.scm                      \
  www/pages/create-project.scm                          \
  www/pages/dashboard.scm                               \
  www/pages/edit-connection.scm                         \
  www/pages/error.scm                                   \
  www/pages/exploratory.scm                             \
  www/pages/import.scm                                  \
  www/pages/login.scm                                   \
  www/pages/project-details.scm                         \
  www/pages/prompt-session-table.scm                    \
  www/pages/prompt.scm                                  \
  www/pages/query-history.scm                           \
  www/pages/query-response.scm                          \
  www/pages/query.scm                                   \
  www/pages/report.scm                                  \
  www/pages/structure.scm                               \
  www/requests-api.scm                                  \
  www/requests-beacon.scm                               \
  www/requests.scm                                      \
  www/util.scm

WWW_STATIC_RESOURCES =                                  \
  static/css/datatables.min.css                         \
  static/css/main.css                                   \
  static/fonts/FiraMono-Bold.ttf                        \
  static/fonts/FiraMono-Regular.ttf                     \
  static/fonts/Roboto-Bold.ttf                          \
  static/fonts/Roboto-Light.ttf                         \
  static/fonts/Roboto-LightItalic.ttf                   \
  static/images/icons/check-white.png                   \
  static/images/icons/check-white.svg                   \
  static/images/icons/check.png                         \
  static/images/icons/check.svg                         \
  static/images/icons/checkmark.svg                     \
  static/images/icons/locked.png                        \
  static/images/icons/locked.svg                        \
  static/images/icons/plus.png                          \
  static/images/icons/plus.svg                          \
  static/images/icons/return-white.png                  \
  static/images/icons/return-white.svg                  \
  static/images/icons/return.png                        \
  static/images/icons/return.svg                        \
  static/images/icons/save.png                          \
  static/images/icons/save.svg                          \
  static/images/icons/unlocked.png                      \
  static/images/icons/unlocked.svg                      \
  static/images/icons/up-white.png                      \
  static/images/icons/up-white.svg                      \
  static/images/icons/up.png                            \
  static/images/icons/up.svg                            \
  static/images/icons/x-white.png                       \
  static/images/icons/x-white.svg                       \
  static/images/icons/x.png                             \
  static/images/icons/x.svg                             \
  static/images/logo.png                                \
  static/images/sort_asc.png                            \
  static/images/sort_asc_disabled.png                   \
  static/images/sort_both.png                           \
  static/images/sort_desc.png                           \
  static/js/ace/ace.js                                  \
  static/js/ace/ext-language_tools.js                   \
  static/js/ace/mode-sparql.js                          \
  static/js/ace/theme-crimson_editor.js                 \
  static/js/d3.min.js                                   \
  static/js/exploratory.js                              \
  static/js/import.js                                   \
  static/js/jquery-3.2.1.min.js                         \
  static/js/jquery.dataTables.min.js                    \
  static/js/plottable-query.js                          \
  static/js/prompt.js                                   \
  static/js/query-editor.js                             \
  static/js/query-history.js

bin_SCRIPTS     = sg-web sg-auth-manager sg-web-test
webdir          = $(pkgdatadir)/web
nobase_web_DATA = $(WWW_STATIC_RESOURCES)
webconf_DATA    = sg-web.xml sg-auth-manager.xml
webconfdir      = @webconfdir@
EXTRA_DIST     += $(WWW_STATIC_RESOURCES) sg-web.service.in $(webconf_DATA)

systemdservicedir = $(libdir)/systemd/system
nodist_systemdservice_DATA = sg-web.service
CLEANFILES     += $(nodist_systemdservice_DATA)
