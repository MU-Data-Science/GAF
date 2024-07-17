/* Copyright © 2019  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 */

function split_triplet (line)
{
    var regexp = /[^\s"]+|"([^"]*)"/gi;
    var tokens = [];
    do {
        var match = regexp.exec(line);
        if (match != null) tokens.push(match[0]);
    } while (match != null);

    return tokens;
}

function keyHandler (e)
{
    var TAB = 9;
    var SPACE = 32;
    var BACKSPACE = 8;
    var RETURN = 13;

    switch(e.keyCode) {
    case TAB:
        ac = autocomplete(this.value);
        if (ac) {
            this.value = ac;
        }
        if (e.preventDefault) { e.preventDefault(); }
        break;
    case SPACE:
        //this.value += " ";
        break;
    case BACKSPACE:
        break;
    case RETURN:
        if (e.preventDefault) { e.preventDefault(); }

        var tokens = split_triplet (this.value);
        if (tokens.length > 2) {
            post_data = {
                "prompt-id": jQuery("#prompt-id").val(),
                "subject":   tokens[0],
                "predicate": tokens[1],
                "object":    tokens[2]
            };
            jQuery.ajax ("/api/prompt-add-triplet", {
                headers: {
                    "Accept": "application/json",
                    "Content-Type": "application/json"
                },
                method: "POST",
                data: JSON.stringify(post_data),
                success: function (data) {
                    jQuery("input[name^='prompt-field']").val(tokens[0] + " ");
                    jQuery.get('/prompt-session-table', function(data){
                        jQuery('#prompt-session-table').replaceWith(data);
                    });
                }
            });
        }
        else {
            this.value += " ";
        }
        break;
    default:
        break;
    }

    return true;
}

function commit_session ()
{
    url = window.location.href;
    jQuery.ajax('/api/prompt-commit', {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify({
            "prompt-id": jQuery("#prompt-id").val(),
            "graph": jQuery("#select-graph").val(),
            "project_hash": url.substr(url.lastIndexOf('/') + 1)
        }),
        success: function (data) { location.reload(); }
    });
}

function clear_session ()
{
    var headers = {
        "Accept": "application/json",
        "Content-Type": "application/json"
    };

    jQuery.ajax('/api/prompt-delete', {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify({ "prompt-id": jQuery("#prompt-id").val() }),
    }).done(function(data, textStatus, jqXHR) { location.reload(); });
}

function remove_triplet (triplet_id)
{
    post_data = { "triplet-id": triplet_id };
    jQuery.ajax('/api/prompt-remove-triplet', {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (data) {
            jQuery.get('/prompt-session-table', function(data){
                jQuery('#prompt-session-table').replaceWith(data);
            });
        }
    });
}

function enable_prompt (element)
{
    jQuery("#content").on('keydown', element, keyHandler);
}
