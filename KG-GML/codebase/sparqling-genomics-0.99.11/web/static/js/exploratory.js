function select_connection (connection)
{
    jQuery(".connection-item-active")
        .removeClass('exploratory-item-active')
        .removeClass('connection-item-active');
    jQuery('#graphs').empty();
    jQuery('#types').empty();
    jQuery('#predicates').empty();

    jQuery("#connection-"+ connection)
        .addClass('exploratory-item-active')
        .addClass('connection-item-active');

    url = window.location.href;
    project_hash = url.substr(url.lastIndexOf('/') + 1);
    post_data = { "project-hash": project_hash,
                  "connection": connection };

    jQuery.ajax("/api/graphs-by-project", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (graphs) {
            if (Object.keys(graphs).length == 0)
                jQuery('#graphs').append('<p>No graphs found.</p>');
            else {
                graphs.map (function (data) {
                    jQuery('#graphs').append(
                        '<div id="graph-'+ base32.encode(data.graph) +'" class="exploratory-item"'+
                            ' onclick="javascript:select_graph(\''+ base32.encode(data.graph) +'\');' +
                            ' return false;">'+ data.graph +'</div>');
                });
            }
        }
    });
}

function with_children_types (connection, graph, type, callback)
{
    url = window.location.href;
    project_hash = url.substr(url.lastIndexOf('/') + 1);
    post_data = { "project-hash": project_hash,
                  "connection": connection,
                  "graph":      graph,
                  "type":       type };

    jQuery.ajax("/api/children-by-type", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        async: false,
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (types) { callback(types, type); }
    });
}

function select_graph (graph)
{
    jQuery(".graph-item-active")
        .removeClass('exploratory-item-active')
        .removeClass('graph-item-active');
    jQuery('#types').empty();
    jQuery('#predicates').empty();

    jQuery("#graph-"+ graph)
        .addClass('exploratory-item-active')
        .addClass('graph-item-active');

    url = window.location.href;
    post_data = { "connection": jQuery(".connection-item-active").text(),
                  "graph": base32.decode(graph),
                  "project-hash": url.substr(url.lastIndexOf('/') + 1)
                };

    jQuery.ajax("/api/root-types-by-graph", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (types) {
            if (Object.keys(types).length == 0)
                jQuery('#types').append('<p>No types found.</p>');
            else {
                for (var index = 0, len = types.length; index < len; index++) {
                    with_children_types (post_data["connection"], post_data["graph"], types[index],
                                         function (ctypes, type) {
                        if (ctypes.length > 0) {
                            encoded_children = base32.encode(JSON.stringify(ctypes));
                            jQuery('#types').append('<div id="type-'+ base32.encode(type) +'" class="exploratory-item"' +
                                                    ' onclick="javascript:select_hierarchical_type(\''+ base32.encode(type) +
                                                    '\', \''+ encoded_children +'\');' +
                                                    ' return false;"><span class="arrow-spacer">▸</span>' + type +'</div>');
                        } else {
                            jQuery('#types').append('<div id="type-'+ base32.encode(type) +'" class="exploratory-item"'+
                                                    ' onclick="javascript:select_type(\''+ base32.encode(type) +'\');' +
                                                    ' return false;"><span class="arrow-spacer"></span>' + type +'</div>');
                        }
                      });
                }
            }
        }
    });
}

function select_hierarchical_type (encoded_type, encoded_children)
{
    // Display the predicates of the root type.
    select_type (encoded_type);

    child_item = document.querySelector('#type-' + encoded_type);
    if (child_item.classList.contains('collapsed-item')) {
        return;
    }

    // Add children types after the root type.
    type         = base32.decode(encoded_type);
    children     = JSON.parse(base32.decode(encoded_children));
    connection   = jQuery(".connection-item-active").text();
    graph        = jQuery(".graph-item-active").text();

        jQuery('#type-'+ encoded_type)
            .addClass("collapsed-item")
            .html('<span class="arrow-spacer">▾</span>' + type);

    children.map(function (child) {
        with_children_types (post_data["connection"], post_data["graph"], child,
           function (types) {
               encoded_child = base32.encode(child);
               if (types.length > 0) {
                   encoded_children = base32.encode(JSON.stringify(types));
                   jQuery('#type-'+ encoded_type)
                       .after('<div id="type-'+ encoded_child +'" class="exploratory-child-item"'+
                              ' onclick="javascript:select_hierarchical_type(\''+ encoded_child +
                              '\', \''+ encoded_children +'\'); return false;">' +
                              '<span class="arrow-spacer">▸</span>'+ child +'</div>');
               }
               else {
                   jQuery('#type-'+ encoded_type)
                       .after('<div id="type-'+ encoded_child +'" class="exploratory-child-item"'+
                              ' onclick="javascript:select_type(\''+ encoded_child +'\');' +
                              ' return false;"><span class="arrow-spacer"></span>' + child +'</div>');
               }

               // Dynamically indent the children relative to their parent.
               parent_margin = jQuery('#type-'+ encoded_type).css("marginLeft");
               jQuery('#type-'+ encoded_child).css("marginLeft", parent_margin);
               jQuery('#type-'+ encoded_child).css("marginLeft", "+=10pt");
           });
    });
}

function select_type (type)
{
    child_item = document.querySelector('#type-' + type);
    if (! child_item.classList.contains('exploratory-child-item')) {
        collapsed_item = document.querySelector('.collapsed-item');
        if (collapsed_item !== null) {
            previous_id = collapsed_item.id;
            previous_type = previous_id.slice(5);

            jQuery("#"+ previous_id)
                .html('<span class="arrow-spacer">▸</span>' + base32.decode(previous_type))
                .removeClass("collapsed-item");

            jQuery(".exploratory-child-item").remove();
        }
    }

    jQuery(".type-item-active")
        .removeClass('exploratory-item-active')
        .removeClass('type-item-active');
    jQuery('#predicates').empty();

    jQuery("#type-"+ type)
        .addClass('exploratory-item-active')
        .addClass('type-item-active');

    url = window.location.href;
    post_data = { "connection": jQuery(".connection-item-active").text(),
                  "graph": jQuery(".graph-item-active").text(),
                  "project-hash": url.substr(url.lastIndexOf('/') + 1),
                  "type": base32.decode(type) };

    jQuery.ajax("/api/predicates-by-type", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (predicates) {
            if (Object.keys(predicates).length == 0)
                jQuery('#predicates').append('<p>No predicates found.</p>');
            else {
                predicates.map(function (p){
                    jQuery('#predicates').append(
                        '<div id="predicate-'+ base32.encode(p.predicate) +
                        '" class="exploratory-item">'+ p.predicate +'</div>');
                });
            }
        }
    });
}

function clear_cache () {
    
}

jQuery(document).ready(function(){
    jQuery.ajax("/api/connections", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        success: function (connections) {
            connections.map(function (data){
                jQuery('#connections').append(
                    '<div id="connection-'+ data.name +'" class="exploratory-item"'+
                    ' onclick="javascript: select_connection(\''+ data.name +'\');'+
                    ' return false;">'+ data.name +'</div>');
            });
        }
    });
});
