function set_command (token, connection_uri, graph_uri, format)
{
    jQuery("#command-box").empty();
    jQuery("#command-box").html(
        '<span class="terminal-prompt">$ </span>curl --cookie \"SGSession=' + token + '\" \\\n' +
        '       --request POST \\\n' +
        '       --header \"Accept: application/xml\" \\\n' +
        '       --header \"Content-Type: '+ format +'\" \\\n' +
        '       --data-urlencode "graph=' + graph_uri + '" \\\n' +
        '       --get '+ connection_uri +'/api/import-rdf \\\n' +
        '       --upload-file <strong>/path/to/your/file</strong>');
}

function set_inital_command ()
{
    jQuery("#command-box").empty();
    jQuery("#command-box").html("A cURL command will be shown here.");
}

function update_command ()
{
    connection_name = "";
    graph_uri = "";
    graph_value = jQuery("#select-graph").val();

    if (graph_value == "") {
        graph_uri = "{graph-uri}";
        connection_uri = "{connection-uri}";
    } else {
        tokens = graph_value.split(" ");
        graph_uri = tokens[0];
        connection_name = tokens[1];

        jQuery.ajax ("/api/connection-by-name", {
            headers: { "Accept": "application/json",
                       "Content-Type": "application/json" },
            method: "POST",
            data: JSON.stringify({ "name": connection_name }),
            success: function (data) {
                token = jQuery("#select-token").val();
                format = jQuery("#select-format").val();

                if (token != "" && format != "")
                    set_command (token, data.uri, graph_uri, format);
                else
                    set_inital_command();
            },
            fail: function(data) {
                jQuery("#command-box").empty();
                jQuery("#command-box").html('<span style="color: #ff0000">Could not gather connection information.</span>');
            }
        });
    }

}

jQuery(document).ready(function(){
    url = window.location.href;
    project_hash = url.substr(url.lastIndexOf('/') + 1);

    set_inital_command();
});
