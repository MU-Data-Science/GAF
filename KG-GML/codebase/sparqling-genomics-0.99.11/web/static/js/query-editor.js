var editor = null;

function execute_query (editor) {

    url = window.location.href;
    project_hash = url.substr(url.lastIndexOf('/') + 1);

    jQuery("#execute-query-button").after(function(){ return "<div class=\"query-data-loader\"><div class=\"title\">Loading data ...</div><div class=\"content\">Please wait for the results to appear.</div></div>" });

    /* Remove the previous query results. */
    jQuery(".query-error").remove();
    jQuery("#query-results").remove();
    jQuery("#query-output").remove();
    jQuery("#query-output_wrapper").remove();

    post_data = { query: editor.getValue(), connection: jQuery("#connection").val() };
    jQuery.post("/query-response/" + project_hash,
      JSON.stringify(post_data),
      function (data) {
          /* Insert the results HTML table into the page. */
          jQuery("#execute-query-button").after(data);
          jQuery(".query-data-loader").remove();
          jQuery("#note-five-thousand").remove();

          /* Detect an error response. */
          if (jQuery(".query-error").length == 0) {
              jQuery("#execute-query-button").after(function() {
                  return "<h3 id=\"query-results\">Query results</h3><p id=\"note-five-thousand\"><strong>Note:</strong> Query results are limited to a maximum of 5000 rows.  Programmatic access does not have this limitation.</p>" });

              /* Initialize DataTables. */
              jQuery("#query-output").addClass("display");
              var dt = jQuery("#query-output").DataTable({ "sDom": "lrtip", "aaSorting": [] });
              dt.draw();

              jQuery.get("/query-history/" + project_hash, function (data){
                  jQuery("#query-history-table").replaceWith(data);
              });
          }
      });
}

jQuery(document).ready(function() {

  editor = ace.edit("editor");
  var session = editor.getSession();
  editor.setTheme("ace/theme/crimson_editor");
  editor.setShowPrintMargin(false);
  editor.setAutoScrollEditorIntoView(true);
  editor.setOptions({ maxLines: 120,
                      minLines: 2,
                      enableBasicAutocompletion: true,
                      enableLiveAutocompletion: true });
  session.setMode("ace/mode/sparql");
  session.setTabSize(2);

  /* Add keybindings for copying the text and for running the query. */
  editor.commands.addCommand({
    name: "copyCommand",
    bindKey: {win: "Ctrl-C",  mac: "Command-C"},
    exec: function(editor) {
      jQuery("#content").after("<textarea id=\"copyText\"></textarea>");
      var temp = document.getElementById("copyText");
      temp.value = editor.getSelectedText();
      temp.select();
      document.execCommand("copy");
      temp.remove();
      jQuery(".ace_text-input").focus();
      }, readOnly: true
    });

  editor.commands.addCommand({
    name: "executeQueryCommand",
    bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
      exec: execute_query, readOnly: true
    });
});
