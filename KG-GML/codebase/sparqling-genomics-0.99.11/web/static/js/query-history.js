function toggle_marker (id, queryId) {
  var state = document.getElementById("mark-"+ id).checked;
  jQuery.ajax('/api/query-mark', {
    headers: {
      "Accept": "application/json",
      "Content-Type": "application/json"
    },
    method: 'POST',
    data: JSON.stringify({ "query-id": queryId, "state": state }),
    success: function(data) {
      document.getElementById("mark-"+ id).checked = data.state;
    }
  });
}
