function changeVibe(room, vibe) {
  $.ajax({
    type: "GET",
    url: "vibe" + "/" + room + "/" + vibe,
    beforeSend: function(xhr) {
      xhr.setRequestHeader("Authorization", "Bearer " + localStorage.getItem(API_TOKEN));
      xhr.setRequestHeader("Content-type", "application/json");
    },
    success: function(rooms) {
      renderHouseControls(rooms);
    }
  });
}

function login() {
  var username = document.login.client_id.value;
  var password = document.login.client_secret.value;

  $.ajax({
    type: "GET",
    contentType: "application/json",
    url: "report",
    beforeSend: function(xhr) {
      xhr.setRequestHeader ("Authorization", "Basic " + btoa(username + ":" + password));
    },
    success: function(response) {
      $("#loginDiv").hide();
      var chart = new CanvasJS.Chart("chartContainer", {
        animationEnabled: true,
        title: {
          text: `${response.user}'s Meatbar Consumptions`
        },
        data: [{
          type: "pie",
          startAngle: 240,
          indexLabel: "{label} {y}",
          dataPoints: response.consumptions
        }]
      });
      chart.render();
    }
  });
}
