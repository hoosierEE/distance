function getLoc() {
    navigator.geolocation.getCurrentPosition(showMap);
}

function showMap(pos) {
    var lat = pos.coords.latitude;
    var lon = pos.coords.longitude;
}
