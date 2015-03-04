function getLoc() {
    navigator.geolocation.getCurrentPosition(howFar);
}

function howFar(pos) {
    var lat = pos.coords.latitude;
    var lon = pos.coords.longitude;
    // document.getElementById('lat').innerHTML = lat;
    // document.getElementById('lon').innerHTML = lon;
    document.getElementById('dfb').innerHTML = getDistance(lat,lon).toFixed(4) + ' km';
}

//function getDistance(lat2,lon2) {
function getDistance(rawLat,rawLon) {
    // Brain coords;
    // 39.171989, -86.520674
    // get great circle distance
    meanEarthRadius = 6371;
    var d;
    function toRad(deg) {
        return deg * Math.PI / 180;
    }

    with(Math) {
        var lat2 = toRad(rawLat);
        var lon2 = toRad(rawLon);
        var lat1 = toRad(39.171989);
        var lon1 = toRad(-86.520674);
        var dlat = abs(lat2 - lat1);
        var dlon = abs(lon2 - lon1);
        // https://en.wikipedia.org/wiki/Great-circle_distance
        d = acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(dlon));
    }
    return d * meanEarthRadius;
}
