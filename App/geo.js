function getLoc() {
    var watchID = navigator.geolocation.watchPosition(howFar, geoErrorHandler);
    return watchID;
}

function howFar(pos) {
    var lat = pos.coords.latitude;
    var lon = pos.coords.longitude;
    return getDistance(lat,lon);
    // document.getElementById('dfb').innerHTML = getDistance(lat,lon).toFixed(4) + ' km';
}

function geoErrorHandler(error) {
    var sError = "";
    switch(error.code) {
        case error.PERMISSION_DENIED:
            sError = "No permission for Geolocation";
            break;
        case error.POSITION_UNAVAILABLE:
            sError = "Unable to get Geolocation position.";
            break;
        case error.TIMEOUT:
            sError = "Request for Geolocation timed out.";
            break;
        case error.UNKNOWN_ERROR:
            sError = "Unknown error with Geolocation.";
            break;
    }
    // document.getElementById('log').innerHTML = sError;
}

function getDistance(rawLat,rawLon) {
    // return great circle distance, in meters
    var meanEarthRadius = 6371 * 1000;
    var d;
    function toRad(deg) { return deg*Math.PI/180; }
    with(Math) {
        var lat2 = toRad(rawLat);
        var lon2 = toRad(rawLon);
        // the brain coordinates: 39.171989, -86.520674
        var lat1 = toRad(39.171989);
        var lon1 = toRad(-86.520674);
        var dlat = abs(lat2-lat1);
        var dlon = abs(lon2-lon1);
        // https://en.wikipedia.org/wiki/Great-circle_distance
        d = acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(dlon));
    }
    return d * meanEarthRadius;
}


/* possible additions */
////////////////////////

// // return true if dist < 1m
// function touchstone(dist) { return false; }

// // enable sharing
// function share(level) { return {}; }

// // other units
// function otherUnits(dist, unit) { return 0; }

// // history (be careful not to scare people)
// function appendToHistory(newDistanceRecording) { return {}; }

// window.onload = getLoc;
