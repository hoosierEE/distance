// declare a port with default values
const defaultLat = 91.0;
const defaultLon = 181.0;
var g = Elm.fullscreen(Elm.Main, {
    geo: { lat: defaultLat, lon: defaultLon }
});

// callbacks
function geoSuccess(p) {
    var la = p.coords.latitude || defaultLat;
    var lo = p.coords.longitude || defaultLon;
    // send the values to the Elm program
    g.ports.geo.send({lat:la,lon:lo});
}

function geoError(e) {
    console.log ('Geolocation error: ' + e.code + ' ( ' + e.message + ' ) ');
}

var options = {
    timeout: 500,
    maximumAge: 0
};

// actually get the values
navigator.geolocation.watchPosition(geoSuccess,geoError,options);

