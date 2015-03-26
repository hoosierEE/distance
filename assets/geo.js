// declare a port with default values
const defaultLat = 91.0,
      defaultLon = 181.0,
      defaultHdg = 361.0;
var g = Elm.fullscreen(Elm.WhereBrain, {geo:{lat:defaultLat,lon:defaultLon,hdg:defaultHdg}});


// actually get the values
navigator.geolocation.watchPosition(geoSuccess,geoError);
//navigator.geolocation.watchPosition(geoSuccess,geoError,geoOpts);


// callbacks and options
function geoSuccess(p) {
    var la = p.coords.latitude || defaultLat,
        lo = p.coords.longitude || defaultLon,
        hd = p.coords.heading || defaultHdg;
    // send the values to the Elm program
    g.ports.geo.send({lat:la,lon:lo,hdg:hd});
}
function geoError(e) {
    console.log ('Geolocation error: ' + e.code + ' ( ' + e.message + ' ) ');
}
// disabling these for now; more testing required
// var geoOpts = {
//     enableHighAccuracy: true,
//     maximumAge: 30000,
//     timeout: 27000
// };
