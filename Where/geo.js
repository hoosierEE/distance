var stamps = Elm.fullscreen(Elm.Stamps, {geo:{"lat":0,"lon":0,"hdg":0}});

navigator.geolocation.watchPosition(function(p) {
    var la = p.coords.latitude,
        lo = p.coords.longitude,
        hd = p.coords.heading || 0;
stamps.ports.geo.send({lat:la,lon:lo,hdg:hd});
});

