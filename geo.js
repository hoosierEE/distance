var g = Elm.fullscreen(Elm.WhereBrain, {geo:{lat:0,lon:0,hdg:0}});

navigator.geolocation.watchPosition(function(p) {
    var la = p.coords.latitude || 91,
        lo = p.coords.longitude || 91,
        hd = p.coords.heading || 361;
    g.ports.geo.send({lat:la,lon:lo,hdg:hd});
});
