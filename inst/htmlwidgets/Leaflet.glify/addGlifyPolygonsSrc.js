LeafletWidget.methods.addGlifyPolygonsSrc = function(fillColor, fillOpacity, group, layerId) {

  var map = this;

  // color
  var clrs;
  if (fillColor === null) {
    clrs = function(index, feature) { return col[group][0][index]; };
  } else {
    clrs = fillColor;
  }

  var pop;
  if (typeof(popup) === "undefined") {
    pop = null;
  } else {
    pop = function (e, feature) {
      if (map.hasLayer(shapeslayer.glLayer)) {
        var idx = data[group][0].features.findIndex(k => k==feature);
        L.popup()
          .setLatLng(e.latlng)
          .setContent(popup[group][0][idx].toString())
          .openOn(map);
      }
    };
  }

  var shapeslayer = L.glify.shapes({
    map: map,
    click: pop,
    data: data[group][0],
    color: clrs,
    opacity: fillOpacity,
    className: group
  });

  map.layerManager.addLayer(shapeslayer.glLayer, null, null, group);

};
