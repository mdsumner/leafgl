addDeckglPolygons_file = function(map,
                             data,
                             fillColor = "#0033ff",
                             fillOpacity = 0.8,
                             radius = 10,
                             group = "deckglpoints",
                             popup = NULL,
                             label = NULL,
                             layerId = NULL,
                             src = FALSE,
                             pane = "overlayPane",
                             ...) {

  dotopts = list(...)

 # geom_colname = attr(data, which = "sf_colum")

  #if (is.null(layerId)) layerId = group
  #layerId = gsub("[[:punct:] ]", "_", layerId)
  layerId <- "deckglpoints"
  radius_column = FALSE #inherits(radius, "character") && radius %in% colnames(data)
  min_rad = radius #ifelse(radius_column, min(data[[radius]], na.rm = TRUE), 10)
  max_rad = radius #ifelse(radius_column, max(data[[radius]], na.rm = TRUE), 10)


  ## currently we only support single (fill)opacity!
  fillOpacity = fillOpacity[1]

  if (is.null(group)) group = "group" #deparse(substitute(data))
  args <- list(...)

  fillColor = jsonify::to_json(c(as.vector(col2rgb(fillColor)), fillOpacity * 255))

  # popup
  if (!is.null(popup)) {
    htmldeps <- htmltools::htmlDependencies(popup)
    if (length(htmldeps) != 0) {
      map$dependencies = c(
        map$dependencies,
        htmldeps
      )
    }
    popup = makePopup(popup, data)
    popup = jsonify::to_json(popup)
  } else {
    popup = NULL
  }


  path_layer = data #tempfile()
  geom_colname <- "geometry" # attr(data, "sf_column")

  # dependencies
  map$dependencies = c(
    map$dependencies
    , arrowDependencies()
    , geoarrowjsDependencies()
    , deckglDependencies()
    , geoarrowDeckglLayersDependencies()
    , deckglLeafletDependencies()
    , deckglDataAttachmentSrc(path_layer, layerId)
    , deckglBindingDependencies()
    , chromajsDependencies()
  )


  map = leaflet::invokeMethod(
    map
    , leaflet::getMapData(map)
    , 'addDeckglPolygons'
    , geom_colname
    , fillColor
    , popup
    , label
    , fillOpacity
    , radius
    , min_rad
    , max_rad
    , group
    , layerId
    , dotopts
    , pane
  )
bounds <- c(-84.3, -75.5,  34,  37)[c(1, 3, 2, 4)]
  leaflet::expandLimits(
    map,
    c(bounds[2], bounds[4]),
    c(bounds[1], bounds[3])
  )
}
