library(terra)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(rsconnect)
library(leaflet.extras)
library(shinyjs)
library(signal)
library(shinyalert)


extract <- terra::extract


# CONFIGURACIÓN BASE
is_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
default_base <- normalizePath(".", winslash = "/", mustWork = TRUE)
BASE <- Sys.getenv("BASE_DIR", unset = default_base)
if (is_windows) {
  win_base <- "D:/OneDrive - CGIAR/Documents/Zamorano/Indices_automatizados/SMA"
  if (dir.exists(win_base)) BASE <- win_base
}

# Shapes
SHAPE_PATH <- file.path(BASE, "shapes", "Zamorano", "Zamorano_Shape.shp")
SHAPE_VARIEDADES_PATH <- file.path(
  BASE, "shapes", "Parcela_Variedades_Shape", "Parcela_Variedades.shp"
)

CENTER <- list(lon = -86.9974762, lat = 14.0066746, zoom = 16)


# Rutas índices
rutas_indices <- list(
  GCI       = file.path(BASE, "output", "GCI_tif"),
  LAI       = file.path(BASE, "output", "LAI_tif"),
  NDVI      = file.path(BASE, "output", "NDVI_tif"),
  NDWI      = file.path(BASE, "output", "NDWI_tif"),
  EVI       = file.path(BASE, "output", "EVI_tif"),
  MSAVI2    = file.path(BASE, "output", "MSAVI_tif"),
  GNDVI     = file.path(BASE, "output", "GNDVI_tif")
)


# PALETAS E INFO
paletas_colores <- list(
  LAI    = colorNumeric(palette = c("#A0522D", "#F4A460", "#9ACD32", "#32CD32", "#228B22","#004d00"), domain = NULL, na.color = "transparent"),
  NDVI   = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent"),
  NDWI   = colorNumeric(palette = c("#d13415", "#f5d856", "#9fcf70", "#388a47", "#74b0d6"), domain = NULL, na.color = "transparent"),
  GCI    = colorNumeric(palette = c("#A0522D", "#F4A460", "#9ACD32", "#32CD32", "#228B22","#004d00"), domain = NULL, na.color = "transparent"),
  EVI    = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent"),
  MSAVI2 = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent"),
  GNDVI  = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent")
)

info_indices <- list(
  LAI    = "The Leaf Area Index (LAI) quantifies the amount of foliage in a canopy. It is used to estimate biomass and crop health.",
  NDVI   = "The Normalized Difference Vegetation Index (NDVI) is an indicator of photosynthetically active biomass, used to analyze plant health and detect abnormal growth changes.",
  NDWI   = "The Normalized Difference Water Index (NDWI) is used to detect water bodies and moisture in vegetation.",
  GCI    = "The Green Chlorophyll Index (GCI) estimates chlorophyll content in vegetation, helping to assess nutritional status.",
  EVI    = "The Enhanced Vegetation Index (EVI) measures canopy structure and vegetation vigor.",
  MSAVI2 = "The Modified Soil-Adjusted Vegetation Index 2 (MSAVI2) measures vegetation greenness while minimizing the influence of bare soil.",
  GNDVI  = "The Green Normalized Difference Vegetation Index (GNDVI) uses the green band to estimate chlorophyll content."
)

crear_leyenda_discreta <- function(indice) {
  if (indice == "NDVI") {
    etiquetas <- c("Soil/Water", "Very Low", "Low", "Moderately Low", "Moderately High", "High")
    colores   <- c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907")
    list(etiquetas = etiquetas, colores = colores)
  } else NULL
}

date_from_filename <- function(fname) {
  m <- regexpr("\\d{4}-\\d{2}-\\d{2}", fname)
  if (m[1] < 0) return(NA)
  as.Date(substr(fname, m[1], m[1] + attr(m, "match.length") - 1))
}

safe_range <- function(rango) {
  if (is.null(rango)) return(c(NA, NA))
  if (is.list(rango)) c(as.Date(rango[[1]]), as.Date(rango[[2]])) else as.Date(rango)
}


# Mostrar raster promedio
mostrar_raster <- function(indice, fecha_rango, rutas_indices) {
  rf <- safe_range(fecha_rango)
  fecha_inicio <- rf[1]; fecha_fin <- rf[2]
  ruta_rasters <- rutas_indices[[indice]]
  archivos_raster <- list.files(ruta_rasters, full.names = TRUE, pattern = "\\.(tif|nc)$", ignore.case = TRUE)
  if (!length(archivos_raster)) return(NULL)
  ffechas <- sapply(basename(archivos_raster), date_from_filename)
  sel <- !is.na(ffechas) & ffechas >= fecha_inicio & ffechas <= fecha_fin
  archivos_sel <- archivos_raster[sel]
  if (!length(archivos_sel)) return(NULL)
  rs <- rast(archivos_sel)
  mean(rs, na.rm = TRUE)
}

obtener_resultados_poligono <- function(puntos, indice, fecha_rango, rutas_indices) {
  req(puntos, indice, fecha_rango)
  polygon_sf <- vect(cbind(puntos$x, puntos$y), type = "polygons")
  crs(polygon_sf) <- "EPSG:4326"
  rf <- safe_range(fecha_rango)
  fecha_inicio <- rf[1]; fecha_fin <- rf[2]
  indice_analisis <- indice
  ruta_rasters    <- rutas_indices[[indice_analisis]]
  archivos        <- list.files(ruta_rasters, full.names = TRUE, pattern = "\\.(tif|nc)$", ignore.case = TRUE)
  resultados       <- data.frame(fecha = character(), promedio = numeric(), stringsAsFactors = FALSE)
  valores_globales <- numeric(0)
  for (archivo in archivos) {
    f <- date_from_filename(basename(archivo))
    if (is.na(f) || f < fecha_inicio || f > fecha_fin) next
    r <- rast(archivo)
    vals_df <- extract(r, polygon_sf)
    if (is.null(vals_df) || !nrow(vals_df)) next
    col_vals <- setdiff(colnames(vals_df), "ID")
    if (!length(col_vals)) next
    vals_vec <- suppressWarnings(as.numeric(vals_df[[col_vals[1]]]))
    vals_vec <- vals_vec[!is.na(vals_vec)]
    if (length(vals_vec)) {
      prom <- mean(vals_vec)
      valores_globales <- c(valores_globales, vals_vec)
      resultados <- rbind(resultados, data.frame(fecha = as.character(f), promedio = prom))
    }
  }
  if (length(valores_globales)) {
    prom_g <- mean(valores_globales)
    resultados <- rbind(data.frame(fecha = "Global Average", promedio = round(prom_g, 2)), resultados)
  }
  resultados
}

obtener_resultados_lote <- function(variedad, indice, fecha_rango, rutas_indices, lotes_sf) {
  req(variedad, indice, fecha_rango)
  rf <- safe_range(fecha_rango)
  fecha_inicio <- rf[1]; fecha_fin <- rf[2]
  indice_analisis <- indice
  ruta_rasters    <- rutas_indices[[indice_analisis]]
  archivos        <- list.files(ruta_rasters, full.names = TRUE, pattern = "\\.(tif|nc)$", ignore.case = TRUE)
  lote_sf <- lotes_sf[lotes_sf$Variedad == variedad, ]
  if (nrow(lote_sf) == 0) return(data.frame(fecha = character(), promedio = numeric()))
  lote_geom <- vect(lote_sf)
  resultados       <- data.frame(fecha = character(), promedio = numeric(), stringsAsFactors = FALSE)
  valores_globales <- numeric(0)
  for (archivo in archivos) {
    f <- date_from_filename(basename(archivo))
    if (is.na(f) || f < fecha_inicio || f > fecha_fin) next
    r <- rast(archivo)
    vals_df <- extract(r, lote_geom)
    if (is.null(vals_df) || !nrow(vals_df)) next
    col_vals <- setdiff(colnames(vals_df), "ID")
    if (!length(col_vals)) next
    vals_vec <- suppressWarnings(as.numeric(vals_df[[col_vals[1]]]))
    vals_vec <- vals_vec[!is.na(vals_vec)]
    if (length(vals_vec)) {
      prom <- mean(vals_vec)
      valores_globales <- c(valores_globales, vals_vec)
      resultados <- rbind(resultados, data.frame(fecha = as.character(f), promedio = prom))
    }
  }
  if (length(valores_globales)) {
    prom_g <- mean(valores_globales)
    resultados <- rbind(data.frame(fecha = "Global Average", promedio = round(prom_g, 2)), resultados)
  }
  resultados
}

suavizar_serie <- function(serie, window_size = 5, poly_order = 2) {
  if (length(serie) >= window_size) sgolayfilt(serie, p = poly_order, n = window_size) else serie
}

binEdgesByIndex <- list(
  NDVI = seq(-1, 1, by = 0.1),
  NDWI = seq(-1, 1, by = 0.1),
  GCI  = 0:10,
  LAI  = 0:10,
  GNDVI = seq(-1, 1, by = 0.1),
  EVI   = seq(-1, 1, by = 0.1),
  MSAVI2= seq(0, 1, by = 0.1)
)

hist_area_por_geom <- function(r, geom, edges) {
  if (!is.null(crs(r)) && !is.null(crs(geom)) && crs(r) != crs(geom)) {
    geom <- project(geom, crs(r))
  }
  r_clip <- try(mask(crop(r, geom), geom), silent = TRUE)
  if (inherits(r_clip, "try-error")) return(NULL)
  a_m2 <- cellSize(r_clip, unit = "m"); names(a_m2) <- "area_m2"
  nbins <- length(edges) - 1
  rcl <- cbind(edges[-length(edges)], edges[-1], seq_len(nbins))
  r_bins <- classify(r_clip, rcl = rcl, include.lowest = TRUE, right = FALSE)
  names(r_bins) <- "bin_id"
  z <- tryCatch(zonal(a_m2, r_bins, fun = "sum", na.rm = TRUE), error = function(e) NULL)
  area_ha <- rep(0, nbins)
  if (!is.null(z) && nrow(z) > 0) {
    colnames(z)[1] <- "bin_id"
    val_cols <- setdiff(names(z), "bin_id")
    if (length(val_cols) > 0) {
      val_col <- val_cols[1]
      full <- data.frame(bin_id = seq_len(nbins))
      z <- merge(full, z[, c("bin_id", val_col), drop = FALSE], by = "bin_id", all.x = TRUE)
      z[[val_col]][is.na(z[[val_col]])] <- 0
      area_ha <- as.numeric(z[[val_col]]) / 10000
    }
  }
  df <- data.frame(
    bin_id = seq_len(nbins),
    from   = edges[-length(edges)],
    to     = edges[-1],
    label  = sprintf("%.2f–%.2f", edges[-length(edges)], edges[-1]),
    area_ha = area_ha
  )
  df[order(df$from), ]
}

# CARGA DE SHAPES
shape_sf <- NULL
if (!is.null(SHAPE_PATH) && file.exists(SHAPE_PATH)) {
  s <- try(vect(SHAPE_PATH), silent = TRUE)
  if (!inherits(s, "try-error")) {
    shape_sf <- st_as_sf(s)
    if (!is.na(st_crs(shape_sf))) {
      if (!identical(st_crs(shape_sf)$epsg, 4326)) {
        shape_sf <- st_transform(shape_sf, 4326)
      }
    }
  }
}

lotes_sf <- NULL
if (!is.null(SHAPE_VARIEDADES_PATH) && file.exists(SHAPE_VARIEDADES_PATH)) {
  sv <- try(vect(SHAPE_VARIEDADES_PATH), silent = TRUE)
  if (!inherits(sv, "try-error")) {
    lotes_sf <- st_as_sf(sv)
    if (!is.na(st_crs(lotes_sf))) {
      if (!identical(st_crs(lotes_sf)$epsg, 4326)) {
        lotes_sf <- st_transform(lotes_sf, 4326)
      }
    }
  }
}


# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #loading-screen { position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        background: rgba(255, 255, 255, 0.8); display: flex; justify-content: center;
        align-items: center; z-index: 9999; }
      .spinner { display: flex; justify-content: center; align-items: center; }
      .spinner div { width: 12px; height: 12px; margin: 5px; background-color: #007bff;
        border-radius: 50%; animation: bounce 1.5s infinite ease-in-out; }
      .spinner div:nth-child(1){animation-delay:0s;} .spinner div:nth-child(2){animation-delay:0.2s;}
      .spinner div:nth-child(3){animation-delay:0.4s;}
      @keyframes bounce {0%,80%,100%{transform:scale(0);} 40%{transform:scale(1);} }
      .boton-seleccion { background-color:#f8f9fa !important; color:black !important; border:1px solid #ced4da; }
      .boton-activo { background-color:#007bff !important; color:white !important; border:1px solid #0056b3; }
      #borrar_seleccion { background-color:#f8f9fa !important; color:black !important; border:1px solid #ced4da; }
      input[type='checkbox'] { accent-color:#007bff; }
      #chart-container { position: relative; margin-top: 12px; padding-top: 52px; }
      #chart-toolbar {
        position: absolute; left: 12px; top: 8px; background: rgba(255,255,255,0.92);
        padding: 6px 10px; border-radius: 10px; border: 1px solid #ddd;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1); z-index: 1;
      }
      #chart-toolbar .shiny-options-group { margin: 0; display: flex; gap: 10px; align-items: center; flex-wrap: wrap; }
      #chart-toolbar label { margin-bottom: 0; font-weight: 600; color: #444; }
    "))
  ),
  div(id = "loading-screen", div(class = "spinner", div(), div(), div())),
  titlePanel(HTML("&nbsp;")),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      helpText(tags$strong("Choose a mode to analyze the area:", style= "color:#343a40; font-weight:bold;")),
      div(style = "display:flex; flex-wrap:wrap; gap:5px; justify-content:center;",
          actionButton("seleccion_lote", "Select by Variety", class = "boton-seleccion"),
          actionButton("dibujar_poligono", "Draw Polygon", class = "boton-seleccion"),
          actionButton("borrar_seleccion", "Clear Selection", icon = icon("redo"),
                       style = "white-space: nowrap; padding: 6px 10px; text-align: center; border: 1px solid #ced4da;")
      ),
      div(style = "margin-top: 15px;",
          dateRangeInput(
            "fecha_rango", "Select date range:",
            start = "2025-08-07", end = "2026-01-09",
            format = "yyyy-mm-dd",  min = "2025-08-07", max = "2026-01-09",
            separator = "to"
          )
      ),
      div(style = "display:flex; align-items:center;",
          selectInput("indice", "Select index:", choices = c("NDVI", "EVI", "MSAVI2", "GNDVI", "LAI", "GCI", "NDWI"), width = "90%"),
          actionButton("info_button", label = icon("info"),
                       style = "background-color:#007bff; color:white; border-radius:50%; width:32px; height:32px; font-size:16px; border:none; margin-left:8px;")
      ),
      checkboxInput("toggle_raster", "Show Raster", TRUE),
      uiOutput("infoLote_ui"),
      div(style = "max-height: 450px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin-top: 10px;",
          tableOutput("resultados")),
      downloadButton("descargar_datos", "Download Data", style = "margin-top: 10px; width: 100%;")
    ),
    mainPanel(
      width = 7,
      leafletOutput("mapPlot", height = "450px"),
      tags$div(style = "height: 10px;"),
      div(
        id = "chart-container",
        div(
          id = "chart-toolbar",
          radioButtons(
            "chart_type", label = NULL,
            choices = c("📈 Time series" = "ts", "📊 Area distribution" = "hist"),
            selected = "ts", inline = TRUE
          )
        ),
        plotlyOutput("chartPlot", height = "420px")
      )
    )
  )
)


# SERVER
server <- function(input, output, session) {
  observe({ hide("loading-screen") })
  
  observeEvent(input$info_button, {
    indice <- input$indice
    showModal(modalDialog(title = paste("Information about", indice),
                          info_indices[[indice]], easyClose = TRUE, footer = NULL))
  })
  
  ndvi_files <- list.files(rutas_indices$NDVI, full.names = TRUE, pattern = "\\.(tif|nc)$")
  if (length(ndvi_files) == 0) stop("No NDVI rasters found in rutas_indices$NDVI")
  raster_referencia <- rast(ndvi_files[1])
  
  get_bounds <- function() {
    if (!is.null(shape_sf)) {
      bb <- sf::st_bbox(shape_sf)
      return(list(xmin = as.numeric(bb["xmin"]), ymin = as.numeric(bb["ymin"]),
                  xmax = as.numeric(bb["xmax"]), ymax = as.numeric(bb["ymax"]), ok = TRUE))
    } else if (!is.null(raster_referencia)) {
      e <- terra::ext(raster_referencia)
      if (all(is.finite(c(e$xmin, e$ymin, e$xmax, e$ymax)))) {
        return(list(xmin = e$xmin, ymin = e$ymin, xmax = e$xmax, ymax = e$ymax, ok = TRUE))
      }
    }
    list(lon = CENTER$lon, lat = CENTER$lat, zoom = CENTER$zoom, ok = FALSE)
  }
  bounds <- get_bounds()
  
  puntos_seleccionados <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  instruccion          <- reactiveVal(FALSE)
  instruccion_lote     <- reactiveVal(FALSE)
  modo_seleccion       <- reactiveVal("lote")
  selected_variedades  <- reactiveVal(character(0))
  color_variedades     <- c("#007bff", "#9D25E5")   # azul, morado
  
  # Leyenda lateral con ícono de planta
  output$infoLote_ui <- renderUI({
    if (modo_seleccion() != "lote") return(NULL)
    sel <- selected_variedades()
    if (length(sel) == 0) {
      return(tags$div(
        style = "font-size: 0.9rem; color: #555;",
        "No variety selected."
      ))
    }
    tags$div(
      tags$h5(
        style = "display:flex; align-items:center; gap:6px;",
        icon("leaf"),
        "Selected varieties"
      ),
      lapply(seq_along(sel), function(i) {
        tags$div(
          style = "display:flex; align-items:center; gap:6px; margin-bottom:4px;",
          tags$span(
            style = paste0(
              "display:inline-block; width:14px; height:14px; border-radius:3px;",
              "background-color:", color_variedades[i], ";"
            )
          ),
          tags$span(sel[i])
        )
      })
    )
  })
  
  output$mapPlot <- renderLeaflet({
    m <- leaflet() %>% addProviderTiles("Esri.WorldImagery")
    if (!is.null(shape_sf)) {
      m <- m %>% addPolygons(data = shape_sf, color = "gray", weight = 1, fillOpacity = 0.0, group = "boundary")
    }
    if (!is.null(lotes_sf)) {
      m <- m %>% addPolygons(
        data = lotes_sf, color = "black", weight = 1,
        fillOpacity = 0.0, group = "variedades"
      )
    }
    if (isTRUE(bounds$ok)) {
      m %>% fitBounds(lng1 = bounds$xmin, lat1 = bounds$ymin, lng2 = bounds$xmax, lat2 = bounds$ymax) %>%
        addEasyButton(easyButton(
          icon = "fa-map-marker", title = "Center on Area",
          onClick = JS(sprintf("function(btn, map){ map.fitBounds([[%.6f, %.6f],[%.6f, %.6f]]); }",
                               bounds$ymin, bounds$xmin, bounds$ymax, bounds$xmax))
        ))
    } else {
      m %>% setView(lng = CENTER$lon, lat = CENTER$lat, zoom = CENTER$zoom) %>%
        addEasyButton(easyButton(
          icon = "fa-map-marker", title = "Center on Area",
          onClick = JS(sprintf("function(btn, map){ map.setView([%.6f, %.6f], %d); }",
                               CENTER$lat, CENTER$lon, CENTER$zoom))
        ))
    }
  })
  
  observeEvent(list(input$fecha_rango, input$indice, input$toggle_raster), {
    show("loading-screen"); on.exit(hide("loading-screen"), add = TRUE)
    raster_data <- if (input$toggle_raster) mostrar_raster(input$indice, input$fecha_rango, rutas_indices) else NULL
    lp <- leafletProxy("mapPlot") %>%
      clearGroup("raster") %>%
      removeControl("legend_discreta") %>%
      removeControl("legend")
    
    if (!is.null(raster_data)) {
      vals_vec <- as.vector(terra::values(raster_data, mat = FALSE))
      vals_vec <- vals_vec[is.finite(vals_vec)]
      if (!length(vals_vec)) return(NULL)
      rng <- range(vals_vec, na.rm = TRUE)
      paleta <- colorNumeric(
        palette = if (input$indice == "LAI") {
          c("#A0522D", "#F4A460", "#9ACD32", "#32CD32", "#228B22","#004d00")
        } else if (input$indice == "NDWI") {
          c("#d13415", "#f5d856", "#9fcf70", "#388a47", "#74b0d6")
        } else if (input$indice == "GCI") {
          c("#A0522D", "#F4A460", "#9ACD32", "#32CD32", "#228B22","#004d00")
        } else {
          c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907")
        },
        domain = rng, na.color = "transparent"
      )
      lp <- lp %>% addRasterImage(raster_data, colors = paleta, opacity = 0.7, group = "raster") %>%
        addLegend(pal = paleta, values = vals_vec, title = input$indice,
                  position = "bottomright", layerId = "legend")
    }
  })
  
  observe({
    runjs('$(".boton-seleccion").removeClass("boton-activo");$("#seleccion_lote").addClass("boton-activo");')
  })
  
  observeEvent(input$seleccion_lote, {
    modo_seleccion("lote")
    selected_variedades(character(0))
    puntos_seleccionados(data.frame(x = numeric(), y = numeric()))
    leafletProxy("mapPlot") %>% clearGroup("selected_lote") %>% clearGroup("Área de Estudio") %>% clearMarkers()
    runjs('$(".boton-seleccion").removeClass("boton-activo");$("#seleccion_lote").addClass("boton-activo");')
    if (!instruccion_lote()) {
      showModal(modalDialog(
        title = "Instructions to select varieties",
        "Click inside one of the polygons to select a variety. You can select up to two varieties.",
        easyClose = TRUE, footer = modalButton("Got it")
      ))
      instruccion_lote(TRUE)
    }
  })
  
  observeEvent(input$dibujar_poligono, {
    modo_seleccion("poligono")
    selected_variedades(character(0))
    puntos_seleccionados(data.frame(x = numeric(), y = numeric()))
    leafletProxy("mapPlot") %>% clearGroup("selected_lote") %>% clearGroup("Área de Estudio") %>% clearMarkers()
    runjs('$(".boton-seleccion").removeClass("boton-activo");$("#dibujar_poligono").addClass("boton-activo");')
    if (!instruccion()) {
      showModal(modalDialog(
        title = "Instruction to draw the polygon",
        "Connect the last point to the first to close the polygon.",
        easyClose = TRUE, footer = modalButton("Got it")
      ))
      instruccion(TRUE)
    }
  })
  
  observeEvent(input$borrar_seleccion, {
    runjs('$("#borrar_seleccion").css("background-color","#dc3545").css("color","white");')
    selected_variedades(character(0))
    puntos_seleccionados(data.frame(x = numeric(), y = numeric()))
    leafletProxy("mapPlot") %>% clearGroup("selected_lote") %>% clearGroup("Área de Estudio") %>% clearMarkers()
    runjs('setTimeout(function(){ $("#borrar_seleccion").css("background-color","#f8f9fa").css("color","black");},500);')
  })
  
  # CLICK EN MAPA 
  observeEvent(input$mapPlot_click, {
    if (modo_seleccion() == "lote") {
      req(!is.null(lotes_sf))
      lon <- input$mapPlot_click$lng
      lat <- input$mapPlot_click$lat
      pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
      idx <- which(sf::st_intersects(pt, lotes_sf, sparse = FALSE)[1, ])
      if (!length(idx)) {
        showModal(modalDialog(
          title = "No variety selected",
          "The click is outside the defined varieties.",
          easyClose = TRUE, footer = modalButton("Close")
        ))
        return()
      }
      lote_info  <- lotes_sf[idx[1], ]
      var_clicked <- as.character(lote_info$Variedad)
      current     <- selected_variedades()
      if (var_clicked %in% current) {
        new_sel <- c(setdiff(current, var_clicked), var_clicked)
      } else {
        new_sel <- c(current, var_clicked)
      }
      if (length(new_sel) > 2) {
        new_sel <- tail(new_sel, 2)
      }
      selected_variedades(new_sel)
      
      lp <- leafletProxy("mapPlot") %>% clearGroup("selected_lote")
      if (length(new_sel) > 0) {
        for (i in seq_along(new_sel)) {
          var_i  <- new_sel[i]
          lote_i <- lotes_sf[lotes_sf$Variedad == var_i, ]
          if (nrow(lote_i) > 0) {
            lp <- lp %>% addPolygons(
              data = lote_i,
              color = color_variedades[i],
              weight = 3,
              fillColor = "transparent",
              fillOpacity = 0.5,
              group = "selected_lote"
            )
          }
        }
      }
      return()
    }
    
    if (modo_seleccion() != "poligono") return()
    
    # dibujo de polígono 
    pts <- puntos_seleccionados()
    nuevo <- data.frame(x = input$mapPlot_click$lng, y = input$mapPlot_click$lat)
    valor <- extract(raster_referencia, cbind(nuevo$x, nuevo$y), cells = TRUE)
    if (is.na(valor[1,2])) {
      showModal(modalDialog(title = "Error", "The selected point is outside the data area.",
                            easyClose = TRUE, footer = modalButton("Close")))
      return()
    }
    if (nrow(pts) > 0) {
      dist <- sqrt((nuevo$x - pts[nrow(pts), "x"])^2 + (nuevo$y - pts[nrow(pts), "y"])^2)
      if (dist < 0.0001) {
        showModal(modalDialog(title = "Error", "The new point is too close to the last added point.",
                              easyClose = TRUE, footer = modalButton("Close")))
        return()
      }
    }
    if (nrow(pts) >= 3) {
      dc <- sqrt((nuevo$x - pts[1, "x"])^2 + (nuevo$y - pts[1, "y"])^2)
      if (dc < 0.00005) {
        pts <- rbind(pts, pts[1, ])
      } else {
        pts <- rbind(pts, nuevo)
      }
    } else {
      pts <- rbind(pts, nuevo)
    }
    puntos_seleccionados(pts)
  })
  
  observe({
    leafletProxy("mapPlot") %>% clearGroup("Área de Estudio") %>% clearMarkers()
    pts <- puntos_seleccionados()
    if (!is.null(pts) && nrow(pts) > 0 && modo_seleccion() == "poligono") {
      leafletProxy("mapPlot") %>% addCircleMarkers(data = pts, lng = ~x, lat = ~y, color = "#007bff", radius = 2)
      if (nrow(pts) > 3 && pts[1,"x"] == pts[nrow(pts),"x"] && pts[1,"y"] == pts[nrow(pts),"y"]) {
        poly <- vect(cbind(pts$x, pts$y), type = "polygons"); crs(poly) <- "EPSG:4326"
        leafletProxy("mapPlot") %>% addPolygons(data = poly, color = "#9D25E5", weight = 2,
                                                fillColor = "transparent", fillOpacity = 0.4,
                                                group = "Área de Estudio")
      }
    }
  })
  
  
  # TABLA RESULTADOS
  resultados_tabla <- reactive({
    req(input$indice, input$fecha_rango)
    show("loading-screen"); on.exit(hide("loading-screen"), add = TRUE)
    
    if (modo_seleccion() == "lote") {
      vars <- selected_variedades()
      if (length(vars) == 0) {
        return(data.frame(
          Date = "No data",
          Variety = "Select at least one variety to view the results"
        ))
      }
      
      res_list <- list()
      for (i in seq_along(vars)) {
        v <- vars[i]
        r <- obtener_resultados_lote(v, input$indice, input$fecha_rango, rutas_indices, lotes_sf)
        if (is.null(r) || nrow(r) == 0) next
        r <- r[r$fecha != "Global Average", , drop = FALSE]
        if (nrow(r) == 0) next
        r$fecha <- as.Date(r$fecha)
        colnames(r) <- c("Date", v)
        res_list[[length(res_list) + 1]] <- r
      }
      
      if (length(res_list) == 0) {
        return(data.frame(
          Date = "No data",
          Message = "No values inside selected varieties"
        ))
      }
      
      tabla <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), res_list)
      
      if (!inherits(tabla$Date, "Date")) {
        if (is.character(tabla$Date)) {
          num_try <- suppressWarnings(as.numeric(tabla$Date))
          if (!all(is.na(num_try))) {
            tabla$Date <- num_try
          }
        }
        if (is.numeric(tabla$Date)) {
          year  <- floor(tabla$Date / 10)
          month <- tabla$Date %% 10
          month[month < 1 | month > 12] <- 1
          datestr <- sprintf("%04d-%02d-01", year, month)
          tabla$Date <- as.Date(datestr)
        }
      }
      
      tabla <- tabla[order(tabla$Date), ]
      tabla$Date <- format(tabla$Date, "%Y-%m-%d")
      
      return(tabla)
    }
    
    if (modo_seleccion() == "poligono") {
      pts <- puntos_seleccionados()
      if (is.null(pts) || nrow(pts) < 4 ||
          !(pts[1,"x"] == pts[nrow(pts),"x"] && pts[1,"y"] == pts[nrow(pts),"y"])) {
        return(data.frame(
          Date = "No data",
          Average = "Draw and close a polygon to view the results"
        ))
      }
      res <- obtener_resultados_poligono(pts, input$indice, input$fecha_rango, rutas_indices)
      if (nrow(res) == 0) return(data.frame(Date = "No data", Average = "No values inside polygon"))
      colnames(res) <- c("Date", "Average")
      return(res)
    }
    
    data.frame(Date = "No data", Message = "Select a mode")
  })
  
  output$resultados <- renderTable({
    req(input$indice, input$fecha_rango)
    show("loading-screen"); on.exit(hide("loading-screen"), add = TRUE)
    resultados_tabla()
  }, rownames = FALSE)
  

  # GRÁFICOS
  output$chartPlot <- renderPlotly({
    req(input$indice, input$fecha_rango)
    show("loading-screen"); on.exit(hide("loading-screen"), add = TRUE)
    
    if (input$chart_type == "ts") {
      if (modo_seleccion() == "lote") {
        vars <- selected_variedades()
        if (length(vars) == 0) return(NULL)
        
        p <- NULL
        for (i in seq_along(vars)) {
          v <- vars[i]
          col_i <- color_variedades[i]
          
          res <- obtener_resultados_lote(v, input$indice, input$fecha_rango, rutas_indices, lotes_sf)
          if (is.null(res) || nrow(res) == 0) next
          res <- res[res$fecha != "Global Average", ]
          if (nrow(res) == 0) next
          
          res$fecha    <- as.Date(res$fecha)
          res$promedio <- as.numeric(res$promedio)
          if (all(is.na(res$promedio))) next
          
          if (is.null(p)) {
            p <- plot_ly(
              data = res, x = ~fecha, y = ~promedio,
              type = 'scatter', mode = 'lines+markers',
              name = paste("Variety:", v),
              line   = list(width = 2, color = col_i),
              marker = list(color = col_i)
            )
          } else {
            p <- p %>% add_trace(
              data = res, x = ~fecha, y = ~promedio,
              type = 'scatter', mode = 'lines+markers',
              name = paste("Variety:", v),
              line   = list(width = 2, color = col_i),
              marker = list(color = col_i)
            )
          }
          
          if (input$indice %in% c("NDWI","GCI","LAI","NDVI","EVI","MSAVI2","GNDVI")) {
            res$suavizado <- suavizar_serie(res$promedio)
            if (!all(is.na(res$suavizado))) {
              p <- p %>% add_trace(
                data   = res,
                x      = ~fecha,
                y      = ~suavizado,
                type   = 'scatter',
                mode   = 'lines+markers',
                name   = paste("Smoothed", v),
                line   = list(dash = 'dash', color = col_i),
                marker = list(color = col_i)
              )
            }
          }
        }
        if (is.null(p)) return(NULL)
        return(
          p %>% layout(
            title = paste("Average", input$indice, "in selected varieties"),
            xaxis = list(title = "Date", type = "date"),
            yaxis = list(title = paste("Average", input$indice), zeroline = FALSE, showline = FALSE, showgrid = TRUE),
            legend = list(title = list(text = "Area")),
            margin = list(t = 80)
          )
        )
      }
      
      if (modo_seleccion() == "poligono") {
        pts <- puntos_seleccionados()
        poligono_cerrado <- (!is.null(pts) && nrow(pts) >= 4 &&
                               pts[1,"x"] == pts[nrow(pts),"x"] &&
                               pts[1,"y"] == pts[nrow(pts),"y"])
        if (!poligono_cerrado) return(NULL)
        res <- obtener_resultados_poligono(pts, input$indice, input$fecha_rango, rutas_indices)
        if (is.null(res) || nrow(res) == 0) return(NULL)
        res <- res[res$fecha != "Global Average", ]
        if (nrow(res) == 0) return(NULL)
        res$fecha    <- as.Date(res$fecha)
        res$promedio <- as.numeric(res$promedio)
        if (all(is.na(res$promedio))) return(NULL)
        p <- plot_ly(data = res, x = ~fecha, y = ~promedio, type = 'scatter', mode = 'lines+markers',
                     name = "Drawn area", line = list(width = 2))
        if (input$indice %in% c("NDWI","GCI","LAI","NDVI","EVI","MSAVI2","GNDVI")) {
          res$suavizado <- suavizar_serie(res$promedio)
          if (!all(is.na(res$suavizado))) {
            p <- p %>% add_trace(y = ~res$suavizado, mode = 'lines', name = "Smoothed", line = list(dash = 'dash'))
          }
        }
        return(
          p %>% layout(
            title = paste("Average", input$indice, "in drawn polygon"),
            xaxis = list(title = "Date", type = "date"),
            yaxis = list(title = paste("Average", input$indice), zeroline = FALSE, showline = FALSE, showgrid = TRUE),
            legend = list(title = list(text = "Area")),
            margin = list(t = 80)
          )
        )
      }
      return(NULL)
    }
    
    # Histograma / distribución área
    indice_hist <- input$indice
    r_mean <- mostrar_raster(indice_hist, input$fecha_rango, rutas_indices)
    if (is.null(r_mean)) return(NULL)
    edges <- binEdgesByIndex[[input$indice]]
    if (is.null(edges)) {
      rng <- range(values(r_mean), na.rm = TRUE)
      if (!all(is.finite(rng)) || rng[1] == rng[2]) return(NULL)
      steps <- seq(rng[1], rng[2], length.out = 9)
      edges <- unique(steps)
    }
    if (length(edges) < 2) return(NULL)
    
    if (modo_seleccion() == "lote") {
      vars <- selected_variedades()
      if (length(vars) == 0) return(NULL)
      p <- NULL
      for (i in seq_along(vars)) {
        v <- vars[i]
        col_i <- color_variedades[i]
        
        lote_sf_v <- lotes_sf[lotes_sf$Variedad == v, ]
        if (nrow(lote_sf_v) == 0) next
        geom <- vect(lote_sf_v)
        df <- hist_area_por_geom(r_mean, geom, edges)
        if (is.null(df) || nrow(df) == 0) next
        df$label <- factor(df$label, levels = df$label)
        
        if (is.null(p)) {
          p <- plot_ly(
            data = df, x = ~label, y = ~area_ha, type = "bar",
            name = v,
            marker = list(color = col_i),
            hovertemplate = "Range: %{x}<br>Area: %{y:.2f} ha<extra></extra>"
          )
        } else {
          p <- p %>% add_trace(
            data = df, x = ~label, y = ~area_ha, type = "bar",
            name = v,
            marker = list(color = col_i),
            hovertemplate = "Range: %{x}<br>Area: %{y:.2f} ha<extra></extra>"
          )
        }
      }
      if (is.null(p)) return(NULL)
      return(
        p %>% layout(
          title   = paste("Area Distribution —", input$indice, "(Selected varieties)"),
          xaxis   = list(title = "Index range", tickangle = -45, automargin = TRUE),
          yaxis   = list(title = "Area (ha)", rangemode = "tozero"),
          barmode = "group",
          margin  = list(t = 70, b = 90)
        )
      )
    }
    
    if (modo_seleccion() == "poligono") {
      pts <- puntos_seleccionados()
      if (is.null(pts) || nrow(pts) < 4 ||
          pts[1,"x"] != pts[nrow(pts),"x"] ||
          pts[1,"y"] != pts[nrow(pts),"y"]) return(NULL)
      geom <- vect(cbind(pts$x, pts$y), type = "polygons"); crs(geom) <- "EPSG:4326"
      df <- hist_area_por_geom(r_mean, geom, edges)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$label <- factor(df$label, levels = df$label)
      return(
        plot_ly(
          data = df, x = ~label, y = ~area_ha, type = "bar",
          name = "Drawn area",
          hovertemplate = "Range: %{x}<br>Area: %{y:.2f} ha<extra></extra>"
        ) %>%
          layout(
            title   = paste("Area Distribution —", input$indice),
            xaxis   = list(title = "Index range", tickangle = -45, automargin = TRUE),
            yaxis   = list(title = "Area (ha)", rangemode = "tozero"),
            barmode = "relative",
            margin  = list(t = 70, b = 90)
          )
      )
    }
    NULL
  })
  
  observe({
    res <- resultados_tabla()
    if (nrow(res) > 1 && !any(res$Date %in% "No data")) shinyjs::show("descargar_datos") else shinyjs::hide("descargar_datos")
  })
  
  output$descargar_datos <- downloadHandler(
    filename = function() {
      req(nrow(resultados_tabla()) > 1)
      paste0("Results_", input$indice, "_", input$fecha_rango[1], "_", input$fecha_rango[2], ".csv")
    },
    content = function(file) {
      res <- resultados_tabla()
      if (nrow(res) <= 1 || "No data" %in% res$Date) {
        showModal(modalDialog(title = "Error", "No data to download. Select a variety or draw a polygon.", easyClose = TRUE, footer = modalButton("Close")))
        return(NULL)
      }
      write.csv(res, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

