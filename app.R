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

# Extractor de fecha 
date_from_filename <- function(path) {
  fname <- basename(path)
  m <- regexpr("\\d{4}-\\d{2}-\\d{2}", fname)
  if (m[1] < 0) return(NA)
  as.Date(substr(fname, m[1], m[1] + attr(m, "match.length") - 1))
}

# Rutas de índices
rutas_indices <- list(
  GCI      = "Indices_Agroclimaticos/GCI_tif", 
  LAI      = "Indices_Agroclimaticos/LAI_tif",
  NDVI     = "Indices_Agroclimaticos/NDVI_classified_tif",  # visualización
  NDVI_raw = "Indices_Agroclimaticos/NDVI_tif",              # análisis
  NDWI     = "Indices_Agroclimaticos/NDWI_tif",
  VCI      = "Indices_Agroclimaticos/VCI_classified_tif",
  GNDVI    = "Indices_Agroclimaticos/GNDVI_tif",
  EVI      = "Indices_Agroclimaticos/EVI_tif",
  MSAVI    = "Indices_Agroclimaticos/MSAVI_tif"
)

# Paletas e información de los índices
paletas_colores <- list(
  LAI    = colorNumeric(palette = c("#A0522D", "#F4A460", "#9ACD32", "#32CD32", "#228B22","#004d00"), domain = NULL, na.color = "transparent"),
  NDVI   = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent"),
  NDWI   = colorNumeric(palette = c("#d13415", "#f5d856", "#9fcf70", "#388a47", "#74b0d6"), domain = NULL, na.color = "transparent"),
  VCI    = colorNumeric(palette = c("#000", "#a52a2a", "#ff0000", "#ffa500", "#ffff00", "#90ee90", "#006400"), domain = NULL, na.color = "transparent"),
  GCI    = colorNumeric(palette = c("#A0522D", "#F4A460", "#9ACD32", "#32CD32", "#228B22","#004d00"), domain = NULL, na.color = "transparent"),
  EVI    = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent"),
  MSAVI  = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent"),
  GNDVI  = colorNumeric(palette = c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907"), domain = NULL, na.color = "transparent")
)

info_indices <- list(
  LAI   = "The Leaf Area Index (LAI) quantifies the amount of foliage in a canopy. It is used to estimate biomass and crop health.",
  NDVI  = "The Normalized Difference Vegetation Index (NDVI) is an indicator of photosynthetically active biomass, used to analyze plant health and detect abnormal growth changes.",
  NDWI  = "The Normalized Difference Water Index (NDWI) is used to detect water bodies and moisture in vegetation.",
  VCI   = "The Vegetation Condition Index (VCI) compares current vegetation status with historical trends to identify drought conditions.",
  GCI   = "The Green Chlorophyll Index (GCI) estimates chlorophyll content in vegetation, helping to assess nutritional status.",
  EVI   = "The Enhanced Vegetation Index (EVI) measures canopy structure and vegetation vigor. It is optimized to better quantify greenness in areas of high biomass where NDVI may saturate.",
  MSAVI = "The Modified Soil-Adjusted Vegetation Index  (MSAVI) measures vegetation greenness while minimizing the influence of bare soil. It provides a more accurate estimate of canopy cover in areas with sparse vegetation",
  GNDVI = "The Green Normalized Difference Vegetation Index (GNDVI) uses the green band to estimate chlorophyll content, making it effective for monitoring plant stress and nutrient status"
)

# Colores fijos por orden de selección (1 = azul, 2 = morado)
colores_lotes <- c("#007bff", "#9D25E5")

# Bordes de bins por índice
binEdgesByIndex <- list(
  NDVI  = seq(-1, 1, by = 0.1),
  NDWI  = seq(-1, 1, by = 0.1),
  GCI   = 0:10,
  LAI   = 0:10,
  VCI   = 1:8,            # clases 1–7 (bins [1,2),[2,3),...,[7,8))
  GNDVI = seq(-1, 1, by = 0.1),
  EVI   = seq(-1, 1, by = 0.1),
  MSAVI = seq(0, 1, by = 0.1)
)

# Nombres de categorías VCI (bin_id 1..7)
VCI_CATEGORY_NAMES <- c(
  "Extreme Drought", "Severe Drought", "Moderate Drought",
  "Mild Drought", "Normal", "Good", "Very Good"
)

# Utilidades shapefile/leyenda/raster
cargar_shapefile <- function(ruta_shapefile) {
  lotes <- vect(ruta_shapefile)
  return(st_as_sf(lotes))
}

crear_leyenda_discreta <- function(indice) {
  if (indice == "NDVI") {
    etiquetas <- c("Soil/Water", "Very Low", "Low", "Moderately Low", "Moderately High", "High")
    colores   <- c("#d9230f", "#d45313", "#f2db29", "#9acd32", "#07a607", "#076907")
    list(etiquetas = etiquetas, colores = colores)
  } else if (indice == "VCI") {
    etiquetas <- c("Extreme Drought", "Severe Drought", "Moderate Drought", "Mild Drought", "Normal", "Good", "Very Good")
    colores   <- c("#000000", "#a52a2a", "#ff0000", "#ffa500", "#ffff00", "#90ee90", "#006400")
    list(etiquetas = etiquetas, colores = colores)
  } else {
    NULL
  }
}

# Raster agregado 
mostrar_raster <- function(indice, fecha_rango, rutas_indices) {
  fecha_inicio <- as.Date(fecha_rango[1])
  fecha_fin    <- as.Date(fecha_rango[2])
  ruta_rasters <- rutas_indices[[indice]]
  archivos_raster <- list.files(ruta_rasters, full.names = TRUE, pattern = "\\.(tif|nc)$", ignore.case = TRUE)
  if (!length(archivos_raster)) return(NULL)
  
  ffechas <- vapply(archivos_raster, date_from_filename, as.Date(NA))
  sel <- !is.na(ffechas) & ffechas >= fecha_inicio & ffechas <= fecha_fin
  archivos_seleccionados <- archivos_raster[sel]
  if (!length(archivos_seleccionados)) return(NULL)
  
  rasters <- rast(archivos_seleccionados)
  if (indice %in% c("VCI", "NDVI")) modal(rasters, ties = "first", na.rm = TRUE) else mean(rasters, na.rm = TRUE)
}

# Cálculos serie/tabla 
obtener_resultados_lote <- function(selected_lotes, indice, fecha_rango, lotes_sf, rutas_indices) {
  req(selected_lotes, indice, fecha_rango)
  selected_lotes <- tail(selected_lotes, 2)
  fecha_inicio <- as.Date(fecha_rango[1]); fecha_fin <- as.Date(fecha_rango[2])
  indice_para_analisis <- if (indice == "NDVI") "NDVI_raw" else indice
  raster_files <- list.files(rutas_indices[[indice_para_analisis]], full.names = TRUE, pattern = "\\.(tif|nc)$", ignore.case = TRUE)
  
  resultados <- data.frame(fecha = character(), lote = character(), promedio = numeric())
  for (lote_id in selected_lotes) {
    lote_info <- lotes_sf[lotes_sf$Lot_Number == lote_id, ]
    valores_globales <- numeric()
    for (archivo in raster_files) {
      fecha <- date_from_filename(archivo)
      if (!is.na(fecha) && fecha >= fecha_inicio & fecha <= fecha_fin) {
        raster_data <- rast(archivo)
        valores <- extract(raster_data, lote_info)
        valores <- valores[!is.na(valores[, 2]), , drop = FALSE]
        if (!is.null(valores) && nrow(valores) > 0) {
          promedio <- mean(valores[, 2], na.rm = TRUE)
          valores_globales <- c(valores_globales, valores[, 2])
          resultados <- rbind(resultados, data.frame(fecha = as.character(fecha), lote = lote_id, promedio = promedio))
        }
      }
    }
    if (length(valores_globales) > 0) {
      promedio_global <- mean(valores_globales, na.rm = TRUE)
      resultados <- rbind(data.frame(fecha = "Global Average", lote = lote_id, promedio = round(promedio_global, 2)), resultados)
    }
  }
  resultados
}

obtener_resultados_poligono <- function(puntos, indice, fecha_rango, rutas_indices) {
  req(puntos, indice, fecha_rango)
  polygon_sf <- vect(cbind(puntos$x, puntos$y), type = "polygons"); crs(polygon_sf) <- "EPSG:4326"
  fecha_inicio <- as.Date(fecha_rango[1]); fecha_fin <- as.Date(fecha_rango[2])
  indice_para_analisis <- if (indice == "NDVI") "NDVI_raw" else indice
  ruta_rasters <- rutas_indices[[indice_para_analisis]]
  archivos_raster <- list.files(ruta_rasters, full.names = TRUE, pattern = "\\.(tif|nc)$", ignore.case = TRUE)
  
  resultados <- data.frame(fecha = character(), promedio = numeric()); valores_globales <- numeric()
  for (archivo in archivos_raster) {
    fecha <- date_from_filename(archivo)
    if (!is.na(fecha) && fecha >= fecha_inicio & fecha <= fecha_fin) {
      raster_data <- rast(archivo)
      if (!is.null(crs(raster_data)) && crs(raster_data) != crs(polygon_sf)) {
        polygon_sf_proj <- project(polygon_sf, crs(raster_data))
      } else polygon_sf_proj <- polygon_sf
      valores <- extract(raster_data, polygon_sf_proj)
      valores <- valores[!is.na(valores[, 2]), , drop = FALSE]
      if (!is.null(valores) && nrow(valores) > 0) {
        promedio <- mean(valores[, 2], na.rm = TRUE)
        valores_globales <- c(valores_globales, valores[, 2])
        resultados <- rbind(resultados, data.frame(fecha = as.character(fecha), promedio = promedio))
      }
    }
  }
  if (length(valores_globales) > 0) {
    promedio_global <- mean(valores_globales, na.rm = TRUE)
    resultados <- rbind(data.frame(fecha = "Global Average", promedio = round(promedio_global, 2)), resultados)
  }
  resultados
}

# Suavizado Savitzky–Golay
suavizar_serie <- function(serie, window_size = 5, poly_order = 2) {
  if (length(serie) >= window_size) sgolayfilt(serie, p = poly_order, n = window_size) else serie
}

# Helpers histograma
etiquetas_bins <- function(edges) {
  labs <- c()
  for (i in seq_len(length(edges)-1)) labs <- c(labs, sprintf("%.2f–%.2f", edges[i], edges[i+1]))
  labs
}

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
      area_ha <- as.numeric(z[[val_col]]) / 10000  # m² → ha
    }
  }
  
  df <- data.frame(
    bin_id  = seq_len(nbins),
    from    = edges[-length(edges)],
    to      = edges[-1],
    label   = sprintf("%.2f–%.2f", edges[-length(edges)], edges[-1]),
    area_ha = area_ha
  )
  df[order(df$from), ]   # asegura eje X en orden numérico
}

geom_actual <- function(modo, lotes_sf, selected_lotes_ids, puntos, raster_ref = NULL) {
  if (modo == "lote" && length(selected_lotes_ids) > 0) {
    geoms <- lapply(selected_lotes_ids, function(id) {
      g <- vect(lotes_sf[lotes_sf$Lot_Number == id, ])
      if (!is.null(raster_ref) && !is.null(crs(raster_ref)) && !is.null(crs(g)) && crs(raster_ref) != crs(g)) g <- project(g, crs(raster_ref))
      g
    })
    names(geoms) <- selected_lotes_ids
    return(geoms)
  }
  if (modo == "poligono") {
    if (!is.null(puntos) && nrow(puntos) >= 4 &&
        puntos[1, "x"] == puntos[nrow(puntos), "x"] &&
        puntos[1, "y"] == puntos[nrow(puntos), "y"]) {
      g <- vect(cbind(puntos$x, puntos$y), type = "polygons"); crs(g) <- "EPSG:4326"
      if (!is.null(raster_ref) && !is.null(crs(raster_ref))) g <- project(g, crs(raster_ref))
      return(list(`Drawn Area` = g))
    }
  }
  NULL
}

# UI
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      #loading-screen {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        background: rgba(255, 255, 255, 0.8);
        display: flex; justify-content: center; align-items: center;
        z-index: 9999;
      }
      .spinner { display: flex; justify-content: center; align-items: center; }
      .spinner div { width: 12px; height: 12px; margin: 5px; background-color: #007bff; border-radius: 50%; animation: bounce 1.5s infinite ease-in-out; }
      .spinner div:nth-child(1) { animation-delay: 0s; }
      .spinner div:nth-child(2) { animation-delay: 0.2s; }
      .spinner div:nth-child(3) { animation-delay: 0.4s; }
      @keyframes bounce { 0%, 80%, 100% { transform: scale(0); } 40% { transform: scale(1); } }

      .boton-seleccion { background-color: #f8f9fa !important; color: black !important; border: 1px solid #ced4da; }
      .boton-activo { background-color: #007bff !important; color: white !important; border: 1px solid #0056b3; }
      #borrar_seleccion { background-color: #f8f9fa !important; color: black !important; border: 1px solid #ced4da; }
      input[type='checkbox'] { accent-color: #007bff; }
      ::-webkit-scrollbar { width: 6px; }
      ::-webkit-scrollbar-track { background: #f1f1f1; }
      ::-webkit-scrollbar-thumb { background: #888; border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: #555; }

      /* Toolbar flotante sobre el gráfico */
      #chart-container { 
        position: relative;
        margin-top: 12px;  /* separa el chart del mapa */
        padding-top:52px;  /* deja espacio para la toolbar arriba */
      }
      #chart-toolbar {
        position: absolute; 
        left: 12px;
        top: 8px;
        right: auto; 
        bottom: auto;            
        background: rgba(255,255,255,0.92);
        padding: 6px 10px; 
        border-radius: 10px; 
        border: 1px solid #ddd;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1); 
        z-index: 1;              
      }
      #chart-toolbar .radio { margin: 0; }
      #chart-toolbar .shiny-options-group { 
        margin: 0; display: flex; gap: 10px; align-items: center; 
        flex-wrap: wrap;          
      }
      #chart-toolbar label { margin-bottom: 0; font-weight: 600; color: #444; }
    "))
  ),
  
  div(id = "loading-screen", div(class = "spinner", div(), div(), div())),
  
  titlePanel(HTML("&nbsp;")),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      helpText(tags$strong("Choose a mode to analyze the area:", style= "color: #343a40; font-weight: bold;")),
      tags$div(style = "margin-bottom: 15px;"),
      div(style = "display: flex; flex-wrap: wrap; gap: 5px; justify-content: center;",
          actionButton("seleccion_lote", "Select by Plot", class = "boton-seleccion"),
          actionButton("dibujar_poligono", "Draw Polygon", class = "boton-seleccion"),
          actionButton("borrar_seleccion", "Clear Selection", icon = icon("redo"),
                       style = "white-space: nowrap; padding: 6px 10px; text-align: center; border: 1px solid #ced4da;")
      ),
      div(style = "margin-top: 15px;",
          dateRangeInput("fecha_rango", "Select date range:",
                         start = "2025-06-08", end = "2025-12-26",
                         format = "yyyy-mm", min = "2025-06-08", max = "2025-12-26", separator = "to")
      ),
      div(style = "display: flex; align-items: center;",
          selectInput("indice", "Select index:", choices = c("VCI", "NDVI", "EVI", "MSAVI", "GNDVI", "LAI", "GCI","NDWI"), width = "90%"),
          actionButton("info_button", label = icon("info"),
                       style = "background-color: #007bff; color: white; border-radius: 50%;
                                width: 32px; height: 32px; font-size: 16px; border: none; margin-left: 8px;")
      ),
      checkboxInput("toggle_raster", "Show Raster", TRUE),
      
      uiOutput("infoLote_ui"),
      div(style = "max-height: 450px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin-top: 10px;",
          tableOutput("resultados")
      ),
      downloadButton("descargar_datos", "Download Data", style = "margin-top: 10px; width: 100%;")
    ),
    
    mainPanel(
      width = 7,
      leafletOutput("mapPlot", height = "450px"),
      tags$div(style = "height: 10px;"),
      # Contenedor del gráfico con toolbar flotante 
      div(
        id = "chart-container",
        # Toolbar sobre el gráfico:
        div(
          id = "chart-toolbar",
          radioButtons(
            "chart_type", label = NULL,
            choices  = c("📈 Time series" = "ts", "📊 Area distribution" = "hist"),
            selected = "ts", inline = TRUE
          )
        ),
        plotlyOutput("chartPlot", height = "420px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observe({ hide("loading-screen") })
  
  observeEvent(input$info_button, {
    indice_seleccionado <- input$indice
    info_texto <- info_indices[[indice_seleccionado]]
    showModal(modalDialog(
      title = paste("Information about", indice_seleccionado),
      info_texto, easyClose = TRUE, footer = NULL
    ))
  })
  
  lotes_sf <- cargar_shapefile("Main_Shape/Main_Shape.shp")
  
  selected_lote        <- reactiveVal(c())
  puntos_seleccionados <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  modo_seleccion       <- reactiveVal("NULL")
  instruccion          <- reactiveVal(FALSE)
  instruccion_lote     <- reactiveVal(FALSE)
  
  observe({
    runjs('$("#seleccion_lote").css("background-color", "#007bff").css("color", "white");')
    runjs('$("#dibujar_poligono").css("background-color", "#f8f9fa").css("color", "black");')
  })
  
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(
        data = lotes_sf, color = "gray", weight = 1,
        fillOpacity = 0.0, layerId = ~Lot_Number, group = "lotes"
      ) %>%
      setView(lng = -76.36, lat = 3.4977, zoom = 14) %>%
      addEasyButton(easyButton(
        icon = "fa-map-marker", title = "Centrar en el shape",
        onClick = JS("function(btn, map) { map.fitBounds([[3.49, -76.37], [3.51, -76.34]]); }")
      ))
  })
  
  raster_referencia <- rast(list.files(rutas_indices$NDVI, full.names = TRUE, pattern = "\\.(tif|nc)$", ignore.case = TRUE)[1])
  
  observeEvent(list(input$fecha_rango, input$indice, input$toggle_raster), {
    show("loading-screen")
    raster_data <- if (input$toggle_raster) mostrar_raster(input$indice, input$fecha_rango, rutas_indices) else NULL
    paleta <- paletas_colores[[input$indice]]
    
    leaflet_proxy <- leafletProxy("mapPlot") %>%
      clearGroup("raster") %>%
      clearGroup("leyenda_discreta") %>%
      removeControl("legend_discreta") %>%
      removeControl("legend")
    
    if (!is.null(raster_data)) {
      leaflet_proxy %>% addRasterImage(raster_data, colors = paleta, opacity = 0.7, group = "raster")
      if (input$indice %in% c("NDVI", "VCI")) {
        leyenda <- crear_leyenda_discreta(input$indice)
        if (!is.null(leyenda)) {
          leaflet_proxy %>% addLegend(
            colors = leyenda$colores, labels = leyenda$etiquetas,
            title = paste("Categories", input$indice), opacity = 0.7,
            position = "bottomright", layerId = "legend_discreta"
          )
        }
      } else {
        leaflet_proxy %>% addLegend(
          pal = paleta, values = values(raster_data),
          title = input$indice, position = "bottomright", layerId = "legend"
        )
      }
    }
    hide("loading-screen")
  })
  
  observeEvent(input$seleccion_lote, {
    modo_seleccion("lote")
    selected_lote(c())
    puntos_seleccionados(data.frame(x = numeric(), y = numeric()))
    leafletProxy("mapPlot") %>% clearGroup("selected_lote") %>% clearGroup("Área de Estudio")
    runjs('$(".boton-seleccion").removeClass("boton-activo");')
    runjs('$("#seleccion_lote").addClass("boton-activo");')
    if (!instruccion_lote()) {
      showModal(modalDialog(
        title = "Instructions to select plots",
        "Click on a plot to select it. You can select up to two plots for simultaneous analysis.",
        easyClose = TRUE, footer = modalButton("Got it")
      ))
      instruccion_lote(TRUE)
    }
  })
  
  observeEvent(input$dibujar_poligono, {
    modo_seleccion("poligono")
    selected_lote(c())
    puntos_seleccionados(data.frame(x = numeric(), y = numeric()))
    leafletProxy("mapPlot") %>% clearGroup("selected_lote") %>% clearGroup("Área de Estudio")
    output$infoLote_ui <- renderUI({ NULL })
    runjs('$(".boton-seleccion").removeClass("boton-activo");')
    runjs('$("#dibujar_poligono").addClass("boton-activo");')
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
    runjs('$("#borrar_seleccion").css("background-color", "#dc3545").css("color", "white");')
    selected_lote(c())
    puntos_seleccionados(data.frame(x = numeric(), y = numeric()))
    leafletProxy("mapPlot") %>% clearGroup("selected_lote") %>% clearGroup("Área de Estudio")
    runjs('setTimeout(function(){ $("#borrar_seleccion").css("background-color", "#f8f9fa").css("color", "black"); }, 500);')
  })
  
  observeEvent(input$mapPlot_shape_click, {
    if (modo_seleccion() == "NULL") {
      shinyalert(title = "Select an analysis mode",
                 text  = "You must choose either 'Select by Plot' or 'Draw Polygon' before interacting with the map.",
                 type  = "info")
      return()
    }
    req(modo_seleccion() == "lote")
    lote_id <- input$mapPlot_shape_click$id
    if (!is.null(lote_id)) {
      lotes_actuales <- selected_lote()
      if (!(lote_id %in% lotes_actuales)) {
        lotes_actuales <- c(lotes_actuales, lote_id)
        if (length(lotes_actuales) > 2) lotes_actuales <- tail(lotes_actuales, 2)
        selected_lote(lotes_actuales)
      }
      lotes_info <- lotes_sf[lotes_sf$Lot_Number %in% lotes_actuales, ]
      leafletProxy("mapPlot") %>% clearGroup("selected_lote")
      for (i in seq_along(lotes_actuales)) {
        lote_info <- lotes_info[lotes_info$Lot_Number == lotes_actuales[i], ]
        leafletProxy("mapPlot") %>% addPolygons(
          data = lote_info, color = colores_lotes[i], weight = 3,
          fillColor = "transparent", fillOpacity = 0.6, group = "selected_lote"
        )
      }
      output$infoLote_ui <- renderUI({ verbatimTextOutput("infoLote") })
      output$infoLote <- renderPrint({
        for (lote in lotes_actuales) {
          lote_info <- lotes_sf[lotes_sf$Lot_Number == lote, ]
          cat("📍 Selected Plot:", lote_info$Lot_Number, "\n")
          cat("🌱 Crop:", lote_info$Cultivo, "\n\n")
        }
      })
    }
  })
  
  observeEvent(input$mapPlot_click, {
    req(modo_seleccion() == "poligono")
    puntos <- puntos_seleccionados()
    if (nrow(puntos) >= 4 &&
        puntos[1, "x"] == puntos[nrow(puntos), "x"] &&
        puntos[1, "y"] == puntos[nrow(puntos), "y"]) {
      showModal(modalDialog(
        title = "Polygon Completed",
        "The polygon is already closed. To draw a new one, clear the selection.",
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    nuevo_punto <- data.frame(x = input$mapPlot_click$lng, y = input$mapPlot_click$lat)
    valor_raster <- extract(raster_referencia, cbind(nuevo_punto$x, nuevo_punto$y), cells = TRUE)
    if (is.na(valor_raster[1,2])) {
      showModal(modalDialog(
        title = "Error", "The selected point is outside the data area.",
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    if (nrow(puntos) > 0) {
      distancia <- sqrt((nuevo_punto$x - puntos[nrow(puntos), "x"])^2 + (nuevo_punto$y - puntos[nrow(puntos), "y"])^2)
      if (distancia < 0.0001) {
        showModal(modalDialog(
          title = "Error", "The new point is too close to the last added point.",
          easyClose = TRUE, footer = modalButton("Close")
        ))
        return()
      }
    }
    if (nrow(puntos) >= 3) {
      distancia_cierre <- sqrt((nuevo_punto$x - puntos[1, "x"])^2 + (nuevo_punto$y - puntos[1, "y"])^2)
      if (distancia_cierre < 0.00005) puntos <- rbind(puntos, puntos[1, ]) else puntos <- rbind(puntos, nuevo_punto)
    } else puntos <- rbind(puntos, nuevo_punto)
    puntos_seleccionados(puntos)
  })
  
  observe({
    leafletProxy("mapPlot") %>% clearGroup("Área de Estudio") %>% clearMarkers()
    puntos <- puntos_seleccionados()
    if (!is.null(puntos) && nrow(puntos) > 0) {
      leafletProxy("mapPlot") %>% addCircleMarkers(data = puntos, lng = ~x, lat = ~y, color = "#007bff", radius = 2)
      if (nrow(puntos) > 3 &&
          puntos[1, "x"] == puntos[nrow(puntos), "x"] &&
          puntos[1, "y"] == puntos[nrow(puntos), "y"]) {
        polygon_sf <- vect(cbind(puntos$x, puntos$y), type = "polygons"); crs(polygon_sf) <- "EPSG:4326"
        leafletProxy("mapPlot") %>% addPolygons(
          data = polygon_sf, color = "#9D25E5", weight = 2,
          fillColor = "transparent", fillOpacity = 0.4, group = "Área de Estudio"
        )
      }
    }
  })
  
  # Tabla de resultados
  resultados_tabla <- reactive({
    req(input$indice, input$fecha_rango)
    show("loading-screen"); on.exit(hide("loading-screen"), add = TRUE)
    
    if (modo_seleccion() == "lote" && length(selected_lote()) > 0) {
      resultados <- obtener_resultados_lote(selected_lote(), input$indice, input$fecha_rango, lotes_sf, rutas_indices)
      resultados_wide <- reshape(resultados, idvar = "fecha", timevar = "lote", direction = "wide")
      colnames(resultados_wide)[colnames(resultados_wide) == "fecha"] <- "Date"
      colnames(resultados_wide) <- gsub("promedio\\.", "Lote_", colnames(resultados_wide))
    } else if (modo_seleccion() == "poligono") {
      puntos <- puntos_seleccionados()
      if (!is.null(puntos) && nrow(puntos) >= 4 &&
          puntos[1, "x"] == puntos[nrow(puntos), "x"] &&
          puntos[1, "y"] == puntos[nrow(puntos), "y"]) {
        resultados_wide <- obtener_resultados_poligono(puntos, input$indice, input$fecha_rango, rutas_indices)
        colnames(resultados_wide) <- c("Date", "Average")
      } else {
        return(data.frame(Date = "No data", Average = "Draw and close a polygon to view the results"))
      }
    } else {
      return(data.frame(Date = "No data", Average = "Select a plot to view results"))
    }
    resultados_wide
  })
  
  output$resultados <- renderTable({
    req(input$indice, input$fecha_rango)
    show("loading-screen"); on.exit(hide("loading-screen"), add = TRUE)
    resultados <- resultados_tabla()
    if (nrow(resultados) == 0) return(data.frame(Date = "No data", Mensaje = "Select a plot or draw a polygon"))
    resultados
  }, rownames = FALSE)
  
  # Único gráfico (líneas o histograma) 
  output$chartPlot <- renderPlotly({
    req(input$indice, input$fecha_rango)
    show("loading-screen"); on.exit(hide("loading-screen"), add = TRUE)
    
    if (input$chart_type == "ts") {
      # Serie de tiempo
      resultados <- NULL
      if (modo_seleccion() == "lote" && !is.null(selected_lote())) {
        resultados <- obtener_resultados_lote(selected_lote(), input$indice, input$fecha_rango, lotes_sf, rutas_indices)
      } else if (modo_seleccion() == "poligono") {
        puntos <- puntos_seleccionados()
        if (!is.null(puntos) && nrow(puntos) >= 4 &&
            puntos[1, "x"] == puntos[nrow(puntos), "x"] &&
            puntos[1, "y"] == puntos[nrow(puntos), "y"]) {
          resultados <- obtener_resultados_poligono(puntos, input$indice, input$fecha_rango, rutas_indices)
          if (!is.null(resultados) && nrow(resultados) > 0) resultados$lote <- "Drawn Area"
        } else return(NULL)
      } else return(NULL)
      
      if (is.null(resultados) || nrow(resultados) == 0) return(NULL)
      resultados <- resultados[resultados$fecha != "Global Average", ]
      if (!("fecha" %in% colnames(resultados)) || !("promedio" %in% colnames(resultados))) return(NULL)
      
      resultados$fecha    <- as.Date(resultados$fecha)
      resultados$promedio <- as.numeric(resultados$promedio)
      if (nrow(resultados) == 0 || all(is.na(resultados$promedio))) return(NULL)
      
      # Orden y colores consistentes con selección
      lotes_unicos <- unique(resultados$lote)
      orden_lotes <- if (modo_seleccion() == "lote") intersect(selected_lote(), lotes_unicos) else lotes_unicos
      resultados$lote <- factor(resultados$lote, levels = orden_lotes)
      
      plot <- plot_ly(
        data = resultados, x = ~fecha, y = ~promedio,
        type = 'scatter', mode = 'lines+markers',
        color = ~lote,
        colors = colores_lotes[seq_along(orden_lotes)],
        name = ~lote
      )
      
      if (input$indice %in% c("NDWI", "GCI", "LAI", "NDVI")) {
        resultados$suavizado <- suavizar_serie(resultados$promedio)
        if (!all(is.na(resultados$suavizado))) {
          plot <- plot %>% add_trace(y = ~resultados$suavizado,
                                     mode = 'lines', name = paste("Smoothed", resultados$lote),
                                     line = list(dash = 'dash'))
        }
      }
      
      plot %>% layout(
        title  = paste("Average", input$indice, "by Plot"),
        xaxis  = list(title = "Date", type = "date"),
        yaxis  = list(title = paste("Average", input$indice), zeroline = FALSE, showline = FALSE, showgrid = TRUE),
        legend = list(title = list(text = "Plots")),
        margin = list(t = 80)
      )
      
    } else {
      # Histograma de área 
      # Para NDVI usamos el raster crudo para distribución (no clasificado)
      indice_hist <- if (input$indice == "NDVI") "NDVI_raw" else input$indice
      r_mean <- mostrar_raster(indice_hist, input$fecha_rango, rutas_indices)
      if (is.null(r_mean)) return(NULL)
      
      edges <- binEdgesByIndex[[input$indice]]
      if (is.null(edges)) {
        rng <- range(values(r_mean), na.rm = TRUE)
        if (!all(is.finite(rng)) || rng[1] == rng[2]) return(NULL)
        steps <- seq(rng[1], rng[2], length.out = 8)
        edges <- unique(steps)
      }
      if (length(edges) < 2) return(NULL)
      
      geoms <- NULL
      if (modo_seleccion() == "lote" && length(selected_lote()) > 0) {
        geoms <- geom_actual("lote", lotes_sf, selected_lote(), NULL, raster_ref = r_mean)
      } else if (modo_seleccion() == "poligono") {
        geoms <- geom_actual("poligono", lotes_sf, NULL, puntos_seleccionados(), raster_ref = r_mean)
      } else return(NULL)
      if (is.null(geoms) || length(geoms) == 0) return(NULL)
      
      resultados_list <- lapply(names(geoms), function(nm) {
        df <- hist_area_por_geom(r_mean, geoms[[nm]], edges)
        df$lote <- nm
        df
      })
      df_all <- do.call(rbind, resultados_list)
      if (is.null(df_all) || !nrow(df_all)) return(NULL)
      
      # Orden de etiquetas fijo 
      if (!("label" %in% names(df_all))) return(NULL)
      df_all$label <- factor(df_all$label, levels = unique(df_all$label))
      
      # Etiquetas por nombre para VCI
      if (input$indice == "VCI" && all(binEdgesByIndex$VCI == 1:8)) {
        df_all$label <- factor(VCI_CATEGORY_NAMES[df_all$bin_id],
                               levels = VCI_CATEGORY_NAMES, ordered = TRUE)
      }
      
      # Orden y colores iguales a los de la serie
      lotes_unicos <- unique(df_all$lote)
      orden_lotes <- if (modo_seleccion() == "lote") intersect(selected_lote(), lotes_unicos) else lotes_unicos
      colores_asignados <- setNames(colores_lotes[seq_along(orden_lotes)], orden_lotes)
      
      p <- plot_ly()
      for (lt in orden_lotes) {
        dsub <- df_all[df_all$lote == lt, ]
        p <- add_bars(
          p,
          x = dsub$label, y = dsub$area_ha, name = lt,
          marker = list(color = colores_asignados[[lt]]),
          hovertemplate = paste0(lt, "<br>Range: %{x}<br>Area: %{y:.2f} ha<extra></extra>")
        )
      }
      
      # Eje X: título y orden si es VCI
      xaxis_title <- if (input$indice == "VCI") "VCI category" else paste(input$indice, "range")
      xaxis_cfg <- list(title = xaxis_title, tickangle = -45, automargin = TRUE)
      if (input$indice == "VCI") {
        xaxis_cfg$categoryorder <- "array"
        xaxis_cfg$categoryarray <- VCI_CATEGORY_NAMES
      }
      
      p %>% layout(
        barmode = if (length(orden_lotes) > 1) "group" else "relative",
        title   = paste("Area Distribution —", input$indice),
        xaxis   = xaxis_cfg,
        yaxis   = list(title = "Area (ha)", rangemode = "tozero"),
        legend  = list(title = list(text = "Plots / AOI")),
        margin  = list(t = 70, b = 90)
      )
    }
  })
  
  observe({
    resultados <- resultados_tabla()
    if (nrow(resultados) > 1) shinyjs::show("descargar_datos") else shinyjs::hide("descargar_datos")
  })
  
  output$descargar_datos <- downloadHandler(
    filename = function() {
      req(nrow(resultados_tabla()) > 1)
      indice <- input$indice
      fecha_inicio <- input$fecha_rango[1]
      fecha_fin    <- input$fecha_rango[2]
      paste0("Results_", indice, "_", fecha_inicio, "_", fecha_fin, ".csv")
    },
    content = function(file) {
      resultados <- resultados_tabla()
      if (nrow(resultados) <= 1 || "No data" %in% resultados$Date) {
        showModal(modalDialog(
          title = "Error", "No data to download. Select a plot or draw a polygon.",
          easyClose = TRUE, footer = modalButton("Close")
        ))
        return(NULL)
      }
      write.csv(resultados, file, row.names = FALSE)
    }
  )
}

# Ejecutar app
shinyApp(ui = ui, server = server)

