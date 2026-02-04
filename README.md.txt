# Agroclimatic Monitoring System (Shiny App)

Interactive **Shiny** dashboard to visualize and analyze vegetation/spectral indices over:
- **Predefined plots (shapefile polygons)**, or
- **A user-drawn polygon** (AOI) on the map

The app supports:
- Map visualization (Leaflet) + raster overlay
- Time series of mean index values
- Area distribution (histogram by index bins)
- CSV download of results
- Optional Savitzky–Golay smoothing for selected indices

---

## Features

- **Two analysis modes**
  - **Select by Plot**: click polygons (up to 2 plots at once)
  - **Draw Polygon**: click to draw and close an AOI

- **Indices supported**
  - NDVI (visualization uses classified raster; analysis uses raw NDVI)
  - NDWI, VCI, LAI, GCI, EVI, GNDVI, MSAVI

- **Charts**
  - **Time series** of mean values per plot/AOI
  - **Area distribution** (ha) by index ranges (bins)

---

## Project structure

Expected folder layout:

```
.
├─ app.R
├─ Main_Shape/
│  ├─ Main_Shape.shp
│  ├─ Main_Shape.shx
│  ├─ Main_Shape.dbf
│  ├─ Main_Shape.prj
│  └─ (optional) .cpg
└─ Indices_Agroclimaticos/
   ├─ GCI_tif/
   ├─ LAI_tif/
   ├─ NDVI_classified_tif/   # for map visualization
   ├─ NDVI_tif/              # raw NDVI for analysis
   ├─ NDWI_tif/
   ├─ VCI_classified_tif/
   ├─ GNDVI_tif/
   ├─ EVI_tif/
   └─ MSAVI_tif/
```

### File naming convention (important)

Raster filenames must include a date in the form:

- `YYYY-MM-DD`

Examples:
- `NDVI_2025-06-08.tif`
- `VCI_2025-12-26.tif`

The app extracts dates from filenames to filter by the selected date range.

---

## Data requirements

### Shapefile attributes
The shapefile **must** contain at least these fields:
- `Lot_Number` (unique plot ID used for click selection)
- `Cultivo` (crop name shown in the sidebar)

### Raster data
- Supported extensions: `.tif` or `.nc`
- Recommended CRS: **EPSG:4326** (WGS84), but the app reprojects polygons to match rasters when needed.

> Tip: For large rasters, consider using tiled GeoTIFFs and keep a reasonable resolution to avoid memory issues.

---

## Installation

### 1) Install R packages
In R:

```r
install.packages(c(
  "terra","shiny","plotly","leaflet","sf","rsconnect",
  "leaflet.extras","shinyjs","signal","shinyalert"
))
```

### 2) Run the app
From the project root:

```r
shiny::runApp()
```

Or open `app.R` and click **Run App** in RStudio.

---

## Notes on basemaps

This app uses:

- `Esri.WorldImagery` via `leaflet::addProviderTiles()`

Use of this basemap is subject to Esri/Leaflet provider terms and conditions.

---

## Publishing / Deployment

### shinyapps.io
If deploying to shinyapps.io:
- Do **not** upload large rasters to the platform (size limits + performance).
- Prefer mounting data from a server, or keep demo datasets only.

Typical deploy command:

```r
rsconnect::deployApp(appDir = ".", forceUpdate = TRUE)
```

---

## Reproducibility & configuration

By default, the app expects data paths as relative folders:
- `Main_Shape/Main_Shape.shp`
- `Indices_Agroclimaticos/<INDEX_FOLDER>/`

If you want to customize paths, edit the `rutas_indices` list and the shapefile path inside the server.

