# 🌱 Agroclimatic Monitoring System — Zamorano (Shiny App)

Interactive **Shiny** dashboard to visualize and analyze vegetation/spectral indices over:

- ✅ **Predefined variety plots (shapefile polygons)**, or  
- ✅ **A user-drawn polygon (AOI)** directly on the map  

This version is configured for the **Zamorano** study area and reads rasters from an automated index workflow.

---

## 🚀 Features

### 🧭 Two analysis modes

- **Select by Variety**
  - Click inside variety polygons to select a variety
  - Supports up to **2 varieties** at the same time

- **Draw Polygon**
  - Draw an AOI by clicking points on the map
  - Close the polygon by clicking near the first point

---

## 🛰 Supported indices

- **NDVI**
- **EVI**
- **MSAVI2**
- **GNDVI**
- **LAI**
- **GCI**
- **NDWI**

---

## 📊 Outputs

### 📈 Time series
- Mean index values by date for the selected variety(ies) or drawn AOI
- Optional **Savitzky–Golay smoothing** in the time-series view

### 📊 Area distribution
- Histogram of **area (ha)** per index bin within:
  - Selected variety polygon(s), or
  - Drawn AOI polygon

### 📥 Download
- Export results to **CSV** using the *Download Data* button  
  *(button is enabled only when valid results exist)*

---

## 📂 Project structure

```text
.
├── app.R
├── shapes/
│   ├── Zamorano/
│   │   └── Zamorano_Shape.shp
│   └── Parcela_Variedades_Shape/
│       └── Parcela_Variedades.shp
└── output/
    ├── GCI_tif/
    ├── LAI_tif/
    ├── NDVI_tif/
    ├── NDWI_tif/
    ├── EVI_tif/
    ├── MSAVI_tif/
    └── GNDVI_tif/
```

---

## File naming convention (required)

Raster filenames **must include a date** in the form:

- `YYYY-MM-DD`

Examples:

- `NDVI_2025-08-07.tif`
- `LAI_2025-12-26.tif`

The app extracts dates from filenames to filter rasters based on the selected date range.

---

## 📌 Data requirements

### Shapefiles

#### 1) Study boundary (optional)
- Path: `shapes/Zamorano/Zamorano_Shape.shp`
- Used to:
  - Define map bounds (`fitBounds`)
  - Show study extent boundary

#### 2) Variety polygons (required for “Select by Variety”)
- Path: `shapes/Parcela_Variedades_Shape/Parcela_Variedades.shp`
- Must include the field:
  - `Variedad` *(used for selection, labeling and plotting)*

> The app automatically transforms shapefiles to **EPSG:4326** if needed.

---
