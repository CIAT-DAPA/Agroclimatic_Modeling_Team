# 🌱 Agroclimatic Monitoring System (Shiny App)

Interactive **Shiny** dashboard to visualize and analyze vegetation/spectral indices over:

- ✅ **Predefined plots (shapefile polygons)**  
- ✅ **User-drawn polygon (AOI)** directly on the map  

---

## 🚀 Features

### 🔎 Two Analysis Modes

- **Select by Plot**
  - Click shapefile polygons
  - Supports up to **2 plots simultaneously**

- **Draw Polygon**
  - Draw and close a custom AOI on the map
  - Automatic raster extraction within AOI

---

### 🌿 Supported Indices

| Index | Visualization | Analysis |
|-------|--------------|----------|
| NDVI  | Classified raster | Raw NDVI |
| NDWI  | Raw | Raw |
| VCI   | Classified raster 
| LAI   | Raw | Raw |
| GCI   | Raw | Raw |
| EVI   | Raw | Raw |
| GNDVI | Raw | Raw |
| MSAVI | Raw | Raw |

---

### 📊 Outputs

- 📈 **Time Series**
  - Mean index values per plot or AOI
  - Optional Savitzky–Golay smoothing

- 📉 **Area Distribution**
  - Histogram by index bins
  - Area expressed in hectares (ha)

- 📥 **CSV Download**
  - Export processed results

---

## 📂 Project Structure

Expected folder layout:
├─ app.R
├─ Main_Shape/
│  ├─ Main_Shape.shp
│  ├─ Main_Shape.shx
│  ├─ Main_Shape.dbf
│  ├─ Main_Shape.prj
│  └─ (optional) Main_Shape.cpg
└─ Indices_Agroclimaticos/
   ├─ GCI_tif/
   ├─ LAI_tif/
   ├─ NDVI_classified_tif/   # Map visualization
   ├─ NDVI_tif/              # Raw NDVI for analysis
   ├─ NDWI_tif/
   ├─ VCI_classified_tif/
   ├─ GNDVI_tif/
   ├─ EVI_tif/
   └─ MSAVI_tif/

---

## File Naming Convention (Important)

Raster filenames **must include a date** in this format: YYYY-MM-DD

### Examples:
NDVI_2025-06-08.tif
VCI_2025-12-26.tif

The app extracts the date automatically from filenames to filter data by selected time range.

---

## 📌 Data Requirements

### 🗺 Shapefile Attributes

The shapefile must contain at least:

- `Lot_Number` → Unique plot identifier
- `Cultivo` → Crop name (displayed in sidebar)

---

### 🛰 Raster Data

- Supported formats:
  - `.tif`
  - `.nc`
- Recommended CRS:
  - **EPSG:4326 (WGS84)**

> ⚠️ The app reprojects polygons to match rasters if necessary.

---

🔁 Reproducibility & Configuration

By default, the app uses relative paths:

-Main_Shape/Main_Shape.shp
-Indices_Agroclimaticos/<INDEX_FOLDER>/

To customize paths:

-Edit the rutas_indices list
-Modify the shapefile path inside server in app.R
