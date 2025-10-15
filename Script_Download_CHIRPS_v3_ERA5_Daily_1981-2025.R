## sapa
################################################################################
## Este script descarga datos diarios de precipitación histórica de CHIRPS v3 ##
## (producto derivado de ERA5).                                               ##
## Desarrollado por: Camilo Barrios-Pérez (c.barrios@cgiar.org)               ##
################################################################################

# Ruta base donde se ubicará la carpeta de descargas
base_parent_dir = "D:/CHIRPS_v3_ERA5"
# Carpeta raíz de salida dentro de la ruta base
base_out_dir = file.path(base_parent_dir, "chirps_daily_1981_1983")

# crear carpeta padre y carpeta raíz de una sola vez (recursive = TRUE)
if (!dir.exists(base_out_dir)) {
  dir.create(base_out_dir, recursive = TRUE)
}

# Secuencia de fechas
dates <- seq(as.Date("1981-01-01"), as.Date("2025-06-30"), by = "day")

# base CHIRPS URL
base_url = "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/daily/final/ERA5"

# Bucle de descarga
for (i in 1:length(dates)) {
  d = dates[i]
  year_str = format(d, "%Y")
  date_str = format(d, "%Y.%m.%d")
  file_name = paste0("chirps-v3.0.", date_str, ".tif")
  
  # Path de la subcarpeta del año
  year_dir = file.path(base_out_dir, year_str)
  if (!dir.exists(year_dir)) {
    dir.create(year_dir)  # Crea cada carpeta de año
  }
  
  # Destino final
  destfile = file.path(year_dir, file_name)
  # URL remota
  url = paste0(base_url, "/", year_str, "/", file_name)
  
  # Descarga si no existe
  if (!file.exists(destfile)) {
    message("Descargando ", file_name, " en ", year_dir, " …")
    try(
      download.file(url, destfile, mode = "wb", quiet = TRUE),
      silent = TRUE
    )
  } else {
    message(file_name, " ya existe en ", year_dir, "; omitiendo.")
  }
}
