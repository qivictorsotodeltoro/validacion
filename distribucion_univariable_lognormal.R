# ============================================================
# Producción de gráfico Plotly
# Distribución univariable: lognormal
# ============================================================

# ---- Locale (evita errores UTF-8 en htmlwidgets) ------------
Sys.setlocale("LC_CTYPE", "Spanish_Mexico.utf8")

# ---- Librerías ----------------------------------------------
library(plotly)

# ---- Datos --------------------------------------------------
set.seed(123)

# Datos lognormales
x <- rlnorm(1000, meanlog = 3.8, sdlog = 0.3)

# ---- Estimación de densidad --------------------------------
dens <- density(x)

# ---- Gráfico Plotly ----------------------------------------
p <- plot_ly() %>%
  
  # Histograma normalizado
  add_histogram(
    x = x,
    histnorm = "probability density",
    name = "Histograma",
    opacity = 0.6,
    marker = list(color = "#265129")
  ) %>%
  
  # Curva de densidad
  add_lines(
    x = dens$x,
    y = dens$y,
    name = "Densidad",
    line = list(color = "black", width = 3)
  ) %>%
  
  # Layout controlado (clave para Quarto / Reveal)
  layout(
    width  = 1000,
    height = 480,
    margin = list(
      l = 60,
      r = 40,
      t = 80,
      b = 40
    ),
    title = "Datos univariables: distribucion lognormal",
    xaxis = list(title = "Variable X"),
    yaxis = list(title = "Densidad")
  )

# ---- Visualización local (opcional) -------------------------
p

# ---- Congelamiento ------------------------------------------
saveRDS(
  p,
  file = "p4_distribucion_univariable_lognormal.rds"
)
