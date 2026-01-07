# ============================================================
# Producción de gráfico Plotly
# Solapamiento de distribuciones lognormales
# ============================================================

# ---- Locale (evita errores UTF-8 / htmlwidgets) -------------
Sys.setlocale("LC_CTYPE", "Spanish_Mexico.utf8")

# ---- Librerías ----------------------------------------------
library(plotly)

# ---- Datos --------------------------------------------------
set.seed(456)

# Procesos sesgados (lognormal)
x1 <- rlnorm(1000, meanlog = 3.8, sdlog = 0.25)
x2 <- rlnorm(1000, meanlog = 4.0, sdlog = 0.30)

# ---- Estimaciones de densidad ------------------------------
dens1 <- density(x1)
dens2 <- density(x2)

# ---- Gráfico Plotly ----------------------------------------
p <- plot_ly() %>%
  
  # Histograma Proceso A
  add_histogram(
    x = x1,
    histnorm = "probability density",
    name = "Proceso A",
    opacity = 0.5,
    marker = list(color = "#265129")
  ) %>%
  
  # Histograma Proceso B
  add_histogram(
    x = x2,
    histnorm = "probability density",
    name = "Proceso B",
    opacity = 0.5,
    marker = list(color = "#9E2A2B")
  ) %>%
  
  # Densidad A
  add_lines(
    x = dens1$x,
    y = dens1$y,
    name = "Densidad A",
    line = list(color = "#1B4332", width = 3)
  ) %>%
  
  # Densidad B
  add_lines(
    x = dens2$x,
    y = dens2$y,
    name = "Densidad B",
    line = list(color = "#7F1D1D", width = 3)
  ) %>%
  
  # Layout controlado
  layout(
    width  = 1050,
    height = 480,
    margin = list(
      l = 60,
      r = 40,
      t = 80,
      b = 40
    ),
    title = "Solapamiento de Distribuciones en Datos IRREGULARES",
    xaxis = list(title = "Variable de Procesos A y B"),
    yaxis = list(title = "Densidad de Probabilidad"),
    barmode = "overlay"
  )

# ---- Visualización local (opcional) -------------------------
p

# ---- Congelamiento ------------------------------------------
saveRDS(
  p,
  file = "p6_solapamiento_distribuciones_lognormales.rds"
)
