# ============================================================
# Producción de gráfico Plotly
# Distribución elíptica bivariada (Mahalanobis, 95%)
# ============================================================

# ---- Locale (evita problemas UTF-8 / htmlwidgets) ------------
Sys.setlocale("LC_CTYPE", "Spanish_Mexico.utf8")

# ---- Librerías -----------------------------------------------
library(plotly)
library(ellipse)

# ---- Datos ---------------------------------------------------
set.seed(123)

x <- rnorm(1000, mean = 50, sd = 10)
y <- 0.6 * x + rnorm(1000, sd = 5)

df <- data.frame(x = x, y = y)

# ---- Parámetros poblacionales -------------------------------
mu    <- as.numeric(colMeans(df))
Sigma <- cov(df)

# ---- Elipse de confianza (95%) -------------------------------
ell <- as.data.frame(
  ellipse::ellipse(
    Sigma,
    centre  = mu,
    level   = 0.95,
    npoints = 300
  )
)

# ---- Clasificación por Mahalanobis ---------------------------
d2        <- mahalanobis(df, mu, Sigma)
threshold <- qchisq(0.95, df = 2)

df$fuera <- d2 > threshold

# ---- Gráfico Plotly -----------------------------------------
p <- plot_ly() %>%
  
  # Puntos dentro
  add_markers(
    data = df[!df$fuera, ],
    x = ~x,
    y = ~y,
    marker = list(
      color   = "#265129",
      size    = 6,
      opacity = 0.45
    ),
    name = "Dentro 95%"
  ) %>%
  
  # Puntos fuera
  add_markers(
    data = df[df$fuera, ],
    x = ~x,
    y = ~y,
    marker = list(
      color   = "red",
      size    = 6,
      opacity = 0.8
    ),
    name = "Fuera 95%"
  ) %>%
  
  # Región elíptica
  add_polygons(
    x = ell$x,
    y = ell$y,
    fillcolor = "rgba(38,81,41,0.25)",
    line = list(
      color = "#265129",
      width = 2
    ),
    name = "Región de confianza 95%"
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
    title = "Distribución de probabilidad ELÍPTICA para dos variables",
    xaxis = list(title = "Variable A"),
    yaxis = list(title = "Variable B"),
    showlegend = FALSE
  )

# ---- Visualización local (opcional) --------------------------
p

# ---- Congelamiento -------------------------------------------
saveRDS(
  p,
  file = "p7_distribucion_eliptica_bivariada_95.rds"
)
