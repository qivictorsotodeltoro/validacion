Sys.setlocale("LC_CTYPE", "Spanish_Mexico.utf8")

# ============================================================
# Producción de gráfico Plotly 3D
# Región elipsoidal de decisión (Hotelling T²)
# ============================================================

library(plotly)
library(MASS)

# --- Parámetros ------------------------------------------------
set.seed(123)

Mean <- c(1, 2, 3)

Sigma <- matrix(
  c(10, 3, 0,
    3,  2, 0,
    0,  0, 1),
  nrow = 3,
  byrow = TRUE
)

alpha <- 0.95
n     <- 1000

# --- Datos multivariados --------------------------------------
x <- MASS::mvrnorm(n = n, mu = Mean, Sigma = Sigma)

# --- Hotelling T² / Mahalanobis -------------------------------
T2        <- mahalanobis(x, center = Mean, cov = Sigma)
threshold <- qchisq(alpha, df = ncol(x))

inside  <- T2 <= threshold
outside <- T2 >  threshold

# --- Construcción del elipsoide -------------------------------
eig   <- eigen(Sigma)
U     <- eig$vectors
radii <- sqrt(eig$values * threshold)

phi   <- seq(0, pi, length.out = 50)
theta <- seq(0, 2*pi, length.out = 50)

sphere <- expand.grid(phi = phi, theta = theta)

xs <- radii[1] * sin(sphere$phi) * cos(sphere$theta)
ys <- radii[2] * sin(sphere$phi) * sin(sphere$theta)
zs <- radii[3] * cos(sphere$phi)

ellipsoid <- t(U %*% rbind(xs, ys, zs))
ellipsoid <- sweep(ellipsoid, 2, Mean, "+")

X <- matrix(ellipsoid[,1], nrow = length(phi))
Y <- matrix(ellipsoid[,2], nrow = length(phi))
Z <- matrix(ellipsoid[,3], nrow = length(phi))

# --- Plotly ----------------------------------------------------
p <- plot_ly() %>%
  
  # Puntos dentro de la región
  add_markers(
    x = x[inside, 1],
    y = x[inside, 2],
    z = x[inside, 3],
    marker = list(
      color   = "#265129",
      size    = 4,
      opacity = 0.5
    ),
    name = "Dentro de región válida"
  ) %>%
  
  # Puntos fuera
  add_markers(
    x = x[outside, 1],
    y = x[outside, 2],
    z = x[outside, 3],
    marker = list(
      color = "red",
      size  = 6
    ),
    name = "Fuera de región (outliers)"
  ) %>%
  
  # Elipsoide de decisión
  add_surface(
    x = X,
    y = Y,
    z = Z,
    opacity    = 0.35,
    showscale = FALSE,
    colorscale = list(c(0, 1), c("#265129", "#265129")),
    name = "Región de decisión 95%"
  ) %>%
  
  layout(
    width  = 1000,
    height = 480,
    margin = list(l = 40, r = 40, t = 60, b = 40),
    title  = "Región elipsoidal de decisión multivariada (Hotelling T², 95%)",
    scene  = list(
      xaxis = list(title = "Eigen 1"),
      yaxis = list(title = "Eigen 2"),
      zaxis = list(title = "Eigen 3"),
      aspectmode = "cube"
    ),
    legend = list(x = 0.02, y = 0.98)
  )

# --- Congelamiento ---------------------------------------------
saveRDS(p, file = "p1_ellipsoid_hotelling.rds")





htmlwidgets::saveWidget(
  p,
  "ellipsoid_hotelling.html",
  selfcontained = TRUE
)

browseURL("ellipsoid_hotelling.html")

