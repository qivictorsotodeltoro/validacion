# ============================================================
# Produccion de grafico Plotly
# Analisis vs Estudio vs Validacion
# ============================================================

# ---- Locale (evita errores UTF-8 en htmlwidgets) ------------
Sys.setlocale("LC_CTYPE", "Spanish_Mexico.utf8")

# ---- Librerias ----------------------------------------------
library(ggplot2)
library(plotly)
library(dplyr)
library(tibble)

# ---- Datos --------------------------------------------------
df <- tibble(
  Tipo = c("Analisis", "Estudio", "Validacion"),
  x = c(1.2, 2.2, 3.2),
  y = c(1.2, 2.2, 3.2),
  Descripcion = c(
    "Describe el estado actual del fenomeno. No conduce a decisiones obligatorias.",
    "Explora relaciones y escenarios posibles. Orienta, pero no autoriza.",
    "Autoriza decisiones tecnicas bajo riesgo explicito. Es exigible y auditable."
  )
)

# ---- Grafico ggplot -----------------------------------------
p_gg <- ggplot(
  df,
  aes(
    x = x,
    y = y,
    label = Tipo,
    text = paste0(
      "<b>", Tipo, "</b><br><br>",
      Descripcion
    )
  )
) +
  geom_point(size = 6, color = "#265129") +
  geom_text(
    nudge_y = 0.28,
    size = 5,
    color = "#265129",
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(0.5, 3.8),
    breaks = c(1, 2, 3),
    labels = c("Exploratorio", "Caracterizacion", "Operativo"),
    name = "Tipo de proyecto"
  ) +
  scale_y_continuous(
    limits = c(0.5, 3.8),
    breaks = c(1, 2, 3),
    labels = c("Bajo", "Medio", "Alto"),
    name = "Riesgo / Consecuencia"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18)
  ) +
  ggtitle("Cuando analizar y cuando validar")

# ---- Conversión a Plotly -----------------------------------
p_plotly <- ggplotly(
  p_gg,
  tooltip = "text",
  width   = 1000,
  height  = 520
) %>%
  layout(
    margin = list(
      l = 60,
      r = 40,
      t = 80,
      b = 40
    )
  )

# ---- Visualizacion local (opcional) -------------------------
p_plotly

# ---- Congelamiento ------------------------------------------
saveRDS(
  p_plotly,
  file = "p2_analisis_estudio_validacion.rds"
)
