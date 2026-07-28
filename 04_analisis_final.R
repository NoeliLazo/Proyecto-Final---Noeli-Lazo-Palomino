#============================================================#
# PROYECTO FINAL DE RSTUDIO                                   #
# PARTE 2: ANALISIS FINAL                                     #
#============================================================#

rm(list = ls())
graphics.off()

if (!dir.exists("data") && dir.exists("../data")) {
  setwd("..")
}

library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

paquetes <- c(
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "forcats",
  "scales",
  "patchwork"
)

instalados <- rownames(
  installed.packages()
)

faltantes <- paquetes[
  !paquetes %in% instalados
]

if (length(faltantes) > 0) {
  install.packages(
    faltantes,
    repos = "https://cloud.r-project.org",
    dependencies = TRUE
  )
}

cat(
  "Paquetes disponibles:",
  paste(paquetes, collapse = ", "),
  "\n"
)

# 1. Cargar la base limpia de R -------------------------------
# La forma principal es readRDS(), despues de ejecutar EDA.R.
# El archivo .R incluido permite abrir el proyecto de inmediato.
if (file.exists("data/base_limpia_EPEN_Junin_2025.rds")) {
  datos <- readRDS(
    "data/base_limpia_EPEN_Junin_2025.rds"
  )
} else {
  source(
    "data/base_limpia_EPEN_Junin_2025.R",
    encoding = "UTF-8"
  )
}

poblacion_14_mas <- datos %>%
  filter(
    residente_codigo == 1,
    edad >= 14,
    !is.na(factor_expansion),
    factor_expansion > 0,
    !is.na(condicion_actividad)
  )

ocupados <- poblacion_14_mas %>%
  filter(
    condicion_actividad_codigo == 1,
    !is.na(empleo_informal)
  )

# 2. Pregunta de analisis -------------------------------------
# ¿Como se relacionan el nivel educativo y el sexo con
# la informalidad laboral en Junin durante 2025?

# 3. Indicadores ----------------------------------------------
indicadores_generales <- ocupados %>%
  summarise(
    cantidad_muestral = n(),
    poblacion_ocupada_estimada = sum(
      factor_expansion,
      na.rm = TRUE
    ),
    tasa_informalidad = weighted.mean(
      empleo_informal,
      factor_expansion,
      na.rm = TRUE
    )
  )

tabla_informalidad_sexo <- ocupados %>%
  filter(
    !is.na(sexo)
  ) %>%
  group_by(sexo) %>%
  summarise(
    cantidad_muestral = n(),
    poblacion_ocupada_estimada = sum(
      factor_expansion,
      na.rm = TRUE
    ),
    tasa_informalidad = weighted.mean(
      empleo_informal,
      factor_expansion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

tabla_informalidad_educacion_sexo <- ocupados %>%
  filter(
    !is.na(nivel_educativo),
    !is.na(sexo)
  ) %>%
  group_by(
    nivel_educativo,
    sexo
  ) %>%
  summarise(
    cantidad_muestral = n(),
    poblacion_ocupada_estimada = sum(
      factor_expansion,
      na.rm = TRUE
    ),
    tasa_informalidad = weighted.mean(
      empleo_informal,
      factor_expansion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

tabla_ingreso_formalidad <- ocupados %>%
  filter(
    !is.na(informalidad),
    !is.na(ingreso_laboral),
    ingreso_laboral >= 0
  ) %>%
  group_by(informalidad) %>%
  summarise(
    cantidad_muestral = n(),
    ingreso_promedio = weighted.mean(
      ingreso_laboral,
      factor_expansion,
      na.rm = TRUE
    ),
    ingreso_mediano = median(
      ingreso_laboral,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

tabla_informalidad_area <- ocupados %>%
  filter(
    !is.na(area)
  ) %>%
  group_by(area) %>%
  summarise(
    cantidad_muestral = n(),
    poblacion_ocupada_estimada = sum(
      factor_expansion,
      na.rm = TRUE
    ),
    tasa_informalidad = weighted.mean(
      empleo_informal,
      factor_expansion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

indicadores_generales
tabla_informalidad_sexo
tabla_informalidad_educacion_sexo
tabla_ingreso_formalidad
tabla_informalidad_area

write_csv(
  indicadores_generales,
  "tables/analisis_final_indicadores_generales.csv"
)

write_csv(
  tabla_informalidad_sexo,
  "tables/analisis_final_informalidad_sexo.csv"
)

write_csv(
  tabla_informalidad_educacion_sexo,
  "tables/analisis_final_informalidad_educacion_sexo.csv"
)

write_csv(
  tabla_ingreso_formalidad,
  "tables/analisis_final_ingreso_formalidad.csv"
)

write_csv(
  tabla_informalidad_area,
  "tables/analisis_final_informalidad_area.csv"
)

# 4. Grafico final --------------------------------------------
grafico_final <- tabla_informalidad_educacion_sexo %>%
  ggplot(
    aes(
      x = tasa_informalidad,
      y = nivel_educativo,
      fill = sexo
    )
  ) +
  geom_col(
    position = position_dodge(
      width = 0.76
    ),
    width = 0.68
  ) +
  geom_text(
    aes(
      label = label_percent(
        accuracy = 0.1,
        decimal.mark = ","
      )(tasa_informalidad)
    ),
    position = position_dodge(
      width = 0.76
    ),
    hjust = -0.08,
    size = 3.3,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Hombres" = "#15558D",
      "Mujeres" = "#20A0A0"
    )
  ) +
  scale_x_continuous(
    labels = label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    limits = c(0, 1.08),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  labs(
    title = paste(
      "Educacion e informalidad laboral:",
      "una brecha persistente en Junin"
    ),
    subtitle = paste(
      "La informalidad disminuye con el nivel educativo",
      "y presenta diferencias por sexo"
    ),
    x = "Tasa de empleo informal",
    y = "Nivel educativo",
    fill = "Sexo",
    caption = paste(
      "Fuente: INEI - Encuesta Permanente de Empleo Nacional",
      "(EPEN), base anual 2025.",
      "Tasas ponderadas con FAC300_ANUAL."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = "#15324B"
    ),
    plot.subtitle = element_text(
      color = "#5F6B7A"
    ),
    plot.caption = element_text(
      size = 8.5,
      color = "#6B7280",
      hjust = 0
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(
  "figures/grafico_final_informalidad.png",
  grafico_final,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

print(grafico_final)

# 5. Conclusiones ---------------------------------------------
tasa_total <- indicadores_generales$tasa_informalidad[1]

tasa_hombres <- tabla_informalidad_sexo %>%
  filter(sexo == "Hombres") %>%
  pull(tasa_informalidad)

tasa_mujeres <- tabla_informalidad_sexo %>%
  filter(sexo == "Mujeres") %>%
  pull(tasa_informalidad)

cat(
  "\nCONCLUSIONES\n",
  "1. Tasa total de empleo informal: ",
  percent(tasa_total, accuracy = 0.1),
  ".\n",
  "2. Tasa de las mujeres: ",
  percent(tasa_mujeres, accuracy = 0.1),
  ".\n",
  "3. Tasa de los hombres: ",
  percent(tasa_hombres, accuracy = 0.1),
  ".\n",
  "4. La informalidad disminuye conforme aumenta el nivel educativo.\n",
  "5. Los resultados son descriptivos y no demuestran causalidad.\n",
  sep = ""
)
