#============================================================#
# PROYECTO FINAL DE RSTUDIO                                   #
# PARTE 1: ANALISIS EXPLORATORIO DE DATOS (EDA)               #
# Base: Encuesta Permanente de Empleo Nacional - Junin 2025   #
#============================================================#

# 0. Limpiar el entorno ---------------------------------------
rm(list = ls())
graphics.off()

# Si el directorio actual es scripts, regresar a la raiz.
if (!dir.exists("data") && dir.exists("../data")) {
  setwd("..")
}

# 1. Cargar paquetes ------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(patchwork)

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


# 2. Importar la base ORIGINAL --------------------------------
# Todas las columnas se leen como texto para conservar los codigos.
df <- read_csv(
  "data/12_Junin_EPEN_Anual_2025.csv",
  col_types = cols(
    .default = col_character()
  ),
  na = c("", "NA", "N/A", "NULL"),
  show_col_types = FALSE
)

# 3. Exploracion inicial --------------------------------------
dim(df)
glimpse(df)
summary(df)

df %>%
  is.na() %>%
  colSums()

sum(duplicated(df))

variables_necesarias <- c(
  "ANIO", "MES", "CCDD", "AREA", "C207", "C208",
  "C366", "OCUP300", "INGTOT", "RESIDENT",
  "Informal_P", "FAC300_ANUAL"
)

variables_faltantes <- setdiff(
  variables_necesarias,
  names(df)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "Faltan estas variables en la base: ",
      paste(variables_faltantes, collapse = ", ")
    )
  )
}

a_numero <- function(x) {
  suppressWarnings(
    as.numeric(x)
  )
}

# 4. Limpieza y preparacion -----------------------------------
datos <- df %>%
  distinct() %>%
  transmute(
    anio = as.integer(a_numero(ANIO)),
    mes = as.integer(a_numero(MES)),
    departamento = as.character(CCDD),
    area_codigo = as.integer(a_numero(AREA)),
    sexo_codigo = as.integer(a_numero(C207)),
    edad = a_numero(C208),
    nivel_educativo_codigo = as.integer(a_numero(C366)),
    condicion_actividad_codigo = as.integer(a_numero(OCUP300)),
    ingreso_laboral = a_numero(INGTOT),
    residente_codigo = as.integer(a_numero(RESIDENT)),
    informalidad_codigo = as.integer(a_numero(Informal_P)),
    factor_expansion = a_numero(FAC300_ANUAL)
  ) %>%
  mutate(
    area = factor(
      area_codigo,
      levels = c(1, 2),
      labels = c("Urbana", "Rural")
    ),
    sexo = factor(
      sexo_codigo,
      levels = c(1, 2),
      labels = c("Hombres", "Mujeres")
    ),
    condicion_actividad = factor(
      condicion_actividad_codigo,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Ocupada",
        "Desempleada abierta",
        "Desempleada oculta",
        "Inactiva"
      )
    ),
    nivel_educativo = case_when(
      nivel_educativo_codigo %in% c(1, 2, 7) ~
        "Sin nivel, inicial o especial",
      nivel_educativo_codigo %in% c(3, 4) ~
        "Primaria",
      nivel_educativo_codigo %in% c(5, 6) ~
        "Secundaria",
      nivel_educativo_codigo %in% c(8, 9) ~
        "Superior no universitaria",
      nivel_educativo_codigo %in% c(10, 11) ~
        "Superior universitaria",
      nivel_educativo_codigo == 12 ~
        "Posgrado",
      TRUE ~ NA_character_
    ),
    nivel_educativo = factor(
      nivel_educativo,
      levels = c(
        "Sin nivel, inicial o especial",
        "Primaria",
        "Secundaria",
        "Superior no universitaria",
        "Superior universitaria",
        "Posgrado"
      )
    ),
    informalidad = factor(
      informalidad_codigo,
      levels = c(1, 2),
      labels = c(
        "Empleo informal",
        "Empleo formal"
      )
    ),
    empleo_informal = case_when(
      informalidad_codigo == 1 ~ 1,
      informalidad_codigo == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    grupo_edad = case_when(
      edad >= 14 & edad <= 24 ~ "14 a 24 anos",
      edad >= 25 & edad <= 34 ~ "25 a 34 anos",
      edad >= 35 & edad <= 44 ~ "35 a 44 anos",
      edad >= 45 & edad <= 54 ~ "45 a 54 anos",
      edad >= 55 & edad <= 64 ~ "55 a 64 anos",
      edad >= 65 ~ "65 anos y mas",
      TRUE ~ NA_character_
    ),
    grupo_edad = factor(
      grupo_edad,
      levels = c(
        "14 a 24 anos",
        "25 a 34 anos",
        "35 a 44 anos",
        "45 a 54 anos",
        "55 a 64 anos",
        "65 anos y mas"
      )
    )
  ) %>%
  filter(
    departamento == "12",
    anio == 2025
  )

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
    condicion_actividad_codigo == 1
  )

# 5. Comprobaciones posteriores -------------------------------
dim(datos)
glimpse(datos)

datos %>%
  is.na() %>%
  colSums()

# Guardar la base limpia como objeto propio de R, NO como CSV.
saveRDS(
  datos,
  "data/base_limpia_EPEN_Junin_2025.rds"
)

# 6. Estadisticas descriptivas --------------------------------
tabla_general <- tibble(
  indicador = c(
    "Registros de la base original",
    "Registros duplicados exactos",
    "Registros despues de eliminar duplicados",
    "Poblacion de 14 anos y mas en la muestra",
    "Poblacion ocupada en la muestra",
    "Poblacion de 14 anos y mas estimada",
    "Poblacion ocupada estimada"
  ),
  valor = c(
    nrow(df),
    sum(duplicated(df)),
    nrow(datos),
    nrow(poblacion_14_mas),
    nrow(ocupados),
    sum(poblacion_14_mas$factor_expansion, na.rm = TRUE),
    sum(ocupados$factor_expansion, na.rm = TRUE)
  )
)

tabla_general
write_csv(
  tabla_general,
  "tables/estadisticas_generales.csv"
)

tabla_edad_sexo <- poblacion_14_mas %>%
  filter(
    !is.na(grupo_edad),
    !is.na(sexo)
  ) %>%
  group_by(
    grupo_edad,
    sexo
  ) %>%
  summarise(
    cantidad_muestral = n(),
    poblacion_estimada = sum(
      factor_expansion,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write_csv(
  tabla_edad_sexo,
  "tables/poblacion_edad_sexo.csv"
)

tabla_actividad <- poblacion_14_mas %>%
  group_by(
    condicion_actividad
  ) %>%
  summarise(
    cantidad_muestral = n(),
    poblacion_estimada = sum(
      factor_expansion,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada /
      sum(poblacion_estimada)
  )

write_csv(
  tabla_actividad,
  "tables/condicion_actividad.csv"
)

tabla_ingreso_sexo <- ocupados %>%
  filter(
    !is.na(sexo),
    !is.na(ingreso_laboral),
    ingreso_laboral >= 0,
    !is.na(factor_expansion),
    factor_expansion > 0
  ) %>%
  group_by(sexo) %>%
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

write_csv(
  tabla_ingreso_sexo,
  "tables/ingreso_promedio_sexo.csv"
)

tabla_informalidad <- ocupados %>%
  filter(
    !is.na(nivel_educativo),
    !is.na(empleo_informal),
    !is.na(factor_expansion),
    factor_expansion > 0
  ) %>%
  group_by(nivel_educativo) %>%
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

write_csv(
  tabla_informalidad,
  "tables/informalidad_nivel_educativo.csv"
)

# 7. Tema de los graficos -------------------------------------
tema_proyecto <- theme_minimal(
  base_size = 12
) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = "#15324B"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#5F6B7A"
    ),
    plot.caption = element_text(
      size = 8.5,
      color = "#6B7280",
      hjust = 0
    ),
    axis.title = element_text(
      face = "bold",
      color = "#374151"
    ),
    axis.text = element_text(
      color = "#374151"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    plot.margin = margin(15, 18, 15, 15)
  )

fuente_grafico <- paste(
  "Fuente: INEI - Encuesta Permanente de Empleo Nacional",
  "(EPEN), base anual 2025."
)

# 8. Visualizacion de datos -----------------------------------

p1 <- tabla_edad_sexo %>%
  ggplot(
    aes(
      x = grupo_edad,
      y = poblacion_estimada / 1000,
      fill = sexo
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.70
  ) +
  geom_text(
    aes(
      label = label_number(
        accuracy = 1,
        big.mark = ".",
        decimal.mark = ","
      )(poblacion_estimada / 1000)
    ),
    position = position_dodge(width = 0.78),
    vjust = -0.35,
    size = 3.2
  ) +
  scale_fill_manual(
    values = c(
      "Hombres" = "#15558D",
      "Mujeres" = "#20A0A0"
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      accuracy = 1,
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    title = "Poblacion de 14 anos y mas por grupo de edad y sexo",
    subtitle = "Estimaciones ponderadas para Junin, 2025",
    x = "Grupo de edad",
    y = "Poblacion estimada (miles)",
    fill = "Sexo",
    caption = fuente_grafico
  ) +
  tema_proyecto

ggsave(
  "figures/grafico_01_edad_sexo.png",
  p1,
  width = 11,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

p2 <- tabla_actividad %>%
  ggplot(
    aes(
      x = condicion_actividad,
      y = porcentaje,
      fill = condicion_actividad
    )
  ) +
  geom_col(
    width = 0.68,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = label_percent(
        accuracy = 0.1,
        decimal.mark = ","
      )(porcentaje)
    ),
    vjust = -0.45,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Ocupada" = "#15558D",
      "Desempleada abierta" = "#F28E2B",
      "Desempleada oculta" = "#D1495B",
      "Inactiva" = "#20A0A0"
    )
  ) +
  scale_y_continuous(
    labels = label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.18)
    )
  ) +
  labs(
    title = "Distribucion de la condicion de actividad",
    subtitle = "Poblacion residente de 14 anos y mas en Junin, 2025",
    x = "Condicion de actividad",
    y = "Porcentaje de la poblacion",
    caption = paste(
      fuente_grafico,
      "Porcentajes ponderados."
    )
  ) +
  tema_proyecto +
  theme(
    axis.text.x = element_text(
      angle = 8,
      hjust = 0.6
    )
  )

ggsave(
  "figures/grafico_02_condicion_actividad.png",
  p2,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

p3 <- tabla_ingreso_sexo %>%
  ggplot(
    aes(
      x = sexo,
      y = ingreso_promedio,
      fill = sexo
    )
  ) +
  geom_col(
    width = 0.58,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = paste0(
        "S/ ",
        label_number(
          accuracy = 1,
          big.mark = ".",
          decimal.mark = ","
        )(ingreso_promedio)
      )
    ),
    vjust = -0.45,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Hombres" = "#15558D",
      "Mujeres" = "#20A0A0"
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      prefix = "S/ ",
      accuracy = 1,
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.20)
    )
  ) +
  labs(
    title = "Ingreso laboral mensual promedio por sexo",
    subtitle = "Poblacion ocupada de Junin, 2025",
    x = "Sexo",
    y = "Ingreso laboral mensual promedio",
    caption = paste(
      fuente_grafico,
      "Promedios ponderados."
    )
  ) +
  tema_proyecto

ggsave(
  "figures/grafico_03_ingreso_sexo.png",
  p3,
  width = 9,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

p4 <- tabla_informalidad %>%
  ggplot(
    aes(
      x = tasa_informalidad,
      y = fct_rev(nivel_educativo),
      fill = tasa_informalidad
    )
  ) +
  geom_col(
    width = 0.66,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = label_percent(
        accuracy = 0.1,
        decimal.mark = ","
      )(tasa_informalidad)
    ),
    hjust = -0.08,
    size = 3.7,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#20A0A0",
    high = "#F28E2B"
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
    title = "Informalidad laboral segun nivel educativo",
    subtitle = "Poblacion ocupada de Junin, 2025",
    x = "Tasa de empleo informal",
    y = "Nivel educativo",
    caption = paste(
      fuente_grafico,
      "Tasas ponderadas."
    )
  ) +
  tema_proyecto +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "#E5E7EB"
    )
  )

ggsave(
  "figures/grafico_04_informalidad_educacion.png",
  p4,
  width = 11,
  height = 7.2,
  dpi = 300,
  bg = "white"
)

# 9. Collage de graficos --------------------------------------
collage <- (
  p1 |
    p2
) / (
  p3 |
    p4
) +
  plot_annotation(
    title = "ANALISIS EXPLORATORIO DEL MERCADO LABORAL DE JUNIN",
    subtitle = "Encuesta Permanente de Empleo Nacional - EPEN 2025",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 20,
        color = "#15324B",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12,
        color = "#5F6B7A",
        hjust = 0.5
      )
    )
  )

ggsave(
  "figures/collage_graficos.png",
  collage,
  width = 18,
  height = 13,
  dpi = 300,
  bg = "white"
)

# 10. Mostrar los graficos en RStudio -------------------------
# No utilizar View() con objetos ggplot.
print(p1)
print(p2)
print(p3)
print(p4)
print(collage)

cat(
  "\nEDA FINALIZADO CORRECTAMENTE\n",
  "Los graficos se guardaron en la carpeta figures.\n"
)
