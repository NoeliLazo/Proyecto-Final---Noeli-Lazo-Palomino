#============================================================#
# BASE LIMPIA EPEN JUNIN 2025                                 #
# Al ejecutar source(), crea el objeto `datos`.               #
#============================================================#

# Corregir el directorio cuando se ejecuta desde scripts o data.
if (!dir.exists("data") && dir.exists("../data")) {
  setwd("..")
}

library(readr)
library(dplyr)

df <- read_csv(
  "data/12_Junin_EPEN_Anual_2025.csv",
  col_types = cols(
    .default = col_character()
  ),
  na = c("", "NA", "N/A", "NULL"),
  show_col_types = FALSE
)

a_numero <- function(x) {
  suppressWarnings(
    as.numeric(x)
  )
}

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
