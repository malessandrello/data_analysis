
library(readr)
library(dplyr)
library(tidyverse)

df1 <- read_delim("SR_25_22.csv", delim = ";")
df2 <- read_delim("SR_22_16.csv", delim = ";")
# df3 <- read_delim("FF_pera_22_21.csv", delim = ";")
# df4 <- read_delim("FF_pera_20_21.csv", delim = ";")
# df5 <- read_delim("FF_pera_19_20.csv", delim = ";")
# df6 <- read_delim("FF_pera_18_19.csv", delim = ";")
# df7 <- read_delim("FF_pera_17_18.csv", delim = ";")
# df8 <- read_delim("FF_pera_16_17.csv", delim = ";")

dfs <- list(df1, df2) #df3, df4, df5, df6, df7, df8)

datos <- reduce(dfs, full_join)

#datos <- datos %>% filter(!is.na(Resultado) & is.na(`Modificador de resultado`))


datos <- datos %>%
select(
  Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, `Resultado convertido`, `Unidad inicial`, `Modificador de resultado`,
  `Límite de detección`, `Límite de cuantificación`
) %>%
  rename(Resultado_conv = `Resultado convertido`, "Modificador de resultado"= `Modificador de resultado`, Rótulo = Rótulo) %>%
  mutate(Resultado_conv = as.numeric(Resultado_conv))


datos <- datos %>% mutate(Resultado_conv = as.numeric(Resultado_conv),
                          Resultado = as.numeric(Resultado))
# Filtra filas donde 'Unidad inicial' empieza con "x"
dat1 <- datos %>%
  filter(str_detect(`Unidad inicial`, "x1"))

# Extrae superíndices de 'Unidad inicial'
supin <- str_extract(dat1$`Unidad inicial`, "\\W")

# Convierte los superíndices en factores y luego en números
dat1 <- dat1 %>%
  mutate(
    y = as.numeric(as.factor(supin)),
    Resultado = as.numeric(Resultado)
  )

# Multiplica el 'Resultado' según el valor del superíndice en la columna "y"
dat1 <- dat1 %>%
  mutate(Resultado = case_when(
    y == 1 ~ Resultado * 100,
    y == 2 ~ Resultado * 1000,
    y == 3 ~ Resultado * 10000,
    y == 4 ~ Resultado * 100000,
    y == 5 ~ Resultado * 1000000,
    y == 6 ~ Resultado * 10000000
  ))

# Elimina las filas con 'Unidad inicial' que contiene "x" del dataframe original
datos <- datos %>%
  filter(!str_detect(`Unidad inicial`, "x1"))

# Une ambos dataframes
datos <- full_join(dat1, datos)

# Selecciona las columnas necesarias y convierte los "NA" en 0
datos <- datos %>%
  select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, Resultado_conv, `Unidad inicial` ) %>%
  replace(is.na(.), 0)

# Modifica 'Resultado_conv' en función de las reglas especificadas
datos <- datos %>%
  mutate(Resultado_conv = ifelse(Resultado_conv == 0, Resultado, Resultado_conv)) %>%
  replace(. == 0, NA)

# Selecciona columnas necesarias y elimina filas con 'NA'
datos <- datos %>%
  select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado_conv, `Unidad inicial`) %>%
  drop_na()

datos <- datos %>% mutate(Resultado_conv = case_when(`Unidad inicial` == "g/l" ~ Resultado_conv * 1000,
                                                     `Unidad inicial` == "mS/cm" ~ Resultado_conv * 1000,
                                                     `Unidad inicial` == "g/kg" ~ Resultado_conv * 1000,
                                                     .default = Resultado_conv
))

# Convierte el dataframe, cada análisis se convierte en columna
datos <- pivot_wider(data = datos, names_from = Análisis, values_from = Resultado_conv)

# Reemplaza el último dígito de la fracción por "1"
datos <- datos %>%
  mutate(Fracción = str_replace_all(Fracción, "\\d$", "1")) %>%
  replace(is.na(.), 0)


# Agrupa las fracciones y suma las columnas numéricas
datos <- datos %>%
  group_by(Fracción) %>%
  summarise( Rótulo = first(Rótulo), Entidad = first(Entidad), across(where(is.numeric), sum)) %>%
  ungroup()

# Ordena
r <- sort(unique(datos$Entidad))
datos <- datos %>%
  mutate(Entidad = factor(Entidad, levels = r))

# Convierte los 0 a NA nuevamente
datos <- datos %>%
  replace(. == 0, NA)


saveRDS(datos, "suelo_remed.rds")


data <- datos %>% select(-(1:3))

analisis1 <- 1:ncol(data)

analisis2 <- names(data)

write_rds(analisis2, "analisis_SR.rds")

analisis_func <- function(analisis){data %>% filter(!is.na(data[,analisis])) %>% nrow()}

x <- sapply(analisis1, analisis_func)

y <- which(x>100)
data1 <- data[,y]
matriz <- cor(data1, method = "spearman", use= "pairwise.complete.obs") 


correl <- as_tibble(matriz) %>% round(2)

analisis3 <- names(correl)

correl <- correl %>% mutate(Analisis = analisis3)

write_rds(correl, "correl_SR.rds")


