
library(readr)
library(dplyr)
library(tidyverse)



carga_datos <- function(archivo, guardado){

datos <- fread(file = archivo) %>%
  select(
    Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, `Modificador de resultado`,
    `Límite de detección`, `Límite de cuantificación`, `Resultado convertido`,`Unidad inicial` 
  ) %>%
  rename(Resultado_conv = `Resultado convertido`) %>%
  mutate(Resultado_conv = as.numeric(Resultado_conv)) 

 datos <- datos %>% mutate(Resultado_conv = case_when(`Modificador de resultado` == "nd" ~ as.numeric(`Límite de detección`),
                                                      `Modificador de resultado` == "<" ~ as.numeric(`Límite de cuantificación`),
                                                      .default = Resultado_conv))
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
    y == 1 ~ Resultado * 1000,
    y == 2 ~ Resultado * 10000,
    y == 3 ~ Resultado * 100000,
    y == 4 ~ Resultado * 1000000,
    y == 5 ~ Resultado * 10000000,
    y == 6 ~ Resultado * 100000000
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

 # datos <- datos %>% mutate(Resultado_conv = case_when(`Unidad inicial` == "g/l" ~ Resultado_conv * 1000,
 #                                                       `Unidad inicial` == "mS/cm" ~ Resultado_conv * 1000,
 #                                                     `Unidad inicial` == "g/kg" ~ Resultado_conv * 1000,
 #                                                       .default = Resultado_conv
 #                                                      ))

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


 saveRDS(datos, guardado)
}


correlaciones <- function(archivo_input, archivo_output){

  datos <- fread(file = archivo_input) %>%
    #filter(!is.na(Resultado) & `Modificador de resultado` == c("")) %>%
  select(
      Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, `Resultado convertido`, `Unidad inicial`, `Modificador de resultado`,
      `Límite de detección`, `Límite de cuantificación`
    ) %>%
    rename(Resultado_conv = `Resultado convertido`, Rótulo = Rótulo) %>%
    mutate(Resultado_conv = as.numeric(Resultado_conv))
  
   datos <- datos %>% mutate(Resultado_conv = case_when(`Modificador de resultado` == "nd" ~ as.numeric(`Límite de detección`),
                                                        `Modificador de resultado` == "<" ~ as.numeric(`Límite de cuantificación`),
                                                        .default = Resultado_conv))
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
      y == 1 ~ Resultado * 1000,
      y == 2 ~ Resultado * 10000,
      y == 3 ~ Resultado * 100000,
      y == 4 ~ Resultado * 1000000,
      y == 5 ~ Resultado * 10000000,
      y == 6 ~ Resultado * 100000000
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
  
  # datos <- datos %>% mutate(Resultado_conv = case_when(`Unidad inicial` == "g/l" ~ Resultado_conv * 1000,
  #                                                      `Unidad inicial` == "mS/cm" ~ Resultado_conv * 1000,
  #                                                      `Unidad inicial` == "g/kg" ~ Resultado_conv * 1000,
  #                                                      .default = Resultado_conv
  # ))
  
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
  
data <- datos %>% select(-(1:3))

analisis1 <- 1:ncol(data)

analisis2 <- names(data)

write_rds(analisis2, "analisis_hort_tomate.rds")

analisis_func <- function(analisis){data %>% filter(!is.na(data[,analisis])) %>% nrow()}

x <- sapply(analisis1, analisis_func)

y <- which(x>100)
data1 <- data[,y]
matriz <- cor(data1, method = "spearman", use= "pairwise.complete.obs") 


correl <- as_tibble(matriz) %>% round(2)

analisis3 <- names(correl)

correl <- correl %>% mutate(Analisis = analisis3)

write_rds(correl, archivo_output)

}








carga_datos("hort_tomate.csv", "hort_tomate.rds")

correlaciones("hort_tomate.csv", "correl_hort_tomate.rds")

# a <- read_rds("appDatos/bayas_uva.rds")
# 
# 
# 
# p <- a %>% ggplot(aes(log10(`2658 - TOLUENO (GC/MS)`), log10(`2700 - m-XILENO (GC/MS)`), color= Entidad))+
#   geom_point()+ labs(title = "98 % de las muestras cumplen con la ecuación:")+
#   geom_abline(slope = 1.1, intercept = 0, color = "green")+
#  # geom_hline(yintercept = 0.25, color = "red")+
#   geom_abline(slope = 1.1, intercept = -1.5, color = "red")+
#   annotate("text", label = " 1.1 * log[tolueno] -1.5<= log[xileno] <=  1.1*log[tolueno]", x = 0, y =5, color = "red")
# ggplotly(p)