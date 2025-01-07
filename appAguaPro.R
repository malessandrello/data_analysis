# Load packages

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(shinylive)
library(DT)


matrices <- c("AGUA PROCESO", "AGUA SUPERFICIAL", "AGUA EFLUENTE", "SUELO REMEDIACIÓN", "JCONT LIMÓN", "JCONT NARANJA",
              "DESH TE")

# Define UI

ui <- page_navbar(
  id = "nav",
  fillable = FALSE,
  tags$head(
    tags$style(HTML("
.bslib-page-navbar>.navbar, .bslib-page-dashboard>.navbar {
    --bslib-navbar-default-bg: #5EB2C6;
    --bslib-navbar-inverse-bg: #5EB2C6f;
}
    body{
      background-color: #2EA1AC;
    }
      .card{
      background-color: #5EB2C6;
      font-family: Raleway,  Arial;
      font-weight: bold;
      }"))
  ),
  # Menú nav
  nav_panel(
    "Dos variables",
    card(
      card_header(htmlOutput(outputId = "encabezado")),
      card_body(withSpinner(DTOutput(outputId = "tablacorr")))),
    card(
      height = "500px",
      withSpinner(plotlyOutput(outputId = "scatterplot")),
      full_screen = TRUE, class = "card"
    )
  ),
  nav_panel(
    "Una variable",
    card(
      height = "500px",
      withSpinner(plotlyOutput(outputId = "scatterplot2")),
      full_screen = TRUE, class = "card"
    )
  ),
  nav_spacer(),
  sidebar = sidebar(
    selectInput(
      inputId = "matrices",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Seleccione producto y matriz", "</b>")),
      selected = "AGUA PROCESO",
      choices = matrices
    ),
    hr(),
    uiOutput("selec_analisis_x"),
    hr(),
    conditionalPanel(
      condition = "input.nav == 'Dos variables'",
      uiOutput("selec_analisis_y"),
    hr(),
    checkboxInput(
      inputId = "logy",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Convertir eje Y a escala logarítmica base 10", "</b>")),
      value = FALSE
    )),
    checkboxInput(
      inputId = "logx",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Convertir eje X a escala logarítmica base 10", "</b>")),
      value = FALSE
    ),
    numericInput(
      inputId = "vert",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar linea vertical en x = ", "</b>")),
      value = 0,
      min = 0
    ),
    conditionalPanel(
      condition = "input.nav == 'Dos variables'",
      numericInput(
        inputId = "hor",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar linea horizontal en y = ", "</b>")),
        value = 0
      ),
      hr()
    ),
    checkboxInput(
      inputId = "ent",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Mostrar entidades", "</b>")),
      value = FALSE
    ),
    uiOutput("selec_entidades"),
    card(
      textInput(
        inputId = "rot",
        label = "Filtro por rótulo"
      )
    ),
    hr()
  )
)


# Define server

server <- function(input, output, session) {
  # LEE EL ARCHIVO CON LOS DATOS
  datos_full <- eventReactive(input$matrices, {
    req(input$matrices)
    datos <- switch(input$matrices,
      "AGUA PROCESO" = read_csv(file = "agua_proceso.csv", col_names = TRUE),
      "AGUA SUPERFICIAL" = read_csv(file = "super_agua.csv", col_names = TRUE),
      "AGUA EFLUENTE" = read_csv(file = "agua_eflu.csv", col_names = TRUE),
      "SUELO REMEDIACIÓN" = read_csv(file = "suelo_remed.csv", col_names = TRUE),
      "JCONT LIMÓN" = read_csv(file = "jcont_limon.csv", col_names = TRUE),
      "JCONT NARANJA" = read_csv(file = "jcont_naran.csv", col_names = TRUE),
      "DESH TE" = read_csv(file = "desh-te.csv", col_names = TRUE)
    )

    # Selecciona columnas necesarias y cambia el nombre de 'Resultado convertido'
    datos <- datos %>%
      select(
        Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, `Resultado convertido`, `Unidad inicial`, `Modificador de resultado`,
        `Límite de detección`
      ) %>%
      rename(Resultado_conv = `Resultado convertido`)

    datos <- datos %>% mutate(Resultado_conv = case_when(`Modificador de resultado` == "nd" ~ `Límite de detección`,
                                                         .default = Resultado_conv))
    datos <- datos %>% mutate(Resultado_conv = as.numeric(Resultado_conv))
    # Filtra filas donde 'Unidad inicial' empieza con "x"
    dat1 <- datos %>%
      filter(str_detect(`Unidad inicial`, "x"))

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
      filter(!str_detect(`Unidad inicial`, "x"))

    # Une ambos dataframes
    datos <- full_join(dat1, datos)

    # Selecciona las columnas necesarias y convierte los "NA" en 0
    datos <- datos %>%
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, Resultado_conv) %>%
      replace(is.na(.), 0)

    # Modifica 'Resultado_conv' en función de las reglas especificadas
    datos <- datos %>%
      mutate(Resultado_conv = ifelse(Resultado_conv == 0, Resultado, Resultado_conv)) %>%
      replace(. == 0, NA)

    # Selecciona columnas necesarias y elimina filas con 'NA'
    datos <- datos %>%
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado_conv) %>%
      drop_na()

    # Convierte el dataframe, cada análisis se convierte en columna
    datos <- pivot_wider(data = datos, names_from = Análisis, values_from = Resultado_conv)

    # Reemplaza el último dígito de la fracción por "1"
    datos <- datos %>%
      mutate(Fracción = str_replace_all(Fracción, "\\d$", "1")) %>%
      replace(is.na(.), 0)


    # Agrupa las fracciones y suma las columnas numéricas
    datos <- datos %>%
      group_by(Fracción) %>%
      summarise(Rótulo = first(Rótulo), Entidad = first(Entidad), across(where(is.numeric), sum)) %>%
      ungroup()

    # Ordena
    r <- sort(unique(datos$Entidad))
    datos <- datos %>%
      mutate(Entidad = factor(Entidad, levels = r))

    # Convierte los 0 a NA nuevamente
    datos <- datos %>%
      replace(. == 0, NA)
    

    datos
  })
  analisis <- reactive(names(datos_full()[4:ncol(datos_full())]))

  entidades <- reactive({
    datos_full()$Entidad
  })
  output$selec_entidades <- renderUI({
    pickerInput(
      inputId = "enti",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Seleccione entidad", "</b>")),
      choices = sort(unique(entidades())),
      options = list("actions-box" = TRUE),
      selected = unique(entidades()),
      multiple = TRUE
    )
  })
  output$selec_analisis_x <- renderUI({
    req(analisis())
    selectInput(
      inputId = "x",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Eje X", "</b>")),
      choices = analisis(),
      selected = NULL
    )
  })

  output$selec_analisis_y <- renderUI({
    req(analisis())
    selectInput(
      inputId = "y",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Eje Y", "</b>")),
      choices = analisis(),
      selected = NULL
    )
  })

  rotulos <- reactive({
    if (input$rot != "") {
      datos_full() %>%
        filter(str_detect(Rótulo, regex(input$rot, ignore_case = TRUE))) %>%
        pull(Rótulo)
    } else {
      datos_full() %>% pull(Rótulo)
    }
  })

  subsetted <- reactive({
    req(input$enti)
    datos_full() %>% filter(Entidad %in% input$enti & Rótulo %in% rotulos())
  })



  output$scatterplot <- renderPlotly({
    req(input$matrices)
    if (input$ent == FALSE & input$logx == FALSE & input$logy == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red")
      ggplotly(p)
    } else if (input$ent == FALSE & input$logx == TRUE & input$logy == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        scale_x_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == FALSE & input$logx == TRUE & input$logy == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == FALSE & input$logx == FALSE & input$logy == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        scale_y_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == FALSE & input$logy == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == TRUE & input$logy == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        scale_x_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == FALSE & input$logy == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        scale_y_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == TRUE & input$logy == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        scale_y_continuous(trans = "log10") +
        scale_x_continuous(trans = "log10")
      ggplotly(p)
    }
  })



  output$scatterplot2 <- renderPlotly({
    req(input$matrices)
    if (input$ent == FALSE & input$logx == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        ylab("N° de muestras")

      ggplotly(p, tooltip = c("x", "text"))
    } else if (input$ent == FALSE & input$logx == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        scale_x_continuous(trans = "log10") +
        ylab("N° de muestras")

      ggplotly(p, tooltip = c("x", "text"))
    } else if (input$ent == TRUE & input$logx == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], fill = Entidad, text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        ylab("N° de muestras")

      ggplotly(p, tooltip = c("x", "Entidad", "text"))
    } else if (input$ent == TRUE & input$logx == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], fill = Entidad, text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        scale_x_continuous(trans = "log10") +
        ylab("N° de muestras")

      ggplotly(p, tooltip = c("x", "Entidad", "text"))
    } 
    
  })
  output$tablacorr <- renderDT({
    data <- datos_full() %>% select(-(1:3))
    
    analisis1 <- 1:ncol(data)
    
    analisis2 <- names(data)
    
    analisis_func <- function(analisis){data %>% filter(!is.na(data[,analisis])) %>% nrow()}
    
    x <- sapply(analisis1, analisis_func)
    
    y <- which(x>100)
    data1 <- data[,y]
    matriz <- cor(data1, method = "spearman", use= "pairwise.complete.obs") 
    
    
    correl <- as_tibble(matriz) %>% round(2)
    
    analisis3 <- names(correl)
    
    correl <- correl %>% mutate(Analisis = analisis3)
    
    analisis_correl <- correl %>%  filter(.data[[input$x]]>0.5 | .data[[input$x]]< (-0.5)) %>% 
    select(Analisis, .data[[input$x]]) %>%
    dplyr::arrange(desc(.data[[input$x]]))
    
    datatable(analisis_correl)
 
  })
  output$encabezado <- renderUI({
    HTML(paste0("Correlaciones de Spearman de ", input$x, " con distintos análsis"))
  })
}





# Create a Shiny app object

shinyApp(ui = ui, server = server)
