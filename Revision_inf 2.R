# Load packages

library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(tidyverse)
library(shinyWidgets)
library(caret)
library(insight)
library(shinycssloaders)
library(ranger)
library(tools)
library(Metrics)
library(UBL)
library(plotly)
library(ggiraph)



ui <- page_navbar(
  id = "page",
  fillable = FALSE,
  position = c("fixed-top"),
  tags$head(
    tags$style(HTML("

     body{
      background-color: #73B77B;
      padding-top: 70px;
    }
      .card{
      font-family: Raleway,  Arial;
      font-weight: bold;
      }"))
  ),
  nav_panel(title = "Predicciones",
  titlePanel(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Revisión de informes mediante aprendizaje automático", "</b>"))),
  hr(),
  fileInput(inputId = "subir", NULL, buttonLabel = "Subir archivo", accept = c(".xls", ".xlsx")),
  hr(),
  actionBttn("pred", "PREDECIR"),
  hr(),
  HTML(paste0(
    "<b style = 'font-family: Raleway, Sans-serif';>", "CONSIDERACIONES:", "</br>", "EL APRENDIZAJE AUTOMÁTICO NO REEMPLAZA AL ANÁLISIS HUMANO.", "</br>",
    "CUANDO EL RESULTADO ES MENOR AL LC, SE TOMA AL LC COMO RESULTADO INFORMADO.", "</br>",
    "EL COLOR VERDE INDICA QUE LA PREDICCIÓN CONCUERDA CON EL VALOR INFORMADO, MIENTRAS QUE EL COLOR ROJO INDICA DISCREPANCIA ENTRE LA PREDICCIÓN Y EL RESULTADO INFORMADO.", "</b>"
  )),
  hr(),
  conditionalPanel(
    condition = "output.suelo",
    card(
      class = "card",
      card_header(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "PREDICCIONES", "</b>"))),
      card_body(
        htmlOutput(outputId = "suelo"),
        hr(),
        withSpinner(htmlOutput(outputId = "predHTP2")),
        hr(),
        htmlOutput(outputId = "prednaft"),
        hr(),
        htmlOutput(outputId = "predfen"),
        hr(),
        htmlOutput(outputId = "predpir"),
        hr(),
        htmlOutput(outputId = "predtolu"),
        hr(),
        htmlOutput(outputId = "predbenc"),
        hr(),
        htmlOutput(outputId = "predetilben"),
        hr(),
        htmlOutput(outputId = "predexil"),
        hr(),
        htmlOutput(outputId = "predplomo"),
        hr(),
        htmlOutput(outputId = "predarsenico"),
        hr(),
        htmlOutput(outputId = "predbario"),
        hr(),
        htmlOutput(outputId = "predvan"),
        hr(),
        htmlOutput(outputId = "predcromo")
        
      ),
      card_footer(
        card_image(src = "logo1.png", height = "50%", width = "50%", align = "right")
      )
    )
  ),
  conditionalPanel(
    condition = "output.agua",
    
    layout_columns( col_widths = c(4,4,4),
      
    card(
      card_header(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "PAH", "</b>"))),
        card_body(
        withSpinner(htmlOutput(outputId = "agua_fen")),
 
        htmlOutput(outputId = "agua_naft"),
 
        htmlOutput(outputId = "agua_pireno"),
  
        htmlOutput(outputId = "agua_fluoreno"),

        htmlOutput(outputId = "agua_fluoranteno")
        )),

    
    card(
      card_header(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "DRO y GRO", "</b>"))),
      card_body(
        withSpinner(htmlOutput(outputId = "agua_gro")),
   
        htmlOutput(outputId = "agua_dro")
       )),
    

    
    card(
      card_header(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "BTEX", "</b>"))),
      card_body(
      withSpinner(htmlOutput(outputId = "agua_benc")),
    
        htmlOutput(outputId = "agua_etbenc"),
 
        htmlOutput(outputId = "agua_tolu"),
    
        htmlOutput(outputId = "agua_xil")
       )),
    
    
    card(
      card_header(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "FQ", "</b>"))),
      card_body(
      withSpinner(htmlOutput(outputId = "agua_deter")),
   
        htmlOutput(outputId = "agua_fenoles"),
 
        htmlOutput(outputId = "agua_grasas")
      )),
    
    
    card(
      card_header(HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "METALES", "</b>"))),
      card_body( 
      withSpinner(htmlOutput(outputId = "agua_sodio")),
    
        htmlOutput(outputId = "agua_potasio"),
     
        htmlOutput(outputId = "agua_calcio"),
     
        htmlOutput(outputId = "agua_magnesio"),
     
        htmlOutput(outputId = "agua_hierro")
     ))
  
    )
      

  )),
  nav_panel("Gráficos",
          conditionalPanel(condition = "output.agua",
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fen_naf")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fen_dro_gro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fen_etbenc")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fen_fenoles")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fen_fluoreno")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fen_pireno")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "naft_dro_gro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "naft_pir")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "naft_fluoreno")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "pir_fluoranteno")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "pir_fluoreno")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fluoreno_fluoranteno")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "etbenc_tolu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "etbenc_xil")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "etbenc_benc")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "etbenc_dro_gro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "tolu_benc")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "tolu_xil")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "tolu_fenoles")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "benc_fenoles")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "benc_xil")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "gro_xil")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "gro_tolu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "gro_benc")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "gro_deter")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "gro_dro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "dro_xil")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "dro_tolu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "dro_benc")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "dro_deter")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "deter_condu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "deter_ph")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "fenoles_etbenc")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "sodio_cloruro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "sodio_condu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "sodio_calcio")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "sodio_potasio")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "potasio_condu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "potasio_cloruro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "calcio_cloruro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "calcio_condu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "calcio_potasio")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "calcio_magnesio")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "magnesio_sodio")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "magnesio_cloruro")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "magnesio_condu")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "magnesio_potasio")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "hierro_bario")),
              full_screen = TRUE, class = "card"),
            card(
              height = "600px",
              withSpinner(plotlyOutput(outputId = "hierro_estroncio")),
              full_screen = TRUE, class = "card")
            
            
   
            
          )
  ),
  footer = card_image(src = "logo1.png", height = "50%", width = "50%", align = "right")
)








server <- function(input, output, session) {
  data1 <- reactive({
    req(input$subir)
    
    ext <- file_ext(input$subir$name)
    
    switch(ext,
           xls = read_xls(input$subir$datapath, skip = 21),
           xlsx = read_xlsx(input$subir$datapath, skip = 21),
           validate("Archivo inválido; por favor suba un archivo Excel")
    )
  })
  
  matriz <- reactive({
    req(input$subir)
    
    ext <- file_ext(input$subir$name)
    
    switch(ext,
           xls = read_xls(input$subir$datapath, range = "A14:A15"),
           xlsx = read_xlsx(input$subir$datapath, range = "A14:A15"),
           validate("Archivo inválido; por favor suba un archivo Excel")
    )
  })
  
  
  suelo <- reactive({
    if (str_detect(matriz()[1], "SUELO|TIERRA")) {
      suelo
    }
  })
  output$suelo <- renderUI({
    if (!is.null(suelo())) {
      HTML(paste0(
        "<b style = 'font-family: Raleway, Sans-serif';>", "MATRIZ: SUELO", "</b>"
      ))
    }
  })
  
  agua <- reactive({
    if (str_detect(matriz()[1], "AGUA")) {
      agua
    }
  })
  output$agua <- renderUI({
    if (!is.null(agua())) {
      HTML(paste0(
        "<b style = 'font-family: Raleway, Sans-serif';>", "MATRIZ: AGUA", "</b>"
      ))
    }
  })
  
  
  newData <- reactive({
    req(input$subir, suelo())
    data1() %>%
      select(Análisis, `Resultado Convertido`) %>%
      mutate(`Resultado Convertido` = as.numeric(`Resultado Convertido`),
      Análisis = str_remove_all(Análisis, "\\* "))
         
  })
  newData_agua <- reactive({
    req(input$subir, agua())
    data1() %>%
      select(Análisis, Resultado) %>%
      mutate(Resultado = as.numeric(Resultado),
             Análisis = str_remove_all(Análisis, "\\* "))
  })
  
  ################ ANÁLISIS SUELO ############################
  cobre <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "COBRE")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 10, dato)
  })
  htp <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "HIDROCARBUROS TOTALES DE PETROLEO \\(C6-C35\\)")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 100, dato)
  })
  benc <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "^BENCENO")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 0.05, dato)
  })
  tolu <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "TOLUENO")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 0.05, dato)
  })
  etbenc <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "^ETILBENCENO")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 0.05, dato)
  })
  xil <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "SUMA DE XILENOS")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 0.05, dato)
  })
  naft <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "NAFTALENO")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 0.1, dato)
  })
  fen <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "FENANTRENO")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 0.1, dato)
  })
  plo <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "PLOMO TOTAL")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 5, dato)
  })
  bario <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "BARIO TOTAL")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 100, dato)
  })
  arse <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "ARSÉNICO TOTAL")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 3, dato)
  })
  van <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "VANADIO TOTAL")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 50, dato)
  })
  cromo <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "CROMO TOTAL$")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 5, dato)
  })
  
  pireno <- reactive({
    req(input$pred, suelo())
    dato <- newData() %>%
      filter(str_detect(Análisis, "^PIRENO")) %>%
      pull(`Resultado Convertido`)
    ifelse(is.na(dato), 0.1, dato)
  })
  
  ############## ANÁLISIS AGUA #######################################
  
  benc_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "^BENCENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 10, dato)
  })
  tolu_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "TOLUENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 10, dato)
  })
  etbenc_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "^ETILBENCENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 10, dato)
  })
  xil_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "SUMA DE XILENOS")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 10, dato)
  })
  naft_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "NAFTALENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.02, dato)
  })
  fen_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "FENANTRENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.02, dato)
  })
  dro_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "DIESEL")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 1, dato)
  })
  
  gro_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "GASOLINA")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.3, dato)
  })
  grasas_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "GRASAS")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 5, dato)
  })
  fenoles_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "^FENOLES")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.002, dato)
  })
  deter_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "DETERGENTES")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.30, dato)
  })
  pireno_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>%
      filter(str_detect(Análisis, "^PIRENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.02, dato)
  })
  sol_dis <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "180")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 100, dato)
  })
  
  ph_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "pH \\(25°C\\)")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 7, dato)
  })
  condu_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "^CONDUCTIVIDAD \\(a 25°C\\)")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 7, dato)
  })
  fluoreno_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "FLUORENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.02, dato)
  })
  
  potasio_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "POTASIO DISUELTO|POTASIO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.3, dato)
  })
  
  sodio_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "SODIO DISUELTO|SODIO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.3, dato)
  })
  
  cloruro_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "CLORURO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 3, dato)
  })
  
  calcio_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "CALCIO|CALCIO DISUELTO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.3, dato)
  })
  magnesio_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "MAGNESIO|MAGNESIO DISUELTO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.3, dato)
  })
  cinc_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "CINC|CINC DISUELTO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 10, dato)
  })
  hierro_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "HIERRO|HIERRO DISUELTO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 150, dato)
  })
  manganeso_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "MANGANESO|MANGANESO DISUELTO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 5, dato)
  })
  estroncio_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "ESTRONCIO|ESTRONCIO DISUELTO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 10, dato)
  })
  bario_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "BARIO|BARIO DISUELTO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 10, dato)
  })
  fluoranteno_a <- reactive({
    req(input$pred, agua())
    dato <- newData_agua() %>% 
      filter(str_detect(Análisis, "^FLUORANTENO")) %>%
      pull(Resultado)
    ifelse(is.na(dato), 0.02, dato)
  })
  
  ######################### DATOS SUELO ##########################################
  datos <- eventReactive(input$pred, {
    req(suelo())
    # LEE EL ARCHIVO CON LOS DATOS
    datos <- read_csv(file = "suelo_remed.csv", col_names = TRUE)
    
    # Selecciona columnas necesarias y cambia el nombre de 'Resultado convertido'
    datos <- datos %>%
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, `Resultado convertido`, `Unidad inicial`) %>%
      rename(Resultado_conv = `Resultado convertido`)
    
    
    
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
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, Resultado_conv, `Unidad inicial`) %>%
      replace(is.na(.), 0)
    
    # Modifica 'Resultado_conv' en función de las reglas especificadas
    datos <- datos %>%
      mutate(Resultado_conv = ifelse(Resultado_conv == 0, Resultado, Resultado_conv)) %>%
      replace(. == 0, NA)
    
    # Selecciona columnas necesarias y elimina filas con 'NA'
    datos <- datos %>%
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado_conv, `Unidad inicial`) %>%
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
      summarise(Rótulo = Rótulo, Entidad = Entidad, Unidad = `Unidad inicial`,  across(where(is.numeric), sum)) %>%
      ungroup()
    
    # Ordena y crea una columna adicional "DRO + GRO"
    r <- sort(unique(datos$Entidad))
    datos <- datos %>%
      mutate(
        Entidad = factor(Entidad, levels = r),
        `DRO + GRO` = `2591 - RANGO ORGANICO DE GASOLINA (GRO)` + `2590 - RANGO ORGANICO DE DIESEL (DRO)`
      )
    
    # Convierte los 0 a NA nuevamente
    datos <- datos %>%
      replace(. == 0, NA)
    
    datos
  })
  
  ############################ DATOS AGUA ##########################################
  datos_ag <- eventReactive(input$pred, {
    req(agua())
    
    # LEE EL ARCHIVO CON LOS DATOS
    datos_ag <- read_csv(file = "agua_proceso.csv", col_names = TRUE)
    
    # Selecciona columnas necesarias y cambia el nombre de 'Resultado convertido'
    datos_ag <- datos_ag %>%
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, `Resultado convertido`, `Unidad inicial`) %>%
      rename(Resultado_conv = `Resultado convertido`)
    
    
    
    # Filtra filas donde 'Unidad inicial' empieza con "x"
    dat1 <- datos_ag %>%
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
    datos_ag <- datos_ag %>%
      filter(!str_detect(`Unidad inicial`, "x"))
    
    # Une ambos dataframes
    datos_ag <- full_join(dat1, datos_ag)
    
    # Selecciona las columnas necesarias y convierte los "NA" en 0
    datos_ag <- datos_ag %>%
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado, Resultado_conv) %>%
      replace(is.na(.), 0)
    
    # Modifica 'Resultado_conv' en función de las reglas especificadas
    datos_ag <- datos_ag %>%
      mutate(Resultado_conv = ifelse(Resultado_conv == 0, Resultado, Resultado_conv)) %>%
      replace(. == 0, NA)
    
    # Selecciona columnas necesarias y elimina filas con 'NA'
    datos_ag <- datos_ag %>%
      select(Fracción, `Tipo de producto`, Matriz, Rótulo, Entidad, Análisis, Resultado_conv) %>%
      drop_na()
    
    # Convierte el dataframe, cada análisis se convierte en columna
    datos_ag <- pivot_wider(data = datos_ag, names_from = Análisis, values_from = Resultado_conv)
    
    # Reemplaza el último dígito de la fracción por "1"
    datos_ag <- datos_ag %>%
      mutate(Fracción = str_replace_all(Fracción, "\\d$", "1")) %>%
      replace(is.na(.), 0)
    
    
    # Agrupa las fracciones y suma las columnas numéricas
    datos_ag <- datos_ag %>%
      group_by(Fracción) %>%
      summarise(Rótulo = first(Rótulo), Entidad = first(Entidad), across(where(is.numeric), sum)) %>%
      ungroup()
    
    # Ordena y crea una columna adicional "DRO + GRO"
    r <- sort(unique(datos_ag$Entidad))
    datos_ag <- datos_ag %>%
      mutate(
        `DRO + GRO` = `2591 - RANGO ORGANICO DE GASOLINA (GRO)` + `2590 - RANGO ORGANICO DE DIESEL (DRO)`
      )
    
    # Convierte los 0 a NA nuevamente
    datos_ag <- datos_ag %>%
      replace(. == 0, NA)
    datos_ag
  })
  
  ########################################### PREDICCIÓN HTP ###########################################
  # predicted_data_htp2 <- eventReactive(input$pred, {
  #   req(xil(), tolu(), etbenc(), benc(), suelo())
  #   set.seed(5)
  #   
  #   datos_predicc <- datos() %>%
  #     select(
  #       `2569 - HIDROCARBUROS TOTALES DE PETROLEO (C6-C35)`,
  #       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       `2658 - TOLUENO (GC/MS)`,
  #       `2659 - ETILBENCENO (GC/MS)`,
  #       `2657 - BENCENO (GC/MS)`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       a = `2569 - HIDROCARBUROS TOTALES DE PETROLEO (C6-C35)`,
  #       c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       d = `2658 - TOLUENO (GC/MS)`,
  #       e = `2659 - ETILBENCENO (GC/MS)`,
  #       f = `2657 - BENCENO (GC/MS)`
  #     )
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 700, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 1000, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 10000, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   smote <- SMOTE(datos_predicc1[, -5], datos_predicc1[, 5], K = 3, dup_size = 13)
  #   smote2 <- SMOTE(datos_predicc2[, -5], datos_predicc2[, 5], K = 5, dup_size = 5)
  #   smote3 <- SMOTE(datos_predicc3[, -5], datos_predicc3[, 5], K = 5, dup_size = 1)
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.5, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.5, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.5, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   
  #   data <- with(test_set, tibble(
  #     f = benc(),
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     f = benc(),
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     f = benc(),
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # 
  # ########################## PREDICCIÓN NAFTALENO ###############################
  # 
  # predicted_data_naft <- eventReactive(input$pred, {
  #   req(xil(), tolu(), etbenc(), suelo())
  #   set.seed(5)
  #   
  #   datos_predicc <- datos() %>%
  #     select(
  #       `2574 - NAFTALENO`,
  #       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       `2658 - TOLUENO (GC/MS)`,
  #       `2659 - ETILBENCENO (GC/MS)`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       a = `2574 - NAFTALENO`,
  #       c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       d = `2658 - TOLUENO (GC/MS)`,
  #       e = `2659 - ETILBENCENO (GC/MS)`
  #     )
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.1, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 5, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 50, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   smote <- SMOTE(datos_predicc1[, -4], datos_predicc1[, 4], K = 5, dup_size = 31)
  #   smote2 <- SMOTE(datos_predicc2[, -4], datos_predicc2[, 4], K = 5, dup_size = 2)
  #   smote3 <- SMOTE(datos_predicc3[, -4], datos_predicc3[, 4], K = 3, dup_size = 45)
  #   
  #   
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   data <- with(test_set, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # 
  # #################################### PREDICCIÓN TOLUENO #################################
  # 
  # predicted_data_tol <- eventReactive(input$pred, {
  #   req(xil(), etbenc(), suelo(), tolu())
  #   set.seed(5)
  #   datos_predicc <- datos() %>%
  #     select(
  #       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       `2658 - TOLUENO (GC/MS)`,
  #       `2659 - ETILBENCENO (GC/MS)`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       a = `2658 - TOLUENO (GC/MS)`,
  #       b = `2659 - ETILBENCENO (GC/MS)`
  #     )
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.1, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 5, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 50, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   smote <- SMOTE(datos_predicc1[, -3], datos_predicc1[, 3], K = 5, dup_size = 5)
  #   smote2 <- SMOTE(datos_predicc2[, -3], datos_predicc2[, 3], K = 5, dup_size = 2)
  #   smote3 <- SMOTE(datos_predicc3[, -3], datos_predicc3[, 3], K = 3, dup_size = 10)
  #   
  #   
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   predicted <- predict(fit, test_set, type = "se")
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   predicted2 <- predict(fit2, test_set2, type = "se")
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   predicted3 <- predict(fit3, test_set3, type = "se")
  #   
  #   
  #   data <- with(test_set, tibble(
  #     c = xil(),
  #     b = etbenc()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     c = xil(),
  #     b = etbenc()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     c = xil(),
  #     b = etbenc()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # 
  # ################################# ETILBENCENO ######################################
  # 
  # predicted_data_etbenc <- eventReactive(input$pred, {
  #   req(xil(), tolu(), suelo(), etbenc())
  #   set.seed(5)
  #   datos_predicc <- datos() %>%
  #     select(
  #       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       `2658 - TOLUENO (GC/MS)`,
  #       `2659 - ETILBENCENO (GC/MS)`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       d = `2658 - TOLUENO (GC/MS)`,
  #       a = `2659 - ETILBENCENO (GC/MS)`
  #     )
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.1, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 5, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 50, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   smote <- SMOTE(datos_predicc1[, -3], datos_predicc1[, 3], K = 5, dup_size = 5)
  #   smote2 <- SMOTE(datos_predicc2[, -3], datos_predicc2[, 3], K = 5, dup_size = 3)
  #   smote3 <- SMOTE(datos_predicc3[, -3], datos_predicc3[, 3], K = 3, dup_size = 20)
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   data <- with(test_set, tibble(
  #     c = xil(),
  #     d = tolu()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     c = xil(),
  #     d = tolu()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     c = xil(),
  #     d = tolu()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # #################################### BENCENO #######################################
  # predicted_data_benc <- eventReactive(input$pred, {
  #   req(xil(), tolu(), etbenc(), benc(), suelo())
  #   set.seed(5)
  #   datos_predicc <- datos() %>%
  #     select(
  #       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       `2658 - TOLUENO (GC/MS)`,
  #       `2659 - ETILBENCENO (GC/MS)`,
  #       `2657 - BENCENO (GC/MS)`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       d = `2658 - TOLUENO (GC/MS)`,
  #       b = `2659 - ETILBENCENO (GC/MS)`,
  #       a = `2657 - BENCENO (GC/MS)`
  #     )
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.05, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.5, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 5, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   smote <- SMOTE(datos_predicc1[, -4], datos_predicc1[, 4], K = 5, dup_size = 7)
  #   smote2 <- SMOTE(datos_predicc2[, -4], datos_predicc2[, 4], K = 5, dup_size = 1)
  #   smote3 <- SMOTE(datos_predicc3[, -4], datos_predicc3[, 4], K = 5, dup_size = 5)
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   data <- with(test_set, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     b = benc()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     b = benc()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     b = benc()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # ##################################### XILENOS #################################
  # 
  # predicted_data_xil <- eventReactive(input$pred, {
  #   req(tolu(), etbenc(), suelo(), xil())
  #   set.seed(5)
  #   datos_predicc <- datos() %>%
  #     select(
  #       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       `2658 - TOLUENO (GC/MS)`,
  #       `2659 - ETILBENCENO (GC/MS)`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       a = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       d = `2658 - TOLUENO (GC/MS)`,
  #       c = `2659 - ETILBENCENO (GC/MS)`
  #     )
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.1, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 5, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 50, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   smote <- SMOTE(datos_predicc1[, -3], datos_predicc1[, 3], K = 2, dup_size = 90)
  #   smote2 <- SMOTE(datos_predicc2[, -3], datos_predicc2[, 3], K = 5, dup_size = 0)
  #   smote3 <- SMOTE(datos_predicc3[, -3], datos_predicc3[, 3], K = 3, dup_size = 4)
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 2, min.node.size = 1, num.trees = 1000)
  #   
  #   data <- with(test_set, tibble(
  #     c = etbenc(),
  #     d = tolu()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     c = etbenc(),
  #     d = tolu()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     c = etbenc(),
  #     d = tolu()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # 
  # ######################## FENANTRENO #############################################
  # predicted_data_fen <- eventReactive(input$pred, {
  #   req(xil(), tolu(), etbenc(), suelo(), fen())
  #   
  #   datos_predicc <- datos() %>%
  #     select(
  #       `2578 - FENANTRENO`,
  #       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       `2658 - TOLUENO (GC/MS)`,
  #       `2659 - ETILBENCENO (GC/MS)`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       a = `2578 - FENANTRENO`,
  #       c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #       d = `2658 - TOLUENO (GC/MS)`,
  #       e = `2659 - ETILBENCENO (GC/MS)`
  #     )
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.1, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 5, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 25, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   set.seed(5)
  #   smote <- SMOTE(datos_predicc1[, -4], datos_predicc1[, 4], K = 3, dup_size = 45)
  #   smote2 <- SMOTE(datos_predicc2[, -4], datos_predicc2[, 4], K = 5, dup_size = 2)
  #   smote3 <- SMOTE(datos_predicc3[, -4], datos_predicc3[, 4], K = 2, dup_size = 15)
  #   
  #   
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   data <- with(test_set, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     c = xil(),
  #     d = tolu(),
  #     e = etbenc()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # ################################### PIRENO ###################################
  # 
  # predicted_data_pir <- eventReactive(input$pred, {
  #   
  #   req(xil(), tolu(), etbenc(), suelo(), pireno())
  #   
  #   datos_predicc <- datos() %>% select(`2581 - PIRENO`,
  #                                       `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #                                       `2658 - TOLUENO (GC/MS)`,
  #                                       `2659 - ETILBENCENO (GC/MS)`
  #                                       
  #                                       
  #   ) %>% drop_na() %>% dplyr::rename(a = `2581 - PIRENO`,
  #                                     b = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
  #                                     c = `2658 - TOLUENO (GC/MS)`,
  #                                     d = `2659 - ETILBENCENO (GC/MS)`)
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a>0.1, 1,0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel)) 
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 0.3,1,0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 1,1,0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   set.seed(5)
  #   smote <- SMOTE(datos_predicc1[,-4], datos_predicc1[,4], K = 5, dup_size = 0)
  #   smote2 <- SMOTE(datos_predicc2[,-4], datos_predicc2[,4], K = 5, dup_size = 1)
  #   smote3 <- SMOTE(datos_predicc3[,-4], datos_predicc3[,4], K = 5, dup_size = 7)
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class= as.factor(class))
  #   
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class= as.factor(class))
  #   
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class= as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index,]
  #   test_set <- datos1[-index,]
  #   
  #   train_set2 <- datos2[index2,]
  #   test_set2 <- datos2[-index2,]
  #   
  #   train_set3 <- datos3[index3,]
  #   test_set3 <- datos3[-index3,]
  #   
  #   fit<- ranger(class ~., data = train_set, probability = TRUE, keep.inbag = TRUE,  mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2<- ranger(class ~., data = train_set2, probability = TRUE, keep.inbag = TRUE,  mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3<- ranger(class ~., data = train_set3, probability = TRUE, keep.inbag = TRUE,  mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   data <- with(test_set, tibble(
  #     b = xil(),
  #     c = tolu(),
  #     d = etbenc()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     b = xil(),
  #     c = tolu(),
  #     d = etbenc()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     b = xil(),
  #     c = tolu(),
  #     d = etbenc()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  #   
  # })
  # 
  # ###############################  PLOMO  #######################################
  # 
  # predicted_data_plo <- eventReactive(input$pred, {
  #   req(bario(), arse(), cromo(), suelo(), plo())
  #   datos_predicc <- datos() %>%
  #     select(
  #       `1157 - PLOMO TOTAL`,
  #       `1165 - ARSÉNICO TOTAL`,
  #       `1173 - BARIO TOTAL`,
  #       `1163 - CROMO TOTAL`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       a = `1157 - PLOMO TOTAL`,
  #       b = `1165 - ARSÉNICO TOTAL`,
  #       c = `1173 - BARIO TOTAL`,
  #       d = `1163 - CROMO TOTAL`
  #     )
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 375, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 500, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 1000, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   set.seed(5)
  #   smote <- SMOTE(datos_predicc1[, -4], datos_predicc1[, 4], K = 5, dup_size = 0)
  #   smote2 <- SMOTE(datos_predicc2[, -4], datos_predicc2[, 4], K = 3, dup_size = 0)
  #   smote3 <- SMOTE(datos_predicc3[, -4], datos_predicc3[, 4], K = 5, dup_size = 0)
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   
  #   data <- with(test_set, tibble(
  #     b = arse(),
  #     c = bario(),
  #     d = cromo()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     b = arse(),
  #     c = bario(),
  #     d = cromo()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     b = arse(),
  #     c = bario(),
  #     d = cromo()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # 
  # ########################################## ARSÉNICO ###############################################
  # 
   predicted_data_arse <- eventReactive(input$pred, {
     req(plo(), bario(), cromo(), suelo(), arse())
     
     set.seed(5)
     datos_predicc <- datos %>% select(`1157 - PLOMO TOTAL`,
                                       `1165 - ARSÉNICO TOTAL`,
                                       `1173 - BARIO TOTAL`,
                                       `1163 - CROMO TOTAL` 
                                       
                                       
     ) %>% drop_na() %>% dplyr::rename(b = `1157 - PLOMO TOTAL`,
                                       a = `1165 - ARSÉNICO TOTAL`,
                                       c = `1173 - BARIO TOTAL`,
                                       d = `1163 - CROMO TOTAL`)
     
     
     over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                          thr.rel = 0.0025,
                          C.perc = "balance")
     
     
     
     
     index <- createDataPartition(datos_predicc$a, p = 0.5, times = 1, list = FALSE)
     
     
     train_set <- over[index,] %>% drop_na()
     test_set <- datos_predicc[-index,]
     
     
     
     fit<- ranger(a ~., data = train_set, mtry = 3, min.node.size = 1, num.trees = 1000)
     predicted <- predict(fit, test_set, type = "response")
     
     data <- with(test_set, tibble(
       b = plo(),
       c = bario(),
       d = cromo()
     ))
     predicted_data <- predict(fit, data, type = "response")
     
     
     pre1 <- round(predicted_data$predictions, 2)
     
     err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
     
     tibble(pre1, err)
     
   })
  # 
  # ################################# BARIO ###############################################
  # 
  # predicted_data_bario <- eventReactive(input$pred, {
  #   req(cromo(), arse(), plo(), suelo(), bario())
  #   
  #   datos_predicc <- datos() %>%
  #     select(
  #       `1157 - PLOMO TOTAL`,
  #       `1165 - ARSÉNICO TOTAL`,
  #       `1173 - BARIO TOTAL`,
  #       `1163 - CROMO TOTAL`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       c = `1157 - PLOMO TOTAL`,
  #       b = `1165 - ARSÉNICO TOTAL`,
  #       a = `1173 - BARIO TOTAL`,
  #       d = `1163 - CROMO TOTAL`
  #     )
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 500, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc2 <- datos_predicc %>% mutate(nivel = ifelse(a > 750, 1, 0))
  #   datos_predicc2 <- datos_predicc2 %>% mutate(nivel = as.factor(nivel))
  #   datos_predicc3 <- datos_predicc %>% mutate(nivel = ifelse(a > 2000, 1, 0))
  #   datos_predicc3 <- datos_predicc3 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   datos_predicc2 <- datos_predicc2 %>% select(-a)
  #   datos_predicc3 <- datos_predicc3 %>% select(-a)
  #   
  #   set.seed(5)
  #   smote <- SMOTE(datos_predicc1[, -4], datos_predicc1[, 4], K = 5, dup_size = 2)
  #   smote2 <- SMOTE(datos_predicc2[, -4], datos_predicc2[, 4], K = 5, dup_size = 0)
  #   smote3 <- SMOTE(datos_predicc3[, -4], datos_predicc3[, 4], K = 5, dup_size = 5)
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   datos2 <- smote2$data
  #   datos2 <- datos2 %>% mutate(class = as.factor(class))
  #   datos3 <- smote3$data
  #   datos3 <- datos3 %>% mutate(class = as.factor(class))
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   index2 <- createDataPartition(datos2$class, p = 0.8, times = 1, list = FALSE)
  #   index3 <- createDataPartition(datos3$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   train_set2 <- datos2[index2, ]
  #   test_set2 <- datos2[-index2, ]
  #   train_set3 <- datos3[index3, ]
  #   test_set3 <- datos3[-index3, ]
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit2 <- ranger(class ~ ., data = train_set2, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   fit3 <- ranger(class ~ ., data = train_set3, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 1000)
  #   
  #   
  #   
  #   data <- with(test_set, tibble(
  #     b = arse(),
  #     c = plo(),
  #     d = cromo()
  #   ))
  #   
  #   data2 <- with(test_set2, tibble(
  #     b = arse(),
  #     c = plo(),
  #     d = cromo()
  #   ))
  #   
  #   data3 <- with(test_set3, tibble(
  #     b = arse(),
  #     c = plo(),
  #     d = cromo()
  #   ))
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   predicted_data2 <- predict(fit2, data2, type = "se")
  #   predicted_data3 <- predict(fit3, data3, type = "se")
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   pre2 <- round(predicted_data2$predictions, 2)
  #   pre3 <- round(predicted_data3$predictions, 2)
  #   
  #   tibble(pre1 = pre1, pre2 = pre2, pre3 = pre3)
  # })
  # 
  # ############################## VANADIO ###########################################
  # 
  # predicted_data_van <- eventReactive(input$pred, {
  #   req(plo(), arse(), bario(), cromo(), suelo(), van())
  #   datos_predicc <- datos() %>%
  #     select(
  #       `1157 - PLOMO TOTAL`,
  #       `1165 - ARSÉNICO TOTAL`,
  #       `1173 - BARIO TOTAL`,
  #       `1163 - CROMO TOTAL`,
  #       `1118 - VANADIO TOTAL`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       b = `1157 - PLOMO TOTAL`,
  #       e = `1165 - ARSÉNICO TOTAL`,
  #       c = `1173 - BARIO TOTAL`,
  #       d = `1163 - CROMO TOTAL`,
  #       a = `1118 - VANADIO TOTAL`
  #     )
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 200, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   
  #   
  #   set.seed(5)
  #   smote <- SMOTE(datos_predicc1[, -5], datos_predicc1[, 5], K = 5, dup_size = 85)
  #   
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 500)
  #   
  #   
  #   
  #   data <- with(test_set, tibble(
  #     e = arse(),
  #     c = bario(),
  #     b = plo(),
  #     d = cromo()
  #   ))
  #   
  #   
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   
  #   
  #   tibble(pre1 = pre1)
  # })
  # 
  # ############################ CROMO ############################################
  # 
  # predicted_data_cromo <- eventReactive(input$pred, {
  #   req(arse(), bario(), plo(), suelo(), cromo())
  #   datos_predicc <- datos() %>%
  #     select(
  #       `1157 - PLOMO TOTAL`,
  #       `1165 - ARSÉNICO TOTAL`,
  #       `1173 - BARIO TOTAL`,
  #       `1163 - CROMO TOTAL`
  #     ) %>%
  #     drop_na() %>%
  #     dplyr::rename(
  #       d = `1157 - PLOMO TOTAL`,
  #       b = `1165 - ARSÉNICO TOTAL`,
  #       c = `1173 - BARIO TOTAL`,
  #       a = `1163 - CROMO TOTAL`
  #     )
  #   
  #   
  #   datos_predicc1 <- datos_predicc %>% mutate(nivel = ifelse(a > 250, 1, 0))
  #   datos_predicc1 <- datos_predicc1 %>% mutate(nivel = as.factor(nivel))
  #   
  #   datos_predicc1 <- datos_predicc1 %>% select(-a)
  #   
  #   
  #   set.seed(5)
  #   smote <- SMOTE(datos_predicc1[, -4], datos_predicc1[, 4], K = 5, dup_size = 0)
  #   
  #   
  #   datos1 <- smote$data
  #   datos1 <- datos1 %>% mutate(class = as.factor(class))
  #   
  #   
  #   index <- createDataPartition(datos1$class, p = 0.8, times = 1, list = FALSE)
  #   
  #   
  #   train_set <- datos1[index, ]
  #   test_set <- datos1[-index, ]
  #   
  #   
  #   fit <- ranger(class ~ ., data = train_set, probability = TRUE, keep.inbag = TRUE, mtry = 3, min.node.size = 1, num.trees = 500)
  #   
  #   
  #   data <- with(test_set, tibble(
  #     b = arse(),
  #     c = bario(),
  #     d = plo()
  #   ))
  #   
  #   
  #   predicted_data <- predict(fit, data, type = "se")
  #   
  #   
  #   pre1 <- round(predicted_data$predictions, 2)
  #   
  #   
  #   tibble(pre1 = pre1)
  # })
  # 
  ############################# OUTPUT SUELO HTP ############################# 
  # output$predHTP2 <- renderUI({
  #   req(htp(), input$pred, suelo())
  #   if (htp() <= 700 & predicted_data_htp2()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "HTP (C6-C35)", "</br>", "Probabilidad de que la muestra tenga más de 700 µg/g de HTP: ", predicted_data_htp2()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 µg/g de HTP: ", predicted_data_htp2()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 10000 µg/g de HTP: ", predicted_data_htp2()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", htp(), " µg/g", "</b>"
  #     ))
  #   } else if (between(htp(), 700, 1000) & predicted_data_htp2()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "HTP (C6-C35)", "</br>", "Probabilidad de que la muestra tenga más de 700 µg/g de HTP: ", predicted_data_htp2()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 µg/g de HTP: ", predicted_data_htp2()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 10000 µg/g de HTP: ", predicted_data_htp2()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", htp(), " µg/g", "</b>"
  #     ))
  #   } else if (between(htp(), 1000, 10000) & predicted_data_htp2()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "HTP (C6-C35)", "</br>", "Probabilidad de que la muestra tenga más de 700 µg/g de HTP: ", predicted_data_htp2()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 µg/g de HTP: ", predicted_data_htp2()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 10000 µg/g de HTP: ", predicted_data_htp2()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", htp(), " µg/g", "</b>"
  #     ))
  #   } else if (between(htp(), 700, 1000) & predicted_data_htp2()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "HTP (C6-C35)", "</br>", "Probabilidad de que la muestra tenga más de 700 µg/g de HTP: ", predicted_data_htp2()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 µg/g de HTP: ", predicted_data_htp2()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 10000 µg/g de HTP: ", predicted_data_htp2()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", htp(), " µg/g", "</b>"
  #     ))
  #   } else if (htp() > 10000 & predicted_data_htp2()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "HTP (C6-C35)", "</br>", "Probabilidad de que la muestra tenga más de 700 µg/g de HTP: ", predicted_data_htp2()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 µg/g de HTP: ", predicted_data_htp2()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 10000 µg/g de HTP: ", predicted_data_htp2()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", htp(), " µg/g", "</b>"
  #     ))
  #   } else if (between(htp(), 1000, 10000) & predicted_data_htp2()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "HTP (C6-C35)", "</br>", "Probabilidad de que la muestra tenga más de 700 µg/g de HTP: ", predicted_data_htp2()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 µg/g de HTP: ", predicted_data_htp2()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 10000 µg/g de HTP: ", predicted_data_htp2()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", htp(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "HTP (C6-C35)", "</br>", "Probabilidad de que la muestra tenga más de 700 µg/g de HTP: ", predicted_data_htp2()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 µg/g de HTP: ", predicted_data_htp2()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 10000 µg/g de HTP: ", predicted_data_htp2()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", htp(), " µg/g", "</b>"
  #     ))
  #   }
  # })
  # 
  #################################### OUTPUT SUELO NAFT ##########################################
  # output$prednaft <- renderUI({
  #   req(naft(), input$pred, suelo())
  #   if (naft() <= 0.1 & predicted_data_naft()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "NAFTALENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Naftaleno: ", predicted_data_naft()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Naftaleno: ", predicted_data_naft()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Naftaleno: ", predicted_data_naft()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", naft(), " µg/g", "</b>"
  #     ))
  #   } else if (between(naft(), 0.1, 5) & predicted_data_naft()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "NAFTALENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Naftaleno: ", predicted_data_naft()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Naftaleno: ", predicted_data_naft()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Naftaleno: ", predicted_data_naft()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", naft(), " µg/g", "</b>"
  #     ))
  #   } else if (between(naft(), 0.1, 5) & predicted_data_naft()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "NAFTALENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Naftaleno: ", predicted_data_naft()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Naftaleno: ", predicted_data_naft()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Naftaleno: ", predicted_data_naft()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", naft(), " µg/g", "</b>"
  #     ))
  #   } else if (between(naft(), 5, 50) & predicted_data_naft()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "NAFTALENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Naftaleno: ", predicted_data_naft()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Naftaleno: ", predicted_data_naft()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Naftaleno: ", predicted_data_naft()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", naft(), " µg/g", "</b>"
  #     ))
  #   } else if (between(naft(), 5, 50) & predicted_data_naft()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "NAFTALENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Naftaleno: ", predicted_data_naft()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Naftaleno: ", predicted_data_naft()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Naftaleno: ", predicted_data_naft()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", naft(), " µg/g", "</b>"
  #     ))
  #   } else if (naft() > 50 & predicted_data_naft()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "NAFTALENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Naftaleno: ", predicted_data_naft()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Naftaleno: ", predicted_data_naft()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Naftaleno: ", predicted_data_naft()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", naft(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "NAFTALENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Naftaleno: ", predicted_data_naft()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Naftaleno: ", predicted_data_naft()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Naftaleno: ", predicted_data_naft()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", naft(), " µg/g", "</b>"
  #     ))
  #   }
  # })
  # ###################################### OUTPUT SUELO TOLU ###############################################
  # output$predtolu <- renderUI({
  #   req(tolu(), input$pred, suelo())
  #   if (between(tolu(), 0.1, 3) & predicted_data_tol()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "TOLUENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Tolueno: ", predicted_data_tol()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 3 µg/g de Tolueno: ", predicted_data_tol()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 30 µg/g de Tolueno: ", predicted_data_tol()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", tolu(), " µg/g", "</b>"
  #     ))
  #   } else if (tolu() <= 0.1 & predicted_data_tol()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "TOLUENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Tolueno: ", predicted_data_tol()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 3 µg/g de Tolueno: ", predicted_data_tol()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 30 µg/g de Tolueno: ", predicted_data_tol()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", tolu(), " µg/g", "</b>"
  #     ))
  #   } else if (between(tolu(), 3, 30) & predicted_data_tol()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "TOLUENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Tolueno: ", predicted_data_tol()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 3 µg/g de Tolueno: ", predicted_data_tol()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 30 µg/g de Tolueno: ", predicted_data_tol()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", tolu(), " µg/g", "</b>"
  #     ))
  #   } else if (between(tolu(), 0.1, 3) & predicted_data_tol()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "TOLUENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Tolueno: ", predicted_data_tol()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 3 µg/g de Tolueno: ", predicted_data_tol()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 30 µg/g de Tolueno: ", predicted_data_tol()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", tolu(), " µg/g", "</b>"
  #     ))
  #   } else if (tolu() > 30 & predicted_data_tol()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "TOLUENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Tolueno: ", predicted_data_tol()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 3 µg/g de Tolueno: ", predicted_data_tol()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 30 µg/g de Tolueno: ", predicted_data_tol()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", tolu(), " µg/g", "</b>"
  #     ))
  #   } else if (between(tolu(), 3, 30) & predicted_data_tol()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "TOLUENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Tolueno: ", predicted_data_tol()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 3 µg/g de Tolueno: ", predicted_data_tol()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 30 µg/g de Tolueno: ", predicted_data_tol()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", tolu(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "TOLUENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Tolueno: ", predicted_data_tol()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 3 µg/g de Tolueno: ", predicted_data_tol()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 30 µg/g de Tolueno: ", predicted_data_tol()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", tolu(), " µg/g", "</b>"
  #     ))
  #   }
  # })
  #################################### OUTPUT SUELO ETBENC #############################
  # output$predetilben <- renderUI({
  #   req(etbenc(), input$pred, suelo())
  #   if (etbenc() <= 0.1 & predicted_data_etbenc()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "ETILBENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", etbenc(), " µg/g", "</b>"
  #     ))
  #   } else if (between(etbenc(), 0.1, 5) & predicted_data_etbenc()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "ETILBENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", etbenc(), " µg/g", "</b>"
  #     ))
  #   } else if (between(etbenc(), 0.1, 5) & predicted_data_etbenc()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "ETILBENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", etbenc(), " µg/g", "</b>"
  #     ))
  #   } else if (between(etbenc(), 5, 25) & predicted_data_etbenc()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "ETILBENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", etbenc(), " µg/g", "</b>"
  #     ))
  #   } else if (between(etbenc(), 5, 25) & predicted_data_etbenc()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "ETILBENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", etbenc(), " µg/g", "</b>"
  #     ))
  #   } else if (etbenc() > 25 & predicted_data_etbenc()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "ETILBENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", etbenc(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "ETILBENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Etilbenceno: ", predicted_data_etbenc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", etbenc(), " µg/g", "</b>"
  #     ))
  #   }
  # })
  ################################ OUTPUT SUELO XILENOS ############################################
  # output$predexil <- renderUI({
  #   req(xil(), input$pred, suelo())
  #   if (xil() <= 0.1 & predicted_data_xil()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "SUMA DE XILENOS", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Xilenos: ", predicted_data_xil()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Xilenos: ", predicted_data_xil()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Xilenos: ", predicted_data_xil()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", xil(), " µg/g", "</b>"
  #     ))
  #   } else if (between(xil(), 0.1, 5) & predicted_data_xil()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "SUMA DE XILENOS", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Xilenos: ", predicted_data_xil()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Xilenos: ", predicted_data_xil()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Xilenos: ", predicted_data_xil()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", xil(), " µg/g", "</b>"
  #     ))
  #   } else if (between(xil(), 0.1, 5) & predicted_data_xil()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "SUMA DE XILENOS", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Xilenos: ", predicted_data_xil()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Xilenos: ", predicted_data_xil()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Xilenos: ", predicted_data_xil()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", xil(), " µg/g", "</b>"
  #     ))
  #   } else if (between(xil(), 5, 50) & predicted_data_xil()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "SUMA DE XILENOS", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Xilenos: ", predicted_data_xil()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Xilenos: ", predicted_data_xil()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Xilenos: ", predicted_data_xil()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", xil(), " µg/g", "</b>"
  #     ))
  #   } else if (between(xil(), 5, 50) & predicted_data_xil()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "SUMA DE XILENOS", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Xilenos: ", predicted_data_xil()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Xilenos: ", predicted_data_xil()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Xilenos: ", predicted_data_xil()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", xil(), " µg/g", "</b>"
  #     ))
  #   } else if (xil() > 50 & predicted_data_xil()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "SUMA DE XILENOS", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Xilenos: ", predicted_data_xil()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Xilenos: ", predicted_data_xil()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Xilenos: ", predicted_data_xil()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", xil(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "SUMA DE XILENOS", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Xilenos: ", predicted_data_xil()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Xilenos: ", predicted_data_xil()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 50 µg/g de Xilenos: ", predicted_data_xil()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", xil(), " µg/g", "</b>"
  #     ))
  #   }
  # })
  # ####################################### OUTPUT SUELO FENANTRENO #####################################3
  # output$predfen <- renderUI({
  #   req(fen(), input$pred, suelo())
  #   if (fen() <= 0.1 & predicted_data_fen()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "FENANTRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Fenantreno: ", predicted_data_fen()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Fenantreno: ", predicted_data_fen()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Fenantreno: ", predicted_data_fen()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", fen(), " µg/g", "</b>"
  #     ))
  #   } else if (between(fen(), 0.1, 5) & predicted_data_fen()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "FENANTRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Fenantreno: ", predicted_data_fen()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Fenantreno: ", predicted_data_fen()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Fenantreno: ", predicted_data_fen()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", fen(), " µg/g", "</b>"
  #     ))
  #   } else if (between(fen(), 5, 25) & predicted_data_fen()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "FENANTRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Fenantreno: ", predicted_data_fen()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Fenantreno: ", predicted_data_fen()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Fenantreno: ", predicted_data_fen()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", fen(), " µg/g", "</b>"
  #     ))
  #   } else if (between(fen(), 0.1, 5) & predicted_data_fen()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "FENANTRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Fenantreno: ", predicted_data_fen()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Fenantreno: ", predicted_data_fen()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Fenantreno: ", predicted_data_fen()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", fen(), " µg/g", "</b>"
  #     ))
  #   } else if (between(fen(), 5, 25) & predicted_data_fen()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "FENANTRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Fenantreno: ", predicted_data_fen()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Fenantreno: ", predicted_data_fen()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Fenantreno: ", predicted_data_fen()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", fen(), " µg/g", "</b>"
  #     ))
  #   } else if (fen() > 25 & predicted_data_fen()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "FENANTRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Fenantreno: ", predicted_data_fen()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Fenantreno: ", predicted_data_fen()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Fenantreno: ", predicted_data_fen()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", fen(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "FENANTRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Fenantreno: ", predicted_data_fen()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Fenantreno: ", predicted_data_fen()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 25 µg/g de Fenantreno: ", predicted_data_fen()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", fen(), " µg/g", "</b>"
  #     ))
  #   }
  # })
  # ################################################ OUTPUT SUELO PIRENO ##########################################
  # output$predpir <- renderUI({
  #   req(pireno(), input$pred, suelo())
  #   if (pireno() <= 0.1 & predicted_data_pir()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PIRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Pireno: ", predicted_data_pir()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.3 µg/g de Pireno: ", predicted_data_pir()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1 µg/g de Pireno: ", predicted_data_pir()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", pireno(), " µg/g", "</b>"
  #     ))
  #   } else if (between(pireno(), 0.1, 0.3) & predicted_data_pir()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PIRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Pireno: ", predicted_data_pir()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.3 µg/g de Pireno: ", predicted_data_pir()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1 µg/g de Pireno: ", predicted_data_pir()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", pireno(), " µg/g", "</b>"
  #     ))
  #   } else if (between(pireno(), 0.1, 0.3) & predicted_data_pir()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PIRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Pireno: ", predicted_data_pir()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.3 µg/g de Pireno: ", predicted_data_pir()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1 µg/g de Pireno: ", predicted_data_pir()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", pireno(), " µg/g", "</b>"
  #     ))
  #   } else if (between(pireno(), 0.3, 1) & predicted_data_pir()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PIRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Pireno: ", predicted_data_pir()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.3 µg/g de Pireno: ", predicted_data_pir()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1 µg/g de Pireno: ", predicted_data_pir()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", pireno(), " µg/g", "</b>"
  #     ))
  #   } else if (between(pireno(), 0.3, 1) & predicted_data_pir()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PIRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Pireno: ", predicted_data_pir()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.3 µg/g de Pireno: ", predicted_data_pir()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1 µg/g de Pireno: ", predicted_data_pir()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", pireno(), " µg/g", "</b>"
  #     ))
  #   } else if (pireno() > 1 & predicted_data_pir()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PIRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Pireno: ", predicted_data_pir()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.3 µg/g de Pireno: ", predicted_data_pir()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1 µg/g de Pireno: ", predicted_data_pir()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", pireno(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "PIRENO", "</br>", "Probabilidad de que la muestra tenga más de 0.1 µg/g de Pireno: ", predicted_data_pir()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.3 µg/g de Pireno: ", predicted_data_pir()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1 µg/g de Pireno: ", predicted_data_pir()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", pireno(), " µg/g", "</b>"
  #     ))
  #   }
  # })
   ##################################### OUTPUT SUELO PLOMO ###################################################
  # output$predplomo <- renderUI({
  #   req(plo(), input$pred, suelo())
  #   if (plo() <= 375 & predicted_data_plo()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PLOMO", "</br>", "Probabilidad de que la muestra tenga más de 375 mg/kg de Plomo: ", predicted_data_plo()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 500 mg/kg de Plomo: ", predicted_data_plo()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 mg/kg de Plomo: ", predicted_data_plo()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", plo(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(plo(), 375, 500) & predicted_data_plo()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PLOMO", "</br>", "Probabilidad de que la muestra tenga más de 375 mg/kg de Plomo: ", predicted_data_plo()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 500 mg/kg de Plomo: ", predicted_data_plo()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 mg/kg de Plomo: ", predicted_data_plo()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", plo(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(plo(), 500, 1000) & predicted_data_plo()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PLOMO", "</br>", "Probabilidad de que la muestra tenga más de 375 mg/kg de Plomo: ", predicted_data_plo()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 500 mg/kg de Plomo: ", predicted_data_plo()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 mg/kg de Plomo: ", predicted_data_plo()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", plo(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(plo(), 375, 500) & predicted_data_plo()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PLOMO", "</br>", "Probabilidad de que la muestra tenga más de 375 mg/kg de Plomo: ", predicted_data_plo()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 500 mg/kg de Plomo: ", predicted_data_plo()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 mg/kg de Plomo: ", predicted_data_plo()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", plo(), " mg/kg", "</b>"
  #     ))
  #   } else if (plo() > 1000 & predicted_data_plo()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PLOMO", "</br>", "Probabilidad de que la muestra tenga más de 375 mg/kg de Plomo: ", predicted_data_plo()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 500 mg/kg de Plomo: ", predicted_data_plo()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 mg/kg de Plomo: ", predicted_data_plo()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", plo(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(plo(), 500, 1000) & predicted_data_plo()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "PLOMO", "</br>", "Probabilidad de que la muestra tenga más de 375 mg/kg de Plomo: ", predicted_data_plo()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 500 mg/kg de Plomo: ", predicted_data_plo()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 mg/kg de Plomo: ", predicted_data_plo()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", plo(), " mg/kg", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "PLOMO", "</br>", "Probabilidad de que la muestra tenga más de 375 mg/kg de Plomo: ", predicted_data_plo()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 500 mg/kg de Plomo: ", predicted_data_plo()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 1000 mg/kg de Plomo: ", predicted_data_plo()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", plo(), " mg/kg", "</b>"
  #     ))
  #   }
  # })
   ##################################### OUTPUT SUELO ARSE #######################################################
   output$predarsenico <- renderUI({
     req(arse(), input$pred, suelo())
          c <- ifelse(between(arse(), 
  (predicted_data_arse()$pre1 - predicted_data_arse()$err) - (predicted_data_arse()$pre1 - predicted_data_arse()$err)*0.5, 
  (predicted_data_arse()$pre1 + predicted_data_arse()$err) + (predicted_data_arse()$pre1 + predicted_data_arse()$err)*0.5),
"<b style= 'color: green;'>", "<b style= 'color: red;'>")
HTML(paste0(c,
            "ARSÉNICO", "</br>",
            "Resultado informado: ", arse(), " mg/kg", "</br>",
            "Resultado predicho: ", predicted_data_arse()[1], " mg/kg ± ", 
            predicted_data_arse()[2]," mg/kg", "</b>"))
   })
   ###################################### OUTPUT SUELO BARIO ###########################################
  # output$predbario <- renderUI({
  #   req(bario(), input$pred, suelo())
  #   if (bario() <= 500 & predicted_data_bario()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BARIO", "</br>", "Probabilidad de que la muestra tenga más de 500 mg/kg de Bario: ", predicted_data_bario()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 750 mg/kg de Bario: ", predicted_data_bario()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 2000 mg/kg de Bario: ", predicted_data_bario()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", bario(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(bario(), 500, 750) & predicted_data_bario()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BARIO", "</br>", "Probabilidad de que la muestra tenga más de 500 mg/kg de Bario: ", predicted_data_bario()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 750 mg/kg de Bario: ", predicted_data_bario()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 2000 mg/kg de Bario: ", predicted_data_bario()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", bario(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(bario(), 500, 750) & predicted_data_bario()$pre2[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BARIO", "</br>", "Probabilidad de que la muestra tenga más de 500 mg/kg de Bario: ", predicted_data_bario()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 750 mg/kg de Bario: ", predicted_data_bario()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 2000 mg/kg de Bario: ", predicted_data_bario()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", bario(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(bario(), 750, 2000) & predicted_data_bario()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BARIO", "</br>", "Probabilidad de que la muestra tenga más de 500 mg/kg de Bario: ", predicted_data_bario()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 750 mg/kg de Bario: ", predicted_data_bario()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 2000 mg/kg de Bario: ", predicted_data_bario()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", bario(), " mg/kg", "</b>"
  #     ))
  #   } else if (bario() > 2000 & predicted_data_bario()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BARIO", "</br>", "Probabilidad de que la muestra tenga más de 500 mg/kg de Bario: ", predicted_data_bario()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 750 mg/kg de Bario: ", predicted_data_bario()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 2000 mg/kg de Bario: ", predicted_data_bario()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", bario(), " mg/kg", "</b>"
  #     ))
  #   } else if (between(bario(), 750, 2000) & predicted_data_bario()$pre3[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BARIO", "</br>", "Probabilidad de que la muestra tenga más de 500 mg/kg de Bario: ", predicted_data_bario()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 750 mg/kg de Bario: ", predicted_data_bario()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 2000 mg/kg de Bario: ", predicted_data_bario()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", bario(), " mg/kg", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "BARIO", "</br>", "Probabilidad de que la muestra tenga más de 500 mg/kg de Bario: ", predicted_data_bario()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 750 mg/kg de Bario: ", predicted_data_bario()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 2000 mg/kg de Bario: ", predicted_data_bario()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", bario(), " mg/kg", "</b>"
  #     ))
  #   }
  # })
   ################################# OUTPUT SUELO VANADIO ###############################################
  # output$predvan <- renderUI({
  #   req(van(), input$pred, suelo())
  #   if (van() > 200 & predicted_data_van()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "VANADIO", "</br>", "Probabilidad de que la muestra tenga más de 200 mg/kg de Vanadio: ", predicted_data_van()$pre1[2] * 100, " %", "</br>",
  #       "Resultado informado: ", van(), " mg/kg", "</b>"
  #     ))
  #   } else if (van() <= 200 & predicted_data_van()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "VANADIO", "</br>", "Probabilidad de que la muestra tenga más de 200 mg/kg de Vanadio: ", predicted_data_van()$pre1[2] * 100, " %", "</br>",
  #       "Resultado informado: ", van(), " mg/kg", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "VANADIO", "</br>", "Probabilidad de que la muestra tenga más de 200 mg/kg de Vanadio: ", predicted_data_van()$pre1[2] * 100, " %", "</br>",
  #       "Resultado informado: ", van(), " mg/kg", "</b>"
  #     ))
  #   }
  # })
  # ########################################### OUTPUT SUELO CROMO ##############################################
  # output$predcromo <- renderUI({
  #   req(cromo(), input$pred, suelo())
  #   if (cromo() <= 250 & predicted_data_cromo()$pre1[2] <= 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "CROMO", "</br>", "Probabilidad de que la muestra tenga más de 250 mg/kg de Cromo: ", predicted_data_cromo()$pre1[2] * 100, " %", "</br>",
  #       "Resultado informado: ", cromo(), " mg/kg", "</b>"
  #     ))
  #   } else if (cromo() > 250 & predicted_data_cromo()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "CROMO", "</br>", "Probabilidad de que la muestra tenga más de 250 mg/kg de Cromo: ", predicted_data_cromo()$pre1[2] * 100, " %", "</br>",
  #       "Resultado informado: ", cromo(), " mg/kg", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "CROMO", "</br>", "Probabilidad de que la muestra tenga más de 250 mg/kg de Cromo: ", predicted_data_cromo()$pre1[2] * 100, " %", "</br>",
  #       "Resultado informado: ", cromo(), " mg/kg", "</b>"
  #     ))
  #   }
  # })
 ############################################# OUTPUT SUELO BENCENO ############################################
  # output$predbenc <- renderUI({
  #   req(benc(), input$pred, suelo())
  #   if (between(benc(), 0.05, 0.5) & predicted_data_benc()$pre1[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.05 µg/g de Benceno: ", predicted_data_benc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.5 µg/g de Benceno: ", predicted_data_benc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Benceno: ", predicted_data_benc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", benc(), " µg/g", "</b>"
  #     ))
  #   } else if (benc() <= 0.05 & predicted_data_benc()$pre1[2] < 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.05 µg/g de Benceno: ", predicted_data_benc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.5 µg/g de Benceno: ", predicted_data_benc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Benceno: ", predicted_data_benc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", benc(), " µg/g", "</b>"
  #     ))
  #   } else if (between(benc(), 0.05, 0.5) & predicted_data_benc()$pre2[2] < 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.05 µg/g de Benceno: ", predicted_data_benc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.5 µg/g de Benceno: ", predicted_data_benc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Benceno: ", predicted_data_benc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", benc(), " µg/g", "</b>"
  #     ))
  #   } else if (between(benc(), 0.5, 5) & predicted_data_benc()$pre2[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.05 µg/g de Benceno: ", predicted_data_benc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.5 µg/g de Benceno: ", predicted_data_benc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Benceno: ", predicted_data_benc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", benc(), " µg/g", "</b>"
  #     ))
  #   } else if (benc() > 5 & predicted_data_benc()$pre3[2] > 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.05 µg/g de Benceno: ", predicted_data_benc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.5 µg/g de Benceno: ", predicted_data_benc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Benceno: ", predicted_data_benc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", benc(), " µg/g", "</b>"
  #     ))
  #   } else if (between(benc(), 0.5, 5) & predicted_data_benc()$pre3[2] < 0.7) {
  #     HTML(paste0(
  #       "<b style= 'color: green;'>", "BENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.05 µg/g de Benceno: ", predicted_data_benc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.5 µg/g de Benceno: ", predicted_data_benc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Benceno: ", predicted_data_benc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", benc(), " µg/g", "</b>"
  #     ))
  #   } else {
  #     HTML(paste0(
  #       "<b style= 'color: red;'>", "BENCENO", "</br>", "Probabilidad de que la muestra tenga más de 0.05 µg/g de Benceno: ", predicted_data_benc()$pre1[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 0.5 µg/g de Benceno: ", predicted_data_benc()$pre2[2] * 100, " %", "</br>",
  #       "Probabilidad de que la muestra tenga más de 5 µg/g de Benceno: ", predicted_data_benc()$pre3[2] * 100, " %", "</br>",
  #       "Resultado informado: ", benc(), " µg/g", "</b>"
  #     ))
  #   }
  # })
  ####################### PRED AGUA FENANTRENO ##############################
  predicted_agua_fen <- eventReactive(input$pred, {
    req(dro_a(), agua(), fenoles_a(), fen_a(), pireno_a(), gro_a())
    
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% select(`DRO + GRO`,
                                      `2509 - FENOLES`,
                                      `2578 - FENANTRENO`,
                                      `2581 - PIRENO`,
                                     `2574 - NAFTALENO`,
                                      `2659 - ETILBENCENO (GC/MS)`,
                                      `2577 - FLUORENO`
                                      
                                      
                                      
                                      
    ) %>% drop_na() %>% dplyr::rename(b = `DRO + GRO`,
                                      c = `2509 - FENOLES`,
                                      a = `2578 - FENANTRENO`,
                                      d = `2581 - PIRENO`,
                                      e = `2574 - NAFTALENO`,
                                      f = `2659 - ETILBENCENO (GC/MS)`,
                                      g = `2577 - FLUORENO`
    ) %>% filter(c < 6.5)
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 0.7,
                         C.perc = "balance")
    
    
    
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    
    train_set <- over[index,] %>% drop_na()
    test_set <- datos_predicc[-index,]
    
    
    
    fit<- ranger(a ~., data = train_set, mtry = 6, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    
    data <- with(test_set, tibble(
      
      b = dro_a() + gro_a(),
      c = fenoles_a(),
      d = pireno_a(),
      e = naft_a(),
      f = etbenc_a(),
      g = fluoreno_a()
    ))
    
    
    predicted_data <- predict(fit, data, type = "response")
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
  })
  
  ############################ ETBENC AGUA ####################################
  predicted_agua_etbenc <- eventReactive(input$pred, {
    req(xil_a(), tolu_a(), etbenc_a())
    set.seed(5)
    datos_predicc <- datos_ag() %>% select(`SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                      `2658 - TOLUENO (GC/MS)`,
                                      `2659 - ETILBENCENO (GC/MS)`,
                                      `2657 - BENCENO (GC/MS)`,
                                      `DRO + GRO`,
                                      `2578 - FENANTRENO`
                                      
                                      
                                      
    ) %>% drop_na() %>% dplyr::rename(c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                      d = `2658 - TOLUENO (GC/MS)`,
                                      a = `2659 - ETILBENCENO (GC/MS)`,
                                      b = `2657 - BENCENO (GC/MS)`,
                                      e = `DRO + GRO`,
                                      f = `2578 - FENANTRENO`
                                      
    )
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel =0.5,
                         C.perc = "balance")
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    
    train_set <- over[index,] %>% drop_na()
    test_set <- datos_predicc[-index,]
    
    
    fit<- ranger(a ~., data = train_set, mtry = 4, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    data <- with(test_set, tibble(
      c = xil_a(),
      d = tolu_a(),
      b = benc_a(),
      e = dro_a() + gro_a(),
      f = fen_a()
      
      
    ))
    
    
    
    predicted_data <- predict(fit, data, type = "response")
    
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
    
  })
  
  ################ NAFTALENO AGUA #############################
  predicted_agua_naft <- eventReactive(input$pred, {
    req(naft_a(), etbenc_a(), benc_a(), tolu_a(), dro_a(), gro_a(), xil_a())
    set.seed(5)
    datos_predicc <- datos_ag() %>% select(`2574 - NAFTALENO`,
                                       `DRO + GRO`,
                                      `2578 - FENANTRENO`,
                                       `2581 - PIRENO`,
                                      `2577 - FLUORENO`
                              
                                      
    ) %>% drop_na() %>% dplyr::rename(a = `2574 - NAFTALENO`,
                                      f = `2578 - FENANTRENO`,
                                      g = `2581 - PIRENO`,
                                      h = `DRO + GRO`,
                                      i = `2577 - FLUORENO`
                                      
                                      
    )
    
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel =0.7,
                         C.perc = "balance")
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    
    train_set <- over[index,] %>% drop_na()
    test_set <- datos_predicc[-index,]
    
    
    fit<- ranger(a ~., data = train_set, mtry = 2, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    
    data <- with(test_set, tibble(

      f = fen_a(),
      g = pireno_a(),
      h = dro_a() + gro_a(),
      i = fluoreno_a()
      
    ))
    
    
    
    predicted_data <- predict(fit, data, type = "response")

    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)

    tibble(pre1, err)
    
   
  })
  ############################ PREDICCIÓN GRO #####################################
  predicted_agua_gro <- eventReactive(input$pred, {
    req(gro_a(), xil_a(), tolu_a(), etbenc_a(), benc_a())
    
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% select(`2591 - RANGO ORGANICO DE GASOLINA (GRO)`,
                                      `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                      `2658 - TOLUENO (GC/MS)`,
                                      `2657 - BENCENO (GC/MS)`,
                                      `2519 - DETERGENTES ANIÓNICOS`,
                                      `2590 - RANGO ORGANICO DE DIESEL (DRO)`
                                      
                                      
    ) %>% drop_na() %>% dplyr::rename(a = `2591 - RANGO ORGANICO DE GASOLINA (GRO)`,
                                      b = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                      c = `2658 - TOLUENO (GC/MS)`,
                                      e = `2657 - BENCENO (GC/MS)`,
                                      i = `2519 - DETERGENTES ANIÓNICOS`,
                                      j = `2590 - RANGO ORGANICO DE DIESEL (DRO)`
                                      
    )
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 1,
                         C.perc = "balance")
    
    
    
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    
    train_set <- over[index,] %>% drop_na()
    test_set <- datos_predicc[-index,]
    
    
    
    fit<- ranger(a ~., data = train_set, mtry = 5, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response", na.rm = TRUE)
    
    data <- with(test_set, tibble(
      
      b = xil_a(),
      c = tolu_a(),
      e = benc_a(),
      i = deter_a(),
      j = dro_a()
      
      
    ))
    
    predicted_data <- predict(fit, data, type = "response")
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
    
    
  })
  
  ###########################PREDICCIÓN DRO #######################################
   predicted_agua_dro <- eventReactive(input$pred, {
     req(dro_a(), xil_a(), tolu_a(), etbenc_a(), benc_a(), gro_a())
     
  
  set.seed(5)
  
  datos_predicc <- datos_ag() %>% select(`2590 - RANGO ORGANICO DE DIESEL (DRO)`,
                                    `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                    `2658 - TOLUENO (GC/MS)`,
                                    `2657 - BENCENO (GC/MS)`,
                                    `2519 - DETERGENTES ANIÓNICOS`,
                                
                                    
                                    
  ) %>% drop_na() %>% dplyr::rename(a = `2590 - RANGO ORGANICO DE DIESEL (DRO)`,
                                    b = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                    c = `2658 - TOLUENO (GC/MS)`,
                                    e = `2657 - BENCENO (GC/MS)`,
                                    i = `2519 - DETERGENTES ANIÓNICOS`,
                             
                                    
  
                                    
  )
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel = 0.005,
                       C.perc = "balance")
  
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  
  train_set <- over[index,] %>% drop_na()
  test_set <- datos_predicc[-index,]
  
  
  
  fit<- ranger(a ~., data = train_set, mtry = 4, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response", na.rm = TRUE)
  
  
  
  data <- with(test_set, tibble(
    
    b = xil_a(),
    c = tolu_a(),
    e = benc_a(),
    i = deter_a(),

    
    
  ))
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  
  
  
  
     
   })
   
  # ########################### PREDICCIÓN BENCENO ###################################
   predicted_agua_benc <- eventReactive(input$pred, {
     req(xil_a(), tolu_a(), etbenc_a(), benc_a())
    set.seed(5)
  datos_predicc <- datos_ag() %>%
    select(
      `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
      `2658 - TOLUENO (GC/MS)`,
      `2659 - ETILBENCENO (GC/MS)`,
      `2657 - BENCENO (GC/MS)`,
      `2509 - FENOLES`
      
      
    ) %>%
    drop_na() %>%
    dplyr::rename(
      c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
      d = `2658 - TOLUENO (GC/MS)`,
      b = `2659 - ETILBENCENO (GC/MS)`,
      a = `2657 - BENCENO (GC/MS)`,
      e =  `2509 - FENOLES`
      
    ) %>% filter(e < 6.5)
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel = 0.001,
                       C.perc = "balance")
  
  
  
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  
  train_set <- over[index,] %>% drop_na()
  test_set <- datos_predicc[-index,]
  
  
  
  fit<- ranger(a ~., data = train_set, mtry = 3, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  
  
  data <- with(test_set, tibble(
    
    c = xil_a(),
    d = tolu_a(),
    b = etbenc_a(),
    e = fenoles_a()
    
    
    
  ))
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  })
   
  # #############################PREDICCIÓN TOLUENO ###############################
     predicted_agua_tolu <- eventReactive(input$pred, {
     req(xil_a(), tolu_a(), etbenc_a())
    
  set.seed(5)
  datos_predicc <- datos_ag() %>% select(`SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                    `2658 - TOLUENO (GC/MS)`,
                                    `2659 - ETILBENCENO (GC/MS)`,
                                    `2509 - FENOLES`,
                                    `2657 - BENCENO (GC/MS)`
                                    
                                    
  ) %>% drop_na() %>% dplyr::rename(c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                    a = `2658 - TOLUENO (GC/MS)`,
                                    b = `2659 - ETILBENCENO (GC/MS)`,
                                    e = `2657 - BENCENO (GC/MS)`,
                                    d = `2509 - FENOLES`) %>% filter(d < 6.5)
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel = 0.001,
                       C.perc = "balance")
  
  
  
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  
  train_set <- over[index,] %>% drop_na()
  test_set <- datos_predicc[-index,]
  
  
  
  fit<- ranger(a ~., data = train_set, mtry = 2, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  data <- with(test_set, tibble(
    
    
    b = etbenc_a(),
    c = xil_a(),
    d = fenoles_a(),
    e = benc_a()
    
  ))
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  })
  
  # #############################PREDICCIÓN SUMA DE XILENOS ################################
   predicted_agua_xil <- eventReactive(input$pred, {
     req(xil_a(), tolu_a(), etbenc_a())
    set.seed(5)
  datos_predicc <- datos_ag() %>%
    select(
      `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
      `2658 - TOLUENO (GC/MS)`,
      `2659 - ETILBENCENO (GC/MS)`
    ) %>%
    drop_na() %>%
    dplyr::rename(
      a = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
      b = `2658 - TOLUENO (GC/MS)`,
      c = `2659 - ETILBENCENO (GC/MS)`
    )
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel =0.7,
                       C.perc = "balance")
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  
  train_set <- over[index,] %>% drop_na()
  test_set <- datos_predicc[-index,]
  
  
  fit<- ranger(a ~., data = train_set, mtry = 2, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  data <- with(test_set, tibble(
    c = etbenc_a(),
    b = tolu_a(),
    
    
    
  ))
  
  
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  
   })
   
  # ######################### PREDICCIÓN DETERGENTES ###############################
   predicted_agua_deter <- eventReactive(input$pred, {
     req(benc_a(), condu_a(), ph_a(), fen_a(), deter_a())
    
  set.seed(5)
  datos_predicc <- datos_ag() %>%
    select(
      `2517 - CONDUCTIVIDAD (a 25°C)`,
      `2500 - pH (25°C)`,
      `2519 - DETERGENTES ANIÓNICOS`
    ) %>%
    drop_na() %>%
    dplyr::rename(
      a = `2519 - DETERGENTES ANIÓNICOS`,
      d = `2517 - CONDUCTIVIDAD (a 25°C)`,
      e = `2500 - pH (25°C)`
    )
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel =0.01,
                       C.perc = "balance")
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  
  train_set <- over[index,] %>% drop_na()
  test_set <- datos_predicc[-index,]
  
  
  fit<- ranger(a ~., data = train_set, mtry = 2, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  data <- with(test_set, tibble(

    d = condu_a(),
    e = ph_a()
    
    
    
  ))
  
  
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  
   })
  
  
  # ############################ PREDICCIÓN FENOLES ####################################
   predicted_agua_fenoles <- eventReactive(input$pred, {
     req(deter_a(), fenoles_a(), etbenc_a(), benc_a(), fen_a())
    set.seed(5)
  
  datos_predicc <- datos_ag() %>% select(`2519 - DETERGENTES ANIÓNICOS`,
                                    `2509 - FENOLES`,
                                    `2659 - ETILBENCENO (GC/MS)`,
                                    `2657 - BENCENO (GC/MS)`,
                                    `2578 - FENANTRENO`
                                    
                                    
  ) %>% drop_na() %>% dplyr::rename(d = `2519 - DETERGENTES ANIÓNICOS`,
                                    b = `2659 - ETILBENCENO (GC/MS)`,
                                    c = `2657 - BENCENO (GC/MS)`,
                                    a = `2509 - FENOLES`,
                                    e = `2578 - FENANTRENO`
  ) %>% filter(a < 6.5)
  
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel =0.7,
                       C.perc = "balance")
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  
  train_set <- over[index,] %>% drop_na()
  test_set <- datos_predicc[-index,]
  
  
  fit<- ranger(a ~., data = train_set, mtry = 2, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  data <- with(test_set, tibble(
    c = benc_a(),
    b = etbenc_a(),
    d = deter_a(),
    e = fen_a()
    
    
    
  ))
  
  
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  
  
  
   })
  # ######################### PREDICCIÓN GRASAS Y ACEITES ###############################
   predicted_agua_grasas <- eventReactive(input$pred, {
     req(dro_a(), gro_a(), xil_a(), grasas_a())
    set.seed(5)
  
  datos_predicc <- datos_ag() %>% select(`DRO + GRO`,
                                    `2616 - GRASAS Y ACEITES`,
                                    `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                    `2658 - TOLUENO (GC/MS)`,
                                    `2659 - ETILBENCENO (GC/MS)`,
                                    `2657 - BENCENO (GC/MS)`,
                                    
                                    
                                    
  ) %>% drop_na() %>% dplyr::rename(b = `DRO + GRO`,
                                    c = `SXIL(MS) - SUMA DE XILENOS (o,m,p)`,
                                    a = `2616 - GRASAS Y ACEITES`,
                                    d = `2658 - TOLUENO (GC/MS)`,
                                    e = `2659 - ETILBENCENO (GC/MS)`,
                                    f = `2657 - BENCENO (GC/MS)`
                                    
  )
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel =0.05,
                       C.perc = "balance")
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  
  train_set <- over[index,] %>% drop_na()
  test_set <- datos_predicc[-index,]
  
  
  fit<- ranger(a ~., data = train_set, mtry = 4, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  data <- with(test_set, tibble(
    c = xil_a(),
    b = gro_a() + dro_a(),
    d = tolu_a(),
    e = etbenc_a(),
    f = benc_a()
    
    
    
    
  ))
  
  
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  
  
   })
  
  ################################ PREDICCIÓN SODIO ##########################################
  predicted_agua_sodio <- eventReactive(input$pred, {
    req(sodio_a(), cloruro_a(), condu_a(), potasio_a(), calcio_a())
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% 
      select(`1147 - SODIO`,
             `2506 - CLORURO`,
             `2517 - CONDUCTIVIDAD (a 25°C)`,
             `1149 - CALCIO`,
             `1148 - POTASIO`
             
             
             
      ) %>% drop_na() %>% dplyr::rename(b = `2506 - CLORURO`,
                                        c = `2517 - CONDUCTIVIDAD (a 25°C)`,
                                        a = `1147 - SODIO`,
                                        d = `1149 - CALCIO`,
                                        e = `1148 - POTASIO`
                                        
      ) %>% filter(d>100 & a> 100)
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 0.7,
                         C.perc = "balance")
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    train_set <- over[index,] %>% drop_na() 
    test_set <- datos_predicc[-index,]
    
    
    
    
    
    fit<- ranger(a ~., data = train_set, mtry = 4, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    data <- with(test_set, tibble(
      b = cloruro_a(),
      c = condu_a(),
      d = calcio_a()[1],
      e = potasio_a()[1]

      
      
      
      
    ))
    
    
  
    predicted_data <- predict(fit, data, type = "response")
    
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
  })
  
  #############################PREDICCION POTASIO AGUA #####################
  predicted_agua_potasio <- eventReactive(input$pred, {
    req(sodio_a(), cloruro_a(), condu_a(), potasio_a()) 
  
  set.seed(5)
  
  datos_predicc <- datos_ag() %>% 
    select(`1147 - SODIO`,
           `2506 - CLORURO`,
           `2517 - CONDUCTIVIDAD (a 25°C)`,
           `1148 - POTASIO`
           
           
           
    ) %>% drop_na() %>% dplyr::rename(b = `2506 - CLORURO`,
                                      c = `2517 - CONDUCTIVIDAD (a 25°C)`,
                                      e = `1147 - SODIO`,
                                      a = `1148 - POTASIO`
                                      
    ) %>% filter(e>100)
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel = 0.001,
                       C.perc = "balance")
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  train_set <- over[index,] %>% drop_na() 
  test_set <- datos_predicc[-index,]
  
  
  
  fit<- ranger(a ~., data = train_set, mtry = 3, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  
  data <- with(test_set, tibble(
    b = cloruro_a(),
    c = condu_a(),
    d = calcio_a(),
    e = sodio_a()[1]
    
    
  ))
  
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
  })
  
  ###############################PREDICCIÓN CALCIO AGUA ###############
  predicted_agua_calcio <- eventReactive(input$pred, {
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% 
      select(`1147 - SODIO`,
             `2506 - CLORURO`,
             `2517 - CONDUCTIVIDAD (a 25°C)`,
             `1149 - CALCIO`,
             `1148 - POTASIO`,
             `1150 - MAGNESIO`
             
             
             
             
      ) %>% drop_na() %>% dplyr::rename(b = `2506 - CLORURO`,
                                        c = `2517 - CONDUCTIVIDAD (a 25°C)`,
                                        d = `1147 - SODIO`,
                                        a = `1149 - CALCIO`,
                                        e = `1148 - POTASIO`,
                                        f = `1150 - MAGNESIO`
                                        
                                        
      ) %>% filter(a>100 & d>100)
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 0.01,
                         C.perc = "balance")
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    train_set <- over[index,] %>% drop_na() 
    test_set <- datos_predicc[-index,]
    
    

    fit<- ranger(a ~., data = train_set, mtry = 4, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    data <- with(test_set, tibble(
      b = cloruro_a(),
      c = condu_a(),
      d = sodio_a()[1],
      e = potasio_a()[1],
      f = magnesio_a()[1]
      
    ))
    
    
    predicted_data <- predict(fit, data, type = "response")
    
  
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
    
  })
  
  #############################PREDICCIÓN AGUA MAGNESIO ###############
predicted_agua_magnesio <- eventReactive(input$pred, {
  
  set.seed(5)
  
  datos_predicc <- datos_ag() %>% 
    select(`1147 - SODIO`,
           `2506 - CLORURO`,
           `2517 - CONDUCTIVIDAD (a 25°C)`,
           `1149 - CALCIO`,
           `1148 - POTASIO`,
           `1150 - MAGNESIO`
           
           
           
           
    ) %>% drop_na() %>% dplyr::rename(b = `2506 - CLORURO`,
                                      c = `2517 - CONDUCTIVIDAD (a 25°C)`,
                                      d = `1147 - SODIO`,
                                      f = `1149 - CALCIO`,
                                      e = `1148 - POTASIO`,
                                      a = `1150 - MAGNESIO`
                                      
                                      
    ) %>% filter(f > 100 & d > 100)
  
  over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                       thr.rel = 0.001,
                       C.perc = "balance")
  
  index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
  
  train_set <- over[index,] %>% drop_na() 
  test_set <- datos_predicc[-index,]
  
  
  fit<- ranger(a ~., data = train_set, mtry = 4, min.node.size = 1, num.trees = 1000)
  predicted <- predict(fit, test_set, type = "response")
  
  
  data <- with(test_set, tibble(
    b = cloruro_a(),
    c = condu_a(),
    d = sodio_a()[1],
    e = potasio_a()[1],
    f = calcio_a()[1]
    
  ))
  
  
  predicted_data <- predict(fit, data, type = "response")
  
  
  pre1 <- round(predicted_data$predictions, 2)
  
  err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
  
  tibble(pre1, err)
  
})  
############################PREDICCIÓN AGUA HIERRO #############################
  predicted_agua_hierro <- eventReactive(input$pred, {
    req(estroncio_a(), bario_a(), hierro_a())
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% 
      select(`1279 - ESTRONCIO TOTAL`,
             `1173 - BARIO TOTAL`,
             `1151 - HIERRO TOTAL`
             
             
      ) %>% drop_na() %>% dplyr::rename(
                                        c = `1173 - BARIO TOTAL`,
                                        e = `1279 - ESTRONCIO TOTAL`,
                                        a = `1151 - HIERRO TOTAL`
                                        
                                        
      )
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 0.25,
                         C.perc = "balance")
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    train_set <- over[index,] %>% drop_na() 
    test_set <- datos_predicc[-index,]
    
    
    fit<- ranger(a ~., data = train_set, mtry = 2, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    
    data <- with(test_set, tibble(
      

      c = bario_a()[1],
      e = estroncio_a()[1],
      
      
      
    ))
    
    
    
    
    
    predicted_data <- predict(fit, data, type = "response")
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
    
    
  })
  
  #########################PREDICCIÓN PIRENO AGUA #############################################
  predicted_agua_pireno <- eventReactive(input$pred, {
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% select(`2580 - FLUORANTENO`,
                                      `2577 - FLUORENO`,
                                      `2578 - FENANTRENO`,
                                      `2581 - PIRENO`
                                      
    ) %>% drop_na() %>% dplyr::rename(b = `2580 - FLUORANTENO`,
                                      c = `2577 - FLUORENO`,
                                      d = `2578 - FENANTRENO`,
                                      a = `2581 - PIRENO`
                                      
    ) 
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 0.00001,
                         C.perc = "balance")
    
    
    
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    
    train_set <- over[index,] %>% drop_na()
    test_set <- datos_predicc[-index,]
    
    
    
    fit<- ranger(a ~., data = train_set, mtry = 2, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    
    data <- with(test_set, tibble(
      
      b = fluoranteno_a(),
      c = fluoreno_a(),
      d = fen_a(),
      
    ))
    
    
    
    predicted_data <- predict(fit, data, type = "response")
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
    
    
  })
   #################################PREDICCIÓN FLUORENO AGUA ########################
  predicted_agua_fluoreno <- eventReactive(input$pred, {
    
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% select(`2580 - FLUORANTENO`,
                                      `2577 - FLUORENO`,
                                      `2578 - FENANTRENO`,
                                      `2581 - PIRENO`
                                      
    ) %>% drop_na() %>% dplyr::rename(b = `2580 - FLUORANTENO`,
                                      a = `2577 - FLUORENO`,
                                      d = `2578 - FENANTRENO`,
                                      c = `2581 - PIRENO`
                                      
    ) 
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 0.07,
                         C.perc = "balance")
    
    
    
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    
    train_set <- over[index,] %>% drop_na()
    test_set <- datos_predicc[-index,]
    
    
    
    fit<- ranger(a ~., data = train_set, mtry = 3, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    
    data <- with(test_set, tibble(
      
      b = fluoranteno_a(),
      c = pireno_a(),
      d = fen_a(),
      
    ))
    
    
    
    predicted_data <- predict(fit, data, type = "response")
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
    
    
  })
  
  #############################PREDICCIÓN FLUORANTENO AGUA ###############################
  predicted_agua_fluoranteno <- eventReactive(input$pred, {
    set.seed(5)
    
    datos_predicc <- datos_ag() %>% select(`2580 - FLUORANTENO`,
                                           `2577 - FLUORENO`,
                                           `2578 - FENANTRENO`,
                                           `2581 - PIRENO`
                                           
    ) %>% drop_na() %>% dplyr::rename(a = `2580 - FLUORANTENO`,
                                      b = `2577 - FLUORENO`,
                                      d = `2578 - FENANTRENO`,
                                      c = `2581 - PIRENO`
                                      
    ) 
    
    over <- SMOGNRegress(a ~., as.data.frame(datos_predicc),
                         thr.rel = 0.07,
                         C.perc = "balance")
    
    
    
    
    index <- createDataPartition(datos_predicc$a, p = 0.8, times = 1, list = FALSE)
    
    
    train_set <- over[index,] %>% drop_na()
    test_set <- datos_predicc[-index,]
    
    
    
    fit<- ranger(a ~., data = train_set, mtry = 3, min.node.size = 1, num.trees = 1000)
    predicted <- predict(fit, test_set, type = "response")
    
    
    data <- with(test_set, tibble(
      
      b = fluoreno_a(),
      c = pireno_a(),
      d = fen_a(),
      
    ))
    
    
    
    predicted_data <- predict(fit, data, type = "response")
    
    
    pre1 <- round(predicted_data$predictions, 2)
    
    err <- round(Metrics::mdae(test_set$a, predicted$predictions), 2)
    
    tibble(pre1, err)
    
  })
   
  ############################ OUTPUT FENANTRENO #################################  
  
  output$agua_fen <- renderUI({
    c <- ifelse(between(fen_a(), 
                        (predicted_agua_fen()$pre1 - predicted_agua_fen()$err) - (predicted_agua_fen()$pre1 - predicted_agua_fen()$err)*0.5, 
                        (predicted_agua_fen()$pre1 + predicted_agua_fen()$err) + (predicted_agua_fen()$pre1 + predicted_agua_fen()$err)*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    req(fen_a(), input$pred, agua())
      HTML(paste0(c,
        "FENANTRENO", "</br>",
        "Resultado informado: ", fen_a(), " µg/l", "</br>",
        "Resultado predicho: ", predicted_agua_fen()[1], " µg/l ± ", 
        predicted_agua_fen()[2], " µg/l","</b>"
      ))})
      
    
  ############################### OUTPUT ETILBENCENO #################################
  output$agua_etbenc <- renderUI({
    c <- ifelse(between(etbenc_a(), 
                        (predicted_agua_etbenc()$pre1 - predicted_agua_etbenc()$err) - (predicted_agua_etbenc()$pre1 - predicted_agua_etbenc()$err)*0.5, 
                        (predicted_agua_etbenc()$pre1 + predicted_agua_etbenc()$err) + (predicted_agua_etbenc()$pre1 + predicted_agua_etbenc()$err)*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    req(etbenc_a(), input$pred, agua())
    HTML(paste0(c,
      "ETILBENCENO", "</br>",
      "Resultado informado: ", etbenc_a(), " µg/l", "</br>",
      "Resultado predicho: ", predicted_agua_etbenc()[1], " µg/l ± ", 
      predicted_agua_etbenc()[2], " µg/l","</b>"
    ))})
  
  #################### OUTPUT NAFTALENO ######################################
  output$agua_naft <- renderUI({
    c <- ifelse(between(naft_a(), 
                        (predicted_agua_naft()$pre1 - predicted_agua_naft()$err) - (predicted_agua_naft()$pre1 - predicted_agua_naft()$err)*0.5, 
                        (predicted_agua_naft()$pre1 + predicted_agua_naft()$err) + (predicted_agua_naft()$pre1 + predicted_agua_naft()$err)*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    req(naft_a(), input$pred, agua())
    HTML(paste0(c,
      "NAFTALENO", "</br>",
      "Resultado informado: ", naft_a(), " µg/l", "</br>",
      "Resultado predicho: ", predicted_agua_naft()[1], " µg/l ± ", 
      predicted_agua_naft()[2]," µg/l", "</b>"
    ))})
  ################################## OUTPUT GRO ######################################
  
  output$agua_gro <- renderUI({
    c <- ifelse(between(gro_a(), 
                        (predicted_agua_gro()$pre1[1] - predicted_agua_gro()$err[1]) - (predicted_agua_gro()$pre1[1] - predicted_agua_gro()$err[1])*0.5, 
                        (predicted_agua_gro()$pre1[1] + predicted_agua_gro()$err[1]) + (predicted_agua_gro()$pre1[1] + predicted_agua_gro()$err[1])*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    req(gro_a(), input$pred, agua())
    HTML(paste0(c,
      "RANGO ORGÁNICO DE GASOLINA", "</br>",
      "Resultado informado: ", gro_a(), " mg/l", "</br>",
      "Resultado predicho: ", predicted_agua_gro()$pre1[1], " mg/l ± ", 
      predicted_agua_gro()$err[1]," mg/l", "</b>"
    ))
    
  })
  ############################## OUTPUT DRO #######################################
  output$agua_dro <- renderUI({
    c <- ifelse(between(dro_a(), 
                        (predicted_agua_dro()$pre1[1] - predicted_agua_dro()$err[1]) - (predicted_agua_dro()$pre1[1] - predicted_agua_dro()$err[1])*0.5, 
                        (predicted_agua_dro()$pre1[1] + predicted_agua_dro()$err[1]) + (predicted_agua_dro()$pre1[1] + predicted_agua_dro()$err[1])*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
     req(dro_a(), input$pred, agua())
        HTML(paste0(c,
  "RANGO ORGÁNICO DE DIESEL", "</br>",
  "Resultado informado: ", dro_a(), " mg/l", "</br>",
  "Resultado predicho: ", predicted_agua_dro()$pre1[1], " mg/l ± ", 
  predicted_agua_dro()$err[1]," mg/l", "</b>"
  ))
   })
   
  # ################################ OUTPUT BENCENO ##############################################
   output$agua_benc <- renderUI({
     c <- ifelse(between(benc_a(), 
                         (predicted_agua_benc()$pre1 - predicted_agua_benc()$err) - (predicted_agua_benc()$pre1 - predicted_agua_benc()$err)*0.5, 
                         (predicted_agua_benc()$pre1 + predicted_agua_benc()$err) + (predicted_agua_benc()$pre1 + predicted_agua_benc()$err)*0.5),
                 "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    req(benc_a(), input$pred, agua())
      HTML(paste0(c,
  "BENCENO", "</br>",
  "Resultado informado: ", benc_a(), " µg/l", "</br>",
  "Resultado predicho: ", predicted_agua_benc()[1], " µg/l ± ", 
  predicted_agua_benc()[2]," µg/l", "</b>"
  ))})
  # ################################# OUTPUT TOLUENO ##########################################
   output$agua_tolu <- renderUI({
     c <- ifelse(between(tolu_a(), 
                         (predicted_agua_tolu()$pre1 - predicted_agua_tolu()$err) - (predicted_agua_tolu()$pre1 - predicted_agua_tolu()$err)*0.5, 
                         (predicted_agua_tolu()$pre1 + predicted_agua_tolu()$err) + (predicted_agua_tolu()$pre1 + predicted_agua_tolu()$err)*0.5),
                 "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    req(tolu_a(), input$pred, agua())
     HTML(paste0(c,
       "TOLUENO", "</br>",
  "Resultado informado: ", tolu_a(), " µg/l", "</br>",
  "Resultado predicho: ", predicted_agua_tolu()[1], " µg/l ± ", 
  predicted_agua_tolu()[2]," µg/l", "</b>"
   ))})
   
  # ################################## OUTPUT SUMA DE XILENOS ######################################
   output$agua_xil <- renderUI({
     req(xil_a(), input$pred, agua())
     c <- ifelse(between(xil_a(), 
                         (predicted_agua_xil()$pre1 - predicted_agua_xil()$err) - (predicted_agua_xil()$pre1 - predicted_agua_xil()$err)*0.5, 
                         (predicted_agua_xil()$pre1 + predicted_agua_xil()$err) + (predicted_agua_xil()$pre1 + predicted_agua_xil()$err)*0.5),
                 "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
  "SUMA DE XILENOS", "</br>",
  "Resultado informado: ", xil_a(), " µg/l", "</br>",
  "Resultado predicho: ", predicted_agua_xil()[1], " µg/l ± ", 
  predicted_agua_xil()[2]," µg/l", "</b>"
  ))})
  # ################################ OUTPUT DETERGENTE #############################################
   output$agua_deter <- renderUI({
     req(deter_a(), input$pred, agua())
  c <- ifelse(between(deter_a(), 
                      (predicted_agua_deter()$pre1 - predicted_agua_deter()$err) - (predicted_agua_deter()$pre1 - predicted_agua_deter()$err)*0.5, 
                      (predicted_agua_deter()$pre1 + predicted_agua_deter()$err) + (predicted_agua_deter()$pre1 + predicted_agua_deter()$err)*0.5),
              "<b style= 'color: green;'>", "<b style= 'color: red;'>")
  HTML(paste0(c,
              "DETERGENTES", "</br>",
              "Resultado informado: ", deter_a(), " mg/l", "</br>",
              "Resultado predicho: ", predicted_agua_deter()[1], " mg/l ± ", 
              predicted_agua_deter()[2]," mg/l", "</b>"))
   })
  # ################################## OUTPUT FENOLES ###############################################
   output$agua_fenoles <- renderUI({
     req(fenoles_a(), input$pred, agua())
          c <- ifelse(between(fenoles_a(), 
  (predicted_agua_fenoles()$pre1 - predicted_agua_fenoles()$err) - (predicted_agua_fenoles()$pre1 - predicted_agua_fenoles()$err)*0.5, 
  (predicted_agua_fenoles()$pre1 + predicted_agua_fenoles()$err) + (predicted_agua_fenoles()$pre1 + predicted_agua_fenoles()$err)*0.5),
"<b style= 'color: green;'>", "<b style= 'color: red;'>")
HTML(paste0(c,
            "FENOLES", "</br>",
            "Resultado informado: ", fenoles_a(), " mg/l", "</br>",
            "Resultado predicho: ", predicted_agua_fenoles()[1], " mg/l ± ", 
            predicted_agua_fenoles()[2]," mg/l", "</b>"
))})
  
  # ################################## OUTPUT GRASAS Y ACEITES ######################################
   output$agua_grasas <- renderUI({
     req(grasas_a(), input$pred, agua())
     c <- ifelse(between(grasas_a(), 
                         (predicted_agua_grasas()$pre1 - predicted_agua_grasas()$err) - (predicted_agua_grasas()$pre1 - predicted_agua_grasas()$err)*0.5, 
                         (predicted_agua_grasas()$pre1 + predicted_agua_grasas()$err) + (predicted_agua_grasas()$pre1 + predicted_agua_grasas()$err)*0.5),
                 "<b style= 'color: green;'>", "<b style= 'color: red;'>")
     HTML(paste0(c,
                 "GRASAS Y ACEITES", "</br>",
                 "Resultado informado: ", grasas_a(), " mg/l", "</br>",
                 "Resultado predicho: ", predicted_agua_grasas()$pre1, " mg/l ± ", 
                 predicted_agua_grasas()$err," mg/l", "</b>"))
     
   })
  ################################################## OUTPUT SODIO AGUA #################################
  
  output$agua_sodio <- renderUI({
    req(sodio_a(), input$pred, agua())
    c <- ifelse(between(sodio_a()[1], 
                        (predicted_agua_sodio()$pre1[1] - predicted_agua_sodio()$err[1]) - (predicted_agua_sodio()$pre1[1] - predicted_agua_sodio()$err[1])*0.5, 
                        (predicted_agua_sodio()$pre1[1] + predicted_agua_sodio()$err[1]) + (predicted_agua_sodio()$pre1[1] + predicted_agua_sodio()$err[1])*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "SODIO", "</br>",
                "Resultado informado: ", sodio_a()[1], " mg/l", "</br>",
                "Resultado predicho: ", predicted_agua_sodio()$pre1[1], " mg/l ± ", 
                predicted_agua_sodio()$err[1]," mg/l", "</b>"))
    
  })
  
  ############################### OUTPUT POTASIO AGUA ##################
  output$agua_potasio <- renderUI({
    req(potasio_a(), input$pred, agua())
    c <- ifelse(between(potasio_a()[1], 
                        (predicted_agua_potasio()$pre1[1] - predicted_agua_potasio()$err[1]) - (predicted_agua_potasio()$pre1[1] - predicted_agua_potasio()$err[1])*0.5, 
                        (predicted_agua_potasio()$pre1[1] + predicted_agua_potasio()$err[1]) + (predicted_agua_potasio()$pre1[1] + predicted_agua_potasio()$err[1])*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "POTASIO", "</br>",
                "Resultado informado: ", potasio_a()[1], " mg/l", "</br>",
                "Resultado predicho: ", predicted_agua_potasio()$pre1[1], " mg/l ± ", 
                predicted_agua_potasio()$err[1]," mg/l", "</b>"))
    
  })
  ########################## OUTPUT CALCIO AGUA #####################
  output$agua_calcio <- renderUI({
    req(calcio_a(), input$pred, agua())
    c <- ifelse(between(calcio_a()[1], 
                        (predicted_agua_calcio()$pre1[1] - predicted_agua_calcio()$err[1]) - (predicted_agua_calcio()$pre1[1] - predicted_agua_calcio()$err[1])*0.5, 
                        (predicted_agua_calcio()$pre1[1] + predicted_agua_calcio()$err[1]) + (predicted_agua_calcio()$pre1[1] + predicted_agua_calcio()$err[1])*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "CALCIO", "</br>",
                "Resultado informado: ", calcio_a()[1], " mg/l", "</br>",
                "Resultado predicho: ", predicted_agua_calcio()$pre1[1], " mg/l ± ", 
                predicted_agua_calcio()$err[1]," mg/l", "</b>"))
    
  })

  ##################OUTPUT MAGNESIO AGUA #######################
  output$agua_magnesio <- renderUI({
    req(magnesio_a(), input$pred, agua())
    c <- ifelse(between(magnesio_a()[1], 
                        (predicted_agua_magnesio()$pre1[1] - predicted_agua_magnesio()$err[1]) - (predicted_agua_magnesio()$pre1[1] - predicted_agua_magnesio()$err[1])*0.5, 
                        (predicted_agua_magnesio()$pre1[1] + predicted_agua_magnesio()$err[1]) + (predicted_agua_magnesio()$pre1[1] + predicted_agua_magnesio()$err[1])*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "MAGNESIO", "</br>",
                "Resultado informado: ", magnesio_a()[1], " mg/l", "</br>",
                "Resultado predicho: ", predicted_agua_magnesio()$pre1[1], " mg/l ± ", 
                predicted_agua_magnesio()$err[1]," mg/l", "</b>"))
    
  })
  ###################################OUTPUT HIERRO ###############################
  output$agua_hierro <- renderUI({
    req(hierro_a(), input$pred, agua())
    c <- ifelse(between(hierro_a()[1], 
                        (predicted_agua_hierro()$pre1[1] - predicted_agua_hierro()$err[1]) - (predicted_agua_hierro()$pre1[1] - predicted_agua_hierro()$err[1])*0.5, 
                        (predicted_agua_hierro()$pre1[1] + predicted_agua_hierro()$err[1]) + (predicted_agua_hierro()$pre1[1] + predicted_agua_hierro()$err[1])*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "HIERRO", "</br>",
                "Resultado informado: ", hierro_a()[1], " µg/l", "</br>",
                "Resultado predicho: ", predicted_agua_hierro()$pre1[1], " µg/l ± ", 
                predicted_agua_hierro()$err[1]," µg/l", "</b>"))
    
  })
  ############################# OUTPUT PIRENO ##################################################
  output$agua_pireno <- renderUI({
    req(pireno_a(), input$pred, agua())
    c <- ifelse(between(pireno_a(), 
                        (predicted_agua_pireno()$pre1 - predicted_agua_pireno()$err) - (predicted_agua_pireno()$pre1 - predicted_agua_pireno()$err)*0.5, 
                        (predicted_agua_pireno()$pre1 + predicted_agua_pireno()$err) + (predicted_agua_pireno()$pre1 + predicted_agua_pireno()$err)*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "PIRENO", "</br>",
                "Resultado informado: ", pireno_a(), " µg/l", "</br>",
                "Resultado predicho: ", predicted_agua_pireno()$pre1, " µg/l ± ", 
                predicted_agua_pireno()$err," µg/l", "</b>"))
    
  })
  ######################################### OUTPUT FLUORENO AGUA ###############################
  output$agua_fluoreno <- renderUI({
    req(fluoreno_a(), input$pred, agua())
    c <- ifelse(between(fluoreno_a(), 
                        (predicted_agua_fluoreno()$pre1 - predicted_agua_fluoreno()$err) - (predicted_agua_fluoreno()$pre1 - predicted_agua_fluoreno()$err)*0.5, 
                        (predicted_agua_fluoreno()$pre1 + predicted_agua_fluoreno()$err) + (predicted_agua_fluoreno()$pre1 + predicted_agua_fluoreno()$err)*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "FLUORENO", "</br>",
                "Resultado informado: ", fluoreno_a(), " µg/l", "</br>",
                "Resultado predicho: ", predicted_agua_fluoreno()$pre1, " µg/l ± ", 
                predicted_agua_fluoreno()$err," µg/l", "</b>"))
    
  })
  
#################################OUTPUT FLUORANTENO AGUA ######################################
  output$agua_fluoranteno<- renderUI({
    req(fluoranteno_a(), input$pred, agua())
    c <- ifelse(between(fluoranteno_a(), 
                        (predicted_agua_fluoranteno()$pre1 - predicted_agua_fluoranteno()$err) - (predicted_agua_fluoranteno()$pre1 - predicted_agua_fluoranteno()$err)*0.5, 
                        (predicted_agua_fluoranteno()$pre1 + predicted_agua_fluoranteno()$err) + (predicted_agua_fluoranteno()$pre1 + predicted_agua_fluoranteno()$err)*0.5),
                "<b style= 'color: green;'>", "<b style= 'color: red;'>")
    HTML(paste0(c,
                "FLUORANTENO", "</br>",
                "Resultado informado: ", fluoranteno_a(), " µg/l", "</br>",
                "Resultado predicho: ", predicted_agua_fluoranteno()$pre1, " µg/l ± ", 
                predicted_agua_fluoranteno()$err," µg/l", "</b>"))
    
  })
  ####################################### FEN - NAF #########################
  output$fen_naf <- renderPlotly({
      p<-  ggplot(datos_ag(), aes(.data[["2574 - NAFTALENO"]],  .data[["2578 - FENANTRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
        geom_point() + geom_hline(yintercept = c(fen_a(), predicted_agua_fen()$pre1), color = c("red", "green"))+
        geom_vline(xintercept = c(naft_a(), predicted_agua_naft()$pre1), color = c("red", "green")) + 
        scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + ggtitle("Fenantreno - Naftaleno")+
        theme(title = element_text(face = "bold"))
      ggplotly(p, height = 500)
      
    } )
  #################################FEN - DRO + GRO #####################################
  output$fen_dro_gro <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["DRO + GRO"]],  .data[["2578 - FENANTRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + geom_hline(yintercept = c(fen_a(), predicted_agua_fen()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(dro_a()+gro_a(), predicted_agua_dro()$pre1+predicted_agua_gro()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + ggtitle("Fenantreno - DRO+GRO")+
      theme(title = element_text(face = "bold"))
    ggplotly(p, height = 500)
    
  } )
  
  
  ################################### FEN - ETBENC #################################3
  output$fen_etbenc <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2659 - ETILBENCENO (GC/MS)"]],  .data[["2578 - FENANTRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + geom_hline(yintercept = c(fen_a(), predicted_agua_fen()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(etbenc_a(), predicted_agua_etbenc()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + ggtitle("Fenantreno - Etilbenceno")+
      theme(title = element_text(face = "bold"))
    ggplotly(p, height = 500)
    
  } )
  ################################ FEN - FENOLES #################################
  output$fen_fenoles <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2509 - FENOLES"]],  .data[["2578 - FENANTRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + geom_hline(yintercept = c(fen_a(), predicted_agua_fen()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fenoles_a(), predicted_agua_fenoles()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + ggtitle("Fenantreno - Fenoles")+
      theme(title = element_text(face = "bold"))
      
    ggplotly(p, height = 500)
    
  } )
  ############################## FEN - FLUORENO ##############################
  output$fen_fluoreno <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2577 - FLUORENO"]],  .data[["2578 - FENANTRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(fen_a(), predicted_agua_fen()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fluoreno_a(), predicted_agua_fluoreno()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Fenantreno - Fluoreno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  ################################### FEN - PIRENO ###################################
  output$fen_pireno <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2581 - PIRENO"]],  .data[["2578 - FENANTRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(fen_a(), predicted_agua_fen()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(pireno_a(), predicted_agua_pireno()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Fenantreno - Pireno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  #####################################NAFT - DRO + GRO #################################
  
  output$naft_dro_gro <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["DRO + GRO"]],  .data[["2574 - NAFTALENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + geom_hline(yintercept = c(naft_a(), predicted_agua_naft()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(dro_a()+gro_a(), predicted_agua_dro()$pre1+predicted_agua_gro()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + ggtitle("Naftaleno - DRO+GRO")+
      theme(title = element_text(face = "bold"))
    ggplotly(p, height = 500)
    
  } )
  
  ########################################## NAFT - PIR ####################################
  output$naft_pir <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2581 - PIRENO"]],  .data[["2574 - NAFTALENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(naft_a(), predicted_agua_naft()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(pireno_a(), predicted_agua_pireno()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Naftaleno - Pireno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  ######################################## NAFT - FLUORENO ##############################
  output$naft_fluoreno <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2577 - FLUORENO"]],  .data[["2574 - NAFTALENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(naft_a(), predicted_agua_naft()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fluoreno_a(), predicted_agua_fluoreno()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Naftaleno - Fluoreno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  ################################## PIRENO - FLUORANTENO #################################
  output$pir_fluoranteno <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2580 - FLUORANTENO"]],  .data[["2581 - PIRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(pireno_a(), predicted_agua_pireno()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fluoranteno_a(), predicted_agua_fluoranteno()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Pireno - Fluoranteno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  ##################################### PIRENO - FLUORENO ###########################
  output$pir_fluoreno <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2577 - FLUORENO"]],  .data[["2581 - PIRENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(pireno_a(), predicted_agua_pireno()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fluoreno_a(), predicted_agua_fluoreno()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Pireno - Fluoreno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  ################## FLUORENO - FLUORANTENO ##############################
  output$fluoreno_fluoranteno <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2580 - FLUORANTENO"]],  .data[["2577 - FLUORENO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(fluoreno_a(), predicted_agua_fluoreno()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fluoranteno_a(), predicted_agua_fluoranteno()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Fluoreno - Fluoranteno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ######################### ETBENC - TOLUENO #################################
  output$etbenc_tolu <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2658 - TOLUENO (GC/MS)"]],  .data[["2659 - ETILBENCENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(etbenc_a(), predicted_agua_etbenc()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(tolu_a(), predicted_agua_tolu()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Etilbenceno - Tolueno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################### ETBENC - SXIL ##################################
  output$etbenc_xil <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["SXIL(MS) - SUMA DE XILENOS (o,m,p)"]],  .data[["2659 - ETILBENCENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(etbenc_a(), predicted_agua_etbenc()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(xil_a(), predicted_agua_xil()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Etilbenceno - Suma de xilenos")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################### ETBENC - BENC ###############################
  output$etbenc_benc <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2657 - BENCENO (GC/MS)"]],  .data[["2659 - ETILBENCENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(etbenc_a(), predicted_agua_etbenc()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(benc_a(), predicted_agua_benc()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Etilbenceno - Benceno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  ################################### ETBENC - DRO + GRO ########################
  output$etbenc_dro_gro <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["DRO + GRO"]],  .data[["2659 - ETILBENCENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(etbenc_a(), predicted_agua_etbenc()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(dro_a()+gro_a(), predicted_agua_dro()$pre1 + predicted_agua_gro()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Etilbenceno - DRO + GRO")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################# TOLUENO - SXIL ###########################
  output$tolu_xil <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["SXIL(MS) - SUMA DE XILENOS (o,m,p)"]],  .data[["2658 - TOLUENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(tolu_a(), predicted_agua_tolu()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(xil_a(), predicted_agua_xil()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Tolueno - Suma de xilenos")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################## TOLUENO - BENCENO ############################
  
  output$tolu_benc <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2657 - BENCENO (GC/MS)"]],  .data[["2658 - TOLUENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(tolu_a(), predicted_agua_tolu()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(benc_a(), predicted_agua_benc()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Tolueno - Benceno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ##################################### TOLUENO - FENOLES ###########################
  
  output$tolu_fenoles <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2509 - FENOLES"]],  .data[["2658 - TOLUENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(tolu_a(), predicted_agua_tolu()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fenoles_a(), predicted_agua_fenoles()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Tolueno - Fenoles")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ########################## BENCENO - FENOLES ####################################
  
  output$benc_fenoles <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2509 - FENOLES"]],  .data[["2657 - BENCENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(benc_a(), predicted_agua_benc()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(fenoles_a(), predicted_agua_fenoles()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Benceno - Fenoles")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################ BENCENO - SUMA DE XILENOS ##############################
  
  output$benc_xil <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["SXIL(MS) - SUMA DE XILENOS (o,m,p)"]],  .data[["2657 - BENCENO (GC/MS)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(benc_a(), predicted_agua_benc()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(xil_a(), predicted_agua_xil()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Benceno - Suma de xilenos")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################### GRO - SXIL ##################################
  output$gro_xil <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["SXIL(MS) - SUMA DE XILENOS (o,m,p)"]],  .data[["2591 - RANGO ORGANICO DE GASOLINA (GRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(gro_a(), predicted_agua_gro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(xil_a(), predicted_agua_xil()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("GRO - Suma de xilenos")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################### GRO - TOLUENO ###########################
  output$gro_tolu <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2658 - TOLUENO (GC/MS)"]],  .data[["2591 - RANGO ORGANICO DE GASOLINA (GRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(gro_a(), predicted_agua_gro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(tolu_a(), predicted_agua_tolu()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("GRO - Tolueno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ###################################### GRO - BENCENO ##################################
  output$gro_benc <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2657 - BENCENO (GC/MS)"]],  .data[["2591 - RANGO ORGANICO DE GASOLINA (GRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(gro_a(), predicted_agua_gro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(benc_a(), predicted_agua_benc()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("GRO - Benceno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ######################################## GRO - DETERGENTES ################################
  output$gro_deter <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2519 - DETERGENTES ANIÓNICOS"]],  .data[["2591 - RANGO ORGANICO DE GASOLINA (GRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(gro_a(), predicted_agua_gro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(deter_a(), predicted_agua_deter()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("GRO - Detergentes")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################ GRO - DRO ################################
  output$gro_dro <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2590 - RANGO ORGANICO DE DIESEL (DRO)"]],  .data[["2591 - RANGO ORGANICO DE GASOLINA (GRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(gro_a(), predicted_agua_gro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(dro_a(), predicted_agua_dro()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("GRO - DRO")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  ##################################### DRO - SXIL ###################################
  output$dro_xil <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["SXIL(MS) - SUMA DE XILENOS (o,m,p)"]],  .data[["2590 - RANGO ORGANICO DE DIESEL (DRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(dro_a(), predicted_agua_dro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(xil_a(), predicted_agua_xil()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("DRO - Suma de xilenos")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################### DRO - TOLUENO #################################
  output$dro_tolu <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2658 - TOLUENO (GC/MS)"]],  .data[["2590 - RANGO ORGANICO DE DIESEL (DRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(dro_a(), predicted_agua_dro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(tolu_a(), predicted_agua_tolu()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("DRO - Tolueno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################## DRO - BENCENO #################################
  output$dro_benc <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2657 - BENCENO (GC/MS)"]],  .data[["2590 - RANGO ORGANICO DE DIESEL (DRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(dro_a(), predicted_agua_dro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(benc_a(), predicted_agua_benc()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("DRO - Benceno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ########################### DRO - DETERGENTES ##########################################
  output$dro_deter <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2519 - DETERGENTES ANIÓNICOS"]],  .data[["2590 - RANGO ORGANICO DE DIESEL (DRO)"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(dro_a(), predicted_agua_dro()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(deter_a(), predicted_agua_deter()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("DRO - Detergentes")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  

  
########################### DETERGENTES - CONDUCTIVIDAD #################################
  output$deter_condu <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2517 - CONDUCTIVIDAD (a 25°C)"]],  .data[["2519 - DETERGENTES ANIÓNICOS"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(deter_a(), predicted_agua_deter()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(condu_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Detergentes - Conductividad")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################### DETERGENTES - PH ################################
  output$deter_ph <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2500 - pH (25°C)"]],  .data[["2519 - DETERGENTES ANIÓNICOS"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(deter_a(), predicted_agua_deter()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(ph_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Detergentes - pH")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
####################################### FENOLES - ETILBENCENO ##############################
  
  output$fenoles_etbenc <- renderPlotly({
    p<-  ggplot(datos_ag(), aes(.data[["2659 - ETILBENCENO (GC/MS)"]],  .data[["2509 - FENOLES"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(fenoles_a(), predicted_agua_fenoles()$pre1), color = c("red", "green"))+
      geom_vline(xintercept = c(etbenc_a(), predicted_agua_etbenc()$pre1), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Fenoles - Etilbenceno")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################## SODIO - CLORURO ####################################
  
  output$sodio_cloruro <- renderPlotly({
    req(sodio_a(), cloruro_a())
    p<-  ggplot(datos_ag(), aes(.data[["2506 - CLORURO"]],  .data[["1147 - SODIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(sodio_a()[1], predicted_agua_sodio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(cloruro_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Sodio - Cloruro")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ################################# SODIO - CONDU ############################
  output$sodio_condu <- renderPlotly({
    req(sodio_a(), condu_a())
    p<-  ggplot(datos_ag(), aes(.data[["2517 - CONDUCTIVIDAD (a 25°C)"]],  .data[["1147 - SODIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(sodio_a()[1], predicted_agua_sodio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(condu_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Sodio - Conductividad")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################## SODIO - CALCIO #####################################
  
  output$sodio_calcio <- renderPlotly({
    req(sodio_a(), calcio_a())
    p<-  ggplot(datos_ag(), aes(.data[["1149 - CALCIO"]],  .data[["1147 - SODIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(sodio_a()[1], predicted_agua_sodio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(calcio_a()[1], predicted_agua_calcio()$pre1[1]), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Sodio - Calcio")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################### SODIO - POTASIO ################################
  output$sodio_potasio <- renderPlotly({
    req(sodio_a(), potasio_a())
    p<-  ggplot(datos_ag(), aes(.data[["1148 - POTASIO"]],  .data[["1147 - SODIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(sodio_a()[1], predicted_agua_sodio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(potasio_a()[1], predicted_agua_potasio()$pre1[1]), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Sodio - Potasio")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ########################### POTASIO - CLORURO ################################
  output$potasio_cloruro <- renderPlotly({
    req(potasio_a(), cloruro_a())
    p<-  ggplot(datos_ag(), aes(.data[["2506 - CLORURO"]],  .data[["1148 - POTASIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(potasio_a()[1], predicted_agua_potasio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(cloruro_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Potasio - Cloruro")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################### POTASIO - CONDU ##############################
  
  output$potasio_condu <- renderPlotly({
    req(potasio_a(), condu_a())
    p<-  ggplot(datos_ag(), aes(.data[["2517 - CONDUCTIVIDAD (a 25°C)"]],  .data[["1148 - POTASIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(potasio_a()[1], predicted_agua_potasio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(condu_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Potasio - Conductividad")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ########################### CALCIO - CLORURO ##############################
  output$calcio_cloruro <- renderPlotly({
    req(calcio_a(), cloruro_a())
    p<-  ggplot(datos_ag(), aes(.data[["2506 - CLORURO"]],  .data[["1149 - CALCIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(calcio_a()[1], predicted_agua_calcio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(cloruro_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Calcio - Cloruro")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ###################################### CALCIO - CONDU #################################
  output$calcio_condu <- renderPlotly({
    req(calcio_a(), condu_a())
    p<-  ggplot(datos_ag(), aes(.data[["2517 - CONDUCTIVIDAD (a 25°C)"]],  .data[["1149 - CALCIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(calcio_a()[1], predicted_agua_calcio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(condu_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Calcio - Conductividad")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ########################### CALCIO - POTASIO #################################
  output$calcio_potasio <- renderPlotly({
    req(calcio_a(), potasio_a())
    p<-  ggplot(datos_ag(), aes(.data[["1148 - POTASIO"]],  .data[["1149 - CALCIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(calcio_a()[1], predicted_agua_calcio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(potasio_a()[1], predicted_agua_potasio()$pre1[1]), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Calcio - Potasio")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  #################################### CALCIO - MAGNESIO ##############################
  output$calcio_magnesio <- renderPlotly({
    req(calcio_a(), magnesio_a())
    p<-  ggplot(datos_ag(), aes(.data[["1150 - MAGNESIO"]],  .data[["1149 - CALCIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(calcio_a()[1], predicted_agua_calcio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(magnesio_a()[1], predicted_agua_magnesio()$pre1[1]), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Calcio - Magnesio")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
  
  } )
  
  ########################## MAGNESIO - SODIO ######################################
  output$magnesio_sodio <- renderPlotly({
    req(magnesio_a(), sodio_a())
    p<-  ggplot(datos_ag(), aes(.data[["1147 - SODIO"]],  .data[["1150 - MAGNESIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(magnesio_a()[1], predicted_agua_magnesio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(sodio_a()[1], predicted_agua_sodio()$pre1[1]), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Magnesio - Sodio")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################# MAGNESIO - CLORURO #####################################
  output$magnesio_cloruro <- renderPlotly({
    req(cloruro_a(), magnesio_a())
    p<-  ggplot(datos_ag(), aes(.data[["2506 - CLORURO"]],  .data[["1150 - MAGNESIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(magnesio_a()[1], predicted_agua_magnesio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(cloruro_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Magnesio - Cloruro")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  
  ############################# MAGNESIO - CONDU ######################################
  output$magnesio_condu <- renderPlotly({
    req(condu_a(), magnesio_a())
    p<-  ggplot(datos_ag(), aes(.data[["2517 - CONDUCTIVIDAD (a 25°C)"]],  .data[["1150 - MAGNESIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(magnesio_a()[1], predicted_agua_magnesio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(condu_a()), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Magnesio - Conductividad")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################## MAGNESIO - POTASIO ####################################
  
  output$magnesio_potasio <- renderPlotly({
    req(magnesio_a(), bario_a())
    p<-  ggplot(datos_ag(), aes(.data[["1148 - POTASIO"]],  .data[["1150 - MAGNESIO"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(magnesio_a()[1], predicted_agua_magnesio()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(potasio_a()[1], predicted_agua_potasio()$pre1[1]), color = c("red", "green")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Magnesio - Potasio")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ############################# HIERRO - BARIO ################################
  output$hierro_bario <- renderPlotly({
    req(hierro_a(), bario_a())
    p<-  ggplot(datos_ag(), aes(.data[["1173 - BARIO TOTAL"]],  .data[["1151 - HIERRO TOTAL"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(hierro_a()[1], predicted_agua_hierro()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(bario_a()[1]), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Hierro - Bario")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  ###################################### HIERRO - ESTRONCIO ##########################
  
  output$hierro_estroncio <- renderPlotly({
    req(hierro_a(), estroncio_a())
    p<-  ggplot(datos_ag(), aes(.data[["1279 - ESTRONCIO TOTAL"]],  .data[["1151 - HIERRO TOTAL"]], text = paste0("Fracción: ", Fracción, "</br>", "Rótulo: ",Rótulo)))+ 
      geom_point() + 
      geom_hline(yintercept = c(hierro_a()[1], predicted_agua_hierro()$pre1[1]), color = c("red", "green"))+
      geom_vline(xintercept = c(estroncio_a()[1]), color = c("red")) + 
      scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + 
      ggtitle("Hierro - Estroncio")+
      theme(title = element_text(face = "bold"))
    
    ggplotly(p, height = 500)
    
  } )
  
  outputOptions(output, "suelo", suspendWhenHidden = FALSE)
  outputOptions(output, "agua", suspendWhenHidden = FALSE)
}
shinyApp(ui = ui, server = server)
