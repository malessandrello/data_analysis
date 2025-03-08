# Load packages



library(shinyWidgets)
library(bslib)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(DT)


matrices <- c("AGUA PROCESO", "AGUA SUPERFICIAL", "AGUA EFLUENTE", "SUELO REMEDIACIÓN", "JCONT LIMÓN", "AGUA SUMINISTRO",
              "DESH TE", "JCONT NARANJA", "JCONC MANZANA", "FF PERA", "FF MANZANA", "FF DURAZNO",
              "FF LIMÓN", "BAYAS UVA", "HORTALIZA TOMATE")

analisis_SA <- read_rds("analisis_SA.rds")
analisis_SR <- read_rds("analisis_SR.rds")
analisis_te <- read_rds("analisis_desh_te.rds")
analisis_aguapro <- read_rds("analisis_agupr.rds")
analisis_eflu <- read_rds("analisis_agua_eflu.rds")
analisis_jcont_limon <- read_rds("analisis_jcont_limon.rds")
analisis_sumin <- read_rds("analisis_agua_sumin.rds")
analisis_jconc_manz <- read_rds("analisis_jconc_manz.rds")
analisis_jcont_naran <- read_rds("analisis_jcont_naran.rds")
analisis_FF_pera <- read_rds("analisis_FF_pera.rds")
analisis_FF_manz <- read_rds("analisis_FF_manz.rds")
analisis_FF_duraz <- read_rds("analisis_FF_duraz.rds")
analisis_FF_lim <- read_rds("analisis_FF_lim.rds")
analisis_bayas_uva <- read_rds("analisis_bayas_uva.rds")
analisis_hort_tomate <- read_rds("analisis_hort_tomate.rds")

correl_agupr <- read_rds("correl_agupr.rds")
correl_agusuper <- read_rds("correl_SA.rds")
correl_sueloremed <- read_rds("correl_SR.rds")
correl_te <- read_rds("correl_te.rds")
correl_eflu <- read_rds("correl_agua_eflu.rds")
correl_sumin <- read_rds("correl_agua_sumin.rds")
correl_jcont_limon <- read_rds("correl_jcont_limon.rds")
correl_jcont_naran <- read_rds("correl_jcont_naran.rds")
correl_jconc_manz <- read_rds("correl_jconc_manz.rds")
correl_FF_pera <- read_rds("correl_FF_pera.rds")
correl_FF_manz <- read_rds("correl_FF_manz.rds")
correl_FF_duraz <- read_rds("correl_FF_duraz.rds")
correl_FF_lim <- read_rds("correl_FF_lim.rds")
correl_bayas_uva <- read_rds("correl_bayas_uva.rds")
correl_hort_tomate <- read_rds("correl_hort_tomate.rds")


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
      background-color: #297a97;
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
    selectizeInput(
      inputId = "matrices",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Seleccione producto y matriz", "</b>")),
      selected = "AGUA PROCESO",
      choices = NULL
    ),
    hr(),
    uiOutput("selec_analisis_x"),
    hr(),
    conditionalPanel(
      condition = "input.nav == 'Dos variables'",
      uiOutput("selec_analisis_y"),
      hr(),
      checkboxInput(
        inputId = "log",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Transformar valores a logaritmo en base 10", "</b>")),
        value = FALSE
      ),
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
    hr(),
    numericInput(
      inputId = "vert",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar una linea vertical en x = ", "</b>")),
      value = 0,
      min = 0
    ),
    numericInput(
      inputId = "vert2",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar una linea vertical en x = ", "</b>")),
      value = 0,
      min = 0
    ),
    hr(),
    conditionalPanel(
      condition = "input.nav == 'Dos variables'",
      numericInput(
        inputId = "hor",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar linea horizontal en y = ", "</b>")),
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "hor2",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar una linea horizontal en y = ", "</b>")),
        value = 0,
        min = 0
      ),
      hr(),
      numericInput(
        inputId = "pendiente",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar recta con pendiente =", "</b>")),
        value = 0
      ),
      numericInput(
        inputId = "ordenada",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "y ordenada al origen =", "</b>")),
        value = 0
      ),
      hr(),
      numericInput(
        inputId = "pendiente2",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Agregar recta con pendiente =", "</b>")),
        value = 0
      ),
      numericInput(
        inputId = "ordenada2",
        label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "y ordenada al origen =", "</b>")),
        value = 0
      )
    ),
    hr(),
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
                    "AGUA PROCESO" = read_rds("agupr.rds"),
                    "AGUA SUPERFICIAL" = read_rds("super_agua.rds"),
                    "AGUA EFLUENTE" = read_rds("agua_eflu.rds"),
                    "SUELO REMEDIACIÓN" = read_rds("suelo_remed.rds"),
                    "JCONT LIMÓN" = read_rds("jcont_limon.rds"),
                    "DESH TE" = read_rds("desh_te.rds"),
                    "AGUA SUMINISTRO" = read_rds("agua_sumin.rds"),
                    "JCONT NARANJA" = read_rds("jcont_naran.rds"),
                    "JCONC MANZANA" = read_rds("jconc_manz.rds"),
                    "FF PERA" = read_rds("FF_pera.rds"),
                    "FF MANZANA" = read_rds("FF_manz.rds"),
                    "FF DURAZNO" = read_rds("FF_duraz.rds"),
                    "FF LIMÓN" = read_rds("FF_lim.rds"),
                    "BAYAS UVA" = read_rds("bayas_uva.rds"),
                    "HORTALIZA TOMATE" = read_rds("hort_tomate.rds")
                    
    )
    
    datos
  })
  analisis <- eventReactive(input$matrices, {
    req(input$matrices)
  switch(input$matrices,
    "AGUA PROCESO" = analisis_aguapro,
    "AGUA SUPERFICIAL" = analisis_SA,
    "SUELO REMEDIACIÓN" = analisis_SR,
    'DESH TE' = analisis_te,
    "AGUA EFLUENTE" = analisis_eflu,
    "JCONT LIMÓN" = analisis_jcont_limon,
    "AGUA SUMINISTRO" = analisis_sumin,
    "JCONT NARANJA" = analisis_jcont_naran,
    "JCONC MANZANA" = analisis_jconc_manz,
    "FF PERA" = analisis_FF_pera,
    "FF MANZANA" = analisis_FF_manz,
    "FF DURAZNO" = analisis_FF_duraz,
    "FF LIMÓN" = analisis_FF_lim,
    "BAYAS UVA" = analisis_bayas_uva,
    "HORTALIZA TOMATE" = analisis_hort_tomate)

  })
  
  correl <- eventReactive(input$matrices, {
    req(input$matrices)
    switch(input$matrices,
           "AGUA PROCESO" = correl_agupr,
           "AGUA SUPERFICIAL" = correl_agusuper,
           "SUELO REMEDIACIÓN" = correl_sueloremed,
           'DESH TE' = correl_te,
           "AGUA EFLUENTE" = correl_eflu,
           "AGUA SUMINISTRO" = correl_sumin,
           "JCONT LIMÓN" = correl_jcont_limon,
           "JCONT NARANJA" = correl_jcont_naran,
           "JCONC MANZANA" = correl_jconc_manz,
           "FF PERA" = correl_FF_pera,
           "FF MANZANA" = correl_FF_manz,
           "FF DURAZNO" = correl_FF_duraz,
           "FF LIMÓN" = correl_FF_lim,
           "BAYAS UVA" = correl_bayas_uva,
           "HORTALIZA TOMATE" = correl_hort_tomate)
  })
  
  updateSelectizeInput(session, "matrices", choices = matrices, server = TRUE )

  
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
    selectizeInput(
      inputId = "x",
      label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Eje X", "</b>")),
      choices = analisis(),
      selected = NULL
    )
  })
  
  output$selec_analisis_y <- renderUI({
    req(analisis())
    selectizeInput(
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
    if (input$ent == FALSE & input$logx == FALSE & input$logy == FALSE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>")))+ #"Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red")+
        geom_hline(yintercept = input$hor, color = "red") + 
        geom_hline(yintercept = input$hor2, color = "red")+ 
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")
      ggplotly(p)
    } else if (input$ent == FALSE & input$logx == TRUE & input$logy == FALSE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>")))+ #"Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red")+ 
      geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")+
        scale_x_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == FALSE & input$logx == TRUE & input$logy == TRUE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>"))) +# "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red") +
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")+
        scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == FALSE & input$logx == FALSE & input$logy == TRUE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], text = paste0("Fracción: ", Fracción, "</br>"))) +# "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red") + 
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")+
        scale_y_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == FALSE & input$logy == FALSE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>"))) + #"Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$ver2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") + 
        geom_hline(yintercept = input$hor2, color = "red")+
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == TRUE & input$logy == FALSE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>")))+# "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red") + 
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")+
        scale_x_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == FALSE & input$logy == TRUE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>")))+# "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") + 
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red") + 
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green") +
        scale_y_continuous(trans = "log10")
      ggplotly(p)
    } else if (input$ent == TRUE & input$logx == TRUE & input$logy == TRUE & input$log == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], .data[[input$y]], color = Entidad, text = paste0("Fracción: ", Fracción, "</br>")))+# "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red") +
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")+
        scale_y_continuous(trans = "log10") +
        scale_x_continuous(trans = "log10")
      ggplotly(p)
    }
    else if (input$ent == TRUE & input$log == TRUE) {
      p <- ggplot(subsetted(), aes(log10(.data[[input$x]]), log10(.data[[input$y]]), color = Entidad, text = paste0("Fracción: ", Fracción, "</br>")))+# "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red") +
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")
  
      ggplotly(p, dynamicTicks = TRUE)
    }
    else if (input$ent == FALSE & input$log == TRUE) {
      p <- ggplot(subsetted(), aes(log10(.data[[input$x]]), log10(.data[[input$y]]), text = paste0("Fracción: ", Fracción, "</br>")))+#, "Rótulo: ", Rótulo))) +
        geom_point() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        geom_hline(yintercept = input$hor, color = "red") +
        geom_hline(yintercept = input$hor2, color = "red") + 
        geom_abline(slope = input$pendiente, intercept = input$ordenada, color = "green")+
        geom_abline(slope = input$pendiente2, intercept = input$ordenada2, color = "green")
        
      ggplotly(p, dynamicTicks = TRUE)
    }
  })
  
  
  output$scatterplot2 <- renderPlotly({
    req(input$matrices)
    if (input$ent == FALSE & input$logx == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], text = paste0("Fracción: ", Fracción, "</br>")))+#, "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        ylab("N° de muestras")
      
      ggplotly(p, tooltip = c("x", "text"))
    } else if (input$ent == FALSE & input$logx == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], text = paste0("Fracción: ", Fracción, "</br>")))+#, "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        scale_x_continuous(trans = "log10") +
        ylab("N° de muestras")
      
      ggplotly(p, tooltip = c("x", "text"))
    } else if (input$ent == TRUE & input$logx == FALSE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], fill = Entidad, text = paste0("Fracción: ", Fracción, "</br>")))+#, "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        ylab("N° de muestras")
      
      ggplotly(p, tooltip = c("x", "Entidad", "text"))
    } else if (input$ent == TRUE & input$logx == TRUE) {
      p <- ggplot(subsetted(), aes(.data[[input$x]], fill = Entidad, text = paste0("Fracción: ", Fracción, "</br>")))+#, "Rótulo: ", Rótulo))) +
        geom_histogram() +
        geom_vline(xintercept = input$vert, color = "red") +
        geom_vline(xintercept = input$vert2, color = "red") +
        scale_x_continuous(trans = "log10") +
        ylab("N° de muestras")
      
      ggplotly(p, tooltip = c("x", "Entidad", "text"))
    } 
    
  })
  output$tablacorr <- renderDT({
    req(input$x, correl())

    
    analisis_correl <- correl() %>%  filter(.data[[input$x]]>0.5 | .data[[input$x]]< (-0.5)) %>% 
      select(Analisis,  all_of(input$x)) %>%
      dplyr::arrange(desc(.data[[input$x]])) 
    
     datatable(analisis_correl)
    
 
  }) 
  output$encabezado <- renderUI({
    HTML(paste0("Correlaciones de Spearman de ", input$x))
  })
}





# Create a Shiny app object

shinyApp(ui = ui, server = server)