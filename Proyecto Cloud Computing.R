#Solo ejecutar una vez. NO EJECUTAR SI SE ABRE DE CLOUD, YA ESTA INSTALADA LA LIBRERIA.
#install.packages("shiny")#Interfaz en web
#install.packages("dplyr")#Funcion pull
#install.packages("readxl")#Para leer excel
#install.packages('rsconnect') #Para subirlo

#Si solo se desea probar el avance ejecutar la siguiente linea.
shinyApp(ui = ui, server = server)

#Si se desea modificar el codigo, limpiar las variables de entorno y ejecutar las siguientes lineas.
library(shiny)
library(readxl)
library(dplyr)

#Solo ejecutar las siguientes lineas si se realiza cambios al código.
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # Titulo de la app
  titlePanel("Regresion Lineal del curso Cloud Computing"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Para la seleccion del tipo de Dataset.
      selectInput(inputId = "dataset",
                  label = "Seleccione el tipo Dataset:",
                  choices = c("Dataset en Excel", "Direccion web de Dataset")),
      # Input: Entrada de texto de la ubicación o dirección web del Dataset.
      textInput(inputId = "ubicacion",
                label = "Ingrese la direccion del Dataset:",
                value = ""),
      actionButton("Confirmar_Data", "Confirmar Dataset"),
      uiOutput("moreControls"),
      uiOutput("moreControls2"),
      uiOutput("moreControls3")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      tableOutput("view"),
      uiOutput("moreControls4"),
      uiOutput("Descrip_cov"),
      verbatimTextOutput("covarianza"),
      uiOutput("Descrip_cor"),
      verbatimTextOutput("correlacion"),
      uiOutput("moreControls5"),
      uiOutput("moreControls6"),
      verbatimTextOutput("Resumen_modelo"),
      plotOutput("Grafico")
    )
  )
)


# Define server logic to summarize and view selected dataset -----------------------------------------------
server <- function(input, output) {
  
  datasetInput <- eventReactive(input$Confirmar_Data,{
    
    
    print("Test")
    if(input$dataset=="Dataset en Excel"){
      read_excel(input$ubicacion) 
    }
    else{
      read.csv(input$ubicacion)
    }
  }, ignoreNULL = FALSE)
  
  #Boton 1
  observeEvent(input$Confirmar_Data, {
    output$view <- renderTable({
      head(datasetInput(), n = 6)
    })
    output$moreControls <- renderUI({
      
      if(input$ubicacion==""){
        selectInput(inputId = "Variable_1d",
                    label = "Seleccione la variable dependiente:",
                    choices = c("Vacio"))
      }
      else{
        columnas <- colnames(datasetInput())
        selectInput(inputId = "Variable_1d",
                    label = "Seleccione la variable dependiente:",
                    choices = columnas)
      }
    })
    output$moreControls2 <- renderUI({
      if(input$ubicacion==""){
        selectInput(inputId = "Variable_2d",
                    label = "Seleccione la variable independiente:",
                    choices = c("Vacio"))
      }
      else{
        columnas <- colnames(datasetInput())
        selectInput(inputId = "Variable_2d",
                    label = "Seleccione la variable independiente:",
                    choices = columnas)
      }
    })
    output$moreControls3 <- renderUI({
      actionButton("Confirmar_Data2", "Confirmar selección de variables")
    })
    
  })
  
  #Boton 2
  observeEvent(input$Confirmar_Data2, {
    
    if (input$Variable_1d==input$Variable_2d){
      output$moreControls4 <- renderPrint({
        h4("No puede seleccionar la misma variable.")
      })
      output$Descrip_cov <- renderText({})
      output$covarianza <- renderText({})
      output$Descrip_cor <- renderText({})
      output$correlacion <- renderText({})
      output$Resumen_modelo <- renderText({})
      output$Grafico <- renderText({})
      output$moreControls5 <- renderText({})
      output$moreControls6 <- renderText({})
    }
    else{
      output$moreControls4 <- renderText({})
      output$Descrip_cov <- renderPrint({
        h4("La covarianza es:")
      })
      output$covarianza <- renderPrint({
        datasetTemp <- isolate(datasetInput())
        cov(pull(datasetTemp,isolate(input$Variable_1d)),pull(datasetTemp,isolate(input$Variable_2d)))
      })
      output$Descrip_cor <- renderPrint({
        h4("La correlación es:")
      })
      output$correlacion <- renderPrint({
        datasetTemp <- isolate(datasetInput())
        cor(pull(datasetTemp,isolate(input$Variable_1d)),pull(datasetTemp,isolate(input$Variable_2d)))
      })
      datasetTemp <- isolate(datasetInput())
      if (cor(pull(datasetTemp,isolate(input$Variable_1d)),pull(datasetTemp,isolate(input$Variable_2d)))>0.7){
        output$moreControls6 <- renderPrint({
          h4("Resumen del modelo:")
        })
        output$Resumen_modelo <- renderPrint({
          datasetTemp <- isolate(datasetInput())
          modelo = lm(pull(datasetTemp,isolate(input$Variable_1d))~pull(datasetTemp,isolate(input$Variable_2d)))
          summary(modelo)
        })
        output$Grafico <- renderPlot({
          datasetTemp <- isolate(datasetInput())
          modelo = lm(pull(datasetTemp,isolate(input$Variable_2d))~pull(datasetTemp,isolate(input$Variable_1d)))
          plot(pull(datasetTemp,isolate(input$Variable_1d)),pull(datasetTemp,isolate(input$Variable_2d)))
          abline(modelo, col = "red")
        })
        output$moreControls5 <- renderText({})
      }
      else{
        output$Resumen_modelo <- renderText({})
        output$moreControls6 <- renderText({})
        output$Grafico <- renderPlot({
          datasetTemp <- isolate(datasetInput())
          plot(pull(datasetTemp,isolate(input$Variable_1d)),pull(datasetTemp,isolate(input$Variable_2d)))
        })
        output$moreControls5 <- renderPrint({
          h4("No hay suficiente correlación para realizar el modelo de regresión lineal.")
        })
      }
    }
  })
  
}
# Ejecutar esta linea si se desea probar la interfaz actual.
shinyApp(ui = ui, server = server)
