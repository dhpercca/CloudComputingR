#Solo ejecutar una vez. NO EJECUTAR SI SE ABRE DE CLOUD, YA ESTA INSTALADA LA LIBRERIA.
install.packages("shiny")

#Si solo se desea probar el avance ejecutar la siguiente linea.
shinyApp(ui = ui, server = server)

#Si se desea modificar el codigo, limpiar las variables de entorno y ejecutar las siguientes lineas.
library(shiny)

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
                label = "Ingrese la direccion del Dataset:")),
      
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Donde mostraremos el resumen del Dataset.
      verbatimTextOutput("summary"),
      
      # Output: Tabla Html donde se mostrara datos del Dataset.
      tableOutput("view")
      
    )
  )
)

#Solo ejecutar las siguientes lineas si se realiza cambios al código.
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  datasetInput <- reactive({
    if(input$dataset=="Dataset en Excel"){
      read_excel(input$ubicacion) 
    }
    else{
      read.csv(input$ubicacion)
    }
  })
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Mostrar los primeros 'n' datos.
  output$view <- renderTable({
    head(datasetInput(), n = 10)
  })
}

# Ejecutar esta linea si se desea probar la interfaz actual.
shinyApp(ui = ui, server = server)

#Elementos por implementar: 
#Boton para lectura de ubicación.
#Verificar si se puede o no realizar la regresión lineal y mostrar en un mensaje.
#Grafico de dispersión/regresión linear.