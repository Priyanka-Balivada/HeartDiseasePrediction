library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("ModelAda.rds")

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('CardioInsights'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("Age", 
                 label = "Age", 
                 value = 40),
    numericInput("Sex", 
                 label = "Sex", 
                 value = 1),
    numericInput("ChestPainType", 
                 label = "Chest Pain Type", 
                 value = 1),
    numericInput("RestingBP", 
                 label = "Resting Blood Pressure", 
                 value = 140),
    numericInput("Cholesterol", 
                 label = "Cholesterol", 
                 value = 289),
    numericInput("FastingBS", 
                 label = "Fasting Blood Sugar", 
                 value = 0),
    numericInput("RestingECG", 
                 label = "Resting ECG Results", 
                 value = 1),
    numericInput("MaxHR", 
                 label = "Max Heart Rate Achived", 
                 value = 172),
    numericInput("ExerciseAngina", 
                 label = "Exercise Induced Angina", 
                 value = 0),
    numericInput("Oldpeak", 
                 label = "ST Depression Induced by Exercise (Old Peak)", 
                 value = 0),
    numericInput("ST_Slope", 
                 label = "Slope of Peak Exercise ST Segment", 
                 value = 0),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Age",
               "Sex",
               "ChestPainType",
               "RestingBP",
               "Cholesterol",
               "FastingBS",
               "RestingECG",
               "MaxHR",
               "ExerciseAngina",
               "Oldpeak",
               "ST_Slope"),
      Value = as.character(c(input$Age,
                             input$Sex,
                             input$ChestPainType,
                             input$RestingBP,
                             input$Cholesterol,
                             input$FastingBS,
                             input$RestingECG,
                             input$MaxHR,
                             input$ExerciseAngina,
                             input$Oldpeak,
                             input$ST_Slope)),
      stringsAsFactors = FALSE)
    
    Species <- 0
    df <- rbind(df, Species)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

shinyApp(ui = ui, server = server)