setwd("/Users/jo8521kj/Desktop/R/Shiny")
####################################################
'SHINY'
####################################################

testDataBase <- data.frame(Skola = c("A", "B", "C", "D", "E", "F"), Antal_bin = sample(1:100,6))
head(testDataBase)

#install.packages("shiny")
library(shiny)
library(plyr)
library(ggplot2)
library(wesanderson)
?shiny()



saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("testDataBase")) {
    responses <<- rbind(testDataBase, c("Din skola", input$testBin)
  } else {
    testDataBase <<- data
  }
}

loadData <- function() {
  if (exists("testDataBase")) {
    testDataBase
  }
}


ui <- fluidPage("Välkommen till Testsidan",
                sliderInput(inputId = "testBin", label = "Antal bin:", min = 0, max = 100,
                            value = 100),
                actionButton("submit", "Nästa"),
                plotOutput("bar")
                
)




server <- function(input, output){
  output$bar <- renderPlot({
  
   
    
    ggplot(testDataBase, aes(x= reorder(Skola, Antal_bin), y=Antal_bin, fill = Skola, color = Skola)) +
      geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
      theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", 6, type = "continuous")) +
      scale_color_manual(values =c("#000000","#000000","#000000","#000000","#000000","#000000")) +
      scale_y_continuous(limits = c(0,150), expand = c(0,0)) +
      labs(y="Antal bin \n", x="Skola", title = "") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = -0.15),
            text = element_text(size=20, family= "Times"), 
            axis.text.x = element_text(size = 28, angle = 45,
                                       hjust = 1, color = "grey1")) +
      theme(axis.ticks.length=unit(.25, "cm")) +
      
    
    
  })
}

shinyApp(ui = ui, server = server)




#####

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

# Define the fields we want to save from the form
fields <- c("Skola", "Antal bin")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    DT::dataTableOutput("responses", width = 300), tags$hr(),
    textInput("Skola", "Skola", ""),
    sliderInput("Antal bin", "Antal bin",
                0, 25, 2, ticks = TRUE),
    actionButton("submit", "Skicka")
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })     
  }
)


