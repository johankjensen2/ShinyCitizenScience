setwd("/Users/jo8521kj/Desktop/R/Shiny/")
####################################################
'SHINY'
####################################################

#install.packages("shiny")
library(shiny)
library(plyr)
library(ggplot2)
library(wesanderson)
?shiny()


####################################################

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    if (data$Skola %in% responses$Skola) {
      responses[which(responses$Skola %in% data$Skola), ] <<- data
    }
        else{ 
            responses <<- rbind(responses, data)
          }
   
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

plotFunc <- function() {
  if (exists("responses")) {
    ggplot(responses, aes(x =reorder(Skola,-as.numeric(Antal_bin)), y=as.numeric(Antal_bin), 
                          fill = Skola, color = Skola)) +
      geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
      theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(responses$Skola), type = "continuous")) +
      scale_color_manual(values=wes_palette("Moonrise3", length(responses$Skola), type = "continuous")) +
      scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
      labs(y="Antal bin \n", x="", title = "Bin och skolor") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            text = element_text(size=20, family= "Times"), 
            axis.text.x = element_text(size = 20, angle = 45,
                                       hjust = 1, color = "grey1")) +
      theme(axis.ticks.length=unit(.25, "cm"))
  }
}

plotFuncCorrelation <- function() {
  if (exists("responses")) {
    ggplot(responses, aes(x =as.numeric(percent_green), y = as.numeric(Antal_bin), 
                          fill = Skola, color = Skola, size = as.numeric(percent_green))) +
      geom_point(aes(), alpha=0.8) +
      theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(responses$Skola), type = "continuous")) +
      scale_color_manual(values=wes_palette("Moonrise3", length(responses$Skola), type = "continuous")) +
      scale_y_continuous(limits = c(0,60), expand = c(0,0)) + scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
      labs(y="Antal bin \n", x="Procent grönyta", title = "Bin beroende på gård") +
      theme(#legend.position = "",
            plot.title = element_text(hjust = 0.5),
            text = element_text(size=20, family= "Times"), 
            axis.text.x = element_text(size = 20, angle = 45,
                                       hjust = 1, color = "grey1")) +
      theme(axis.ticks.length=unit(.25, "cm"))+ 
      scale_size_continuous(name="Storlek skolgård (m2)")
  }
}

# Define the fields we want to save from the form
fields <- c("Skola", "Antal_bin", "Storlek_skolgard", "percent_green")

# Shiny app with 2 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    textInput("Skola", "Skola"),
    sliderInput("Antal_bin", "Antal bin",
                0, 50, 2, ticks = F),
    sliderInput("Storlek_skolgard", "Storlek skolgård (m)",
                0, 100, 2, ticks = F),
    sliderInput("percent_green", "Procent grönyta",
                0, 100, 2, ticks = F),
    actionButton("submit", "Skicka"),
    HTML("<br/>", "<br/>", "<br/>", "<br/>"),
    "Resultat:",
    plotOutput("bar"),
    plotOutput("scatter"),
    DT::dataTableOutput("responses", width = 300), tags$hr()
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
     
     
    output$bar <- renderPlot({
      
      input$submit 
      loadData()
      plotFunc()
        

    })
    
    output$scatter <- renderPlot({
      
      input$submit 
      loadData()
      plotFuncCorrelation()
      
      
    })
  }
)


