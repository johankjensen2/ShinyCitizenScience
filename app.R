
####################################################
'SHINY'
####################################################

#install.packages("shiny")
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(shiny.i18n)
library(shinydashboard)
library(shinyTime)
library(shinyWidgets)
library(leaflet)
library(plotly)
#?shiny()


####################################################

#Save the data into the main data frame - if responses are updated w/ same school name, the row will be over-written

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
      if (data$School %in% responses$School) {
          responses[which(responses$School %in% data$School), ] <<- data
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


plot_func1_habitat <- function() {
                        if (exists("responses")) {
                          plotDataHabitat <- responses %>%
                                                      count(dominant_habitat)

ggplot(plotDataHabitat, aes(x = reorder(as.character(dominant_habitat),-as.numeric(n)), y=as.numeric(n), 
                            fill = as.character(dominant_habitat), color = as.character(dominant_habitat))) +
       geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) + theme_classic() + 
       scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
       scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
       scale_y_continuous(limits = c(0,5), expand = c(0,0)) + labs(y="Number of schools", x="", title = "") +
       theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
       text = element_text(size=20, family= "Times"), axis.text.x = element_text(size = 20, angle = 45, 
                                                                                 hjust = 1, color = "grey1")) + 
       theme(axis.ticks.length=unit(.25, "cm"))
  }
}

plot_func2_yard <- function() {
                        if (exists("responses")) {
                            School <- as.character(responses$School)
                            yard <- as.numeric(responses$yard_size)
                            WoodySpecies <- as.numeric(responses$trees_tot_species) + as.numeric(responses$shrubs_tot_species)

                            plotDataHabitatTrees <- data.frame(School,yard,WoodySpecies)


ggplot(plotDataHabitatTrees, aes(x = as.numeric(yard), y=as.numeric(WoodySpecies), 
                                 fill = School, color = School)) +
  geom_point(aes(fill=School), colour="black", size=5, alpha = 0.9, pch=21) +
  theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
  scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +  scale_x_continuous(limits = c(0,2000), expand = c(0,0)) +
  labs(y="Species richness (number of woody species) \n", x="School grounds size (m2)", title = "") +
  geom_smooth(method='lm', se=F, size = 1.5, colour ="Black") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=20, family= "Times"), 
        axis.text.x = element_text(size = 20, angle = 45,
                                   hjust = 1, color = "grey1")) +
  theme(axis.ticks.length=unit(.25, "cm"))
  }
}

plot_func3_birds <- function() {
                     if (exists("responses")) {

                       plotDataBirds <- data.frame()
                       for(row in 1:nrow(responses)){
                         temp <- sum(as.numeric(c(responses$turdus_no[row], responses$fringilla_no[row], 
                                                     responses$parus_no[row], responses$cyanistes_no[row],
                                                     responses$passer_dom_no[row], responses$passer_mont_no[row],
                                                     responses$motacilla_no[row], responses$motacilla_no[row],
                                                     responses$sturnus_no[row], responses$apus_no[row], 
                                                     responses$streptopelia_no[row], responses$erithacus_no[row],
                                                     responses$columba_no[row],responses$phoenicurus_no[row],
                                                     responses$pica_no[row],responses$sylvia_arti_no[row],
                                                     responses$sylvia_melano_no[row])))
                         plotDataBirds <- rbind(plotDataBirds, temp)
                       }
                      
                          plotDataBirds$School <- as.character(responses$School)
                          plotDataBirds$WoodySpecies <- as.numeric(responses$trees_tot_species) + as.numeric(responses$shrubs_tot_species)
                          colnames(plotDataBirds) <- c("BirdSum", "School", "WoodySpecies")


                ggplot(plotDataBirds, aes(x = WoodySpecies, y=BirdSum, fill = School, color = School)) +
                  geom_point(aes(fill=School), colour="black", size=5, alpha = 0.9, pch=21) +
                  theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataBirds$School), 
                                                                         type = "continuous")) +
                  scale_color_manual(values=wes_palette("Moonrise3", length(plotDataBirds$School), type = "continuous")) +
                  scale_y_continuous(limits = c(0,150), expand = c(0,0)) +  scale_x_continuous(limits = c(0,50), expand = c(0,0)) +
                  labs(y="Total number of birds observed \n", x="Number of tree and shrub species", title = "") +
                  geom_smooth(method='lm', se=F, size = 1.5, colour ="Black") +
                  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
                         text = element_text(size=20, family= "Times"), 
                  axis.text.x = element_text(size = 16, angle = 45,
                                   hjust = 1, color = "grey1")) + theme(axis.ticks.length=unit(.25, "cm"))
 }
}

plot_func4_pollinators <- function() {
                            if (exists("responses")) {
    
                            plotDataPollinators <- t(data.frame(responses$`5x5_bombus_tot_individ`[length(responses$School)],
                                                                responses$`5x5_mellifera_tot_individ`[length(responses$School)],
                                                                responses$`5x5_apis_tot_individ`[length(responses$School)],
                                                                responses$`5x5_vespid_tot_individ`[length(responses$School)],
                                                                responses$`5x5_coleoptera_tot_individ`[length(responses$School)],
                                                                responses$`5x5_hemiptera_tot_individ`[length(responses$School)],
                                                                responses$`5x5_lepidoptera_tot_individ`[length(responses$School)],
                                                                responses$`5x5_moth_tot_individ`[length(responses$School)],
                                                                responses$`5x5_syrphidae_tot_individ`[length(responses$School)],
                                                                responses$`5x5_other_diptera_tot_individ`[length(responses$School)]))

                            plotDataPollinators <- as.data.frame(plotDataPollinators)

                            plotDataPollinators$newName <- c("Bumblebees", "Honeybees", "Solitary bees", "Wasps", "Beetles",
                                                             "True bugs", "Butterflies", "Moths", "Hoverflies", "Other pollinators")
                            
                            colnames(plotDataPollinators) <- c("Number", "ShowName")
                            plotDataPollinators$Number <- as.numeric(plotDataPollinators$Number)


                            pieChartPoll <- plot_ly(plotDataPollinators, labels = ~ShowName, values = ~Number, type = 'pie',
                                                    textposition = 'inside',
                                                    textinfo = 'label+percent',
                                                    insidetextfont = list(color = '#FFFFFF'),
                                                    hoverinfo = 'text',
                                                    text = ~paste(Number, ' individuals observed'),
                                                    marker = list(colors = colors,
                                                    line = list(color = '#FFFFFF', width = 1),showlegend = FALSE))
                            pieChartPoll <- pieChartPoll %>% layout(title = 'The most common pollinators by occurence on your school:',
                                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

                            pieChartPoll
  }
}

plot_func5_invertebrates <- function() {
                              if (exists("responses")) {
                                
                                plotDataInvertebrates <- data.frame()
                                for(row in 1:nrow(responses)){
                                  temp2 <- sum(as.numeric(c(responses$`1x1_annelida_tot_species`[row], responses$`1x1_hymenoptera_tot_species`[row], 
                                                           responses$`1x1_myriapoda_tot_species`[row], responses$`1x1_isopoda_tot_species`[row],
                                                           responses$`1x1_arachnid_tot_species`[row], responses$`1x1_coleoptera_tot_species`[row],
                                                           responses$`1x1_hemiptera_tot_species`[row], responses$`1x1_caterpillar_tot_species`[row],
                                                           responses$`1x1_gastropoda_tot_species`[row], responses$`1x1_other_diptera_tot_species`[row])))
                                  plotDataInvertebrates <- rbind(plotDataInvertebrates, temp2)
                                }
                                colnames(plotDataInvertebrates) <- c("InvertebrateSum")
                                plotDataInvertebrates$School <- as.character(responses$School)
                                plotDataInvertebrates$plotNo <- as.numeric(responses$no_1x1_squares)
                                plotDataInvertebrates$invertebrateMeanPerSquare <- plotDataInvertebrates$InvertebrateSum/plotDataInvertebrates$plotNo
                                

                                  ggplot(plotDataInvertebrates, aes(x = reorder(School,-invertebrateMeanPerSquare), y=invertebrateMeanPerSquare, 
                                         fill = School, color = School)) +
                                         geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
                                         scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), 
                                                                               type = "continuous")) +
                                         scale_color_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), 
                                                                               type = "continuous")) +
                                         scale_y_continuous(limits = c(0,25), expand = c(0,0)) + theme_classic() +
                                         labs(y="Mean number of minibeasts per 1x1 square \n", x="", title = "") +
                                         theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
                                         text = element_text(size=20, family= "Times"), 
                                         axis.text.x = element_text(size = 20, angle = 45,
                                                                    hjust = 1, color = "grey1")) +
                                                                    theme(axis.ticks.length=unit(.25, "cm"))

  }
}

plotFuncTemp <- function() {
  if (exists("responses")) {
    ggplot(responses, aes(x =reorder(School,-as.numeric(temp)), y=as.numeric(temp), 
                          fill = School, color = School)) +
      geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
      theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(responses$School), type = "continuous")) +
      scale_color_manual(values=wes_palette("Moonrise3", length(responses$School), type = "continuous")) +
      scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
      labs(y="Temperature (°C)", x="", title = "") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            text = element_text(size=20, family= "Times"), 
            axis.text.x = element_text(size = 20, angle = 45,
                                       hjust = 1, color = "grey1")) +
      theme(axis.ticks.length=unit(.25, "cm"))
  }
}

mapFunc <- function() {
  if (exists("responses")) {
    points <- cbind(as.numeric(responses$long), as.numeric(responses$lat))
    
      leaflet() %>%
        setView(lat = 56, lng = 11, zoom = 3.4) %>%
        addProviderTiles(provider = "OpenStreetMap.Mapnik",
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircles(data = points, radius = 20,
                   group = NULL, stroke = TRUE, color = "#006400",
                   weight = 10, opacity = 0.8, fill = TRUE, fillColor = "#006400",
                   fillOpacity = 0.5, label = responses$School)
    }
}

#colours for map prototype
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#Set up the translation by selecting the language file
i18n <- Translator$new(translation_json_path="./test.JSON")
i18n$set_translation_language('United Kingdom') #chose initial language to load

# Define the fields we want to save from the form
fields <- c("School", "selected_language", "lat", "long",
            "no_partic_habitat","age_partic_habitat", "yard_size","habitat_comp",'dominant_habitat',"trees_tot_indiv",
            "shrubs_tot_indiv","trees_tot_species","shrubs_tot_species","trees_tot_flower","shrubs_tot_flower",
            "trees_tot_fruit","shrubs_tot_fruit","plantSpecies_freeText","feedback_habitat","turdus_no","turdus_pres",
            "fringilla_no", "fringilla_pres","parus_no","parus_pres","cyanistes_no","cyanistes_pres","passer_dom_no",
            "passer_dom_pres","passer_mont_no","passer_mont_pres","motacilla_no","motacilla_pres","sturnus_no","sturnus_pres",
            "apus_no","apus_pres","streptopelia_no","streptopelia_pres","erithacus_no","erithacus_pres","columba_no","columba_pres",
            "phoenicurus_no","phoenicurus_pres","pica_no","pica_pres","sylvia_arti_no","sylvia_arti_pres","sylvia_melano_no",
            "sylvia_melano_pres","birdSpecies_freeText","feedback_birds","weather","temp","windLevel","date","time","area_5x5_planted",
            "area_5x5_damp","area_5x5_woody","area_5x5_shortGrass","area_5x5_ground","area_5x5_walls","area_5x5_anthro",
            "area_5x5_hardSurface","5x5_bombus_tot_individ","5x5_bombus_tot_species","5x5_mellifera_tot_individ",
            "5x5_mellifera_tot_species","5x5_apis_tot_individ","5x5_apis_tot_species","5x5_vespid_tot_individ",
            "5x5_vespid_tot_species","5x5_coleoptera_tot_individ","5x5_coleoptera_tot_species","5x5_hemiptera_tot_individ",
            "5x5_hemiptera_tot_species","5x5_lepidoptera_tot_individ","5x5_lepidoptera_tot_species","5x5_moth_tot_individ","5x5_moth_tot_species",
            "5x5_syrphidae_tot_individ","5x5_syrphidae_tot_species","5x5_other_diptera_tot_individ","5x5_other_diptera_tot_species", 
            "5x5_floweriness","5x5_flowers_tot_species","5x5_flower_species_freeText","feedback_pollinators","no_1x1_squares",
            "habitat_1x1_freeText","1x1_annelida_tot_indiv","1x1_annelida_tot_species","1x1_hymenoptera_tot_individ",
            "1x1_hymenoptera_tot_species","1x1_myriapoda_tot_individ","1x1_myriapoda_tot_species","1x1_isopoda_tot_individ",
            "1x1_isopoda_tot_species","1x1_arachnid_tot_individ","1x1_arachnid_tot_species","1x1_coleoptera_tot_individ",
            "1x1_coleoptera_tot_species","1x1_hemioptera_tot_individ","1x1_hemiptera_tot_species","1x1_caterpillar_tot_individ",
            "1x1_caterpillar_tot_species","1x1_gastropoda_tot_individ","1x1_gastropoda_tot_species","1x1_other_tot_individ",
            "1x1_other_diptera_tot_species","1x1_leaf_diversity","feedback_minibeasts")

####################################################
# APP
####################################################
shinyApp(
  ui = fluidPage(
    
    fluidRow(
      imageOutput("uniLogo", height = 100), # LU logo
      HTML("<br/>", "<br/>"), # white space
      
      column(9, align="center",
    
    
  
    ## Language and region selector - translates text (eng default if no translation is available)
    shiny.i18n::usei18n(i18n),
    tags$div(
      selectInput(
        inputId='selected_language',
        label=i18n$t('Select your region:'),
        choices = list("","España (Spain)", "Malta", "Sverige (Sweden)", "United Kingdom")
      )
    ),
    
    
    actionButton("countrySelect", i18n$t("Select")), # confirm region
    
    
    HTML("<br/>", "<br/>", "<br/>", "<br/>"), # white space

    ## title text
    titlePanel(i18n$t("Welcome to the data entry prototype!")),
    
    #spaghetti line of instructions
    i18n$t("Here you can enter the data from your survey by using the interactive controls below. You will instantly be able to see how your results compare to other schools. Make sure to carefully enter all the correct data before pressing 'Download' - send this file to [email]. You can then play around with the data if you would like to explore the functions of the website. Thank you for contributing to science!"),
    HTML("<br/>","<br/>","<br/>","<br/>","<br/>"), #whitespace
    
    ## school name
    textInput("School", i18n$t("Enter the name of your school:")),
    
    
    HTML("<br/>", "<br/>"), #whitespace,
    
    i18n$t("Enter the longitude and latitude of your school. You can find these by right-clicking on Google Maps."),
    
    HTML("<br/>", "<br/>"), #whitespace,
    
    fluidRow(
      column(6, align="center",
    numericInput("lat", label = i18n$t("Latitude:"), value = 55.714203) #lat long input
      ),
    
      column(6, align="center",
    numericInput("long", label = i18n$t("Longitude:"), value = 13.207879))
      ),
    
    HTML("<br/>"), #whitespace,
    
    actionButton("showMap", i18n$t("Show map")), #map button
    
    HTML("<br/>", "<br/>", "<br/>", "<br/>"), # white space
    
    leafletOutput("mymap", width = 780, height = 420),
    
    HTML("<br/>", "<br/>", "<br/>"), # white space
    
    # WEATHER DATA
    
    ## weather check box with icons
    tags$style(".fa-sun {color:#E87722}"), #change colour and size of weather icons
    tags$style(".fa-cloud-rain {color:#228fe8}"),
    tags$style(".fa-cloud {color:#8e99ad}"),
    tags$style(".fa-cloud-sun {color:#CECECE}"),
    tags$style(HTML(".fa{font-size: 24px;}")),
    
    ## title text
    
    h3(id="weather-subHeading", i18n$t("Weather and temperature:")),
    tags$style(HTML("#weather-subHeading{color: #000000;}")), #ID call for custom colour
    
    HTML("<br/>"), #whitespace
    
    fluidRow(
      column(6, align="center",
             radioButtons("weather", i18n$t("What was the weather like during the survey? (Single choice)"),
                          choiceNames =
                            list(icon("sun"), icon("cloud-sun"), icon("cloud"), icon("cloud-rain")),
                          choiceValues = list("Sunny", "Sunny and cloudy", "Cloudy", "Rainy"))
      ),
      
      column(6,
             sliderInput("temp", i18n$t("What was the temperature outside during the survey? (°C)"),
                         5, 40, 2, ticks = T)
      )
    ),
    
    
    sliderTextInput(inputId = "windLevel", label = i18n$t("How windy was it during the survey? (Use slider below)"),
                    choices = c("Leaves not moving", "Leaves moving slightly", "Leaves moving much")),
    
    HTML("<br/>","<br/>"), #whitespace
    
    ## Date and time
    fluidRow(
      column(6, align="center",
             dateInput("date", i18n$t("Select the date when the surveying took place:"))
      ),
      
      column(6,
             timeInput("time", i18n$t("Enter the time when the surveying started (use the dials or enter the time directly):"),
                       value = Sys.time(), seconds = FALSE)
      )
    ),
    
    
    HTML("<br/>"), #whitespace
    
    actionButton("showWeather", i18n$t("Show weather data")),
    
    HTML("<br/>","<br/>"), #whitespace
    
    
    #plotOutput("temp"),
    
    HTML("<br/>","<br/>"), #whitespace
    
    
    
    ##########################################
    ######         HABITAT DATA         ######
    ##########################################
    
    ## title text
    h2(id="habitat-heading", i18n$t("Habitat survey")),
    tags$style(HTML("#habitat-heading{color: #2F5496;}")), #ID call for custom colour
    
    i18n$t("Below you can enter the data from the habitat survey (Part A):"), #Instructions
    HTML("<br/>", "<br/>", "<br/>"), #whitespace
    
    # Entry of age and number
    fluidRow(
      column(6, align="center",
             numericInput("no_partic_habitat", label = i18n$t("How many participants are in your survey group?"), 
                          value = 0, width = "75%")),
      
      column(6, align="center",
             numericInput("age_partic_habitat", label = i18n$t("What is the average age of the participants?"), value = 0, width = "75%"))),
    
    HTML("<br/>", "<br/>"), #whitespace
    
    #yard size
    numericInput("yard_size", label = i18n$t("(Optional) How large is your school grounds (m2)?"), 
                value = 0, width = "40%"),
    
    HTML("<br/>", "<br/>"), #whitespace
    
    #habitat characteristics
    fluidRow(
      column(6, align="center", tags$div(align = 'left', 
             checkboxGroupInput("habitat_comp", i18n$t("What of the following habitats did you find on your school grounds? (Multiple choice)"),
                                choiceNames =
                                  list(i18n$t("Plant beds or flowerpots"), i18n$t("Tall grass, wildflowers"), 
                                       i18n$t("Trees and bushes"), i18n$t("Bare ground (soil, sand, gravel, etc.)"), 
                                       i18n$t("Man-made homes: Bird homes"), i18n$t("Man-made homes: Wild Bee homes"), 
                                       i18n$t("Man-made homes: Honeybee homes"), i18n$t("Man-made homes: Minibeast homes"), 
                                       i18n$t("Man-made homes: Other"), i18n$t("Damp places"), i18n$t("Short grass"), 
                                       i18n$t("Bare walls or fences"), i18n$t("Concrete or tarmac")),
                                choiceValues =
                                  list("planted", "tall_grass", "woody", "bare_ground", "anthro_bird",
                                       "anthro_wildbee", "anthro_domestic_bee", "anthro_invertebrate", "anthro_other", "damp_place",
                                       "short_grass", "walls", "hard_surface")))),
    
    column(6, align="center",    
           tags$div(selectInput(inputId='dominant_habitat', label=i18n$t('What is the dominant habitat in your school grounds (select one):'),
                                choices = list("","Plant beds or flowerpots", "Tall grass, wildflowers", "Trees and bushes", "Bare ground",
                                               "Man-made homes", "Damp places", "Short grass", "Bare walls or fences", "Concrete or tarmac"))))),
    
    
    HTML("<br/>", "<br/>"), #whitespace
    
    actionButton("showHabitat", i18n$t("Show habitat data")), #map button
    
    HTML("<br/>", "<br/>"), #whitespace
    
    plotOutput("habitat_1"),
    
    HTML("<br/>", "<br/>"), #whitespace
    
    # Entry of trees and bushes
    fluidRow(
      column(4, align="left", HTML("<br/>"), strong(i18n$t("Total number of tree and shrub individuals:"))),
      
      column(4, align="center",
             numericInput("trees_tot_indiv", label = i18n$t("Trees:"), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("shrubs_tot_indiv", label = i18n$t("Shrubs:"), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(4, align="left", HTML("<br/>"), strong(i18n$t("Total number of different tree and shrub species:"))),
      
      column(4, align="center",
             numericInput("trees_tot_species", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("shrubs_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(4, align="left", HTML("<br/>"), strong(i18n$t("Number of flowering species:"))),
      
      column(4, align="center",
             numericInput("trees_tot_flower", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("shrubs_tot_flower", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(4, align="left", HTML("<br/>"), strong(i18n$t("Number of fruit bearing species:"))),
      
      column(4, align="center",
             numericInput("trees_tot_fruit", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("shrubs_tot_fruit", label = i18n$t(""), value = 0, width = "50%"))),
    
    HTML("<br/>"), #whitespace,
    
    #Free-text entry of specific species
    textInput("plantSpecies_freeText", i18n$t("Enter the three most common species and numbers below:"), 
              value = "E.g.: oak (7), birch (4), Taxus baccata (2)", width = "100%"),
    
    
    HTML("<br/>", "<br/>"), #whitespace
    
    actionButton("showYard", i18n$t("Show tree and shrub data")), #map button
    
    HTML("<br/>", "<br/>"), #whitespace
    
    plotOutput("yard_1"), #graph 2 - species depending on yard size
    
    HTML("<br/>", "<br/>"), #whitespace
    
    sliderInput(inputId = "feedback_habitat", label = i18n$t("How do you rate your experience with using this survey? (Use slider below)"),
                                1, 5, 3, ticks = F),
    
    ## OBS a graph could probably be good here!
    HTML("<br/>","<br/>","<br/>","<br/>"), #whitespace
    
    
    ##########################################
    ######          BIRD SURVEY         ######
    ##########################################
    
    h2(id="bird-heading", i18n$t("Bird survey")),
    tags$style(HTML("#bird-heading{color: #ED7D31;}")), #ID call for custom colour
    
    i18n$t("Below you can enter the data from the survey of birds (Part B). If you know how many individuals you saw of a species, enter the number below. If you do not have the numbers, but know you saw a species, you can tick the box:"), #Instructions
    
    HTML("<br/>","<br/>"), #whitespace
    
    
    
    #turdus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Eurasian blackbird (Turdus merula):"))),
      
      column(5, align="left",
             numericInput("turdus_no", label = i18n$t("Number:"), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("turdus_pres", i18n$t("Present:"),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    #Fringilla
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Common chaffinch (Fringilla coelebs):"))),
      
      column(5, align="left",
             numericInput("fringilla_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("fringilla_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    #Parus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Great tit (Parus major):"))),
      
      column(5, align="left",
             numericInput("parus_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("parus_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    #Cyanistes
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Blue tit (Cyanistes caeruleus):"))),
      
      column(5, align="left",
             numericInput("cyanistes_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("cyanistes_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    #Passer domesticus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("House sparrow (Passer domesticus)"))),
      
      column(5, align="left",
             numericInput("passer_dom_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("passer_dom_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    #Passer montanus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Tree sparrow (Passer montanus)"))),
      
      column(5, align="left",
             numericInput("passer_mont_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("passer_mont_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    #Motacilla alba
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("White wagtail (Motacilla alba)"))),
      
      column(5, align="left",
             numericInput("motacilla_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("motacilla_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    #Sturnus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Common starling (Sturnus vulgaris)"))),
      
      column(5, align="left",
             numericInput("sturnus_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("sturnus_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    
    #Apus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Swift (Apus apus)"))),
      
      column(5, align="left",
             numericInput("apus_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("apus_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    
    #Streptopelia
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Collared dove (Streptopelia decaocto)"))),
      
      column(5, align="left",
             numericInput("streptopelia_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("streptopelia_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    #Erithacus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("European robin (Erithacus rubecula)"))),
      
      column(5, align="left",
             numericInput("erithacus_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("erithacus_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    
    #Columba
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Wood pigeon (Columba palumbus)"))),
      
      column(5, align="left",
             numericInput("columba_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("columba_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    
    #Phoenicurus
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Redstart (Phoenicurus phoenicurus)"))),
      
      column(5, align="left",
             numericInput("phoenicurus_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("phoenicurus_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    
    #Pica
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Magpie (Pica pica)"))),
      
      column(5, align="left",
             numericInput("pica_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("pica_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    
    #Sylvia arti
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Eurasian blackcap (Sylvia articapilla)"))),
      
      column(5, align="left",
             numericInput("sylvia_arti_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("sylvia_arti_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    
    #Sylvia melano
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Sardinian warbler (Sylvia melanocephala)"))),
      
      column(5, align="left",
             numericInput("sylvia_melano_no", label = i18n$t(""), value = 0, width = "50%")),
      
      column(2, align="center",
             checkboxGroupInput("sylvia_melano_pres", i18n$t(""),
                                choiceNames = list(i18n$t("")), choiceValues = list("present")))),
    
    HTML("<br/>"), #whitespace
    
    #Free-text entry of specific species
    textInput("birdSpecies_freeText", i18n$t("If you saw any aditional species, enter the three most common species and numbers below:"), 
              value = "E.g.: hooded crow (3), rook (4), common gull (2)", width = "100%"),
   
    HTML("<br/>", "<br/>"), #whitespace
    
    actionButton("showBirds", i18n$t("Show bird data")), #map button
    
    HTML("<br/>", "<br/>"), #whitespace
    
    plotOutput("birds_1"),
    
    HTML("<br/>", "<br/>"), #whitespace
    sliderInput(inputId = "feedback_birds", label = i18n$t("How do you rate your experience with using this survey? (Use slider below)"),
                1, 5, 3, ticks = F),
    
    ## OBS a graph could probably be good here!
    HTML("<br/>","<br/>","<br/>","<br/>"), #whitespace
    
    ##########################################
    ######        POLLINATORS 5x5       ######
    ##########################################
    
    ## title text
    h2(id="pollinator-heading", i18n$t("Pollinators and flowers in the 5x5m square")),
    tags$style(HTML("#pollinator-heading{color: #7030A0;}")), #ID call for custom colour
    
    i18n$t("Below you can enter the data from the survey of pollinators and flowers in the 5x5m square (Part C):"), #Instructions
    
    HTML("<br/>","<br/>"), #whitespace

    strong(i18n$t("Enter the area (m2) of the following habitat in your 5x5 survey plot:")),
    HTML("<br/>","<br/>"),
    
    fluidRow(
      column(6, align="center",
             numericInput("area_5x5_planted", label = i18n$t("Plant beds or flowerpots"), value = 0, width = "50%")),

      
      column(6,
             numericInput("area_5x5_damp", label = i18n$t("Damp places"), value = 0, width = "50%"))
    ),
    
    fluidRow(
      column(6, align="center",
             numericInput("area_5x5_woody", label = i18n$t("Trees and bushes"), value = 0, width = "50%")),
      
      
      column(6,
             numericInput("area_5x5_shortGrass", label = i18n$t("Short grass"), value = 0, width = "50%"))
    ),
    
    
    fluidRow(
      column(6, align="center",
             numericInput("area_5x5_ground", label = i18n$t("Bare ground"), value = 0, width = "50%")),
      
      
      column(6,
             numericInput("area_5x5_walls", label = i18n$t("Bare walls or fences"), value = 0, width = "50%"))
    ),
    
    fluidRow(
      column(6, align="center",
             numericInput("area_5x5_anthro", label = i18n$t("Man-made homes"), value = 0, width = "50%")),
      
      
      column(6,
             numericInput("area_5x5_hardSurface", label = i18n$t("Concrete or tarmac"), value = 0, width = "50%"))
    ),
        #PUT IN A CALCULATOR TO CHECK THAT IT EQUALS 25!


    
    HTML("<br/>","<br/>"), #whitespace
    
    # Entry of pollinators
    strong(i18n$t("Enter the total number of individuals and species you saw for each pollinator group:")),
    HTML("<br/>","<br/>"),
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Bumblebees"))),
      
      column(4, align="center",
             numericInput("5x5_bombus_tot_individ", label = i18n$t("Individuals:"), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_bombus_tot_species", label = i18n$t("Species:"), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Honeybees"))),
      
      column(4, align="center",
             numericInput("5x5_mellifera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_mellifera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Solitary bees"))),
      
      column(4, align="center",
             numericInput("5x5_apis_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_apis_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Wasps"))),
      
      column(4, align="center",
             numericInput("5x5_vespid_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_vespid_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Beetles"))),
      
      column(4, align="center",
             numericInput("5x5_coleoptera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_coleoptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("True bugs"))),
      
      column(4, align="center",
             numericInput("5x5_hemiptera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_hemiptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Butterflies"))),
      
      column(4, align="center",
             numericInput("5x5_lepidoptera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_lepidoptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Moths"))),
      
      column(4, align="center",
             numericInput("5x5_moth_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_moth_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Hoverflies"))),
      
      column(4, align="center",
             numericInput("5x5_syrphidae_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_syrphidae_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Other flies and mosquitos"))),
      
      column(4, align="center",
             numericInput("5x5_other_diptera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("5x5_other_diptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    HTML("<br/>", "<br/>"), #whitespace
    
    actionButton("showPollinators", i18n$t("Show pollinator data")), #map button
    
    HTML("<br/>", "<br/>"), #whitespace
    
    plotlyOutput("pollinators_1"),
    
    HTML("<br/>", "<br/>"), #whitespace
    
    #floweriness
    sliderInput(inputId = "5x5_floweriness", label = i18n$t("Rate your 5x5m survey plot on the floweriness-scale:"),
                    1, 3, 1, ticks = F),       
    
    
    HTML("<br/>","<br/>"), #whitespace
    
    #flower species
    fluidRow(
      column(6, align="center", 
             numericInput("5x5_flowers_tot_species", label = i18n$t("How many different species of flowering plants did you find?"), 
                                           value = 0, width = "100%")),
      
      column(6, align="center", textInput("5x5_flower_species_freeText", i18n$t("(Optional) If you identified the species, write the three most common below"), 
                                          value = "E.g.: daisy, lavender, orchid", width = "100%"))),

    HTML("<br/>", "<br/>"), #whitespace
    sliderInput(inputId = "feedback_pollinators", label = i18n$t("How do you rate your experience with using this survey? (Use slider below)"),
                1, 5, 3, ticks = F),
    HTML("<br/>", "<br/>","<br/>", "<br/>"), #whitespace
    
    ##########################################
    ######       INVERTEBRATES 1x1      ######
    ##########################################
    
    ## title text
    h2(id="minibeast-heading", i18n$t("Minibeasts and Leaves Survey")),
    tags$style(HTML("#minibeast-heading{color: #92d050;}")), #ID call for custom colour
    
    i18n$t("Below you can enter the data from the survey of minibeasts and leaves in the 1x1m squares (Part D):"), #Instructions
    
    HTML("<br/>","<br/>"), #whitespace
    
    numericInput("no_1x1_squares", label = i18n$t("How many 1x1m squares did you survey?"), 
                 value = 1, width = "30%"),
    
    HTML("<br/>","<br/>"), #whitespace
    
    #Free-text entry of 1x1 habitats
    textInput("habitat_1x1_freeText", i18n$t("What was the dominant habitat for each square? Denote the number of squares with this habitat as the dominant type in parentheses:"), 
              value = "E.g.: Flower beds or pots (1), tall grass and wildflowers (2), short grass (1)", width = "80%"),
    
    HTML("<br/>","<br/>"), #whitespace
    
    # Entry of pollinators
    strong(i18n$t("Enter the total number of individuals and species you saw for each minibeast group:")),
    HTML("<br/>","<br/>"),
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Earthworms"))),
      
      column(4, align="center",
             numericInput("1x1_annelida_tot_indiv", label = i18n$t("Individuals:"), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_annelida_tot_species", label = i18n$t("Species:"), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Ants"))),
      
      column(4, align="center",
             numericInput("1x1_hymenoptera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_hymenoptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Centipedes and millipedes"))),
      
      column(4, align="center",
             numericInput("1x1_myriapoda_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_myriapoda_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Woodlice"))),
      
      column(4, align="center",
             numericInput("1x1_isopoda_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_isopoda_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Spiders and harvestmen"))),
      
      column(4, align="center",
             numericInput("1x1_arachnid_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_arachnid_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Beetles"))),
      
      column(4, align="center",
             numericInput("1x1_coleoptera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_coleoptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("True bugs"))),
      
      column(4, align="center",
             numericInput("1x1_hemioptera_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_hemiptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Insect larvae"))),
      
      column(4, align="center",
             numericInput("1x1_caterpillar_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_caterpillar_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Snails and slugs"))),
      
      column(4, align="center",
             numericInput("1x1_gastropoda_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_gastropoda_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    
    fluidRow(
      column(3, align="left", HTML("<br/>"), strong(i18n$t("Other minibeasts"))),
      
      column(4, align="center",
             numericInput("1x1_other_tot_individ", label = i18n$t(""), value = 0, width = "50%")),
      
      column(4, align="center",
             numericInput("1x1_other_diptera_tot_species", label = i18n$t(""), value = 0, width = "50%"))),
    
    HTML("<br/>", "<br/>"), #whitespace
    
    actionButton("showInvertebrates", i18n$t("Show minibeast data")), #map button
    
    HTML("<br/>", "<br/>"), #whitespace
    
    plotOutput("invertebrates_1"),

    HTML("<br/>","<br/>","<br/>","<br/>"), #whitespace
    
    
    #floweriness
    fluidRow(
      column(6, align="center",
             sliderInput(inputId = "1x1_vegetation_cover", label = i18n$t("Rate the mean of your 1x1m survey plot on the vegetation cover-scale:"),
                         1, 3, 1, ticks = F)),
      
      column(6, align="center",
             numericInput("1x1_leaf_diversity", label = i18n$t("How many different types of leaves do you find in all 1x1m squares?"), 
                          value = 0, width = "100%"))),
    
    
    HTML("<br/>", "<br/>"), #whitespace
    
    #feedback 
    sliderInput(inputId = "feedback_minibeasts", label = i18n$t("How do you rate your experience with using this survey? (Use slider below)"),
                1, 5, 3, ticks = F),
    HTML("<br/>", "<br/>","<br/>", "<br/>"), #whitespace
    
    #######################################
    strong(i18n$t("Thank you for completing the data entry! Please control that all answers are correct before downloading the data. Kindly send this file to [...] so your data can be included in the research project and added to the website. Thank you for your participation.")),
    
    HTML("<br/>", "<br/>"),
    #download data button
    downloadButton('downloadData', 'Download'),
  
  HTML("<br/>", "<br/>", "<br/>", "<br/>"), # white space
  
 
  
  
  HTML("<br/>", "<br/>", "<br/>", "<br/>"), # white space
  imageOutput("allLogo", height = 100) # banner logo
  )
  )
  ),
  
  
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When country is selected, this will feed into shiny.i18n for translation
    observeEvent(input$countrySelect, {
      update_lang(session, input$selected_language)
    })
    
    # When the Submit button is clicked, save the form data BEES
    observeEvent(input$showMap, {
      saveData(formData())
    })

    
    # When the Submit button is clicked, save the form data WEATHER
    observeEvent(input$showWeather, {
      saveData(formData())
    })
    
    
    # When the Submit button is clicked, save the form data HABITAT
    observeEvent(input$showHabitat, {
      saveData(formData())
    })
    
    
    # When the Submit button is clicked, save the form data YARD
    observeEvent(input$showYard, {
      saveData(formData())
    })
    
    # When the Submit button is clicked, save the form data BIRDS
    observeEvent(input$showBirds, {
      saveData(formData())
    })
    
    # When the Submit button is clicked, save the form data POLLINATORS
    observeEvent(input$showPollinators, {
      saveData(formData())
    })
    
    # When the Submit button is clicked, save the form data INVERTEBRATES
    observeEvent(input$showInvertebrates, {
      saveData(formData())
    })
    
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$showWeather
      loadData()
      
    })    
    

    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$showMap
      loadData()
      
    }) 
    
    
    # render map
    output$mymap <- renderLeaflet({
      
      input$showMap 
      loadData()
      mapFunc()
      
      
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$showHabitat
      loadData()
      
    }) 
    
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$showYard
      loadData()
      
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$showBirds
      loadData()
      
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$showPollinators
      loadData()
      
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$showInvertebrates
      loadData()
      
    })
    
    
    # render barplot of most common habitat
    output$habitat_1 <- renderPlot({
      
      input$showHabitat 
      loadData()
      plot_func1_habitat()
      
      
    })
    
    
    # render correlation tree species ~ yard
    output$yard_1 <- renderPlot({
      
      input$showYard
      loadData()
      plot_func2_yard()
      
      
    })
    
    output$birds_1 <- renderPlot({
      
      input$showBirds
      loadData()
      plot_func3_birds()
      
      
    })
    
    output$pollinators_1 <- renderPlotly({
      
      input$showPollinators
      loadData()
      plot_func4_pollinators()
      
      
    })
    
    output$invertebrates_1 <- renderPlot({
      
      input$showInvertebrates
      loadData()
      plot_func5_invertebrates()
      
      
    })
    
    # render barplot of temperature
    output$temp <- renderPlot({
      
      input$showWeather
      loadData()
      plotFuncTemp()
      
      
    })
    # Download button
    
    output$downloadData <- downloadHandler(
    filename = function() {paste('data-', Sys.Date(), '-', length(responses$School), '.csv', sep='')
    },
    content = function(file) {write.csv2(t(formData()), file)
    }
    )
    
    # render uni logo
    output$uniLogo <- renderImage({list(src = "./logo2.png",
                                        contentType = "image/png",alt = "logo", height = 120)}, deleteFile = FALSE)


    # render uni logo
    output$allLogo <- renderImage({list(src = "./logo1.png",
                                    contentType = "image/png",alt = "logo", height = 200)}, deleteFile = FALSE)
}
)





