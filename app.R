####################################################
'SHINY'
####################################################

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
library(shinydashboard)
library(shinyjs)
library(extrafont) 
library(ggrepel) #NEW

####################################################

#Save the data into the main data frame - if responses are updated w/ same school name, the row will be over-written
previousResponses <- read.csv("./Data/public_data.csv", sep = ';')
responses <- read.csv("./Data/public_data.csv", sep = ';')

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
  if (length(responses$School) > length(previousResponses$School)) {
    if (responses$selected_language[length(responses$selected_language)] == 'Sweden (Svenska)') {
      #manually translate swedish responses from selectInput 
      responses$dominant_habitat[responses$dominant_habitat == "Blomsterbäddar och krukor"] <- 'Plant beds or flowerpots'
      responses$dominant_habitat[responses$dominant_habitat == "Högt gräs, vildblomster"] <-  'Tall grass, wildflowers'
      responses$dominant_habitat[responses$dominant_habitat == "Träd och buskar"] <- 'Trees and shrubs'
      responses$dominant_habitat[responses$dominant_habitat == "Barmark (jord, sand, grus, etc.)"] <- 'Bare ground (soil, sand, gravel, etc.)'
      responses$dominant_habitat[responses$dominant_habitat == "Fuktiga platser"] <- 'Damp places'
      responses$dominant_habitat[responses$dominant_habitat == "Kort gräs"] <-'Short grass'
      responses$dominant_habitat[responses$dominant_habitat == "Bara väggar eller staket"] <-  'Bare walls or fences'
      responses$dominant_habitat[responses$dominant_habitat == "Asfalt eller betong"] <-  'Concrete or tarmac'
      responses$dominant_habitat[responses$dominant_habitat == "Konstgjorda hem"] <-  'Man-made homes'
      
      plotDataHabitat <- responses %>%
        count(dominant_habitat)
    }
    else{
    plotDataHabitat <- responses %>%
      count(dominant_habitat)
    }
    
    if (responses$selected_language[length(responses$selected_language)] == 'Sweden (Svenska)') {
     
       plotDataHabitatSwe <- plotDataHabitat #create a new df for translation of data names for plot
      
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Plant beds or flowerpots'] <- "Blomsterbäddar och krukor"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Tall grass, wildflowers'] <- "Högt gräs, vildblomster"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Trees and shrubs'] <- "Träd och buskar"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Bare ground (soil, sand, gravel, etc.)'] <- "Barmark (jord, sand, grus, etc.)"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Damp places'] <- "Fuktiga platser"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Short grass'] <- "Kort gräs"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Bare walls or fences'] <- "Bara väggar eller staket"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Concrete or tarmac'] <- "Asfalt eller betong"
      plotDataHabitatSwe$dominant_habitat[plotDataHabitatSwe$dominant_habitat == 'Man-made homes'] <- "Konstgjorda hem"
      
    ggplot(plotDataHabitatSwe, aes(x = reorder(as.character(dominant_habitat),-as.numeric(n)), y=as.numeric(n), 
                                fill = as.character(dominant_habitat), color = as.character(dominant_habitat))) +
      geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) + theme_classic() + 
      scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
      scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
      scale_y_continuous(limits = c(0, as.numeric(max(plotDataHabitat$n)+2)), expand = c(0,0)) + labs(y="Antal skolor", x="", title = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
            text = element_text(size=20), axis.text.x = element_text(size = 20, angle = 45, 
                                                                                      hjust = 1, color = "grey1")) + 
      theme(axis.ticks.length=unit(.25, "cm"))  +
              theme(
                panel.background = element_rect(fill = "transparent"), # bg of the panel
                plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                panel.grid.major = element_blank(), # get rid of major grid
                panel.grid.minor = element_blank(), # get rid of minor grid
                legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
              )
    }
    else{
      ggplot(plotDataHabitat, aes(x = reorder(as.character(dominant_habitat),-as.numeric(n)), y=as.numeric(n), 
                                  fill = as.character(dominant_habitat), color = as.character(dominant_habitat))) +
        geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) + theme_classic() + 
        scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
        scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
        scale_y_continuous(limits = c(0, as.numeric(max(plotDataHabitat$n)+2)), expand = c(0,0)) + labs(y="Number of schools", x="", title = "") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
              text = element_text(size=20), axis.text.x = element_text(size = 20, angle = 45, 
                                                                                        hjust = 1, color = "grey1")) + 
        theme(axis.ticks.length=unit(.25, "cm"))  +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
    }
  }
}

plot_func2_yard <- function() {
  if (length(responses$School) > length(previousResponses$School)) {
    School <- as.character(responses$School)
    yard <- as.numeric(responses$yard_size)
    WoodySpecies <- as.numeric(responses$trees_tot_species) + as.numeric(responses$shrubs_tot_species)
    number <- 1:length(responses$School)
    
    plotDataHabitatTrees <- data.frame(School,yard,WoodySpecies, number)
    
    if (responses$selected_language[length(responses$selected_language)] == 'Sweden (Svenska)') {
      ggplot(plotDataHabitatTrees, aes(x = as.numeric(yard), y=as.numeric(WoodySpecies), 
                                       fill = School, color = School)) +
        geom_point(aes(fill=School), colour="black", size=6, alpha = 0.9, pch=21) +
        theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
        scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
        scale_y_continuous(breaks=seq(0,1000,2), limits = c(0, as.numeric(max(plotDataHabitatTrees$WoodySpecies)+5)), expand = c(0,0)) +  
        scale_x_continuous(breaks=seq(0,100000,200), limits = c(0,as.numeric(max(plotDataHabitatTrees$yard)+100)), expand = c(0,0)) +
        geom_label_repel(aes(label=ifelse(plotDataHabitatTrees$number == length(plotDataHabitatTrees$School),
                                          as.character(plotDataHabitatTrees$School),'')), box.padding = 2, point.padding = 1.5,
                                          color = "black", segment.color = 'black', alpha = 0.6, size = 6)+
        labs(y="Artrikedom\n(antal träd- och buskarter) \n", x="Skolgårdens storlek (m2)", title = "") +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=20)) +
        theme(axis.ticks.length=unit(.25, "cm")) +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
    }
    else{
      ggplot(plotDataHabitatTrees, aes(x = as.numeric(yard), y=as.numeric(WoodySpecies), 
                                       fill = School, color = School)) +
        geom_point(aes(fill=School), colour="black", size=6, alpha = 0.9, pch=21) +
        theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
        scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
        scale_y_continuous(breaks=seq(0,1000,2), limits = c(0, as.numeric(max(plotDataHabitatTrees$WoodySpecies)+5)), expand = c(0,0)) +  
        scale_x_continuous(breaks=seq(0,100000,200), limits = c(0,as.numeric(max(plotDataHabitatTrees$yard)+100)), expand = c(0,0)) +
        geom_label_repel(aes(label=ifelse(plotDataHabitatTrees$number == length(plotDataHabitatTrees$School),
                                          as.character(plotDataHabitatTrees$School),'')), box.padding = 2, point.padding = 1.5,
                                          color = "black", segment.color = 'black', alpha = 0.6, size = 6) +
        labs(y="Species richness\n(number of woody species) \n", x="School grounds size (m2)", title = "") +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=20)) +
        theme(axis.ticks.length=unit(.25, "cm")) +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
    }

  }
}

plot_func3_birds <- function() {
  if (length(responses$School) > length(previousResponses$School)) {
    
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
    plotDataBirds$number <- 1:length(responses$School)
    colnames(plotDataBirds) <- c("BirdSum", "School", "WoodySpecies", "number")
    
    if (responses$selected_language[length(responses$selected_language)] == 'Sweden (Svenska)') {
      ggplot(plotDataBirds, aes(x = WoodySpecies, y=BirdSum, fill = School, color = School)) +
        geom_point(aes(fill=School), colour="black", size=6, alpha = 0.9, pch=21) +
        theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataBirds$School), 
                                                               type = "continuous")) +
        scale_color_manual(values=wes_palette("Moonrise3", length(plotDataBirds$School), type = "continuous")) +
        scale_y_continuous(breaks=seq(0,1000,5), limits = c(0, as.numeric(max(plotDataBirds$BirdSum)+10)), expand = c(0,0)) +  
        scale_x_continuous(breaks=seq(0,1000,2), limits = c(0, as.numeric(max(plotDataBirds$WoodySpecies)+5)), expand = c(0,0)) +
        labs(y="Totalt antal observerade fåglar \n", x="Antal träd- och buskarter", title = "") +
        geom_label_repel(aes(label=ifelse(plotDataBirds$number == length(plotDataBirds$School),
                                          as.character(plotDataBirds$School),'')), box.padding = 2, point.padding = 1.5,
                                          color = "black", segment.color = 'black', alpha = 0.6, size = 6)+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
              text = element_text(size=20)) + theme(axis.ticks.length=unit(.25, "cm")) +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
    }
    else{
    ggplot(plotDataBirds, aes(x = WoodySpecies, y=BirdSum, fill = School, color = School)) +
      geom_point(aes(fill=School), colour="black", size=6, alpha = 0.9, pch=21) +
      theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataBirds$School), 
                                                             type = "continuous")) +
      scale_color_manual(values=wes_palette("Moonrise3", length(plotDataBirds$School), type = "continuous")) +
      scale_y_continuous(breaks=seq(0,1000,5), limits = c(0, as.numeric(max(plotDataBirds$BirdSum)+10)), expand = c(0,0)) +  
      scale_x_continuous(breaks=seq(0,1000,2), limits = c(0, as.numeric(max(plotDataBirds$WoodySpecies)+5)), expand = c(0,0)) +
        geom_label_repel(aes(label=ifelse(plotDataBirds$number == length(plotDataBirds$School),
                                          as.character(plotDataBirds$School),'')), box.padding = 2, point.padding = 1.5,
                                          color = "black", segment.color = 'black', alpha = 0.6, size = 6)+
      labs(y="Total number of birds observed \n", x="Number of tree and shrub species", title = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
            text = element_text(size=20)) + theme(axis.ticks.length=unit(.25, "cm")) +
                                                                              theme(
                                                                                panel.background = element_rect(fill = "transparent"), # bg of the panel
                                                                                plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                                                                                panel.grid.major = element_blank(), # get rid of major grid
                                                                                panel.grid.minor = element_blank(), # get rid of minor grid
                                                                                legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                                                                                legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
                                                                              )
    }
  }
}

plot_func4_pollinators <- function() {
  if (length(responses$School) > length(previousResponses$School)) {
    
    plotDataPollinators <- t(data.frame(responses$bombus_tot_individ_5x5[length(responses$School)],
                                        responses$mellifera_tot_individ_5x5[length(responses$School)],
                                        responses$apis_tot_individ_5x5[length(responses$School)],
                                        responses$vespid_tot_individ_5x5[length(responses$School)],
                                        responses$coleoptera_tot_individ_5x5[length(responses$School)],
                                        responses$hemiptera_tot_individ_5x5[length(responses$School)],
                                        responses$lepidoptera_tot_individ_5x5[length(responses$School)],
                                        responses$moth_tot_individ_5x5[length(responses$School)],
                                        responses$syrphidae_tot_individ_5x5[length(responses$School)],
                                        responses$other_diptera_tot_individ_5x5[length(responses$School)]))
    
    plotDataPollinators <- as.data.frame(plotDataPollinators)
    
    plotDataPollinators$newName <- c("Bumblebees", "Honeybees", "Solitary bees", "Wasps", "Beetles",
                                     "True bugs", "Butterflies", "Moths", "Hoverflies", "Other pollinators")
    
    colnames(plotDataPollinators) <- c("Number", "ShowName")
    plotDataPollinators$Number <- as.numeric(plotDataPollinators$Number)
    
    if (responses$selected_language[length(responses$selected_language)] == 'Sweden (Svenska)') {
      
      plotDataPollinatorsSwe <- plotDataPollinators #create a new df for translation of data names for plot
      
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Bumblebees'] <- "Humlor"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Honeybees'] <- "Honungsbin"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Solitary bees'] <- "Solitärbin"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Wasps'] <- "Getingar"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Beetles'] <- "Skalbaggar"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'True bugs'] <- "Skinnbaggar"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Butterflies'] <- "Fjärilar"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Moths'] <- "Nattfjärilar"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Hoverflies'] <- "Blomflugor"
      plotDataPollinatorsSwe$ShowName[plotDataPollinatorsSwe$ShowName == 'Other pollinators'] <- "Övriga pollinatörer"
      
      pieChartPoll <- plot_ly(plotDataPollinatorsSwe, labels = ~ShowName, values = ~Number, type = 'pie',
                              textposition = 'inside',
                              textinfo = 'label+percent',
                              insidetextfont = list(color = '#FFFFFF'),
                              hoverinfo = 'text',
                              text = ~paste(Number, 'observerade individer'),
                              marker = list(colors = colors,
                                            line = list(color = '#FFFFFF', width = 1),showlegend = FALSE))
      
      pieChartPoll <- pieChartPoll %>% layout(title = 'De vanligast förekommande pollinatörerna på er skolgård:',
                                              paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)',
                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      pieChartPoll
    }
    
    else{
      pieChartPoll <- plot_ly(plotDataPollinators, labels = ~ShowName, values = ~Number, type = 'pie',
                              textposition = 'inside',
                              textinfo = 'label+percent',
                              insidetextfont = list(color = '#FFFFFF'),
                              hoverinfo = 'text',
                              text = ~paste(Number, ' individuals observed'),
                              marker = list(colors = colors,
                                            line = list(color = '#FFFFFF', width = 1),showlegend = FALSE))
      
      pieChartPoll <- pieChartPoll %>% layout(title = 'The most common pollinators by occurence on your school:',
                                              paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)',
                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      pieChartPoll
    }

  }
}

plot_func5_invertebrates <- function() {
  if (length(responses$School) > length(previousResponses$School)) {
    
    plotDataInvertebrates <- data.frame()
    for(row in 1:nrow(responses)){
      temp2 <- sum(as.numeric(c(responses$annelida_tot_species_1x1[row], responses$hymenoptera_tot_species_1x1[row], 
                                responses$myriapoda_tot_species_1x1[row], responses$isopoda_tot_species_1x1[row],
                                responses$arachnid_tot_species_1x1[row], responses$coleoptera_tot_species_1x1[row],
                                responses$hemiptera_tot_species_1x1[row], responses$caterpillar_tot_species_1x1[row],
                                responses$gastropoda_tot_species_1x1[row], responses$other_diptera_tot_species_1x1[row])))
      plotDataInvertebrates <- rbind(plotDataInvertebrates, temp2)
    }
    colnames(plotDataInvertebrates) <- c("InvertebrateSum")
    plotDataInvertebrates$School <- as.character(responses$School)
    plotDataInvertebrates$plotNo <- as.numeric(responses$no_1x1_squares)
    plotDataInvertebrates$invertebrateMeanPerSquare <- plotDataInvertebrates$InvertebrateSum/plotDataInvertebrates$plotNo
    
    if (responses$selected_language[length(responses$selected_language)] == 'Sweden (Svenska)') {
      ggplot(plotDataInvertebrates, aes(x = reorder(School,-invertebrateMeanPerSquare), y=invertebrateMeanPerSquare, 
                                        fill = School, color = School)) +
        geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
        scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), 
                                             type = "continuous")) +
        scale_color_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), 
                                              type = "continuous")) +
        scale_y_continuous(breaks=seq(0,1000,1), limits = c(0, as.numeric(max(plotDataInvertebrates$invertebrateMeanPerSquare)+2)), expand = c(0,0)) + theme_classic() +
        labs(y="Antal småkryp \n per kvadratmeter \n", x="", title = "") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
              text = element_text(size=20), 
              axis.text.x = element_text(size = 20, angle = 45,
                                         hjust = 1, color = "grey1")) +
        theme(axis.ticks.length=unit(.25, "cm")) +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
    }
    
    else{
      ggplot(plotDataInvertebrates, aes(x = reorder(School,-invertebrateMeanPerSquare), y=invertebrateMeanPerSquare, 
                                        fill = School, color = School)) +
        geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
        scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), 
                                             type = "continuous")) +
        scale_color_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), 
                                              type = "continuous")) +
        scale_y_continuous(breaks=seq(0,1000,1), limits = c(0, as.numeric(max(plotDataInvertebrates$invertebrateMeanPerSquare)+2)), expand = c(0,0)) + theme_classic() +
        labs(y="Mean number of minibeasts \n per 1x1 square \n", x="", title = "") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
              text = element_text(size=20), 
              axis.text.x = element_text(size = 20, angle = 45,
                                         hjust = 1, color = "grey1")) +
        theme(axis.ticks.length=unit(.25, "cm")) +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
    }

    
  }
}


mapFunc <- function() {
  if (length(responses$School) > length(previousResponses$School)) {
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
i18n$set_translation_language('United Kingdom (English)') #chose initial language to load

# Define the fields we want to save from the form
fields <- c("School", "selected_language", "lat", "long",
            "no_partic_habitat","age_partic_habitat", "date_habitat", "time_habitat", "yard_size","habitat_comp",
            'dominant_habitat',"trees_tot_indiv",
            "shrubs_tot_indiv","trees_tot_species","shrubs_tot_species","trees_tot_flower","shrubs_tot_flower",
            "trees_tot_fruit","shrubs_tot_fruit","plantSpecies_freeText","feedback_habitat","no_partic_bird", "age_partic_bird",
            "date_bird", "time_bird", "windLevel_bird", "temp_bird", "weather_bird","turdus_no","turdus_pres",
            "fringilla_no", "fringilla_pres","parus_no","parus_pres","cyanistes_no","cyanistes_pres","passer_dom_no",
            "passer_dom_pres","passer_mont_no","passer_mont_pres","motacilla_no","motacilla_pres","sturnus_no","sturnus_pres",
            "apus_no","apus_pres","streptopelia_no","streptopelia_pres","erithacus_no","erithacus_pres","columba_no","columba_pres",
            "phoenicurus_no","phoenicurus_pres","pica_no","pica_pres","sylvia_arti_no","sylvia_arti_pres","sylvia_melano_no",
            "sylvia_melano_pres","birdSpecies_freeText","feedback_birds","no_partic_pollinators", "age_partic_pollinators",
            "date_pollinators", "time_pollinators", "windLevel_pollinators", "temp_pollinators", "weather_pollinators",
            "area_5x5_planted","area_5x5_damp","area_5x5_woody","area_5x5_shortGrass","area_5x5_ground","area_5x5_walls","area_5x5_anthro",
            "area_5x5_hardSurface","bombus_tot_individ_5x5","bombus_tot_species_5x5","mellifera_tot_individ_5x5",
            "mellifera_tot_species_5x5","apis_tot_individ_5x5","apis_tot_species_5x5","vespid_tot_individ_5x5",
            "vespid_tot_species_5x5","coleoptera_tot_individ_5x5","coleoptera_tot_species_5x5","hemiptera_tot_individ_5x5",
            "hemiptera_tot_species_5x5","lepidoptera_tot_individ_5x5","lepidoptera_tot_species_5x5","moth_tot_individ_5x5","moth_tot_species_5x5",
            "syrphidae_tot_individ_5x5","syrphidae_tot_species_5x5","other_diptera_tot_individ_5x5","other_diptera_tot_species_5x5", 
            "floweriness_5x5","flowers_tot_species_5x5","flower_species_freeText_5x5","feedback_pollinators", "no_partic_minibeast", "age_partic_minibeast",
            "date_minibeast", "time_minibeast", "windLevel_minibeast", "temp_minibeast", "weather_minibeast", "no_1x1_squares",
            "habitat_1x1_freeText","annelida_tot_indiv_1x1","annelida_tot_species_1x1","hymenoptera_tot_individ_1x1",
            "hymenoptera_tot_species_1x1","myriapoda_tot_individ_1x1","myriapoda_tot_species_1x1","isopoda_tot_individ_1x1",
            "isopoda_tot_species_1x1","arachnid_tot_individ_1x1","arachnid_tot_species_1x1","coleoptera_tot_individ_1x1",
            "coleoptera_tot_species_1x1","hemioptera_tot_individ_1x1","hemiptera_tot_species_1x1","caterpillar_tot_individ_1x1",
            "caterpillar_tot_species_1x1","gastropoda_tot_individ_1x1","gastropoda_tot_species_1x1","other_tot_individ_1x1",
            "other_diptera_tot_species_1x1","leaf_diversity_1x1","feedback_minibeasts")

####################################################
# APP
####################################################
usei18n(i18n) # add to sort out translation bug of tab names
ui <- dashboardPage(
  dashboardHeader(title = "Natural Nations"),
  dashboardSidebar(width = "190px",
    sidebarMenu(menuItem(i18n$t("School information"), icon = icon('school'), tabName = 'school'),
                menuItem(i18n$t('Habitat (S1)'), icon = icon('tree'), tabName = 'habitat'),
                menuItem(i18n$t('Birds (S2)'), icon = icon('crow'), tabName = 'birds'),
                menuItem(i18n$t('Pollinators (S3)'), icon = icon('seedling'), tabName = 'pollinators'),
                menuItem(i18n$t('Minibeasts (S4)'), icon = icon('bug'), tabName = 'minibeasts'),
                menuItem(i18n$t('Submit data'), icon = icon('save'), tabName = 'submit'),
                menuItem(i18n$t('About us'), icon = icon('landmark'), tabName = 'about'),
                style = "position:fixed;"),
                tags$head(tags$style(HTML( #customize colors for sidebar
        "/*SIDE BAR STYLE*/
        .skin-blue .main-header .logo {background-color: #8ac062;}
        .skin-blue .main-header .logo:hover {background-color: #70ab44;}
        .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #70ab44;}
        .skin-blue .main-header .navbar {background-color: #8ac062;}
        .skin-blue .sidebar-menu > li.active > a {border-left-color: #8ac062}
        .skin-blue .sidebar-menu > li:hover > a {border-left-color: #64983d}
        .skin-blue .main-sidebar {position: fixed;}
        .main-header .navbar {margin-left: 190px}
        .main-header .logo {width: 187px; font-size:22.5px}
        
        /*FONT SIZE AND TEXT*/
        h1{color: #ffffff; margin-top: -320px; font-size: 4vw; text-shadow: 0.5px 0.5px 0.5px #aaa; font-weight: 700}
        h2{color: #ffffff; margin-top: -320px; font-size: 3vw; text-shadow: 0.3px 0.3px 0.3px #aaa;}
        h5{color: #6a6a6a; font-size: 24px; margin-top: -100px; text-shadow: 0.7px 0.7px 0.7px #edf0f5;}
        h3{font-size: 22px;}
        h4{font-size: 16px; font-weight: 700;}
        
        label{font-size: 16px; font-weight: 700;}
        [type = 'number'] {font-size: 16px; text-align: center;}
        [type = 'text'] {font-size: 16px;}
        
        /*BANNERS FORMAT*/
        #habitat_header img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-top: -30px;}
        #bird_header img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-top: -30px;}
        #plants_header img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-top: -30px;}
        #minibeast_header img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-top: -30px;}
        #habitat_footer img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-bottom: -410px;}
        #bird_footer img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-bottom: -410px;}
        #plants_footer img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right:  0px; margin-bottom: -410px;}
        #minibeast_footer img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-bottom: -410px;}
        #LtL_banner_dwn img {max-width: 110%; width: 104%; height: auto;
                            margin-left: -30px; margin-right: 0px; margin-bottom: -465px;}"
        )))
    ),
              

  ####################################################
  
  dashboardBody(
    #set custom lay-out for check-boxes etc
    setSliderColor(c(rep("#3c6986", 12)), 1:12),
    ## weather check box with icons
    tags$style(".fa-sun {color:#E87722; font-size:28px;}
                .fa-cloud-rain {color:#228fe8; font-size:28px;}
                .fa-cloud {color:#8e99ad; font-size:24px;}
                .fa-cloud-sun {color:#CECECE; font-size:24px;}"), #change colour and size of weather icons
    #tags$style(HTML(".fa{font-size: 24px;}")),
    tags$style("
      .buttonagency .bttn-primary{background-color: #3c6986;}
      
      .btn {background-color: #4c85a9; color: #ffffff; border-color: #447798;}
      .btn:hover {background-color: #447798; color: #ffffff; border-color: #447798;}
      .btn:focus {background-color: #4c85a9; color: #ffffff; border-color: #447798;}
      .btn:active {background-color: #3c6986 !important; color: #ffffff !important; border-color: #447798 !important;}
      
      .btn-accent_blue {background-color: #4c85a9;}
      .btn-accent_blue:hover {background-color: #447798;}
      .btn-accent_blue.active {background-color: #3c6986;}
      
      
      .checkbox {
        line-height: 30px;
        margin-bottom: 15px; /*set the margin, so boxes don't overlap*/}
        
      input[type='checkbox']{ /* style for checkboxes */
        width: 25px; 
        height: 25px; 
        line-height: 25px;
        accent-color: #2c4d63;}
        
      .radio {
        line-height: 30px;
        margin-bottom: 8px; /*set the margin, so boxes don't overlap*/}
        
      input[type='radio']{ /* style for checkboxes */
        width: 18px; 
        height: 18px; 
        line-height: 18px;
        accent-color: #2c4d63;}
        span {margin-left: 4px;  /*set the margin, so boxes don't overlap labels*/}"
               ),
    
    tabItems(
      tabItem(tabName = "school", align = "center",
              
              imageOutput("natNatLogo", height = 100), # natnat logo
              HTML("<br/>", "<br/>", "<br/>"), # white space
            
              ## Language and region selector - translates text (eng default if no translation is available)
              shiny.i18n::usei18n(i18n),
              tags$div(
                selectInput(
                  inputId='selected_language',
                  label=i18n$t('Select region and language:'),
                  choices = list("","Spain (Español)", "Malta (English)", "Sweden (Svenska)", "United Kingdom (English)")
                )
              ),
              
              
              actionButton("countrySelect", i18n$t("Select")), # confirm region
              
              
              HTML("<br/>", "<br/>", "<br/>"), # white space
              
              ## title text
              h3(i18n$t("Welcome to the data entry website!")),
              
              #spaghetti line of instructions
              fluidRow(
              column(2),
              column(8, align = "left",
                     h4(i18n$t("On this interactive website, you will be able to enter your collected data from each of the Natural Nations suite of four surveys:")),
                     h4(i18n$t("• S1 School Grounds and Habitat")),
                     h4(i18n$t("• S2 Birds")),
                     h4(i18n$t("• S3 Pollinator and Flowering Plants")),
                     h4(i18n$t("• S4 Minibeast and leaves")),
                     h4(i18n$t("You will instantly see how your results compare to other UK schools and those in participating European countries. The valuable data will be used to inform a research project by Lund University in Sweden.")),
                     h4(i18n$t("*Note* If you plan to do more than one of the survey protocols (S1, S2, S3 and S4),  please retain all the data and enter it altogether in one session.  While the data will be temporarily stored on the webpage, you will need to complete the data entry within 24 hours and click on the ‘Download data’ button (see instructions on the ‘Submit data’ page via left pane navigation) to allow the data to be permanently saved.  You can go back and correct values before clicking the download button.")),
                     h4(i18n$t("Thank you for contributing to science!")))),
              HTML("<br/>","<br/>","<br/>","<br/>"), #whitespace
              
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
              
              leafletOutput("mymap", width = 920, height = 480),
              
              HTML("<br/>", "<br/>", "<br/>"), # white space
              
              #actionButton("info_map", i18n$t("Tip!")), #button
              div(class = "buttonagency",
              actionBttn("info_map", i18n$t("Tip!"), style = "jelly", color = "primary")),
              
              HTML("<br/>", "<br/>") #whitespace
              
      ),
      
      ##########################################
      ######         HABITAT DATA         ######
      ##########################################
      
      tabItem(tabName = "habitat", align = "center",
              
              imageOutput("habitat_header"),
              
              ## title text
              fluidRow(
                column(1, align="right",
                       h1(i18n$t("S1"))),
                column(6, align="left",
              h2(i18n$t("School Grounds and Habitat Survey")))),
              
              h5(i18n$t("Below you can enter the data from the habitat survey (S1):"), align = "left"),
              
              HTML("<br/>", "<br/>", "<br/>", "<br/>"), #whitespace
              
              # Entry of age and number
              fluidRow(
                column(1),
                column(5, align="center",
                       numericInput("no_partic_habitat", label = i18n$t("How many participants are in your survey group?"), 
                                    value = 0, width = "75%")),
                
                column(5, align="center",
                       numericInput("age_partic_habitat", label = i18n$t("What is the average age of the participants?"),
                                    value = 0, width = "75%"))),
              
              HTML("<br/>", "<br/>"), #whitespace
              
              ## Date and time
              fluidRow(
                column(1),
                column(5, align="center",
                       dateInput("date_habitat", i18n$t("Select the date when the surveying took place:"), width = "75%")),
                
                column(5, align="center",
                       timeInput("time_habitat", i18n$t("Enter the time when the surveying started (use the dials or enter the time directly):"),
                                 value = Sys.time(), seconds = FALSE))),
              
              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              #yard size
              fluidRow(column(2),
                       column(8, align="center",
              numericInput("yard_size", label = i18n$t("How large is your school grounds (m2)?"), 
                           value = 0, width = "50%"))),
              
              HTML("<br/>", "<br/>", "<br/>", "<br/>"), #whitespace
              
              #habitat characteristics
              fluidRow(
                column(2),
                column(4, align="center", tags$div(align = 'left', 
                                                   checkboxGroupInput("habitat_comp", i18n$t("Which of the following habitats did you find in your school grounds? (Click any that apply)"),
                                                                      choiceNames =
                                                                        list(i18n$t("Plant beds or flowerpots"), i18n$t("Tall grass, wildflowers"), 
                                                                             i18n$t("Trees and shrubs"), i18n$t("Bare ground (soil, sand, gravel, etc.)"), 
                                                                             i18n$t("Man-made homes: Bird homes"), i18n$t("Man-made homes: Wild Bee homes"), 
                                                                             i18n$t("Man-made homes: Honeybee homes"), i18n$t("Man-made homes: Minibeast homes"), 
                                                                             i18n$t("Man-made homes: Other"), i18n$t("Damp places"), i18n$t("Short grass"), 
                                                                             i18n$t("Bare walls or fences"), i18n$t("Concrete or tarmac")),
                                                                      choiceValues =
                                                                        list("planted", "tall_grass", "woody", "bare_ground", "anthro_bird",
                                                                             "anthro_wildbee", "anthro_domestic_bee", "anthro_invertebrate", "anthro_other", "damp_place",
                                                                             "short_grass", "walls", "hard_surface")))),
                
                column(4, align="center",    
                       tags$div(selectInput(inputId='dominant_habitat', label=i18n$t('What is the dominant habitat in your school grounds (select one):'),
                                            choices = list("","Plant beds or flowerpots", "Tall grass, wildflowers", "Trees and shrubs", "Bare ground",
                                                           "Man-made homes", "Damp places", "Short grass", "Bare walls or fences", "Concrete or tarmac"))))),
              
              
              HTML("<br/>", "<br/>"), #whitespace
              
              actionButton("showHabitat", i18n$t("Show habitat data")), #map button
              
              HTML("<br/>", "<br/>"), #whitespace
              
              plotOutput("habitat_1", width = 780, height = 540),
              
              HTML("<br/>", "<br/>"), # white space
              
              div(class = "buttonagency",
                  actionBttn("info_habitat_graph_1", i18n$t("Tip!"), style = "jelly", color = "primary")),
              
              HTML("<br/>", "<br/>"), # white space
              
              # Entry of trees and bushes
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Total number of tree and shrub individuals:"))),
                
                column(3, align="center",
                       numericInput("trees_tot_indiv", label = i18n$t("Trees:"), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("shrubs_tot_indiv", label = i18n$t("Shrubs:"), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Total number of different tree and shrub species:"))),
                
                column(3, align="center",
                       numericInput("trees_tot_species", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("shrubs_tot_species", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Number of flowering species:"))),
                
                column(3, align="center",
                       numericInput("trees_tot_flower", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("shrubs_tot_flower", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Number of fruit bearing species:"))),
                
                column(3, align="center",
                       numericInput("trees_tot_fruit", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("shrubs_tot_fruit", label = i18n$t(""), value = 0, width = "75%"))),
              
              HTML("<br/>", "<br/>"), #whitespace,
              
              #Free-text entry of specific species
              fluidRow(
                column(2),
                column(8, align ="center",
              textInput("plantSpecies_freeText", i18n$t("Enter the three most common species and numbers below:"), 
                        placeholder = "E.g.: oak (7), birch (4), Taxus baccata (2)", width = "100%"))),
              
              
              HTML("<br/>", "<br/>"), #whitespace
              
              actionButton("showYard", i18n$t("Show tree and shrub data")), #map button
              
              HTML("<br/>", "<br/>"), #whitespace
              
              plotOutput("yard_1", width = 780, height = 420), #graph 2 - species depending on yard size
              
              HTML("<br/>", "<br/>"), # white space
              
              div(class = "buttonagency",
                  actionBttn("info_habitat_graph_2", i18n$t("Tip!"), style = "jelly", color = "primary")),
              
              HTML("<br/>", "<br/>"), # white space
              
              sliderInput(inputId = "feedback_habitat", label = i18n$t("How do you rate your experience with using this survey? (Use slider below)"),
                          1, 5, 3, ticks = F),
              

              imageOutput("habitat_footer"),
              
              ## OBS a graph could probably be good here!
              
      ),
      ##########################################
      ######          BIRD SURVEY         ######
      ##########################################
      tabItem(tabName = "birds", align = "center",
              
              imageOutput("bird_header"),
              
              ## title text
              fluidRow(
                column(1, align="right",
                       h1(i18n$t("S2"))),
                column(6, align="left",
                       h2(i18n$t("Bird Survey")))),
              
              h5(i18n$t("Below you can enter the data from the bird survey (S2)."), align = "left"),

              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              # Entry of age and number
              h3(i18n$t("Participants and time:")),
              
              HTML("<br/>"), #whitespace
              fluidRow(
                column(1),
                column(5, align="center",
                       numericInput("no_partic_bird", label = i18n$t("How many participants are in your survey group?"), 
                                    value = 0, width = "75%")),
                
                column(5, align="center",
                       numericInput("age_partic_bird", label = i18n$t("What is the average age of the participants?"),
                                    value = 0, width = "75%"))),
              
              HTML("<br/>", "<br/>"), #whitespace
            
              ## Date and time
              fluidRow(
                column(1),
                column(5, align="center",
                       dateInput("date_bird", i18n$t("Select the date when the surveying took place:"), width = "75%")),
                
                column(5, align="center",
                       timeInput("time_bird", i18n$t("Enter the time when the surveying started (use the dials or enter the time directly):"),
                                 value = Sys.time(), seconds = FALSE))),
              
              HTML("<br/>","<br/>"), #whitespace
              
              h3(i18n$t("Weather and temperature:")),
              
              HTML("<br/>"), #whitespace
              
              fluidRow(
                column(6, align="center",
                       radioGroupButtons(inputId = "windLevel_bird", label = i18n$t("How windy was it during the survey? (From leaves being still to moving a lot)"),
                         choices = c(`<i class='fa fa-wind' style='color:#ecf2f6; font-size: 12px;'></i>` = "Leaves not moving",
                                     `<i class='fa fa-wind' style='color:#ecf2f6; font-size: 16px;'></i>` = "Leaves moving slightly", 
                                     `<i class='fa fa-wind' style='color:#ecf2f6; font-size: 20px;'></i>` = "Leaves moving much"),
                         justified = TRUE, status = "accent_blue", size = "lg", width = "50%")),
                
                column(6,
                       sliderInput("temp_bird", i18n$t("What was the temperature outside during the survey? (°C)"),
                                   5, 40, 5, ticks = T))),
              
              radioButtons("weather_bird", i18n$t("What was the weather like during the survey? (Single choice)"),
                           choiceNames =
                             list(icon("sun"), icon("cloud-sun"), icon("cloud"), icon("cloud-rain")),
                           choiceValues = list("Sunny", "Sunny and cloudy", "Cloudy", "Rainy")),
       
              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              #bird instructions
              fluidPage(
                column(2),
                column(8, align = "center",
              h3(i18n$t("Enter your bird data. If you know how many individual birds of a species you saw, please enter the number. If you do not have numbers but know the species, please tick present:")))),
             
               HTML("<br/>"),
              
              #turdus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>","<br/>"), h4(i18n$t("Eurasian blackbird (Turdus merula)"))),
                
                column(3, align="left",
                       numericInput("turdus_no", label = h4(strong(i18n$t("Number:"))), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("turdus_pres", h4(strong(i18n$t("Present:"))),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              #Fringilla
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Common chaffinch (Fringilla coelebs)"))),
                
                column(3, align="left",
                       numericInput("fringilla_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("fringilla_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              #Parus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Great tit (Parus major)"))),
                
                column(3, align="left",
                       numericInput("parus_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("parus_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              #Cyanistes
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Blue tit (Cyanistes caeruleus)"))),
                
                column(3, align="left",
                       numericInput("cyanistes_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("cyanistes_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              #Passer domesticus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("House sparrow (Passer domesticus)"))),
                
                column(3, align="left",
                       numericInput("passer_dom_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("passer_dom_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              #Passer montanus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Tree sparrow (Passer montanus)"))),
                
                column(3, align="left",
                       numericInput("passer_mont_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("passer_mont_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              #Motacilla alba
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("White wagtail (Motacilla alba)"))),
                
                column(3, align="left",
                       numericInput("motacilla_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("motacilla_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              #Sturnus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Common starling (Sturnus vulgaris)"))),
                
                column(3, align="left",
                       numericInput("sturnus_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("sturnus_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              
              #Apus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Swift (Apus apus)"))),
                
                column(3, align="left",
                       numericInput("apus_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("apus_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              
              #Streptopelia
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Collared dove (Streptopelia decaocto)"))),
                
                column(3, align="left",
                       numericInput("streptopelia_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("streptopelia_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              #Erithacus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("European robin (Erithacus rubecula)"))),
                
                column(3, align="left",
                       numericInput("erithacus_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("erithacus_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              
              #Columba
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Wood pigeon (Columba palumbus)"))),
                
                column(3, align="left",
                       numericInput("columba_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("columba_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              
              #Phoenicurus
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Redstart (Phoenicurus phoenicurus)"))),
                
                column(3, align="left",
                       numericInput("phoenicurus_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("phoenicurus_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              
              #Pica
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Magpie (Pica pica)"))),
                
                column(3, align="left",
                       numericInput("pica_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("pica_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              
              #Sylvia arti
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Eurasian blackcap (Sylvia articapilla)"))),
                
                column(3, align="left",
                       numericInput("sylvia_arti_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("sylvia_arti_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              
              #Sylvia melano
              fluidRow(
                column(1),
                column(3, align="left", HTML("<br/>"), h4(i18n$t("Sardinian warbler (Sylvia melanocephala)"))),
                
                column(3, align="left",
                       numericInput("sylvia_melano_no", label = i18n$t(""), value = 0, width = "50%")),
                
                column(1, align="center",
                       checkboxGroupInput("sylvia_melano_pres", i18n$t(""),
                                          choiceNames = list(i18n$t("")), choiceValues = list("present")))),
              
              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              #Free-text entry of specific species
              fluidRow(
                column(2),
                column(8, align="center",
              textInput("birdSpecies_freeText", h4(i18n$t("Any additional species (3 most common and numbers) can be added below:")), 
                        placeholder = "E.g.: hooded crow (3), rook (4), common gull (2)", width = "100%"))),
              
              HTML("<br/>", "<br/>"), #whitespace
              
              fluidRow(
                column(2),
                column(8, align="center",
              actionButton("showBirds", i18n$t("Show bird data")))), #map button
              
              HTML("<br/>", "<br/>"), #whitespace
              
              plotOutput("birds_1", width = 780, height = 420),
              
              HTML("<br/>", "<br/>"), # white space
              
              div(class = "buttonagency",
                  actionBttn("info_bird_graph", i18n$t("Tip!"), style = "jelly", color = "primary")),
              
              HTML("<br/>", "<br/>"), # white space
              
              sliderInput(inputId = "feedback_birds", label = h4(i18n$t("How do you rate your experience with using this survey? (Use slider below)")),
                          1, 5, 3, ticks = F),
              
              imageOutput("bird_footer"),
              
      ),
      ##########################################
      ######        POLLINATORS 5x5       ######
      ##########################################
      tabItem(tabName = "pollinators", align = "center",
              
              imageOutput("plants_header"),
  
              ## title text
              fluidRow(
                column(1, align="right",
                       h1(i18n$t("S3"))),
                column(6, align="left",
                       h2(i18n$t("Pollinators and Flowering Plants Survey")))),
              
              h5(i18n$t("Below you can enter the data from the Pollinators and Flowers survey in the 5x5m survey site (S3):"), align = "left"),
              
              HTML("<br/>", "<br/>", "<br/>", "<br/>"), #whitespace
              
              # Entry of age and number
              h3(i18n$t("Participants and time:")),
              
              HTML("<br/>"), #whitespace
              fluidRow(
                column(1),
                column(5, align="center",
                       numericInput("no_partic_pollinators", label = i18n$t("How many participants are in your survey group?"), 
                                    value = 0, width = "75%")),
                
                column(5, align="center",
                       numericInput("age_partic_pollinators", label = i18n$t("What is the average age of the participants?"),
                                    value = 0, width = "75%"))),
              
              HTML("<br/>", "<br/>"), #whitespace
              
              ## Date and time
              fluidRow(
                column(1),
                column(5, align="center",
                       dateInput("date_pollinators", i18n$t("Select the date when the surveying took place:"), width = "75%")),
                
                column(5, align="center",
                       timeInput("time_pollinators", i18n$t("Enter the time when the surveying started (use the dials or enter the time directly):"),
                                 value = Sys.time(), seconds = FALSE))),
              
              HTML("<br/>","<br/>"), #whitespace
              
              h3(i18n$t("Weather and temperature:")),
              
              HTML("<br/>"), #whitespace
              
              fluidRow(
                column(6, align="center",
                       radioGroupButtons(inputId = "windLevel_pollinators", label = i18n$t("How windy was it during the survey? (From leaves being still to moving a lot)"),
                                         choices = c(`<i class='fa fa-wind' style='color:#ecf2f6; font-size: 12px;'></i>` = "Leaves not moving",
                                                     `<i class='fa fa-wind' style='color:#ecf2f6; font-size: 16px;'></i>` = "Leaves moving slightly", 
                                                     `<i class='fa fa-wind' style='color:#ecf2f6; font-size: 20px;'></i>` = "Leaves moving much"),
                                         justified = TRUE, status = "accent_blue", size = "lg", width = "50%")),
                
                column(6,
                       sliderInput("temp_pollinators", i18n$t("What was the temperature outside during the survey? (°C)"),
                                   5, 40, 5, ticks = T))),
              
              radioButtons("weather_pollinators", i18n$t("What was the weather like during the survey? (Single choice)"),
                           choiceNames =
                             list(icon("sun"), icon("cloud-sun"), icon("cloud"), icon("cloud-rain")),
                           choiceValues = list("Sunny", "Sunny and cloudy", "Cloudy", "Rainy")),
              
              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              h3(i18n$t("Enter the area (m2) of the following habitat in your 5x5 survey plot:")),
              HTML("<br/>","<br/>"),
              
              fluidRow(
                column(3),
                column(3, align="center",
                       numericInput("area_5x5_planted", label = h4(i18n$t("Plant beds or flowerpots")), value = 0, width = "75%")),
                
                
                column(3,
                       numericInput("area_5x5_damp", label = h4(i18n$t("Damp places")), value = 0, width = "75%"))
              ),
              
              fluidRow(
                column(3),
                column(3, align="center",
                       numericInput("area_5x5_woody", label = h4(i18n$t("Trees and shrubs")), value = 0, width = "75%")),
                
                
                column(3,
                       numericInput("area_5x5_shortGrass", label = h4(i18n$t("Short grass")), value = 0, width = "75%"))
              ),
              
              
              fluidRow(
                column(3),
                column(3, align="center",
                       numericInput("area_5x5_ground", label = h4(i18n$t("Bare ground")), value = 0, width = "75%")),
                
                
                column(3,
                       numericInput("area_5x5_walls", label = h4(i18n$t("Bare walls or fences")), value = 0, width = "75%"))
              ),
              
              fluidRow(
                column(3),
                column(3, align="center",
                       numericInput("area_5x5_anthro", label = h4(i18n$t("Man-made homes")), value = 0, width = "75%")),
                
                
                column(3,
                       numericInput("area_5x5_hardSurface", label = h4(i18n$t("Concrete or tarmac")), value = 0, width = "75%"))
              ),
              #PUT IN A CALCULATOR TO CHECK THAT IT EQUALS 25!
              
              
              
              HTML("<br/>","<br/>"), #whitespace
              
              # Entry of pollinators
              h3(i18n$t("Enter the total number of individuals and species you saw for each pollinator group:")),
              HTML("<br/>","<br/>"),
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>", "<br/>"), h4(i18n$t("Bumblebees"))),
                
                column(3, align="center",
                       numericInput("bombus_tot_individ_5x5", label = h4(i18n$t("Individuals:")), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("bombus_tot_species_5x5", label = h4(i18n$t("Species:")), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Honeybees"))),
                
                column(3, align="center",
                       numericInput("mellifera_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("mellifera_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Solitary bees"))),
                
                column(3, align="center",
                       numericInput("apis_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("apis_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Wasps"))),
                
                column(3, align="center",
                       numericInput("vespid_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("vespid_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Beetles"))),
                
                column(3, align="center",
                       numericInput("coleoptera_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("coleoptera_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("True bugs"))),
                
                column(3, align="center",
                       numericInput("hemiptera_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("hemiptera_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Butterflies"))),
                
                column(3, align="center",
                       numericInput("lepidoptera_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("lepidoptera_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Moths"))),
                
                column(3, align="center",
                       numericInput("moth_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("moth_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Hoverflies"))),
                
                column(3, align="center",
                       numericInput("syrphidae_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("syrphidae_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Other flies and mosquitos"))),
                
                column(3, align="center",
                       numericInput("other_diptera_tot_individ_5x5", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("other_diptera_tot_species_5x5", label = i18n$t(""), value = 0, width = "75%"))),
              
              HTML("<br/>", "<br/>"), #whitespace
              
              actionButton("showPollinators", i18n$t("Show pollinator data")), #button
              
              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              plotlyOutput("pollinators_1", width = 780, height = 420),
              
              HTML("<br/>", "<br/>"), # white space
              
              div(class = "buttonagency",
                  actionBttn("info_pollinator_graph", i18n$t("Tip!"), style = "jelly", color = "primary")),
              
              HTML("<br/>", "<br/>"), # white space
              
              #floweriness
              h3(i18n$t("Enter the data for the plants:")),
              
              HTML("<br/>", "<br/>"), #whitespace
              
              sliderInput(inputId = "floweriness_5x5", label = h4(i18n$t("Rate your 5x5m survey plot on the floweriness-scale:")),
                          1, 3, 1, ticks = F),       
              
              
              HTML("<br/>","<br/>", "<br/>"), #whitespace
              
              #flower species
              fluidRow(
                column(2),
                column(4, align="center", 
                       numericInput("flowers_tot_species_5x5", label = h4(i18n$t("How many different species of flowering plants did you find?")), 
                                    value = 0, width = "80%")),
                
                column(4, align="center", textInput("flower_species_freeText_5x5", label = i18n$t("(Optional) If you identified the species, write the three most common below"), 
                                                    placeholder = "E.g.: daisy, lavender, orchid", width = "80%"))),
              
              HTML("<br/>", "<br/>", "<br/>", "<br/>"), #whitespace
              sliderInput(inputId = "feedback_pollinators", label = h4(i18n$t("How do you rate your experience with using this survey? (Use slider below)")),
                          1, 5, 3, ticks = F),
              
              imageOutput("plants_footer"),
      ),
      ##########################################
      ######       INVERTEBRATES 1x1      ######
      ##########################################
      tabItem(tabName = "minibeasts",  align = "center",
              
              imageOutput("minibeast_header"),
              
              ## title text
              fluidRow(
                column(1, align="right",
                       h1(i18n$t("S4"))),
                column(6, align="left",
                       h2(i18n$t("Minibeasts and Leaves Survey")))),
              
              h5(id="minibeast-subhead", i18n$t("Below you can enter the data from the Minibeasts and Leaves survey in the 1x1m squares (S4):"), align = "left"),
              
              HTML("<br/>", "<br/>", "<br/>", "<br/>"), #whitespace
              
              # Entry of age and number
              h3(i18n$t("Participants and time:")),
              
              HTML("<br/>"), #whitespace
              fluidRow(
                column(1),
                column(5, align="center",
                       numericInput("no_partic_minibeast", label = i18n$t("How many participants are in your survey group?"), 
                                    value = 0, width = "75%")),
                
                column(5, align="center",
                       numericInput("age_partic_minibeast", label = i18n$t("What is the average age of the participants?"),
                                    value = 0, width = "75%"))),
              
              HTML("<br/>", "<br/>"), #whitespace
              
              ## Date and time
              fluidRow(
                column(1),
                column(5, align="center",
                       dateInput("date_minibeast", i18n$t("Select the date when the surveying took place:"), width = "75%")),
                
                column(5, align="center",
                       timeInput("time_minibeast", i18n$t("Enter the time when the surveying started (use the dials or enter the time directly):"),
                                 value = Sys.time(), seconds = FALSE))),
              
              HTML("<br/>","<br/>"), #whitespace
              
              h3(i18n$t("Weather and temperature:")),
              
              HTML("<br/>"), #whitespace
              
              fluidRow(
                column(6, align="center",
                       radioGroupButtons(inputId = "windLevel_minibeast", label = i18n$t("How windy was it during the survey? (From leaves being still to moving a lot)"),
                                         choices = c(`<i class='fa fa-wind' style='color:#ecf2f6; font-size: 12px;'></i>` = "Leaves not moving",
                                                     `<i class='fa fa-wind' style='color:#ecf2f6; font-size: 16px;'></i>` = "Leaves moving slightly", 
                                                     `<i class='fa fa-wind' style='color:#ecf2f6; font-size: 20px;'></i>` = "Leaves moving much"),
                                         justified = TRUE, status = "accent_blue", size = "lg", width = "50%")),
                
                column(6,
                       sliderInput("temp_minibeast", i18n$t("What was the temperature outside during the survey? (°C)"),
                                   5, 40, 5, ticks = T))),
              
              radioButtons("weather_minibeast", i18n$t("What was the weather like during the survey? (Single choice)"),
                           choiceNames =
                             list(icon("sun"), icon("cloud-sun"), icon("cloud"), icon("cloud-rain")),
                           choiceValues = list("Sunny", "Sunny and cloudy", "Cloudy", "Rainy")),
              
              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              numericInput("no_1x1_squares", label = i18n$t("How many 1x1m squares did you survey?"), 
                           value = 1, width = "25%"),
              
              HTML("<br/>","<br/>"), #whitespace
              
              #Free-text entry of 1x1 habitats
              textInput("habitat_1x1_freeText", i18n$t("What was the dominant habitat for each square? For each dominant habitat, write the numbers of squares with this habitat in brackets:"), 
                        placeholder = "E.g.: Flower beds or pots (1), tall grass and wildflowers (2), short grass (1)", width = "60%"),
              
              HTML("<br/>","<br/>"), #whitespace
              
              # Entry of pollinators
              h3(i18n$t("Enter the total number of individuals and species you saw for each minibeast group:")),
              HTML("<br/>","<br/>"),
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Earthworms"))),
                
                column(3, align="center",
                       numericInput("annelida_tot_indiv_1x1", label = i18n$t("Individuals:"), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("annelida_tot_species_1x1", label = i18n$t("Species:"), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Ants"))),
                
                column(3, align="center",
                       numericInput("hymenoptera_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("hymenoptera_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Centipedes and millipedes"))),
                
                column(3, align="center",
                       numericInput("myriapoda_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("myriapoda_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Woodlice"))),
                
                column(3, align="center",
                       numericInput("isopoda_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("isopoda_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Spiders and harvestmen"))),
                
                column(3, align="center",
                       numericInput("arachnid_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("arachnid_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Beetles"))),
                
                column(3, align="center",
                       numericInput("coleoptera_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("coleoptera_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("True bugs"))),
                
                column(3, align="center",
                       numericInput("hemioptera_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("hemiptera_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Insect larvae"))),
                
                column(3, align="center",
                       numericInput("caterpillar_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("caterpillar_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Snails and slugs"))),
                
                column(3, align="center",
                       numericInput("gastropoda_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("gastropoda_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              
              fluidRow(
                column(1),
                column(2, align="left", HTML("<br/>"), h4(i18n$t("Other minibeasts"))),
                
                column(3, align="center",
                       numericInput("other_tot_individ_1x1", label = i18n$t(""), value = 0, width = "75%")),
                
                column(3, align="center",
                       numericInput("other_diptera_tot_species_1x1", label = i18n$t(""), value = 0, width = "75%"))),
              
              HTML("<br/>", "<br/>"), #whitespace
              
              actionButton("showInvertebrates", i18n$t("Show minibeast data")), #map button
              
              HTML("<br/>", "<br/>", "<br/>"), #whitespace
              
              plotOutput("invertebrates_1", width = 780, height = 540),
              
              HTML("<br/>", "<br/>"), # white space
              
              div(class = "buttonagency",
                  actionBttn("info_minibeast_graph", i18n$t("Tip!"), style = "jelly", color = "primary")),
              
              
              HTML("<br/>","<br/>","<br/>","<br/>"), #whitespace
              
              
              #floweriness
              fluidRow(
                column(6, align="center",
                       sliderInput(inputId = "vegetation_cover_1x1", label = i18n$t("Rate the mean of your 1x1m survey plot on the vegetation cover-scale:"),
                                   1, 3, 1, ticks = F)),
                
                column(6, align="center",
                       numericInput("leaf_diversity_1x1", label = i18n$t("How many different types of leaves do you find in all 1x1m squares?"), 
                                    value = 0, width = "50%"))),
              
              
              HTML("<br/>", "<br/>"), #whitespace
              
              #feedback 
              sliderInput(inputId = "feedback_minibeasts", label = i18n$t("How do you rate your experience with using this survey? (Use slider below)"),
                          1, 5, 3, ticks = F),
              
              imageOutput("minibeast_footer"),
      ),
      ##########################################
      ######         SUBMIT DATA          ######
      ##########################################
      
      tabItem(tabName = "submit", align = "center",
              
              h3(i18n$t("Submit data")),
              
              HTML("<br/>", "<br/>"), # white space
              
              fluidRow(
                column(2),
                column(8, align = "center",
              h4(i18n$t("Thank you for completing the data entry! Please ensure that all answers are correct. You will then need to download the data and email it to our team at Lund University. Please send this file to anna.persson [at] cec.lu.se so your data can be included in the research project and added to the website. Thank you for your participation.")))),
              
              HTML("<br/>", "<br/>", "<br/>", "<br/>"), # white space
              #download data button
              downloadButton('downloadData', i18n$t('Download'), height ="150%"),
              
              HTML("<br/>", "<br/>", "<br/>", "<br/>"), # white space
              
              
                imageOutput("LtL_banner_dwn"),
      ),
      
       ##########################################
       ######          ABOUT US           ######
       #########################################
       tabItem(tabName = "about", align = "center",
               
               h3(i18n$t("About us")),
               
               HTML("<br/>", "<br/>"), # white space
               
               fluidRow(
                 column(2),
                 column(8, align = "center",
               h4(i18n$t("Natural Nations is an international school grounds biodiversity project funded by Erasmus+ and led by Learning through Landscapes in partnership with Naturskolan i Lund, Lund University, Birdlife Malta and Sociedad Espanola de Ornitologia SEO (Birdlife Spain). To help schools improve their school grounds for wildlife, the Natural Nations project has developed survey methodology to record pollinating insects, minibeasts and leaves, bird populations and habitats and vegetation with supporting educational and cultural heritage resources.")))),
               
               
               HTML("<br/>", "<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>", "<br/>"), # white space
               imageOutput("allLogo", height = 100), # banner logo)
               HTML("<br/>", "<br/>", "<br/>", "<br/>", "<br/>"),
               imageOutput("nbisLogo", height = 100), # banner logo)
               HTML("<br/>", "<br/>")
       
      ) 
      #########################################
    ),
    useSweetAlert(), 
    useShinyjs()) #custom layou for navbar
  
)

server <- function(input, output, session) {
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When country is selected, this will feed into shiny.i18n for translatio
  observeEvent(input$countrySelect, {
    update_lang(session, input$selected_language)
    
  })
  
  i18n_r <- reactive({ #extra function that reads in a new translated function
    i18n
  })
  
  observe({
    updateSelectInput(session, "dominant_habitat", 
                      label = i18n_r()$t('What is the dominant habitat in your school grounds (select one):'),
                      choices = i18n_r()$t(c("","Plant beds or flowerpots", "Tall grass, wildflowers", "Trees and shrubs", "Bare ground",
                                             "Man-made homes", "Damp places", "Short grass", "Bare walls or fences", "Concrete or tarmac")))
    updateTextInput(session, "plantSpecies_freeText", i18n_r()$t("Enter the three most common species and numbers below:"), 
                    placeholder = i18n_r()$t("E.g.: oak (7), birch (4), Taxus baccata (2)"))
    updateTextInput(session, "birdSpecies_freeText", i18n_r()$t("Any additional species (3 most common and numbers) can be added below:"), 
                    placeholder = i18n_r()$t("E.g.: hooded crow (3), rook (4), common gull (2)"))
    updateTextInput(session, "flower_species_freeText_5x5", i18n_r()$t("(Optional) If you identified the species, write the three most common below"), 
                    placeholder = i18n_r()$t( "E.g.: daisy, lavender, orchid"))
    updateTextInput(session, "habitat_1x1_freeText", i18n_r()$t("What was the dominant habitat for each square? For each dominant habitat, write the numbers of squares with this habitat in brackets:"), 
                    placeholder = i18n_r()$t("E.g.: Flower beds or pots (1), tall grass and wildflowers (2), short grass (1)"))
  })

  

  # When the Submit button is clicked, save the form data BEES
  observeEvent(input$showMap, {
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
  
  observeEvent(input$info_map, {
    sendSweetAlert(
      session = session,
      title = "Information",
      text = i18n$t("The map is interactive. Try zooming in on your country to see which other schools have participated. Hover with the mouse to display names. Did you enter the wrong coordinates? Just re-enter the correct ones and press the Show map-button again!"),
      type = "info"
    )
  })
  
  observeEvent(input$info_habitat_graph_1, {
    sendSweetAlert(
      session = session,
      title = i18n$t("About the graph"),
      text = i18n$t("This graph shows the most common habitat on school grounds across all participating schools. Question: Was this what you were expecting? Why do you think school grounds look the way they do today? \n About the graph: This type of graph is called a barplot. The higher the bars, the more schools have selected this answer."),
      type = "info"
    )
  })
  
  observeEvent(input$info_habitat_graph_2, {
    sendSweetAlert(
      session = session,
      title = i18n$t("About the graph"),
      text = i18n$t("This graph shows the number of different tree and shrub species recorded by each school and how large the school grounds are. Question: Do you see any pattern? Do you need a large school ground to have many species of plants? About the graph: This type of graph is called a scatter plot. A dot high up shows that this school has a lot of species. A dot far to the right shows the school grounds are large."),
      type = "info"
    )
  })
  
  observeEvent(input$info_bird_graph, {
    sendSweetAlert(
      session = session,
      title = i18n$t("About the graph"),
      text = i18n$t("This graph shows the number of birds seen by each school and how many trees and shrubs that have been recorded. Question: Is there any pattern? Would you have seen more birds if you had more trees? About the graph: This type of graph is called a scatter plot. A dot high up shows that this school has a lot of birds. A dot far to the right shows the school grounds have many trees and shrubs."),
      type = "info"
    )
  })
  
  observeEvent(input$info_pollinator_graph, {
    sendSweetAlert(
      session = session,
      title = i18n$t("About the graph"),
      text = i18n$t("This graph shows which pollinator group was the most common at your school. Question: Which was the most common pollinator at your school? Were there any groups you did not see and if so, is there anything you can do to help them? About the graph: This type of graph is called a pie-chart. It show the proportion of pollinators recorded in percent. Hover with the mouse to see the absolute values (number of individuals)."),
      type = "info"
    )
  })
  
  observeEvent(input$info_minibeast_graph, {
    sendSweetAlert(
      session = session,
      title = i18n$t("About the graph"),
      text = i18n$t("This graph shows the number of minibeasts per square metre for each school. Question: What does it mean to have a lot of minibeast per square meter? Which school has seen the most minibeasts in total? About the graph: This type of graph is called a bar plot. The higher the bars, the more minibeasts have been recorded per square meter."),
      type = "info"
    )
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
    
    
  }, bg="transparent")
  
  
  # render correlation tree species ~ yard
  output$yard_1 <- renderPlot({
    
    input$showYard
    loadData()
    plot_func2_yard()
    
    
  }, bg="transparent")
  
  output$birds_1 <- renderPlot({
    
    input$showBirds
    loadData()
    plot_func3_birds()
    
    
  }, bg="transparent")
  
  output$pollinators_1 <- renderPlotly({
    
    input$showPollinators
    loadData()
    plot_func4_pollinators()
    
    
  })
  
  output$invertebrates_1 <- renderPlot({
    
    input$showInvertebrates
    loadData()
    plot_func5_invertebrates()
    
    
  }, bg="transparent")
  

  # Download button
  
  output$downloadData <- downloadHandler(
    filename = function() {paste('data-', Sys.Date(), '-', length(responses$School), '.csv', sep='')
    },
    content = function(file) {write.csv2(t(formData()), file)
    }
  )
  
  # render natnat logo
  output$natNatLogo <- renderImage({list(src = "./logo2.png",
                                      contentType = "image/png",alt = "logo", height = 120)}, deleteFile = FALSE)
  
  # render footer logo
  output$footerLogo <- renderImage({list(src = "./footer.png",
                                         contentType = "image/png",alt = "logo", height = 120)}, deleteFile = FALSE)
  
  
  
  # render logo
  output$allLogo <- renderImage({list(src = "./logo1.png",
                                      contentType = "image/png",alt = "logo", height = 200)}, deleteFile = FALSE)
  
  output$nbisLogo <- renderImage({list(src = "./nbisLogo.png",
                                      contentType = "image/png",alt = "logo", height = 120)}, deleteFile = FALSE)
  
  # render banners
  output$bird_footer <- renderImage({list(src = "./banners/bird_footer.png",
                                             contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  output$habitat_footer <- renderImage({list(src = "./banners/habitat_footer.png",
                                          contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  output$minibeast_footer <- renderImage({list(src = "./banners/minibeast_footer.png",
                                             contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  output$plants_footer <- renderImage({list(src = "./banners/plants_footer.png",
                                             contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  output$LtL_banner_dwn <- renderImage({list(src = "./banners/LtLBanner_dwn.png",
                                       contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  
  
  output$bird_header <- renderImage({list(src = "./banners/bird_header.png",
                                          contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  output$habitat_header <- renderImage({list(src = "./banners/habitat_header.png",
                                             contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  output$minibeast_header <- renderImage({list(src = "./banners/minibeast_header.png",
                                               contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  output$plants_header <- renderImage({list(src = "./banners/plants_header.png",
                                            contentType = "image/png",alt = "banner", height = 200)}, deleteFile = FALSE)
  
  

  runjs({'
        var el2 = document.querySelector(".skin-blue");
    el2.className = "skin-blue sidebar-mini";
    var clicker = document.querySelector(".sidebar-toggle");
    clicker.id = "switchState";
    '})
  
  onclick('switchState', 
          
          runjs({'
    var title = document.querySelector(".logo")
    if (title.style.visibility == "hidden") {
    title.style.visibility = "visible";
    } else {
    title.style.visibility = "hidden";}
    }
    '}))
  
}

shinyApp(ui, server)
