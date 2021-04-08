plot_func1_habitat<- function() {
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


# test data
School <- c("Vastraskolan", "Norraskolan", "Sodraskolan", "Ostraskolan") 
dominant_habitat <- c("damp", "sand", "sand", "concrete")

test <- data.frame(School,dominant_habitat)

plotDataHabitat <- test %>%
                        count(dominant_habitat)

ggplot(plotDataHabitat, aes(x = reorder(dominant_habitat,-n), y=as.numeric(n), 
                fill = dominant_habitat, color = dominant_habitat)) +
  geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
  theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
  scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitat$dominant_habitat), type = "continuous")) +
  scale_y_continuous(limits = c(0,5), expand = c(0,0)) +
  labs(y="Temperature (°C)", x="", title = "") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=20, family= "Times"), 
        axis.text.x = element_text(size = 20, angle = 45,
                                   hjust = 1, color = "grey1")) +
  theme(axis.ticks.length=unit(.25, "cm"))



School <- c("Vastraskolan", "Norraskolan", "Sodraskolan", "Ostraskolan") 
yard_size <- c(1000, 833, 1240, 600)
trees_tot_species <- c("12","2","5","7")
shrubs_tot_species <- c("5","6","10","5")

testData2 <- data.frame(School,yard_size,trees_tot_species,shrubs_tot_species)


School <- as.character(testData2$School)
yard <- as.numeric(testData2$yard_size)
WoodySpecies <- as.numeric(testData2$trees_tot_species) + as.numeric(testData2$shrubs_tot_species)

plotDataHabitatTrees <- data.frame(School,yard,WoodySpecies)



ggplot(plotDataHabitatTrees, aes(x = yard, y=WoodySpecies, 
                            fill = School, color = School)) +
  geom_point(aes(fill=School), colour="black", size=5, alpha = 0.9, pch=21) +
  theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
  scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) +  scale_x_continuous(limits = c(500,2000), expand = c(0,0)) +
  labs(y="Species richness (number of woody species) \n", x="School grounds size (m2)", title = "") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=20, family= "Times"), 
        axis.text.x = element_text(size = 20, angle = 45,
                                   hjust = 1, color = "grey1")) +
  theme(axis.ticks.length=unit(.25, "cm"))




School <- c("Vastraskolan", "Norraskolan", "Sodraskolan", "Ostraskolan") 
yard_size <- c(1000, 833, 1240, 600)
trees_tot_species <- c("12","2","5","7")
shrubs_tot_species <- c("5","6","10","5")


turdus_no <- 3
fringilla_no <-5
parus_no <- 0
cyanistes_no <-8
passer_dom_no <-0
passer_mont_no <- 1
motacilla_no <-1
sturnus_no <- 1
apus_no <- 3
streptopelia_no <- 5
erithacus_no <- 0
columba_no <- 9
phoenicurus_no <-8
pica_no <- 0
sylvia_arti_no <- 1
sylvia_melano_no <-0


BirdSum <- sum(c(turdus_no,fringilla_no,parus_no,cyanistes_no,passer_dom_no,passer_mont_no,motacilla_no,motacilla_no,
                 sturnus_no,apus_no,streptopelia_no,erithacus_no,columba_no,phoenicurus_no,pica_no,sylvia_arti_no,
                 sylvia_melano_no))

School <- "Vastra"
WoodySpecies <- plotDataHabitatTrees$WoodySpecies[1]

plotDataBirds <- data.frame(School, WoodySpecies, BirdSum)
  
  
ggplot(plotDataBirds, aes(x = WoodySpecies, y=BirdSum, 
                                 fill = School, color = School)) +
  geom_point(aes(fill=School), colour="black", size=5, alpha = 0.9, pch=21) +
  theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
  scale_color_manual(values=wes_palette("Moonrise3", length(plotDataHabitatTrees$School), type = "continuous")) +
  scale_y_continuous(limits = c(0,150), expand = c(0,0)) +  scale_x_continuous(limits = c(0,50), expand = c(0,0)) +
  labs(y="Total number of birds observed \n", x="Number of tree and shrub species", title = "") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=20, family= "Times"), 
        axis.text.x = element_text(size = 16, angle = 45,
                                   hjust = 1, color = "grey1")) +
  theme(axis.ticks.length=unit(.25, "cm"))



x5x5_bombus_tot_individ <- 0
x5x5_mellifera_tot_individ <-1
x5x5_apis_tot_individ <-5
x5x5_vespid_tot_individ <-0
x5x5_coleoptera_tot_individ <-1
x5x5_hemiptera_tot_individ <-4
x5x5_lepidoptera_tot_individ <-6
x5x5_moth_tot_individ <-3
x5x5_syrphidae_tot_individ <-2
x5x5_other_diptera_tot_individ <-0


testdata4 <- t(data.frame(x5x5_bombus_tot_individ,x5x5_mellifera_tot_individ,x5x5_apis_tot_individ,x5x5_vespid_tot_individ,
                          x5x5_coleoptera_tot_individ,x5x5_hemiptera_tot_individ,x5x5_lepidoptera_tot_individ,
                          x5x5_moth_tot_individ,x5x5_syrphidae_tot_individ,x5x5_other_diptera_tot_individ))

testdata4 <- as.data.frame(testdata4)
 
testdata4$newName <- c("Bumblebees", "Honeybees", "Solitary bees", "Wasps", "Beetles", "True bugs", "Butterflies", "Moths",
                        "Hoverflies", "Other pollinators")
colnames(testdata4) <- c("Number", "ShowName")



bp <- ggplot(testdata4, aes(x="", y=reorder(Number,Number), fill = reorder(PollinatorSpecies, Number)))+
  geom_bar(width = 1, stat = "identity") + labs(y="", x="")
pie <- bp + coord_polar("y", start=0)
pie + theme_void() + theme(legend.position = "none")





USPersonalExpenditure <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[, c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('$', X1960, ' billions'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig



fig <- plot_ly(plotDataPollinators, labels = ~ShowName, values = ~Number, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(Number, ' individuals observed'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1),showlegend = FALSE))
fig <- fig %>% layout(title = 'The most common pollinators by occurence on your school:',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig



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




x1x1_annelida_tot_species <-0
x1x1_hymenoptera_tot_species <-0
x1x1_myriapoda_tot_species <-1
x1x1_isopoda_tot_species <-8
x1x1_arachnid_tot_species <-5
x1x1_coleoptera_tot_species <-1
x1x1_hemiptera_tot_species <-2
x1x1_caterpillar_tot_species <-7
x1x1_gastropoda_tot_species <-3
x1x1_other_diptera_tot_species <-4

no_1x1_squares <- 5

invertebrateSum <- sum(c(x1x1_annelida_tot_species,x1x1_hymenoptera_tot_species,x1x1_myriapoda_tot_species,
                         x1x1_isopoda_tot_species,x1x1_arachnid_tot_species,x1x1_coleoptera_tot_species,
                         x1x1_hemiptera_tot_species,x1x1_caterpillar_tot_species,x1x1_gastropoda_tot_species,
                         x1x1_other_diptera_tot_species))

invertebrateMeanPerSquare <- as.numeric(invertebrateSum)/as.numeric(no_1x1_squares)
School <- "Vastra"


plotDataInvertebrates <- data.frame(School, invertebrateMeanPerSquare)


ggplot(plotDataInvertebrates, aes(x = School, y=invertebrateMeanPerSquare, 
                                 fill = School, color = School)) +
  geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
  theme_classic() + scale_fill_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), type = "continuous")) +
  scale_color_manual(values=wes_palette("Moonrise3", length(plotDataInvertebrates$School), type = "continuous")) +
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) +
  labs(y="Mean number of minibeasts per 1x1 square \n", x="", title = "") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=20, family= "Times"), 
        axis.text.x = element_text(size = 20, angle = 45,
                                   hjust = 1, color = "grey1")) +
  theme(axis.ticks.length=unit(.25, "cm"))



plotDataInvertebrates <- ddply(responses, c("School"), summarise,  #calculate trees by class per territory
                               invertebrateSum = (`1x1_annelida_tot_species` + `1x1_hymenoptera_tot_species`+
                                                    `1x1_myriapoda_tot_species` + `1x1_isopoda_tot_species`+
                                                    `1x1_arachnid_tot_species`+`1x1_coleoptera_tot_species`+
                                                    `1x1_hemiptera_tot_species`+`1x1_caterpillar_tot_species`+
                                                    `1x1_gastropoda_tot_species`+`1x1_other_diptera_tot_species`))
