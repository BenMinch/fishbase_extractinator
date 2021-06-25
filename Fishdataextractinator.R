
library(shiny)
library(rfishbase)
library(plyr)
library(rAverage)
library(gridExtra)
library(ggplot2)
library(ggpubr)
ui <- fluidPage(

    # Application title
    titlePanel("Fishbase Data Extractinator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("ecosystem",label = h3("Ecosystem"), value = "Chesapeake Bay")
        ,
        p("For a full list of ecosystems, see "),
        a("https://fishbase.mnhn.fr/search.php"),
            selectInput("xaxis",label = h3("X axis"), choices = c("TL","BD","ED","Max_Length","Trophic_Level","Max_Age","PredPrey_Ratio","Body_Weight", "Brain_Weight","Temp_Pref_Mean", "Temp_Pref_Max", "Max_Depth"),selected = "TL"),
        selectInput("yaxis",label = h3("Y axis"), choices = c("TL","BD","ED","Max_Length","Trophic_Level","Max_Age","PredPrey_Ratio","Body_Weight", "Brain_Weight","Temp_Pref_Mean", "Temp_Pref_Max", "Max_Depth"), selected = "TL")
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ecoPlot"),
           tableOutput("sample"),
           plotOutput("wholeplot",width = "200%")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

    output$ecoPlot <- renderPlot({
        # background code
        library(rfishbase)
        library(plyr)
        library(rAverage)
        library(gridExtra)
        ##Insert List of Species you want to analyze
        big_list<-species_by_ecosystem(input$ecosystem)
        #Output sample size
        sample_size<- data.frame(nrow(big_list))
        colnames(sample_size)<- c("Sample Size")
        ss<- grid.table(sample_size)
        ##Doing all the data mining
        b_brain<- brains(big_list$Species)
        b_ecology<- ecology(big_list$Species)
        b_ecosystem<- ecosystem(big_list$Species)
        b_estimates<- estimate(big_list$Species)
        b_fecundidty<-fecundity(big_list$Species)
        b_larvae<- larvae(big_list$Species)
        b_length<-length(big_list$Species)
        b_morphology<- morphology(big_list$Species)
        b_morphometrics<- morphometrics(big_list$Species)
        b_popchar<- popchar(big_list$Species)
        b_popgrowth<-popgrowth(big_list$Species)
        b_predatprs<- predators(big_list$Species)
        b_reproduction<- reproduction(big_list$Species)
        b_spawning<- spawning(big_list$Species)
        b_stocks<-stocks(big_list$Species)
        b_swimming<- swimming(big_list$Species)
        b_genetics<-genetics(big_list$Species)
        b_morphometrics2<-ddply(b_morphometrics, "Species", numcolwise(mean))
        b_brain2<-ddply(b_brain, "Species", numcolwise(mean))
        b_estimates2<- ddply(b_estimates, "Species", numcolwise(mean))
        b_genetics2<-ddply(b_genetics, "Species",numcolwise(mean))
        #Making Trends
        b_trends<- data.frame(b_estimates2$Species, b_morphometrics2$TL, 
                              b_morphometrics2$BD, b_morphometrics2$ED, 
                              b_estimates2$MaxLengthTL,b_estimates2$Troph, 
                              b_estimates2$AgeMax, b_estimates2$PredPreyRatioMax,
                              b_brain2$BodyWeight, b_brain2$BrainWeight,
                              b_estimates2$TempPrefMean, b_estimates2$TempPrefMax,
                              b_estimates2$DepthMax, b_genetics2$ChromosomeNo)
        colnames(b_trends)<- c("Species", "TL", "BD", "ED",
                               "Max_Length", "Trophic_Level",
                               "Max_Age", "PredPrey_Ratio", "Body_Weight",
                               "Brain_Weight", "Temp_Pref_Mean", "Temp_Pref_Max",
                               "Max_Depth", "Chromosome_No")
        xa<-input$xaxis
        ya<-input$yaxis
        ##Remove Outliers
        
        # draw the histogram with the specified number of bins
      gg<- ggplot(b_trends, aes_string(x=xa, y=ya))
      gg<- gg+ geom_point()+ggtitle(input$ecosystem)
      gg<-gg+ geom_smooth(method = "lm")+
          stat_regline_equation( (aes(label=..eq.label..)), label.x.npc = "left",label.y.npc = "top")+
          stat_regline_equation((aes(label= ..rr.label..)),label.x.npc = "middle", label.y.npc = "top")
      gg
    })
    output$sample<- renderTable({
        big_list<-species_by_ecosystem(input$ecosystem)
        sample_size<- data.frame(nrow(big_list))
        colnames(sample_size)<- c("Sample Size")
        ss<- grid.table(sample_size)
        print(sample_size)
    })
    output$wholeplot<-renderPlot({
        big_list<-species_by_ecosystem(input$ecosystem)
        #Output sample size
        sample_size<- data.frame(nrow(big_list))
        colnames(sample_size)<- c("Sample Size")
        ss<- grid.table(sample_size)
        ##Doing all the data mining
        b_brain<- brains(big_list$Species)
        b_ecology<- ecology(big_list$Species)
        b_ecosystem<- ecosystem(big_list$Species)
        b_estimates<- estimate(big_list$Species)
        b_fecundidty<-fecundity(big_list$Species)
        b_larvae<- larvae(big_list$Species)
        b_length<-length(big_list$Species)
        b_morphology<- morphology(big_list$Species)
        b_morphometrics<- morphometrics(big_list$Species)
        b_popchar<- popchar(big_list$Species)
        b_popgrowth<-popgrowth(big_list$Species)
        b_predatprs<- predators(big_list$Species)
        b_reproduction<- reproduction(big_list$Species)
        b_spawning<- spawning(big_list$Species)
        b_stocks<-stocks(big_list$Species)
        b_swimming<- swimming(big_list$Species)
        b_genetics<-genetics(big_list$Species)
        b_morphometrics2<-ddply(b_morphometrics, "Species", numcolwise(mean))
        b_brain2<-ddply(b_brain, "Species", numcolwise(mean))
        b_estimates2<- ddply(b_estimates, "Species", numcolwise(mean))
        b_genetics2<-ddply(b_genetics, "Species",numcolwise(mean))
        #Making Trends
        b_trends<- data.frame(b_estimates2$Species, b_morphometrics2$TL, 
                              b_morphometrics2$BD, b_morphometrics2$ED, 
                              b_estimates2$MaxLengthTL,b_estimates2$Troph, 
                              b_estimates2$AgeMax, b_estimates2$PredPreyRatioMax,
                              b_brain2$BodyWeight, b_brain2$BrainWeight,
                              b_estimates2$TempPrefMean, b_estimates2$TempPrefMax,
                              b_estimates2$DepthMax, b_genetics2$ChromosomeNo)
        colnames(b_trends)<- c("Species", "TL", "BD", "ED",
                               "Max_Length", "Trophic_Level",
                               "Max_Age", "PredPrey_Ratio", "Body_Weight",
                               "Brain_Weight", "Temp_Pref_Mean", "Temp_Pref_Max",
                               "Max_Depth", "Chromosome_No")
        plot(b_trends)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
