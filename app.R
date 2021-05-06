# packages
library(shiny)
library(tidyverse)
library(maps)
library(mapproj)
library(plotly)

#------------------------INIT---------------------------
#------------------------SID-----------------------------
# data
yearly_data = readRDS("data/crop_dat.rds")
yearly_data$Year <- as.factor(yearly_data$Year)
# to get the vector of choices
years <- unique(yearly_data$Year)
crops <- colnames(yearly_data)[6:92]
sites <- unique(yearly_data$SiteName.x)
states <- unique(yearly_data$State.x)
# map data
map_state_data <- map_data("state")
colnames(map_state_data)[1] <- 'Lon'
colnames(map_state_data)[2] <- 'Lat'
# to only plot the states which have the aphid sites
state_name <- c("iowa","illinois","indiana","kansas","kentucky","michigan","minnesota","missouri","south dakota","wisconsin")
map_state_data <- map_state_data[map_state_data$region %in% state_name,]

#-----------------------Me----------------------------
model_dat = readRDS("data/datForApp.rds")




#----------------------APP--------------------
ui <- fluidPage(
  
    # multi-tabs
    tabsetPanel(
      
      # tab 1 - crop data
      tabPanel("Visualize Crop Data", fluid = TRUE,
               
               titlePanel("Aphid Project: This Tab - Crop Data Visualization!"),
               

               # div 1
               div(
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     textOutput("generic_text"),
                     selectInput("year_map",label = "YEAR for map",choices = years),
                     selectInput("crop_map",label = "CROP for map",choices = crops)
                   ),
                   mainPanel(
                     width = 9,
                     textOutput("\n\n\n"),
                     textOutput("text_map"),
                     plotOutput("crop_map",brush = "plot_brush"),
                     tableOutput("map_brush")
                   )
                 )
               ),
               
               # div 2
               div(sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   selectInput("site_lineplot",label = "SITE for trend of crop",choices = sites),
                   selectInput("crop_lineplot",label = "CROP for trend of crop", choices = crops)
                 ),
                 mainPanel(
                   width = 9,
                   textOutput("text_lineplot"),
                   plotOutput("crop_lineplot",click="plot_click"),
                   verbatimTextOutput("info")),
               )
               ),
               
               # div 3
               div(sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   selectInput("year_barplot",label = "YEAR for crop in sites",choices = years),
                   selectInput("state_barplot",label = "STATE for crop in sites",choices = states),
                   selectInput("crop_barplot",label = "CROP for crop in sites",choices = crops)
                 ),
                 mainPanel(
                   width = 9,
                   textOutput("text_barplot"),
                   plotOutput("crop_barplot")
                 )
               )
               )
      ),
      
      
      # tab 2 - model vis
      tabPanel("Visualize Model", fluid = TRUE,
               titlePanel("See the predictions of our aphid diversity model!"),
               # panel to select state and year
               div(sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   selectInput("state_pick",label = "Select State", choices = states),
                   selectInput("year_pick",label = "Select Year", choices = years),
                 ),
                 mainPanel(
                   width = 9,
                   textOutput("model_text"),
                   plotOutput("model_plot")
                 )
               )
               )
               
      )
    )
    
  ) 

  server <- function(input, output) {
    
    #-----------------------Mine-----------------------------
    output$model_text <- renderText({
      paste("Visualizing model fit for state:  ", input$state_pick, "\tin year:  ", input$year_pick)
    })
    
    output$generic_text <- renderText({
      paste("Welcome to our Shiny App! It's a work in progress. Right now it allows you to visualize our crop data (see selections below and plots to the right), and check out our aphid diversity model's predictions (see other tab at the top). Enjoy!")
    })
    
    output$model_plot <- renderPlot({
      colorVars <- c("Observed (smoothed)"="red","Model Prediction"="darkblue")
      model_dat %>% 
        filter(State.x == input$state_pick) %>%
        filter(Year == input$year_pick) %>% 
        ggplot() +
          geom_smooth(aes(x = Week, y = diversity, color="Observed (smoothed)"), size=2) +
          geom_point(aes(x = Week, y = Fitted, color="Model Prediction"), alpha = 0.4, size=1) +
          facet_wrap(~SiteName.x) +
          scale_color_manual(name="Source:", values=colorVars) +
          theme( legend.position = "bottom")
        
      
    })
    
    
    #-----------------------SID'S-------------------------------
    output$text_lineplot <- renderText({
      paste(input$crop_lineplot," trend at site ",input$site_lineplot)
    })
    
    output$crop_lineplot <- renderPlot({
      yearly_data %>%
        filter(SiteName.x == input$site_lineplot) %>%
        ggplot(aes(x = Year,y = !!sym(input$crop_lineplot),group=1)) +
        geom_line(colour = "darkgreen",size=1) +
        geom_point(colour = "darkgreen",size=2)
    })
    
    output$info <- renderText({
      paste0("x=",input$plot_click$x,"\ny=",input$plot_click$y)
    })
    
    output$text_barplot <- renderText({
      paste("Amount of Vegetation present at different sites in ",input$state_barplot," States for the year ",input$year_barplot)
    })
    
    output$crop_barplot <- renderPlot({
      yearly_data %>%
        filter(Year==input$year_barplot, State.x==input$state_barplot) %>%
        ggplot(aes(x=`SiteName.x`,y=!!sym(input$crop_barplot))) +
        geom_bar(stat = "identity",fill="darkgreen")
    })
    
    output$text_map <- renderText({
      paste("Quantity of Vegetation present at Sites for the year ",input$year_map)
    })
    
    output$crop_map <- renderPlot({
      data_year <- yearly_data %>%
        filter(Year==input$year_map)
      ggplot(data = map_state_data) +
        geom_polygon(aes(x=Lon,y=Lat,group=group),fill='white',color = 'black')+
        geom_point(data = data_year, aes(x=Lon,y = Lat,size = !!sym(input$crop_map),alpha = !!sym(input$crop_map)), color = 'darkgreen') +
        geom_text(data = data_year,aes(x=Lon-0.7,y = Lat+0.1, label = SiteName.x))
    })
    
    output$map_brush <- renderTable({
      data_year <- yearly_data %>%
        filter(Year==input$year_map)
      n = nrow(brushedPoints(data_year[,c('Year','State.x','SiteName.x','Lat','Lon',input$crop_map)], brush = input$plot_brush))
      if(n==0)
        return()
      else
        brushedPoints(data_year[,c('Year','State.x','SiteName.x','Lat','Lon',input$crop_map)], brush = input$plot_brush)
    })
    
  }


shinyApp(ui = ui, server = server)