library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(shinythemes)
library(ggthemes)
library(DT)
library(htmltools)
library(plotly)
library(gridExtra)

plots <- read_csv("../finalData/Aerial.csv") %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           crs = 4326, agr = "field")
plot1 <- read_csv("../finalData/Aerial.csv") %>% 
  select(LONGITUDE, LATITUDE, seesAOIplot)

pictures <- read_csv("../finalData/pictures.csv") %>% 
  st_as_sf(coords = c("landcoversMeasurementLongitude", "landcoversMeasurementLatitude"), 
           crs = 4326, agr = "field")
picture1 <- read_csv("../finalData/pictures.csv") %>% 
  select(landcoversMeasurementLongitude, landcoversMeasurementLatitude, OBJECTID)

mosquito <- read_csv("../finalData/mosquitoLocation.csv") %>% 
  st_as_sf(coords = c("MeasuredLongitude", "MeasuredLatitude"), 
           crs = 4326, agr = "field")
mosquito1 <- read_csv("../finalData/mosquitoLocation.csv") %>% 
  select(MeasuredLongitude, MeasuredLatitude, OBJECTID)

Aerial <- read_csv("../finalData/Aerial.csv") 

# Tab2
location <- read_csv("../finalData/location.csv")
downward <- read_csv("../finalData/downward.csv")
east <- read_csv("../finalData/east.csv")
north <- read_csv("../finalData/north.csv")
south <- read_csv("../finalData/south.csv")
upward <- read_csv("../finalData/upward.csv")
west <- read_csv("../finalData/west.csv")
feature <- read_csv("../finalData/feature.csv")

# Tab3
mosLocation <- read_csv("../finalData/mosquitoLocation.csv")
mosFeatures <- read_csv("../finalData/mosquitoFeatures.csv") 
mosURL <- read_csv("../finalData/mosquitoURLs.csv", na = "null")

# ShinyApp

ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Adopt a Pixel"),
  tabsetPanel(type = "pills",
              tabPanel("Plots",
                       fluidRow(
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("plotnum", "Choose one or more interest areas:", Aerial$seesAOIplot, multiple = TRUE),
                             actionLink("link1", "Give me a Hint"),
                             textOutput("text1")
                             
                           ),
                           mainPanel(
                             leafletOutput("map")
                           )
                           
                         )
                       ),
                       fluidRow(
                         tabsetPanel(
                                     tabPanel("Table",
                                              dataTableOutput("des1"), 
                                              downloadButton("dldata1", "Download Data")),
                                     tabPanel("Pie Chart",
                                      fluidRow(column(1),
                                               column(10, 
                                                      tags$div(
                                                        tags$h4("How to read below pie chart?"), 
                                                        "The pie chart shows the proportion of each feature in each plot. 
You can see the plot number which you are interested in above each pie chart. 
Eg. For plot 203, you will know the proportion of trees in this plot is 25.6%, the proportion of grass is 31.4% etc.")),
                                               column(1)
                                        ),
                                                fluidRow(plotOutput("pie", width = "100%", height = "800px"),
                                                         downloadButton("dlplot1", "Download Plot"))),
                                     tabPanel("Bar Chart",
                                              fluidRow(column(1),
                                                       column(10, 
                                                              tags$div(
                                                                tags$h4("How to read below bar chart?"), 
                                                                "The bar chart can compare the proportion of each feature in all selected plots.
                                                                You can compare each feature of all plots together."),
                                                              ),
                                                       column(1)
                                              ),
                                              fluidRow(plotlyOutput("bar", width = "80%", height = "600px"),
                                                       downloadButton("dlplot2", "Download Plot")))
                                     )
                         )
                       ),
              tabPanel("Pictures",
                       fluidRow(
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("ObjectID", "Choose one or more interest areas:", location$OBJECTID, multiple = TRUE),
                             actionLink("link2", "Give me a Hint"),
                             textOutput("text2")
                             
                           ),
                           mainPanel(
                             leafletOutput("map2")
                           )
                         )
                       ),
                       fluidRow(
                         tabsetPanel(
                           tabPanel("Location",
                                    dataTableOutput("location"),
                                    downloadButton("dldata21", "Download Data")),
                           tabPanel("Features",
                                    dataTableOutput("feature"),
                                    downloadButton("dldata22", "Download Data")),
                           tabPanel("North",
                                    fluidRow(dataTableOutput("north")),
                                    fluidRow(downloadButton("northdl", "Download as csv"))),
                           tabPanel("South",
                                    dataTableOutput("south"),
                                    fluidRow(downloadButton("southdl", "Download as csv"))),
                           tabPanel("East",
                                    dataTableOutput("east"),
                                    fluidRow(downloadButton("eastdl", "Download as csv"))),
                           tabPanel("West",
                                    dataTableOutput("west"),
                                    fluidRow(downloadButton("westdl", "Download as csv"))),
                           tabPanel("Downward",
                                    dataTableOutput("downward"),
                                    fluidRow(downloadButton("downwarddl", "Download as csv"))),
                           tabPanel("Upward",
                                    dataTableOutput("upward"),
                                    fluidRow(downloadButton("upwarddl", "Download as csv")))
                         )
                       )

                       ),
              tabPanel("Mosquito Habitats",
                       fluidRow(
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("MosID", "Choose one or more interest mosquito habitats:", mosquito$OBJECTID, multiple = TRUE),
                             actionLink("link3", "Give me a Hint"),
                             textOutput("text3")
                           ),
                           mainPanel(
                             leafletOutput("map3")
                           )
                         )
                       ),
                       fluidRow(
                         tabsetPanel(
                           tabPanel("Location",
                                    dataTableOutput("mosLoc"),
                                    downloadButton("dldata31", "Download Data")),
                           tabPanel("Description",
                                    dataTableOutput("mosDes"),
                                    downloadButton("dldata32", "Download Data")),
                           tabPanel("Picture URLs",
                                    fluidRow(dataTableOutput("AbdomenURL"),
                                             downloadButton("abdomendl", "Download AbdomenCloseup as csv")),
                                    fluidRow(dataTableOutput("LarvaURL"),
                                             downloadButton("larvadl", "Download LarvaFullBody as csv")),
                                    fluidRow(dataTableOutput("WaterURL"),
                                             downloadButton("waterdl", "Download WaterSource as csv"))
                                    )
                           
                         )
                       )
                       ),
              tabPanel("About this site",
                       tags$div(
                         tags$h4("About Adopt a Pixel project"), 
                         "The citizen scientists collected data and took photos about the areas that they observed and sent these data to Globe Observer App. 
                         This data can be offered to scientists to help them study earth and land cover issues. If you want to know more about this project, you can click ", 
                         tags$a(href="https://observer.globe.gov/do-globe-observer/land-cover", "here (land cover) "),
                         "and",
                         tags$a(href="https://observer.globe.gov/do-globe-observer/mosquito-habitats", " here (mosquito habitats)."),
                         tags$br(),tags$br(),tags$h4("About this Shiny App"), 
                         "The Shiny App\'s purpose is to display the data collected from Globe Observer App. 
                         The Shiny App has three Tabs including three data sets, respectively.
                         The three Tabs are about plots information, plots pictures and information of mosquito habitats.
                         Here, you can find the information of the plot you are interested in such as location, features, pictures, precipitation level, presence of buildings and trees, and more.
                         ",
                         tags$br(),tags$br(),tags$h4("How to use this Shiny App?"),
                         "In each Tab, hover your cursor on the map to show the number of the plot that you are interested in. 
                         The numbers represent the plot number which can be filled in the left search bar. 
                         After you fill in the left search bar, the map will automatically locate the plot that you are interested in. 
                         Under the map, there are some small tabs which include tables and graphs to show the information of this plot. 
                         You can also select more than one plot in the left options and compare the data across several plots. ",
                         tags$br(),tags$br(),tags$h4("Source"),
                         tags$a(href="https://www.frontiersin.org/articles/10.3389/fclim.2021.658063/full", "Adopt a pixel 3 km: a multiscale data remotely sensed land cover imagery with field based citizen science observation"),
                         tags$br(),tags$br(),tags$h4("Authors"),
                         "Yuting Chen, College of Arts and Sciences, American University",
                         tags$br(),tags$br(),tags$h4("Contact"),
                         "yc3568a@student.american.edu",
                         tags$br(), tags$br()
                       )
                       )
              )
  
)


server <- function(input, output, session) {

  
  # Tab1: Plots
  # Tab1 Text
  output$text1 <- renderText({
    if (input$link1){
      "1. Hover your cursor on the map to see the plot number; 
    2. Fill in the left search bar with plot number;
    3. Below small tabs which include tables and graphs to show the information of this plot
    "
    }
    
  })
  
  # Tab1 Map
    output$map <- renderLeaflet({
      
      if(is.null(input$plotnum)){
        leaflet() %>% 
          addTiles() %>% 
          addCircleMarkers(data = plots,
                           stroke = F,
                           radius = 4,
                           fillColor = "#2C699A",
                           fillOpacity = 1) %>% 
          addMarkers(data = plot1, lng = ~LONGITUDE, lat = ~LATITUDE, label = ~seesAOIplot)
        } else {
            leaflet() %>% 
              addTiles() %>% 
              addCircleMarkers(data = plots %>% filter(seesAOIplot %in% input$plotnum),
                               stroke = F,
                               radius = 4,
                               fillColor = "#2C699A",
                               fillOpacity = 1) %>% 
            addMarkers(data = plot1 %>% filter(seesAOIplot %in% input$plotnum), lng = ~LONGITUDE, lat = ~LATITUDE, label = ~seesAOIplot)
          
        }
      
    })
    
    
    
    # Tab1 Table
    output$des1 <- renderDataTable({
      if (!is.null(input$plotnum)) {
        Aerial %>% 
          filter(seesAOIplot %in% input$plotnum) %>% 
          rename("PlotID" = ceoPLOTID,
                 "Longitude" = LONGITUDE,
                 "Latitude" = LATITUDE,
                 "AOI_Plot" = seesAOIplot,
                 "AOI" = AOI_NUM,
                 "Plot Number" = ceoPLOT_NUM,
                 "Trees Canopy cover" = ceoTREES_CANOPYCOVER,
                 "Bush" = ceoBUSH_SCRUB,
                 "Grass" = ceoGRASS,
                 "Cultivated Vegetables" = ceoCultivatedVeg,
                 "Water Treated Pool" = ceoWaterTreatedPool,
                 "Water Lake Ponded Container" = ceoWaterLakePondedContainer,
                 "Water River Stream" = ceoWaterRiverStream,
                 "Water Irrigation" = ceoWaterIrrigation,
                 "Shadow" = ceoShadow,
                 "Unknow" = ceoUnknown,
                 "Bare Ground" = ceoBareGround,
                 "Building" = ceoBuilding,
                 "Impervious Surface" = ceoImperviousSurface) 
        
      }
      
    })
    
    
    # Tab1 Download Table
    output$dldata1 <- downloadHandler(
        filename = function(){
          paste("PlotsDescription", "csv", sep = ".")
        },
        content = function(file){
          write.csv(Aerial %>% 
                      filter(seesAOIplot %in% input$plotnum) %>% 
                      rename("PlotID" = ceoPLOTID,
                             "Longitude" = LONGITUDE,
                             "Latitude" = LATITUDE,
                             "AOI_Plot" = seesAOIplot,
                             "AOI" = AOI_NUM,
                             "Plot Number" = ceoPLOT_NUM,
                             "Trees Canopy cover" = ceoTREES_CANOPYCOVER,
                             "Bush" = ceoBUSH_SCRUB,
                             "Grass" = ceoGRASS,
                             "Cultivated Vegetables" = ceoCultivatedVeg,
                             "Water Treated Pool" = ceoWaterTreatedPool,
                             "Water Lake Ponded Container" = ceoWaterLakePondedContainer,
                             "Water River Stream" = ceoWaterRiverStream,
                             "Water Irrigation" = ceoWaterIrrigation,
                             "Shadow" = ceoShadow,
                             "Unknow" = ceoUnknown,
                             "Bare Ground" = ceoBareGround,
                             "Building" = ceoBuilding,
                             "Impervious Surface" = ceoImperviousSurface) , file)
        
      }
      
    )
    
    
    
    
    # Tab1 Pie Chart
    output$pie <- renderPlot({
      if (!is.null(input$plotnum)) {
        Aerial %>% 
          pivot_longer(cols = 7:19,
                       names_to = "feature",
                       values_to = "percentage") %>% 
          mutate(feature = str_replace(feature, "ceo", ""),
                 percentage = round(percentage, 1)) %>% 
          filter(seesAOIplot %in% input$plotnum) %>% 
          filter(percentage != 0) %>% 
          ggplot(aes(x = "", y = percentage, fill = feature)) +
          geom_col() +
          geom_text(aes(label = paste0(percentage, "%")),
                    position = position_stack(vjust = 0.5),
                    color = "white",
                    show.legend = FALSE) +
          coord_polar(theta = "y") +
          facet_wrap(~seesAOIplot) +
          labs(x = "",
               y = "") +
          guides(fill = guide_legend(title = "Features")) +
          scale_fill_viridis_d() +
          theme_void() +
          theme(legend.position = "right",
                strip.text = element_text(size = 15, color = "dark green", face = "bold"))
      }
      
    })
    
    
    # Tab1 Pie Chart Download
    output$dlplot1 <- downloadHandler(
      filename = function(){
        paste("PieChart", "png", sep = ".")
      },
      content = function(file){
        png(file, width = 1000, height = 1000, units = "px")
        print(Aerial %>% 
                pivot_longer(cols = 7:19,
                             names_to = "feature",
                             values_to = "percentage") %>% 
                mutate(feature = str_replace(feature, "ceo", ""),
                       percentage = round(percentage, 1)) %>% 
                filter(seesAOIplot %in% input$plotnum) %>% 
                filter(percentage != 0) %>% 
                ggplot(aes(x = "", y = percentage, fill = feature)) +
                geom_col() +
                geom_text(aes(label = paste0(percentage, "%")),
                          position = position_stack(vjust = 0.5),
                          color = "white",
                          show.legend = FALSE) +
                coord_polar(theta = "y") +
                facet_wrap(~seesAOIplot) +
                labs(x = "",
                     y = "") +
                guides(fill = guide_legend(title = "Features")) +
                scale_fill_viridis_d() +
                theme_void() +
                theme(legend.position = "right",
                      strip.text = element_text(size = 15, color = "dark green", face = "bold")))
        dev.off()
        
      }
      
    )

    
    
    # Tab1 Bar Chart
    
    output$bar <- renderPlotly({
      if (!is.null(input$plotnum)) {
        myplot2 <- Aerial %>% 
          pivot_longer(cols = 7:19,
                       names_to = "feature",
                       values_to = "percentage") %>% 
          mutate(feature = str_replace(feature, "ceo", ""),
                 percentage = round(percentage, 1)) %>% 
          filter(seesAOIplot %in% input$plotnum) %>% 
          filter(percentage != 0) %>% 
          group_by(feature) %>% 
          mutate(sum_per = sum(percentage, na.rm = TRUE)) %>% 
          mutate(final_per = round(percentage/sum_per*100, 2)) %>% 
          mutate(plot_number = as.factor(seesAOIplot)) %>% 
          mutate(compared_percentage = paste0(final_per,"%")) %>% 
          ggplot(aes(x = feature, y = percentage, fill = plot_number)) +
          geom_col(position = "fill") +
          geom_text(aes(label= compared_percentage),
                    position=position_fill(vjust=0.5), colour="black", size =3) +
          labs(x = "",
               y = "Percentage") +
          guides(fill = guide_legend(title = "Plots")) +
          coord_flip() +
          theme_bw() 
        ggplotly(myplot2)
        
      }
      
      })
    
    
    # Tab1 Bar Chart Download
    output$dlplot2 <- downloadHandler(
      filename = function(){
        paste("BarChart", "png", sep = ".")
      },
      content = function(file){
        png(file, width = 1000, height = 1000, units = "px")
        print(Aerial %>% 
                pivot_longer(cols = 7:19,
                             names_to = "feature",
                             values_to = "percentage") %>% 
                mutate(feature = str_replace(feature, "ceo", ""),
                       percentage = round(percentage, 1)) %>% 
                filter(seesAOIplot %in% input$plotnum) %>% 
                filter(percentage != 0) %>% 
                group_by(feature) %>% 
                mutate(sum_per = sum(percentage, na.rm = TRUE)) %>% 
                mutate(final_per = round(percentage/sum_per*100, 2)) %>% 
                ggplot(aes(x = feature, y = percentage, fill = as.factor(seesAOIplot))) +
                geom_col(position = "fill") +
                geom_text(aes(label=paste0(final_per,"%")),
                          position=position_fill(vjust=0.5), colour="black", size =5) +
                labs(x = "",
                     y = "Percentage") +
                guides(fill = guide_legend(title = "Plots")) +
                coord_flip() +
                theme_bw())
        dev.off()
        
      }
      
    )
    

    
    # Tab2: Pictures
    # Tab2 Text
    output$text2 <- renderText({
      if (input$link2){
        "1. Hover your cursor on the map to see the plot number; 
    2. Fill in the left search bar with plot number; 
    3. Below small tabs which include tables to show the features and pictures of this plot"
      }
      
    })
    
    # Tab2 Map
    output$map2 <- renderLeaflet({
      
      if(is.null(input$ObjectID)){
        leaflet() %>% 
          addTiles() %>% 
          addCircleMarkers(data = plots,
                           stroke = F,
                           radius = 4,
                           fillColor = "orange",
                           fillOpacity = 1)%>% 
          addMarkers(data = picture1, lng = ~landcoversMeasurementLongitude, lat = ~landcoversMeasurementLatitude, label = ~OBJECTID)
      } else {
         leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(data = pictures %>% filter(OBJECTID %in% input$ObjectID),
                             stroke = F,
                             radius = 4,
                             fillColor = "orange",
                             fillOpacity = 1) %>% 
          addMarkers(data = picture1 %>% filter(OBJECTID %in% input$ObjectID), lng = ~landcoversMeasurementLongitude, lat = ~landcoversMeasurementLatitude, label = ~OBJECTID)
      }
    })
    
    
    # Tab2 location
    output$location <- renderDataTable({
      if (!is.null(input$ObjectID)) {
        location %>% 
          filter(OBJECTID %in% input$ObjectID)
      }
      
    })
    
    # Tab2 Download Table Location
    output$dldata21 <- downloadHandler(
      filename = function(){
        paste("Location", "csv", sep = ".")
      },
      content = function(file){
        write.csv(location %>% 
                    filter(OBJECTID %in% input$ObjectID), file)
        
      }
      
    )
    
    
    # Tab2 feature
    output$feature <- renderDataTable({
      if (!is.null(input$ObjectID)) {
        feature %>% 
          filter(OBJECTID %in% input$ObjectID)
      }
      
    })
    
    
    # Tab2 Download Table feature
    output$dldata22 <- downloadHandler(
      filename = function(){
        paste("Features", "csv", sep = ".")
      },
      content = function(file){
        write.csv(feature %>% 
                    filter(OBJECTID %in% input$ObjectID), file)
        
      }
      
    )
    
    
    
    # Tab2 north
    output$north <- renderDataTable(datatable({
      if (!is.null(input$ObjectID)) {
        north %>% 
          filter(OBJECTID %in% input$ObjectID) %>% 
          mutate(NorthOriginalUrl = paste0("<a href='", NorthOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
          mutate(ThumbPicture = thumbUrl) %>% 
          mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
          mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
          select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
          select(OBJECTID, landcoversNorthClassifications, ThumbPicture, everything())
          
        
      }
    },
    escape = FALSE)
    )
    
    
    # Tab2 north Download
    output$northdl <- downloadHandler(
      filename = function(){
        paste("North", "csv", sep = ".")
      },
      content = function(file){
        write.csv(north %>% 
                     filter(OBJECTID %in% input$ObjectID) %>% 
                     mutate(NorthOriginalUrl = paste0("<a href='", NorthOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
                     mutate(ThumbPicture = thumbUrl) %>% 
                     mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                     mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                     select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                     select(OBJECTID, landcoversNorthClassifications, ThumbPicture, everything()), file)
        
      }
      
    )
    
    
    
    # Tab2 south
    output$south <- renderDataTable(datatable({
      if (!is.null(input$ObjectID)) {
        south %>% 
          filter(OBJECTID %in% input$ObjectID) %>% 
          mutate(SouthOriginalUrl = paste0("<a href='", SouthOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
          mutate(ThumbPicture = thumbUrl) %>% 
          mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
          mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
          select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
          select(OBJECTID, landcoversSouthClassifications, ThumbPicture, everything())
        
      }
    },
    escape = FALSE)
    )
    
    
    # Tab2 south Download
    output$southdl <- downloadHandler(
      filename = function(){
        paste("South", "csv", sep = ".")
      },
      content = function(file){
        write.csv(south %>% 
                    filter(OBJECTID %in% input$ObjectID) %>% 
                    mutate(SouthOriginalUrl = paste0("<a href='", SouthOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
                    mutate(ThumbPicture = thumbUrl) %>% 
                    mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                    mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                    select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                    select(OBJECTID, landcoversSouthClassifications, ThumbPicture, everything()), file)
        
      }
      
    )
    
    
    
    # Tab2 east
    output$east <- renderDataTable(datatable({
      if (!is.null(input$ObjectID)) {
        east %>% 
          filter(OBJECTID %in% input$ObjectID) %>% 
          mutate(EastOriginalUrl = paste0("<a href='", EastOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
          mutate(ThumbPicture = thumbUrl) %>% 
          mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
          mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
          select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
          select(OBJECTID, landcoversEastClassifications, ThumbPicture, everything())
        
      }
    },
    escape = FALSE)
    )
    
    # Tab2 east Download
    output$eastdl <- downloadHandler(
      filename = function(){
        paste("East", "csv", sep = ".")
      },
      content = function(file){
        write.csv(east %>% 
                    filter(OBJECTID %in% input$ObjectID) %>% 
                    mutate(EastOriginalUrl = paste0("<a href='", EastOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
                    mutate(ThumbPicture = thumbUrl) %>% 
                    mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                    mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                    select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                    select(OBJECTID, landcoversEastClassifications, ThumbPicture, everything()), file)
        
      }
      
    )
    
    
    
    
    
    
    # Tab2 west
    output$west <- renderDataTable(datatable({
      if (!is.null(input$ObjectID)) {
        west %>% 
          filter(OBJECTID %in% input$ObjectID) %>% 
          mutate(WestOriginalUrl = paste0("<a href='", WestOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
          mutate(ThumbPicture = thumbUrl) %>% 
          mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
          mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
          select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
          select(OBJECTID, landcoversWestClassifications, ThumbPicture, everything())
        
      }
    },
    escape = FALSE)
    )
    
    # Tab2 west Download
    output$westdl <- downloadHandler(
      filename = function(){
        paste("West", "csv", sep = ".")
      },
      content = function(file){
        write.csv(west %>% 
                     filter(OBJECTID %in% input$ObjectID) %>% 
                     mutate(WestOriginalUrl = paste0("<a href='", WestOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
                     mutate(ThumbPicture = thumbUrl) %>% 
                     mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                     mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                     select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                     select(OBJECTID, landcoversWestClassifications, ThumbPicture, everything()), file)
        
      }
      
    )
    
    
    # Tab2 downward
    output$downward <- renderDataTable(datatable({
      if (!is.null(input$ObjectID)) {
        downward %>% 
          filter(OBJECTID %in% input$ObjectID) %>% 
          mutate(DownwardOriginalUrl = paste0("<a href='", DownwardOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
          mutate(ThumbPicture = thumbUrl) %>% 
          mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
          mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
          select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
          select(OBJECTID, ThumbPicture, everything())
        
      }
    },
    escape = FALSE)
    )
    
    # Tab2 downward Download
    output$downwarddl <- downloadHandler(
      filename = function(){
        paste("Downward", "csv", sep = ".")
      },
      content = function(file){
        write.csv(downward %>% 
                    filter(OBJECTID %in% input$ObjectID) %>% 
                    mutate(DownwardOriginalUrl = paste0("<a href='", DownwardOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
                    mutate(ThumbPicture = thumbUrl) %>% 
                    mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                    mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                    select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                    select(OBJECTID, ThumbPicture, everything()), file)
        
      }
      
    )
    
    
    
    # Tab2 upward
    output$upward <- renderDataTable(datatable({
      if (!is.null(input$ObjectID)) {
        upward %>% 
          filter(OBJECTID %in% input$ObjectID) %>% 
          mutate(UpwardOriginalUrl = paste0("<a href='", UpwardOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
          mutate(ThumbPicture = thumbUrl) %>% 
          mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
          mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
          select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
          select(OBJECTID, ThumbPicture, everything())
        
      }
    },
    escape = FALSE)
    )
    
    
    # Tab2 upward Download
    output$upwarddl <- downloadHandler(
      filename = function(){
        paste("Upward", "csv", sep = ".")
      },
      content = function(file){
        write.csv(upward %>% 
                    filter(OBJECTID %in% input$ObjectID) %>% 
                    mutate(UpwardOriginalUrl = paste0("<a href='", UpwardOriginalUrl,"'>", DownloadOriginalPicture, "</a>")) %>% 
                    mutate(ThumbPicture = thumbUrl) %>% 
                    mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                    mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                    select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                    select(OBJECTID, ThumbPicture, everything()), file)
        
      }
      
    )
    
    
    
  
    # Tab3: Mosquito Habitats
    # Tab3 Text
    output$text3 <- renderText({
      if (input$link3){
        "1. Hover your cursor on the map to see the plot number; 
    2. Fill in the left search bar with plot number; 
    3. Below small tabs which include tables to show the features and pictures of this plot"
      }

    })
    
    # Tab3 Map
    output$map3 <- renderLeaflet({
      
      if(is.null(input$MosID)){
        leaflet() %>% 
          addTiles() %>% 
          addCircleMarkers(data = mosquito,
                           stroke = F,
                           radius = 4,
                           fillColor = "#264653",
                           fillOpacity = 1)%>% 
          addMarkers(data = mosquito1, lng = ~MeasuredLongitude, lat = ~MeasuredLatitude, label = ~OBJECTID)
      } else {
        leaflet() %>% 
          addTiles() %>% 
          addCircleMarkers(data = mosquito %>% filter(OBJECTID %in% input$MosID),
                           stroke = F,
                           radius = 4,
                           fillColor = "#264653",
                           fillOpacity = 1) %>% 
          addMarkers(data = mosquito1 %>% filter(OBJECTID %in% input$MosID), lng = ~MeasuredLongitude, lat = ~MeasuredLatitude, label = ~OBJECTID)
      }
    })
    
    # Tab3 Location
   output$mosLoc <- renderDataTable({
     if (!is.null(input$MosID)) {
       mosLocation %>% 
         filter(OBJECTID %in% input$MosID)
     }
   })
   
   # Tab3 Download Table Location
   output$dldata31 <- downloadHandler(
     filename = function(){
       paste("Mosquito_Habitats_Location", "csv", sep = ".")
     },
     content = function(file){
       write.csv(mosLocation %>% 
                   filter(OBJECTID %in% input$MosID), file)
       
     }
     
   )
   
    
    # Tab3 Description
   output$mosDes <- renderDataTable({
     if (!is.null(input$MosID)) {
       mosFeatures %>% 
         filter(OBJECTID %in% input$MosID)
     }
   })
   
   
   # Tab3 Download Table Description
   output$dldata32 <- downloadHandler(
     filename = function(){
       paste("Mosquito_Habitats_Description", "csv", sep = ".")
     },
     content = function(file){
       write.csv(mosFeatures %>% 
                   filter(OBJECTID %in% input$MosID), file)
       
     }
     
   )
   
  
   # Tab3 Picture URLs 
   ## AbdomenCloseup Table
   output$AbdomenURL <- renderDataTable(datatable({
     if (!is.null(input$MosID)) {
       mosURL %>% 
         filter(OBJECTID %in% input$MosID) %>% 
         mutate(AbdomenCloseupPhotoUrls = ifelse(is.na(AbdomenCloseupPhotoUrls), "null", AbdomenCloseupPhotoUrls)) %>% 
         select(OBJECTID, AbdomenCloseup, AbdomenCloseupPhotoUrls) %>% 
         unique() %>% 
         mutate(thumbUrl = str_replace(AbdomenCloseupPhotoUrls, "original", "thumb")) %>% 
         mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                              str_extract(thumbUrl, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                              "null")) %>% 
         mutate(DownloadThumbPicture = str_replace_all(DownloadThumbPicture, "/", "_")) %>% 
         mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                              str_c("golc", "thumb", DownloadThumbPicture, sep = "_"),
                                              "null")) %>% 
         mutate(DownloadOriginalPicture = ifelse(!AbdomenCloseupPhotoUrls %in% c("null", "rejected"),
                                                 str_extract(AbdomenCloseupPhotoUrls, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                 "null")) %>% 
         mutate(DownloadOriginalPicture = str_replace_all(DownloadOriginalPicture, "/", "_")) %>% 
         mutate(DownloadOriginalPicture = ifelse(!AbdomenCloseupPhotoUrls %in% c("null", "rejected"),
                                                 str_c("gomhm_abdomen", DownloadOriginalPicture, sep = "_"),
                                                 "null")) %>% 
         mutate(AbdomenCloseupPhotoUrls = paste0("<a href='", AbdomenCloseupPhotoUrls,"'>", DownloadOriginalPicture, "</a>")) %>% 
         mutate(ThumbPicture = thumbUrl) %>% 
         mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
         mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
         select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
         select(OBJECTID, AbdomenCloseup, ThumbPicture, thumbUrl, AbdomenCloseupPhotoUrls)
       
     }
   },
   escape = FALSE)
   )
   
   
   ## Abdomen Download
   output$abdomendl <- downloadHandler(
     filename = function(){
       paste("AbdomenCloseup", "csv", sep = ".")
     },
     content = function(file){
       write.csv(mosURL %>% 
                   filter(OBJECTID %in% input$MosID) %>% 
                   mutate(AbdomenCloseupPhotoUrls = ifelse(is.na(AbdomenCloseupPhotoUrls), "null", AbdomenCloseupPhotoUrls)) %>% 
                   select(OBJECTID, AbdomenCloseup, AbdomenCloseupPhotoUrls) %>% 
                   unique() %>% 
                   mutate(thumbUrl = str_replace(AbdomenCloseupPhotoUrls, "original", "thumb")) %>% 
                   mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                                        str_extract(thumbUrl, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                        "null")) %>% 
                   mutate(DownloadThumbPicture = str_replace_all(DownloadThumbPicture, "/", "_")) %>% 
                   mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                                        str_c("golc", "thumb", DownloadThumbPicture, sep = "_"),
                                                        "null")) %>% 
                   mutate(DownloadOriginalPicture = ifelse(!AbdomenCloseupPhotoUrls %in% c("null", "rejected"),
                                                           str_extract(AbdomenCloseupPhotoUrls, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                           "null")) %>% 
                   mutate(DownloadOriginalPicture = str_replace_all(DownloadOriginalPicture, "/", "_")) %>% 
                   mutate(DownloadOriginalPicture = ifelse(!AbdomenCloseupPhotoUrls %in% c("null", "rejected"),
                                                           str_c("gomhm_abdomen", DownloadOriginalPicture, sep = "_"),
                                                           "null")) %>% 
                   mutate(AbdomenCloseupPhotoUrls = paste0("<a href='", AbdomenCloseupPhotoUrls,"'>", DownloadOriginalPicture, "</a>")) %>% 
                   mutate(ThumbPicture = thumbUrl) %>% 
                   mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                   mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                   select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                   select(OBJECTID, AbdomenCloseup, ThumbPicture, everything()), file)
       
     }
     
   )
   
   
   
   ## LarvaFullBody Table
   output$LarvaURL <- renderDataTable(datatable({
     if (!is.null(input$MosID)) {
       mosURL %>% 
         filter(OBJECTID %in% input$MosID) %>% 
         mutate(LarvaFullBodyPhotoUrls = ifelse(is.na(LarvaFullBodyPhotoUrls), "null", LarvaFullBodyPhotoUrls)) %>% 
         select(OBJECTID, LarvaFullBody, LarvaFullBodyPhotoUrls) %>% 
         unique() %>% 
         mutate(thumbUrl = str_replace(LarvaFullBodyPhotoUrls, "original", "thumb")) %>% 
         mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                              str_extract(thumbUrl, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                              "null")) %>% 
         mutate(DownloadThumbPicture = str_replace_all(DownloadThumbPicture, "/", "_")) %>% 
         mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                              str_c("golc", "thumb", DownloadThumbPicture, sep = "_"),
                                              "null")) %>% 
         mutate(DownloadOriginalPicture = ifelse(!LarvaFullBodyPhotoUrls %in% c("null", "rejected"),
                                                 str_extract(LarvaFullBodyPhotoUrls, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                 "null")) %>% 
         mutate(DownloadOriginalPicture = str_replace_all(DownloadOriginalPicture, "/", "_")) %>% 
         mutate(DownloadOriginalPicture = ifelse(!LarvaFullBodyPhotoUrls %in% c("null", "rejected"),
                                                 str_c("gomhm_LarvFullBody", DownloadOriginalPicture, sep = "_"),
                                                 "null")) %>% 
         mutate(LarvaFullBodyPhotoUrls = paste0("<a href='", LarvaFullBodyPhotoUrls,"'>", DownloadOriginalPicture, "</a>")) %>% 
         mutate(ThumbPicture = thumbUrl) %>% 
         mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
         mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
         select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
         select(OBJECTID, LarvaFullBody, ThumbPicture, thumbUrl, LarvaFullBodyPhotoUrls)
       
     }
   },
   escape = FALSE)
   )
   
   
   ## LarvaFullBody  Download
   output$larvadl <- downloadHandler(
     filename = function(){
       paste("LarvaFullBody", "csv", sep = ".")
     },
     content = function(file){
       write.csv(mosURL %>% 
                   filter(OBJECTID %in% input$MosID) %>% 
                   mutate(LarvaFullBodyPhotoUrls = ifelse(is.na(LarvaFullBodyPhotoUrls), "null", LarvaFullBodyPhotoUrls)) %>% 
                   select(OBJECTID, LarvaFullBody, LarvaFullBodyPhotoUrls) %>% 
                   unique() %>% 
                   mutate(thumbUrl = str_replace(LarvaFullBodyPhotoUrls, "original", "thumb")) %>% 
                   mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                                        str_extract(thumbUrl, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                        "null")) %>% 
                   mutate(DownloadThumbPicture = str_replace_all(DownloadThumbPicture, "/", "_")) %>% 
                   mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                                        str_c("golc", "thumb", DownloadThumbPicture, sep = "_"),
                                                        "null")) %>% 
                   mutate(DownloadOriginalPicture = ifelse(!LarvaFullBodyPhotoUrls %in% c("null", "rejected"),
                                                           str_extract(LarvaFullBodyPhotoUrls, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                           "null")) %>% 
                   mutate(DownloadOriginalPicture = str_replace_all(DownloadOriginalPicture, "/", "_")) %>% 
                   mutate(DownloadOriginalPicture = ifelse(!LarvaFullBodyPhotoUrls %in% c("null", "rejected"),
                                                           str_c("gomhm_LarvFullBody", DownloadOriginalPicture, sep = "_"),
                                                           "null")) %>% 
                   mutate(LarvaFullBodyPhotoUrls = paste0("<a href='", LarvaFullBodyPhotoUrls,"'>", DownloadOriginalPicture, "</a>")) %>% 
                   mutate(ThumbPicture = thumbUrl) %>% 
                   mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                   mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                   select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                   select(OBJECTID, LarvaFullBody, ThumbPicture, everything()), file)
       
     }
     
   )
   
   
   ## WaterSource Table
   output$WaterURL <- renderDataTable(datatable({
     if (!is.null(input$MosID)) {
       mosURL %>% 
         filter(OBJECTID %in% input$MosID) %>% 
         mutate(WaterSourcePhotoUrls = ifelse(is.na(WaterSourcePhotoUrls), "null", WaterSourcePhotoUrls)) %>% 
         select(OBJECTID, WaterSource, WaterSourcePhotoUrls) %>% 
         unique() %>% 
         mutate(thumbUrl = str_replace(WaterSourcePhotoUrls, "original", "thumb")) %>% 
         mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                              str_extract(thumbUrl, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                              "null")) %>% 
         mutate(DownloadThumbPicture = str_replace_all(DownloadThumbPicture, "/", "_")) %>% 
         mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                              str_c("golc", "thumb", DownloadThumbPicture, sep = "_"),
                                              "null")) %>% 
         mutate(DownloadOriginalPicture = ifelse(!WaterSourcePhotoUrls %in% c("null", "rejected"),
                                                 str_extract(WaterSourcePhotoUrls, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                 "null")) %>% 
         mutate(DownloadOriginalPicture = str_replace_all(DownloadOriginalPicture, "/", "_")) %>% 
         mutate(DownloadOriginalPicture = ifelse(!WaterSourcePhotoUrls %in% c("null", "rejected"),
                                                 str_c("gomhm_WaterSource", DownloadOriginalPicture, sep = "_"),
                                                 "null")) %>% 
         mutate(WaterSourcePhotoUrls = paste0("<a href='", WaterSourcePhotoUrls,"'>", DownloadOriginalPicture, "</a>")) %>% 
         mutate(ThumbPicture = thumbUrl) %>% 
         mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
         mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
         select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
         select(OBJECTID, WaterSource, ThumbPicture, thumbUrl, WaterSourcePhotoUrls)
       
     }
   },
   escape = FALSE)
   )
   
   
   
   ## WaterSource Download
   output$waterdl <- downloadHandler(
     filename = function(){
       paste("WaterSource", "csv", sep = ".")
     },
     content = function(file){
       write.csv(mosURL %>% 
                   filter(OBJECTID %in% input$MosID) %>% 
                   mutate(WaterSourcePhotoUrls = ifelse(is.na(WaterSourcePhotoUrls), "null", WaterSourcePhotoUrls)) %>% 
                   select(OBJECTID, WaterSource, WaterSourcePhotoUrls) %>% 
                   unique() %>% 
                   mutate(thumbUrl = str_replace(WaterSourcePhotoUrls, "original", "thumb")) %>% 
                   mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                                        str_extract(thumbUrl, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                        "null")) %>% 
                   mutate(DownloadThumbPicture = str_replace_all(DownloadThumbPicture, "/", "_")) %>% 
                   mutate(DownloadThumbPicture = ifelse(!thumbUrl %in% c("null", "rejected"),
                                                        str_c("golc", "thumb", DownloadThumbPicture, sep = "_"),
                                                        "null")) %>% 
                   mutate(DownloadOriginalPicture = ifelse(!WaterSourcePhotoUrls %in% c("null", "rejected"),
                                                           str_extract(WaterSourcePhotoUrls, "\\d{4}/\\d{2}/\\d{2}/\\d+"),
                                                           "null")) %>% 
                   mutate(DownloadOriginalPicture = str_replace_all(DownloadOriginalPicture, "/", "_")) %>% 
                   mutate(DownloadOriginalPicture = ifelse(!WaterSourcePhotoUrls %in% c("null", "rejected"),
                                                           str_c("gomhm_WaterSource", DownloadOriginalPicture, sep = "_"),
                                                           "null")) %>% 
                   mutate(WaterSourcePhotoUrls = paste0("<a href='", WaterSourcePhotoUrls,"'>", DownloadOriginalPicture, "</a>")) %>% 
                   mutate(ThumbPicture = thumbUrl) %>% 
                   mutate(ThumbPicture = str_c('<img src=', thumbUrl, '></img>', sep = "")) %>% 
                   mutate(thumbUrl = paste0("<a href='", thumbUrl,"'>", DownloadThumbPicture,"</a>")) %>% 
                   select(-DownloadOriginalPicture, -DownloadThumbPicture) %>% 
                   select(OBJECTID, WaterSource, ThumbPicture, everything()), file)
       
     }
     
   )

   
   
  
    
}

shinyApp(ui, server)