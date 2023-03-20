library("shinydashboard")
library(sf)
library(leaflet)
library(png)
library(leaflet.extras2)
library(readxl)
library(ggplot2)
library(dplyr)

#load data
noise<-st_read("S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport/noise_study_maps.shp")
noise_WGS84 <- st_transform(noise, 4326)# reproject
#noise
noise_bins<-c(0,50.1,55,60,65, Inf)
noise_labels<-c("<50dB", "(50-55]dB", "(55-60]dB", "(60-65]dB", ">65dB")
pal_fun <- colorBin("YlOrRd", domain = noise_WGS84$Study_Ar_1, bins = noise_bins) #color palette
#confounders
name_vars<-c("Traffic", "Development index", "Ethnicity (Black & Brown)", "Ethnicity (Asians)", "Smoking proxy" )


#Load health data
health<- read.csv("S:/Projects/SAHSU/SAHSU Projects/On-going SAHSU projects/Brazil_collaboration_2017_2019/Results/Model/Adj_model_for_figure.csv")


#WRITE DA
ui <- dashboardPage(
  dashboardHeader(title = "Congonhas Airport Expansion: Health Impact Assessment"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      shinydashboard::menuItem("Cover", tabName = "Cover"),
      shinydashboard::menuItem("Background", tabName = "Background", 
                               menuSubItem("Aircraft-noise",tabName = "Aircraft-noise"),
                               menuSubItem("Noise-Health",tabName = "Noise-Health"),
                               menuSubItem("Congonhas",tabName = "Congonhas")),
      shinydashboard::menuItem("Methods", tabName = "Methods", 
               menuSubItem("Noise model",tabName = "Noise-model"),
               menuSubItem("Health outcomes",tabName = "Health-outcomes"),
               menuSubItem("Confounders",tabName = "Confounders")),
      shinydashboard::menuItem("Results", tabName = "Results",
                               menuSubItem("Noise",tabName = "Noise"),
                               menuSubItem("Health Impacts",tabName = "Health-Impacts")),
      shinydashboard::menuItem("Impact and policy", tabName = "Conclusions")
    )
  ),
  dashboardBody(
    tabItems(
      # COVER
      tabItem(tabName="Cover",
              fluidPage(imageOutput('Congonhas_cover', width = "85vw", height = "91vh")
              )
      ),
      # TAB1: Intro
      tabItem(tabName="Aircraft-noise",
              fluidPage(imageOutput('health_impacts', width = "85vw", height = "91vh")
              )
      ),
      # TAB1: Intro
      tabItem(tabName="Noise-Health",
              fluidPage(
                titlePanel("Noise: is it really that bad?"),
                column(width = 6,
                       imageOutput('noise_banner')),
                fluidRow(
                  tags$style(".nav-tabs {background: #f4f4f4;}.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {background-color: #fff;
                 border-color: #fff; }.nav-tabs-custom .nav-tabs li.active {border-top-color: #314a6d;}"),
                  titlePanel(""),
                  tabBox(side = "right",
                         title = h3("Some facts"),
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "250px",
                    tabPanel("Fact 3", 
                             p("For nighttime deaths, noise exposure levels two hours preceding death were significantly associated with heart-related mortality.",
                               style = "font-size: 20px")),
                    tabPanel("Fact 2", 
                             p("In 2011, the WHO concluded that transportation-related noise—including aircraft—is responsible for the annual loss of up to 1.6 million cumulative years of healthy life among people in Western Europe. ",
                               style = "font-size: 20px")),
                    tabPanel("Fact 1", 
                             p("Noise impacts on health is an understudied topic; specially in the global south - most of research from Europe.",
                               style = "font-size: 20px"))
                ),
                tabBox(side = "right",
                  title = h3("WHO Noise guidelines (Europe)"),
                  p("For aircraft noise, WHO’s guidelines recommend:",style = "font-size: 20px"),
                  p(" - 45dB average daily noise (Ldn)",style = "font-size: 20px"),
                  p(" - 40dB average night-time exposure (Lnight)",style = "font-size: 20px")
                    )
                ))),
      tabItem(tabName="Congonhas",
              fluidPage(
                box(leafletOutput("congonhas",height="90vh"),
                        p()),
                box(h1("Congonhas airport"),
                    p(" - It is the second busiest airport in Brazil (22.7 million passengers in 2019)",style = "font-size: 22px"),
                    p(" - Located right in the middle of a densely populated area of São Paulo.",style = "font-size: 22px"),
                    p(" - According to the Brazilian Civil Aviation Regulation n° 161, only areas with aircraft noise >65dB require 
                      restrictions in the land use and activities allowed.",style = "font-size: 22px")),
                box(imageOutput('congonhas_aerial',height="50vh"))
              )
      ),
      
      # TAB:Noise model
      tabItem(tabName = "Noise-model",
              fluidRow(
                box(h1("Exposure assessment:"),
                    p("We used an area weighting approach to harmonize the noise level at each census tract included 
                      in the study area for belonging to a district intersect any of our noise contours. ",style = "font-size: 21px"))
              ),
              fluidRow(
                imageOutput('noise_contours', width = "77vw", height = "70vh"),
                p("Source: Integrated noise model (INM) developed by Slama and collegues",style = "font-size: 16px")
              )),
      tabItem(tabName = "Health-outcomes",
              fluidPage(imageOutput("mortality",height="90vh"),
                        p())),
      # TAB:Confounders
      tabItem(tabName = "Confounders",
              sidebarLayout(
                sidebarPanel(width=2,
                             selectInput("var",
                                         label = "Select a variable",
                                         choices = name_vars)),
                mainPanel(leafletOutput("confounders_map",height="90vh"))
              )
      ),
      # TAB:Results
      # TAB:Noise
      tabItem(tabName = "Noise",
              fluidPage(leafletOutput("noise_map",height="90vh"),
                        p())),
      tabItem(tabName = "Health-Impacts",
              sidebarLayout(
                sidebarPanel(width=2,
                             selectInput("disease",
                                         label = "Select a variable",
                                         choices = list("CVD", 
                                                        #"Stroke", 
                                                        "CHD"
                                                        ))),
                mainPanel(plotOutput("health",height="600px"),
                          p("Models adjusted by all confounders"))
              )
      ),
      # TAB:Conclusions
      tabItem(tabName = "Conclusions",
              fillPage(
                box(h2("What do these results show and why do we care?"),
                    p("1) Remarkably, 60% of the study population living near the Congonhas airport (1.5 million) were 
                      exposed to aircraft noise levels >50dB, well above those recommended by the WHO (45dB).",style = "font-size: 21px"),
                    p("2) The current levents of aircraft noise are causing remarkable harm to the population living nearby.",style = "font-size: 21px")),
                box(h2("Our recommendations:"),
                    p("1) We recommend against building a new runway",style = "font-size: 21px"),
                    p("2) Government should commit to developing specific long-term targets to protect the public from the health impacts of aircraft noise.",style = "font-size: 21px"),
                    p("3) Setting up a scheme to financially support the soundproofing of the homes of people living in the vicinities.",style = "font-size: 21px"),
                    p("4) Stricter regulations for night-time flights, as they have the strongest impacts on health.",style = "font-size: 21px")),
                imageOutput('Congonhas', width = "77vw", height = "50vh")
                )
              )
      )
  )
)

server <- function(input, output) {
  #LOAD IMAGES
  output$noise_banner <- renderImage({
    return(list(src = "S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport//noise_banner.PNG",contentType = "image/png",
                width = 1200))}, deleteFile = FALSE) #where the src is wherever you have the picture
 
   output$Congonhas_cover <- renderImage({
      list(src = "S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport//Congonhas_cover_hr.PNG",
           width = "100%", height = "100%",alt = "Chart of good stuff")}, deleteFile = FALSE)
  
   output$health_impacts <- renderImage({
    list(src = "S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport//health_impacts.PNG",
         width = "100%", height = "100%",alt = "Chart of good stuff")}, deleteFile = FALSE)
   
   output$mortality <- renderImage({
     list(src = "S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport//mortality.PNG",
          width = "100%", height = "100%",alt = "Chart of good stuff")}, deleteFile = FALSE)
   
   output$congonhas_aerial <- renderImage({
     list(src = "S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport//congonhas_aerial.PNG",
          width = "100%", height = "100%",alt = "Chart of good stuff")}, deleteFile = FALSE)
   
   output$noise_contours <- renderImage({
     list(src = "S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport//noise_contours.PNG",
          width = "100%", height = "100%",alt = "Chart of good stuff")}, deleteFile = FALSE)   
   
   output$Congonhas <- renderImage({
     list(src = "S:/Projects/AinaRB_ClimateHealth/jobs/WRI/ARB_NoiseAirport//Congonhas.PNG",
          width = "100%", height = "100%",alt = "Chart of good stuff")}, deleteFile = FALSE)   
   
#CONGHONHAS MAP
  output$congonhas<-renderLeaflet({
     leaflet()%>% 
      addTiles(group = "base", layerId="basemapID")%>%
      setView(-46.65549739,-23.62333084, zoom = 15)
      })
  
# NOISE LAYER ----
  output$noise_map<-renderLeaflet({
    leaflet(noise_WGS84)%>% 
      addMapPane("left", zIndex = 0) %>%
      addMapPane("right", zIndex = 0) %>%
      #basemap
      addTiles(group = "base", layerId="basemapID", 
                       options = pathOptions(pane = "left")) %>%
      addTiles(group = "base", layerId="noiseID", 
                       options = pathOptions(pane = "right")) %>%
      #noise layer
      addPolygons(options = pathOptions(pane = "right"),
                  fillColor = ~pal_fun(as.numeric(Study_Ar_1)),
                  stroke=F,
                  fillOpacity = 0.65) %>%
      addLegend("bottomright", 
                pal=pal_fun, 
                values=noise_labels,
                labFormat = function(type, cuts, p) { paste0(noise_labels)},
                title="Noise levels")%>%
      #Slide window 
      addSidebyside(layerId="sidecontrols",
                    leftId="basemapID",
                    rightId="noiseID")})
# CONFOUNDERS MAP ----
# Filter data based on selected Style
output$confounders_map <- renderLeaflet({
      var<-switch(input$var, 
                  "Traffic"=as.numeric(noise_WGS84$Study_Ar_2),
                  "Development index"=as.numeric(noise_WGS84$Study_Ar_4),
                  "Ethnicity (Black & Brown)"=as.numeric(noise_WGS84$Study_Ar18),
                  "Ethnicity (Asians)"=noise_WGS84$Study_Ar28,
                  "Smoking proxy"=noise_WGS84$Study_Ar63)
      names<-switch(input$var, 
                    "Traffic"="Traffic",
                    "Development index"="Development index",
                    "Ethnicity (Black & Brown)"="Ethnicity (Black & Brown)",
                    "Ethnicity (Asians)"="Ethnicity (Asians)",
                    "Smoking proxy"="Smoking proxy")
      labels<-c("Q1", "Q2", "Q3", "Q4", "Q5")
      colRamp<-switch(input$var, 
                      "Traffic"=c('gray80', 'black'),
                      "Development index"=c('lavenderblush', 'mediumpurple4'),
                      "Ethnicity (Black & Brown)"=c('navajowhite1', 'sienna4'),
                      "Ethnicity (Asians)"=c('yellow', "orange", "red",NA),
                      "Smoking proxy"=c('green', "yellow", "orange",'red', "red"))
      pal<-switch(input$var, 
                  "Traffic"=colorQuantile(palette = colorRampPalette(colRamp)(length(var)), domain = as.numeric(var), n=5),
                  "Development index"=colorQuantile(palette = colorRampPalette(colRamp)(length(var)), domain = as.numeric(var), n=5),
                  "Ethnicity (Black & Brown)"=colorQuantile(palette = colorRampPalette(colRamp)(length(var)), domain = as.numeric(var), n=5),,
                  "Ethnicity (Asians)"=colorFactor( palette = colRamp, levels = c("1","2", "3",NA )),
                  "Smoking proxy"=colorFactor( palette = colRamp, levels = c("[0,0.25]","(0.25,0.5]", "(0.5,0.75]",  "(0.75,1]",NA )))

leaflet(noise_WGS84) %>%
        #basemap
    addProviderTiles(providers$CartoDB.Positron) %>%
    #Add layer for confounder
    addPolygons(
      fillColor = ~pal(var),
      stroke=F,
      fillOpacity = 0.9) %>%
    addLegend("bottomright", 
              pal=pal, 
              values=~var,
              labFormat = function(type, cuts, p) { paste0(labels)},
              title=names)
  
  })

#HEALTH OUTCOMES
  subhealth<-reactive({
    health %>% 
      dplyr::filter(disease %in% input$disease) })
  
  output$health <- renderPlot({
  ggplot(subhealth(), aes(x=Noises, y=RR))+
      geom_pointrange(aes(ymin=low, ymax=high), size=1.5, na.rm=T, col="steelblue")+
      geom_hline(yintercept=1, linetype="dashed", color = "black")+
      theme(axis.text=element_text(size=17),
            axis.title=element_text(size=19,face="bold"))
})
}

shinyApp(ui, server)
