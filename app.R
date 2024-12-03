require(shiny)
require(shinythemes)
require(shinythemes)
require(shinydashboard)
require(shinyWidgets)
library(shinyjs)
require(htmlwidgets)
library(echarts4r)
library(bslib)
require(leaflet)
library(leafem)
library(tidyverse)
library(paletteer)
library(ggthemes)
library(shinyBS)
library(ggpubr)
library(sf)
sf_use_s2(FALSE)
library(stars)

load("data/metadata.rda")
load("data/distributions_small.rda")
load("data/flyway.rda")
load("data/flegPhen.rda")
load("data/flagPts.rda")
load("data/flagDens.rda")
load("data/sf_tracks.rda")
load("data/HPAIoutbreak.rda")
load("data/distributionsGrid.rda")
load("data/sba.rdata")

outbreakSM <- outbreakDat %>% st_drop_geometry() %>%
  group_by(Year, is_wild) %>% summarise(sample = sum(sample))

legendDistr <- dist %>% st_drop_geometry %>% filter(!duplicated(CODE)) %>%
  dplyr::select(CODE, COLOR)

#### UI ####
ui <- fluidPage(
  div(class = "nav",
      
    tags$style(HTML(".navbar .navbar-header {float: left; }.navbar .navbar-nav {float: right;}.container {min-width: 950px}")),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    navbarPage(theme = shinytheme("flatly"), 
               collapsible = TRUE, fluid = TRUE,
               title = HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">High Pathogenicity Avian Influenza (HPAI) Risk for Australia</a>'), 
               id = "nav",
               windowTitle = "Dashboard",
                
               tabPanel("Home",
                        includeHTML("www/about.html")
               ),
               
               #### Shorebird Movements ####
               {
                 tabPanel("Shorebird Movements",
                          
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              
                              leafletOutput("MovMap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 30, width = 300, fixed=TRUE,
                                            draggable = FALSE, height = "auto",
                                            
                                            h4("Select species:"),
                                            
                                            selectInput(
                                              inputId = "species",
                                              label = "",
                                              selected = "Red-necked Stint",
                                              choices = c("All", meta$species),
                                            ),
                                            
                                            hr(style = "border-top: 1px solid #74b9e1;"),
                                            h4("Species Distribution:"),
                                            
                                            conditionalPanel(condition = "input.species.length > 0",
                                                             fluidRow(
                                                               column(6, materialSwitch("birdlife", "Show", status = "info", value = FALSE))
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.birdlife",
                                                               plotOutput("BirdLife_Legend", width = 220, height = 90)
                                                             )
                                            ),
                                            
                                            hr(style = "border-top: 1px solid #74b9e1;"),
                                            h4("Leg-flag resightings:"),
                                            
                                            conditionalPanel(condition = "input.species.length > 0",
                                                             fluidRow(
                                                               column(6, materialSwitch(
                                                                 inputId = "flags",
                                                                 label = "Show", 
                                                                 status = "info",
                                                                 right = FALSE
                                                               )),
                                                               conditionalPanel(condition = "input.flags",
                                                                                column(6, materialSwitch(
                                                                                  inputId = "flagDens",
                                                                                  label = "Density",
                                                                                  status = "info",
                                                                                  right = TRUE
                                                                                ))
                                                               )
                                                             )
                                            ),
                                            
                                            hr(style = "border-top: 1px solid #74b9e1;"),
                                            
                                            conditionalPanel(condition = paste0("['All', '", paste0(unique(sf_tracks$Species), collapse = "', '"), "'].indexOf(input.species) !== -1"),
                                                             h4("Migration tracks:"),
                                                             fluidRow(
                                                               column(5,
                                                                      materialSwitch(
                                                                        inputId = "tracksNW",
                                                                        label = "Northward", 
                                                                        status = "info",
                                                                        right = FALSE
                                                                      )),
                                                               column(5,
                                                                      materialSwitch(
                                                                        inputId = "tracksSW",
                                                                        label = "Southward", 
                                                                        status = "warning",
                                                                        right = FALSE
                                                                      ))
                                                             ),
                                                             
                                                             # conditionalPanel(condition = "input.tracksNW",
                                                             #   fluidRow(
                                                             #     column(12, sliderTextInput(
                                                             #       inputId = "tracks_day",
                                                             #       label = "Date slider (not working):",
                                                             #       choices = format(seq(as.POSIXct("2012-01-01"), as.POSIXct("2012-12-31"), by = "day"), "%d-%b"),
                                                             #       selected = "01-Dec",
                                                             #       animate = animationOptions(interval = 200, loop = T)
                                                             #     ))
                                                             #   )
                                                             # )
                                            )
                                            
                              ),
                              
                              conditionalPanel(condition = "input.flags==true",
                                               absolutePanel(id = "leg-flag", class = "panel panel-default", 
                                                             bottom = 200, right = 20, fixed=TRUE, 
                                                             draggable = TRUE,
                                                             height = 450, width = 360,
                                                             h3("Leg-flag resightings"),
                                                             h2(""),
                                                             echarts4rOutput("chart", width = "100%", height = "60%"),
                                                             h5("Select month with click on bar-chart"),
                                                             actionButton("reset", "Clear")
                                               )
                              ),
                              
                              # absolutePanel(id = "logo", class = "card", bottom = 40, right = 20, fixed=TRUE, draggable = FALSE, height = "auto", width = "auto",
                              #               tags$a(href='https://wildlifehealthaustralia.com.au/', tags$img(src='logos_small_horizontal.png', height = 75)))
                          )
                 )
               },
               
               #### HPAIV Outbreaks #####
               {
               tabPanel("HPAI Outbreaks",
                        
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            
                            leafletOutput("OutbreakMap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 80, left = 30, width = 300, fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                          
                                          h4("HPAI Outbreaks:"),
                                          br(),
                                          
                                          fluidRow(
                                            column(6, materialSwitch("outbreaks", "Show", status = "info", value = FALSE))),
                                          
                                          fluidRow(
                                            column(12, sliderTextInput("hpai_month", label = "", 
                                                                       grid = TRUE, force_edges = FALSE, hide_min_max = TRUE,
                                                                       choices = as.character(seq(2005, 2024, by = 1)),
                                                                       selected = 2015, animate = T))
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.outbreaks",
                                            plotOutput("Outbreak_Legend", width = 160, height = 80),
                                            br(),
                                            plotOutput("outbreakGraph", width = 275, height = 220)
                                          )
                                          
                                          )
                        )
                            
               )
               },
               
               #### Aggregations #####
               {
               tabPanel("Bird Aggregations",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            
                         leafletOutput("AggrMap", width="100%", height="100%") 
                        )
                    )
               },
               
               tabPanel("Species at Risk",
                        div(class = "outer",
                        )
               )
    ),
  )
)


#### Server ####
server <- function(input, output) {
  
  ###################
  #### Movement #####
  ###################
  
  ### Distribution
  {
  speciesDistr <- reactive({
    if(!input$birdlife) {
        out <- dist[1,] %>% mutate(COLOR = "#ffffff")
      } else {
        if(input$birdlife & !is.null(input$species)) {
          out <- dist %>% filter(SCINAME%in%(meta %>% filter(species %in% input$species) %>% pull(SCINAME)))
        } else {
          out <- dist[1,] %>% mutate(COLOR = "#ffffff")
        }
      }
  })
  
  output$BirdLife_Legend <- renderPlot({
    pl <- ggplot(legendDistr %>% filter(CODE != "Seasonal occurance uncertain"), aes(x = 1, y = 1:nrow(legendDistr), color = CODE)) +
      geom_point(shape = 15, size = 10) +
      scale_color_manual(values = legendDistr$COLOR, breaks = legendDistr$CODE) +
      theme(
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)
      )

    as_ggplot(get_legend(pl))
  })
  }
  
  ### Leg-flags
  {
  res = reactiveVal(0)
  
  observeEvent(input$reset,{
    res(res()+1)
  })
  
  observeEvent(input$custom_bar_click$drilled_place,{
    res(0)
  })
  
  selectMonth  <- reactive({
    if(is.null(input$custom_bar_click) | res()!=0) {
      c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    } else {
      input$custom_bar_click$drilled_place
    }
  })
  
  speciesFlags <- reactive({

    if(length(selectMonth())>1) {
        flagPts %>% filter(
          Species == input$species,
          is.na(Month))
      } else {
      flagPts %>% filter(
        Species == input$species,
        Month == selectMonth())
      }
    
  })
  
  flegPhenData <- reactive({
    if(input$species == "") {
      flegPhen %>% filter(Species = "All")
    } else {
      flegPhen %>% filter(Species == input$species)
    }
  })
  
  flegDensity  <- reactive({
    if(input$species == "") {
      flagDens %>% filter(
        species == "All",
        is.na(month)
      ) %>% slice(1) 
    } else {
      if(length(selectMonth())>1) {
        flagDens %>% filter(
          species == input$species,
          is.na(month)
        )
      } else {
        flagDens %>% filter(
          species == input$species,
          month == selectMonth()
        )
      }
    }
  })
  
  output$chart <- renderEcharts4r({
    
    data <- flegPhenData() |>
      mutate(month = as.character(month(ymd(010101) + months(Month-1),label=TRUE,abbr=TRUE)),
             cond  = ifelse(month%in%selectMonth(), 2, 1),
             colAU = c("#585858", "#2e37fc")[cond],
             colFL = c("#c6c6c6", "#3390de")[cond])
    
    colAU <- paste0("function(params) {var colorList = [",
                     paste(glue::glue("'{data$colAU}'"), collapse = ', '),
                     "]; return colorList[params.dataIndex]}")
    
    colFL <- paste0("function(params) {var colorList = [",
                    paste(glue::glue("'{data$colFL}'"), collapse = ', '),
                    "]; return colorList[params.dataIndex]}")

    data |> as.data.frame() |>
      e_charts(month) |> 
      e_bar(Australia, stack = "grp", itemStyle = list(color = htmlwidgets::JS(colAU))) |>
      e_bar(Flyway,    stack = "grp", itemStyle = list(color = htmlwidgets::JS(colFL))) |>
      e_color(
        c("#2e37fc", "#3390de")
      ) |>
      e_grid(left = "15%", bottom = "10%") |>
      e_on(
        query = "series.bar",
        # Set input values
        handler = "function(params){
         Shiny.setInputValue(
          'custom_bar_click',
          {clicked_level: 'level2', drilled_place: params.name}, {priority: 'event'}
         );
       }",
       event = "click"
      )
  })
  }
  
  ### Tracks 
  {
  observeEvent(input$tracksNW, {
    if(input$tracksNW & input$flags) {
      updateSwitchInput(
        inputId = "flags",
        value = FALSE
      )
    }
    })
  
  observeEvent(input$tracksSW, {
    if(input$tracksSW & input$flags) {
      updateSwitchInput(
        inputId = "flags",
        value = FALSE
      )
    }
  })
  
  observeEvent(input$flags, {
    if(input$flags & (input$tracksNW | input$tracksSW)) {
      updateSwitchInput(
        inputId = "tracksNW",
        value = FALSE
      )
      
      updateSwitchInput(
        inputId = "tracksSW",
        value = FALSE
      )
    }
  })
  
  trackingData <- reactive({
    
    if(input$species!="All") {
      sf_tracks %>%
        mutate(Days = ifelse(Type==2, 1, Days)) %>%
        filter(Species == input$species) %>%
        rename(Weight = spWeight) %>%
        mutate(Weight = ifelse(Type%in%c(0,2), 1, Weight))
    } else {
      sf_tracks %>%
        mutate(Days = ifelse(Type==2, 1, Days)) %>%
        rename(Weight = allWeight)  %>%
        mutate(Weight = ifelse(Type%in%c(0,2), 1, Weight))
    }
    
    # doy <- as.numeric(format(as.POSIXct(glue::glue("2012-{input$tracks_day}"), format = "%Y-%d-%b"), "%j"))+1
    # if(input$species=="All") {
    #   sf_tracks %>% 
    #     filter(Days <= doy,
    #            Split == max(Split))
    # } else {
    #   sf_tracks %>% 
    #     filter(Species==input$species,
    #            Date <= doy) %>%
    #     mutate(Days = ifelse(Type==2, 1, Days)) %>%
    #     filter(Split==max(Split))
    # }
  })
  
  }
  
  ### Maps
  {
  output$MovMap <- renderLeaflet(
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      setView(lng = 170, lat = 30, zoom = 3) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "map") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "label") %>%
      onRender(
        "function(el, x) {
          L.control.zoom({
            position:'topright'
          }).addTo(this);
        }")
  )
  
  observe({
    leafletProxy("MovMap", data = speciesDistr()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~COLOR, fillOpacity = 0.6, weight = 0) %>%
      addPolylines(data = flyway, color = "#5F5962", weight = 1.2)
  })
  
  
  observe({
    proxy <- leafletProxy("MovMap", data = speciesDistr())
    proxy %>% clearControls() %>% clearGroup(group = "flags")

    if(!is.null(input$species) & input$flags) {
      if(!input$flagDens) {
      proxy <- proxy %>%
        addCircles(data = speciesFlags(),
                   group = 'flags',
                   color = ~Color,
                   opacity = 0.75,
                   weight = approx(range(speciesFlags()$Sample),
                                    c(5.5, 25),
                                    speciesFlags()$Sample)$y)
      }
      if(input$flagDens) {
        proxy <- proxy %>%
          addPolygons(data = flegDensity(),
                      fillColor = ~color,
                      fillOpacity = 0.5,
                      color = NA,
                      group = 'flags')
      }
    }
    
    if(input$tracksNW | input$tracksSW) {
      
      if((input$tracksNW & input$tracksSW) | (!input$tracksNW & !input$tracksSW)) {
        splitFilter <- c(1,2)
      }
      if(input$tracksNW & !input$tracksSW) splitFilter <- 1
      if(!input$tracksNW & input$tracksSW) splitFilter <- 2
      
      trackDat <- trackingData() %>%
        filter(Split %in% splitFilter)
      
      for(i in unique(trackingData()$ID)) {
        proxy <- proxy %>%
          addPolylines(data = trackDat %>%
                         filter(ID==i),
                       color = "#888683",
                       fillOpacity = 0.5,
                       weight = ~0.7,
                       lng = ~Lon,
                       lat = ~Lat, group = 'flags')
      }
      
      proxy <- proxy %>%
        addCircles(data = trackDat,
                   color = ~Color,
                   fillOpacity = 0.4,
                   weight = ~Weight, group = 'flags')
    }
  })
  }
  
  
  ###################
  #### Outbreaks ####
  ###################
  
  ### Points
  {
  outbreakData <- reactive({
      outbreakDat %>% 
        bind_rows(outbreakDat %>% st_shift_longitude) %>% filter(Year == input$hpai_month) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 
  })
  
  output$outbreakGraph <- renderPlot({
    ggplot(outbreakSM %>% mutate(color = ifelse(Year==input$hpai_month, "black", "transparent")), 
           aes(x = Year, y = sample, fill = is_wild, color = as.factor(color))) +
      geom_bar(stat="identity", show.legend = FALSE) +
      scale_fill_manual(values = c("chocolate", "slateblue")) +
      scale_color_manual(values = c("black", "transparent")) +
      xlab('') + ylab('# Reported cases') +
      theme_light()
  })
  
  }
  
  ### Maps 
  {
  output$Outbreak_Legend <- renderPlot({
    pl <- ggplot(tibble(y = c(1,1), x = c(1,2), col = c("Domestic birds", "Wild birds")), aes(x = x, y = y, color = col)) +
      geom_point(size = 10) +
      scale_color_manual(values = c("chocolate", "slateblue")) +
      theme(
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1.5, 'cm')
      )
    
    as_ggplot(get_legend(pl))
  })
  
  output$OutbreakMap <- renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      setView(lng = 170, lat = 30, zoom = 3) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "map") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "label") %>%
      onRender(
        "function(el, x) {
          L.control.zoom({
            position:'topright'
          }).addTo(this);
        }")
  )
  
  observe({
    if(input$outbreaks) {
    leafletProxy("OutbreakMap", data = outbreakData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(color = ifelse(outbreakData()$is_wild, "slateblue", "chocolate"), fillOpacity = 0.6, weight = 6)
    }
  })
  }
  
  
  ##########################
  #### Bird aggregations ###
  ##########################
  
  ### Maps  
  {
  cls  <- rev(paletteer_c("ggthemes::Red-Green Diverging", 6))
  brks <- c(0, 100, 1000, 5000, 10000, 50000, 2000000) 
  
  output$AggrMap <- renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "map") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "label") %>%
      addCircles(data = gridDens[[1]],
                 lng = ~lon,
                 lat = ~lat, 
                 stroke = FALSE,
                 fillColor = ~color, fillOpacity = 0.8, radius = 2000, group = 'MaxZoom') %>%
      addCircles(data = gridDens[[2]],
                 lng = ~lon,
                 lat = ~lat, 
                 stroke = FALSE,
                 fillColor = ~color, fillOpacity = 0.8, radius = 12500, group = 'MinZoom') %>%
      addPolygons(data = sba %>% st_transform(4326),
                  color = "black", weight = 2,
                  fill = FALSE, group = "MaxZoom") %>%
      groupOptions("MaxZoom", zoomLevels = 7:50) %>%
      groupOptions("MinZoom", zoomLevels = 1:6) %>%
      onRender(
        "function(el, x) {
          L.control.zoom({
            position:'topright'
          }).addTo(this);
        }")
  )
  }
  
}


#### Run the application ####
shinyApp(ui = ui, server = server)
