require(shiny)
require(shinythemes)
require(shinydashboard)
require(shinyWidgets)
library(shinyjs)
library(shinyBS)
require(htmlwidgets)
library(echarts4r)
library(bslib)
library(colorRamps)
require(leaflet)
library(leafem)
library(tidyverse)
library(paletteer)
library(ggthemes)
library(wesanderson)
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
load("data/sba_trans.rda")
load("data/birdAggr.rda")

### Pre-calculations
{
  sf_tracks <- sf_tracks %>% 
    mutate(Color = ifelse(Species%in%c("Short-tailed Shearwater", "Wedge-tailed Shearwater"),
                   rainbow(365)[Date], Color))
  
  outbreakSM <- outbreakDat %>% st_drop_geometry() %>%
    group_by(Year, is_wild) %>% summarise(sample = sum(sample))
  
  legendDistr <- dist %>% st_drop_geometry %>% filter(!duplicated(CODE)) %>%
    dplyr::select(CODE, COLOR)
  
  outbreakDB <- outbreakDat %>% 
    bind_rows(outbreakDat %>% st_shift_longitude) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    mutate(color = ifelse(is_wild, "slateblue", "chocolate"))
  
  
  grps      <- readxl::read_xlsx("data/SpGroups.xlsx") 
  spPalette <- tibble(group = grps$GroupNew %>% unique(),
                      color = wes_palette("AsteroidCity1")[c(1,4,2,3,5)])
}

#### UI ####
ui <- fluidPage(
  div(class = "nav",
    
    tags$style(HTML(".navbar .navbar-header {float: left; }.navbar .navbar-nav {float: right;}.container {min-width: 950px}")),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    navbarPage(theme = shinytheme("flatly"), 
               collapsible = TRUE, fluid = TRUE,
               id = "navbar",
               windowTitle = "HPAI Dashboard",
               title = HTML("<a style='text-decoration:none;cursor:default;color:#FFFFFF;' class='active' href='#'>AviFluMap: a H5 Bird Flu Model Tool for Australia's Wild birds</a>"), 
                
               #### Home #####
               {
                 tabPanel("Home",
                      fluidRow(
                          column(12, align="center", htmlOutput("Home")),
                      )
                 )
               },
               
               #### Further info #####
               {
                 tabPanel("About the Data",
                          fluidRow(
                            column(12, align="center", htmlOutput("Information")),
                          )
                 )
               },
               
               #### HPAIV Outbreaks #####
               {
                 tabPanel("Global H5 HPAI Event Map",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              
                              leafletOutput("OutbreakMap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 30, width = 300, fixed=TRUE,
                                            draggable = FALSE, height = "auto",
                                            h4("Global H5 HPAI events:"),
                                            br(),
                                            fluidRow(
                                              column(12, sliderTextInput("hpai_month", label = "", 
                                                                         grid = TRUE, force_edges = FALSE, hide_min_max = TRUE,
                                                                         choices = as.character(seq(2005, 2024, by = 1)),
                                                                         selected = 2015, animate = T))
                                            ),
                                            plotOutput("Outbreak_Legend", width = 160, height = 80),
                                            br(),
                                            plotOutput("outbreakGraph", width = 275, height = 220)
                              )
                          )
                          
                 )
               },
               
               #### Bird migrations ####
               {
                 tabPanel("Bird Migrations",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              
                              leafletOutput("MovMap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 30, width = 300, fixed=TRUE,
                                            draggable = FALSE, height = "auto",
                                            
                                            h4("Shorebirds:"),
                                            
                                            selectInput(
                                              inputId = "species",
                                              label = "",
                                              selected = "Red-necked Stint",
                                              choices = c("All", meta$species[!grepl("Shearwater", meta$species)]),
                                            ),
                                            
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
                                                             )
                                            ),
                                            
                                            hr(style = "border-top: 1px solid #74b9e1;"),
                                            
                                            h4("Seabirds:"),
                                            
                                            selectInput(
                                              inputId = "seabirds",
                                              label = "",
                                              selected = "All",
                                              choices = c("All", meta$species[grepl("Shearwater", meta$species)]),
                                            ),
                                            
                                            h4("Migration tracks:"),
                                            
                                            fluidRow(
                                              column(8, materialSwitch("seabirdMigrations", "Migrations", status = "info", value = FALSE)),
                                              conditionalPanel("input.seabirdMigrations==true",
                                                               column(9, plotOutput("pieSeabird", width = 270, height = 270))
                                              )
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
                              )
                          )
                 )
               },
               
               #### Species at Risk ####
               {
                 tabPanel("Species at Risk",
                          fluidRow(
                            column(12, align="center", htmlOutput("SpeciesRisk")),
                          )
                 )
               },
               
               #### Aggregations #####
               {
                 tabPanel("Bird Aggregations",
                          div(class="outer", tags$head(includeCSS("styles.css")),
                              
                              leafletOutput("AggrMap", width="100%", height="100%") ,
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 30, width = 350, fixed = TRUE,
                                            draggable = FALSE, height = "auto",
                                            
                                            br(),
                                            h4('Bird Aggregation Hotspots:'),
                                            h5("Migratory shorebird, waterbird and seabird sites with 5,000 birds or more, and where these birds are spending the majority of their time while in Australia"),
                                            hr(style = "border-top: 1px solid #74b9e1;"),
                                            
                                            h4('Show Special Bird Areas:'),
                                            fluidRow(column(6, materialSwitch("showBirdAreas", "", status = "info", value = FALSE))),
                                            hr(style = "border-top: 1px solid #74b9e1;"),
                                            
                                            h4('Interactve details:'),
                                            fluidRow(column(6, materialSwitch("interactiveDetails", "", status = "info", value = FALSE))),
                                            
                                            br(),
                                            conditionalPanel(
                                              condition = "input.AggrMap_shape_click != null & input.interactiveDetails",
                                              h4("Maximum count:"),
                                              verbatimTextOutput("MaxCount"),
                                              conditionalPanel(
                                                condition = "input.AggrMap_shape_click != null",
                                                plotOutput("speciesPie", width = 330, height = 300),
                                                br(),
                                                fluidRow(
                                                  column(8, materialSwitch("speciesDetail", "Species summary", status = "info", value = FALSE))
                                                ),
                                                conditionalPanel(
                                                  condition = "input.speciesDetail",
                                                  plotOutput("speciesDetail", width = 330, height = 290)
                                                )
                                              )
                                            ),
                                            
                                            hr(style = "border-top: 1px solid #74b9e1;"),
                                            h5("Note: This is not a 'live' map. Whilst it is known that species distribution and density change over time (seasonally and between years) this map is static."),
                                            
                                            
                              )
                              
                          )
                 )
               },
               
               #### About #####
               {
                 tabPanel("About AviFluMap",
                    fluidRow(
                      column(12, align="center", htmlOutput("About")),
                    )
                 )
               }
               
    ),
  )
)


#### Server ####
server <- function(input, output) {
  
  ###############
  #### Home ####
  ###############
  
  {
    output$Home <- renderUI({
      tags$iframe(src="Home.html", style='width:80vw;height:80vh;', scrolling = 'yes', frameBorder = '0')
    })
  }
  
  ###################
  #### Outbreaks ####
  ###################
  
  ### Points
  {
    
    outbreakData <- reactive({
      outbreakDB %>% filter(Year == input$hpai_month)
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
    
    observeEvent(input$navbar, {
      leafletProxy("OutbreakMap", data = outbreakDB %>% filter(Year == input$hpai_month)) %>%
        addCircles(color = ~color, fillOpacity = 0.6, weight = 6)
    })
    
    observe({
      leafletProxy("OutbreakMap", data = outbreakData()) %>%
        clearShapes() %>%
        clearControls() %>%
        addCircles(color = ~color, fillOpacity = 0.6, weight = 6)
    })
    
  }
  
  #####################
  #### Migrations #####
  #####################
  
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
      sf_tracks %>% filter(!grepl("Shearwater", Species)) %>%
        mutate(Days = ifelse(Type==2, 1, Days)) %>%
        filter(Species == input$species) %>%
        rename(Weight = spWeight) %>%
        mutate(Weight = ifelse(Type%in%c(0,2), 1, Weight))
    } else {
      sf_tracks %>% filter(!grepl("Shearwater", Species)) %>%
        mutate(Days = ifelse(Type==2, 1, Days)) %>%
        rename(Weight = allWeight)  %>%
        mutate(Weight = ifelse(Type%in%c(0,2), 1, Weight))
    }
  })
  
  trackingSeabirdData <- reactive({
    if(input$seabirds!="All") {
      sf_tracks %>%
        filter(Species == input$seabirds)
    } else {
      sf_tracks %>%
        filter(Species == meta$species[14:15])
    }
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
    
    if(input$seabirdMigrations) {

      for(i in unique(trackingSeabirdData()$ID)) {
        proxy <- proxy %>%
          addPolylines(data = trackingSeabirdData() %>% filter(ID==i),
                       color = "#888683",
                       fillOpacity = 0.5,
                       weight = ~0.7,
                       lng = ~Lon,
                       lat = ~Lat, group = 'flags')
      }
    
      proxy <- proxy %>%
        addCircles(data = trackingSeabirdData(),
                   color = ~Color,
                   fillOpacity = 1,
                   weight = ~0.4, group = 'flags')
    }
    
  })
  
  output$pieSeabird <- renderPlot({
    data <- tibble(doy = 1:52, p = rep(1, 52), color = rainbow(52))
    
    ggplot(data, mapping = aes(x = rep(1, 52), y = p, fill = as.factor(doy))) +
      geom_bar(stat = "identity", show.legend = FALSE, color = "transparent") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = rainbow(52)) +
      scale_y_continuous(breaks = as.numeric(format(seq(as.POSIXct("2020-01-01"),
                                                        as.POSIXct("2020-12-01"), by = "month"), "%U")),
                         label = format(seq(as.POSIXct("2020-01-01"),
                                            as.POSIXct("2020-12-01"), by = "month"), "%b")) +
      xlim(c(0.01, 1.6)) +
      theme_minimal() +
      labs(x = "", y = "") +
      theme(axis.text.x = element_text(size = 16),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  }
  
  ##########################
  #### Bird aggregations ###
  ##########################
  
  cellSelect <- reactive({
    if(!is.null(input$AggrMap_shape_click$lat)) {
      RID <- st_point(c(input$AggrMap_shape_click$lng, input$AggrMap_shape_click$lat)) %>%
        st_sfc(crs = 4326) %>% st_intersection(birdAggr[[2]],.) %>% pull(RID) %>% suppressMessages() %>% suppressWarnings()
      ifelse(length(RID)>0, RID, -100)
    } else NULL
  })
  
  output$speciesPie <- renderPlot({
    if(cellSelect()>0) {
      tmp <- birdAggr[[1]] %>% filter(RID==cellSelect())
    
      tmp %>% dplyr::select(spPalette$group) %>% pivot_longer(cols = everything()) %>%
        ggplot(., mapping = aes(x = "", y = value, fill = name)) +
        geom_bar(stat="identity", width = 1, show.legend = T) +
        scale_fill_manual(values = spPalette$color, breaks = spPalette$group) +
        coord_polar("y", start=0) +
        geom_text(aes(label = ifelse(value*100<5, '', paste0(round(value*100,0), "%"))),
                  position = position_stack(vjust=0.5),
                  size = 7, color = "grey20") +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_void() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 15)) +
        guides(fill = guide_legend(nrow = 2, byrow=TRUE))
    }
  })
  
  output$speciesDetail <- renderPlot({
    if(cellSelect()>0) {
      tmp <- birdAggr[[1]] %>% filter(RID==cellSelect()) %>% 
        dplyr::select(-spPalette$group, -RID, -Max_of_Max, -SPECIES_COUNT) %>% 
        pivot_longer(cols = everything()) %>%
        filter(!is.na(value), value >0) %>%
        left_join(grps[,1:2], by = join_by("name"=="Group"))
      
      ggplot(tmp, mapping = aes(x = reorder(name, value), y = value*100)) +
          geom_bar(stat="identity", width = 0.7, fill = "grey40", show.legend = F) +
          coord_flip() +
          labs(x = NULL, y = "Percentage") +
          scale_x_discrete(labels = gsub("(break)", "\n", tmp$Label)) +
          theme_light() +
          theme(axis.text.y = element_text(size = 13))
    }
  })
  
  output$MaxCount <- renderText({
    if(cellSelect()>0) {
      birdAggr[[1]] %>% filter(RID==cellSelect()) %>% pull(Max_of_Max)
    } else "no selection"
    })
  
  ### Maps  
  {
  cls  <- rev(paletteer_c("ggthemes::Red-Green Diverging", 6))
  brks <- c(0, 100, 1000, 5000, 10000, 50000, 2000000) 
  labelText <- glue::glue("<b>Special Bird Area: </b> {sba$SBIRD_AREA}")
  
  output$AggrMap <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "Basemap") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Basemap") %>%
      setView(lng = 131, lat = -28, zoom = 5) %>%
      # addGeoRaster(
      #           birdAggr[[4]],
      #           opacity = 0.6,
      #           colorOptions = colorOptions(
      #             breaks = brks,
      #             palette = cls),
      #           group = 'MidZoom') %>%
      addGeoRaster(
                birdAggr[[3]],
                opacity = 0.6,
                colorOptions = colorOptions(
                  breaks = brks,
                  palette = cls),
                group = 'raster') %>%
      # addPolygons(data = birdAggr[[2]],
      #             weight = 1,
      #             color = "grey40",
      #             fillColor = ~ Color,
      #             fillOpacity = 0.8,
      #             layerId = ~ RID,
      #             group = "MaxZoom") %>%
      # groupOptions("MaxZoom", zoomLevels = 7:20) %>%
      # groupOptions("MidZoom", zoomLevels = 1:6) %>%
      addLegend("bottomright", 
                colors = cls,
                labels = c("1-100", "100-1,000", "1,000-5,000", "5,000-10,000", 
                           "10,000-50,000", "50,000-2,000,000"),
                title = "Abundance",
                opacity = 1) %>%
      onRender(
        "function(el, x) {
          L.control.zoom({
            position:'topright'
          }).addTo(this);
        }")
    
  })
  
  
  observe({
    proxy <- leafletProxy("AggrMap") %>%
      clearShapes()
    
    if(input$showBirdAreas & input$interactiveDetails) {
      proxy <- proxy %>% 
        clearGroup('raster') %>%
        addPolygons(data = sba,
                    color = "black", weight = 1,
                    fillColor = "grey50", fillOpacity = 0.2,
                    label = lapply(labelText, htmltools::HTML),
                    labelOptions = labelOptions(noHide = F, direction = "top")) %>%
        addPolygons(data = birdAggr[[2]],
                    weight = 1,
                    color = "grey40",
                    fillColor = ~ Color,
                    fillOpacity = 0.8,
                    layerId = ~ RID)
    }
    
    if(!input$showBirdAreas & input$interactiveDetails) {
      proxy <- proxy %>%
        clearGroup('raster') %>%
        addPolygons(data = birdAggr[[2]],
                    weight = 1,
                    color = "grey40",
                    fillColor = ~ Color,
                    fillOpacity = 0.8,
                    layerId = ~ RID)
    }
    
    if(input$showBirdAreas & !input$interactiveDetails) {
      proxy <- proxy %>% 
        addGeoRaster(
          birdAggr[[3]],
          opacity = 0.6,
          colorOptions = colorOptions(
            breaks = brks,
            palette = cls),
          group = 'raster') %>%
        addPolygons(data = sba,
                    color = "black", weight = 1,
                    fillColor = "grey50", fillOpacity = 0.2,
                    label = lapply(labelText, htmltools::HTML),
                    labelOptions = labelOptions(noHide = F, direction = "top"))
    }
    
    if(!input$showBirdAreas & !input$interactiveDetails) {
    proxy <- proxy %>%
      addGeoRaster(
        birdAggr[[3]],
        opacity = 0.6,
        colorOptions = colorOptions(
          breaks = brks,
          palette = cls),
        group = 'raster')
    }
  })
  
  
  
  }
  
  ##########################
  #### Species at Risk ####
  ##########################
  
  {
  output$SpeciesRisk <- renderUI({
    tags$iframe(src="Australian-birds-at-risk-of-HPAI.html", style='width:90vw;height:80vh;', scrolling = 'yes', frameBorder = '1')
  })
  }
  
  #####################
  #### Information ####
  #####################
  
  {
    output$Information <- renderUI({
      tags$iframe(src="Information.html", style='width:80vw;height:80vh;', scrolling = 'yes', frameBorder = '0')
    })
  }
  
  ###############
  #### About ####
  ###############
  
  {
    output$About <- renderUI({
      tags$iframe(src="About.html", style='width:80vw;height:80vh;', scrolling = 'yes', frameBorder = '0')
    })
  }

}


#### Run the application ####
shinyApp(ui = ui, server = server)
