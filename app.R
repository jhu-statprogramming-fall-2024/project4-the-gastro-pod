library(shiny)
library(bslib)
library(osmdata)
library(tidyverse)
library(ggplot2); theme_set(theme_bw()); theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
library(rinat)
library(plotly)
library(lubridate)
library(DT)
library(paleobioDB)
library(httr)
library(jsonlite)
library(sf)
library(viridis)

# Function to Fetch GBIF Image
get_gbif_image <- function(taxon_name) {
  url <- paste0("https://api.gbif.org/v1/occurrence/search?mediaType=StillImage&scientificName=", URLencode(taxon_name))
  res <- httr::GET(url)
  
  if (res$status_code == 200) {
    data <- jsonlite::fromJSON(content(res, "text"))
    if (!is.null(data$results) && length(data$results) > 0 && !is.null(data$results$media[[1]])) {
      img_url <- data$results$media[[1]]$identifier[1]
      return(img_url)
    }
  }
  return(NULL)
}

# Define UI
ui <- page_sidebar(
    title = "SNASHBOARD",
    # Make the sidebar
    sidebar = sidebar(
        helpText(
            "Welcome to Snashboard, the snail dashboard! Technically this is a gastropod dashboard, but Gashboard just didn't have the same ring to it."
        ),
        # User enters their location
        textInput(
            "location",
            label = "Enter your location"
        ),
        actionButton(
            "enter",
            label = "Find snails near me"
        ),
        
        #Putting in sliderInput so that users can adjust size of image that pops up
        
        sliderInput(
          "image_size", 
          label = "Adjust Image Size:", 
          min = 100, max = 400, value = 200, step = 50
        ),
        
        # User can filter inaturalist observation dates
        uiOutput("yearControl"),
    
    # Data Source Selector
    selectInput(
      "data_source", 
      label = "Select Data Source:", 
      choices = c("iNaturalist" = "inat", "PBDB" = "pbdb"),
      selected = "inat"
    ),
    
    # Column Selector (Populated Dynamically)
    uiOutput("column_selector"),
    
    # Adding the download button inside the sidebar so that users can download csv of data
    downloadButton("download_combined", "Download Data (CSV)")
    ),
    # Inaturalist and paleobio db output
    # Card for iNaturalist output
    navset_card_underline(
        title = "Snails near you",
        nav_panel("Explore", 
        layout_columns(
                plotlyOutput("inat_map"),
                uiOutput("clicked_image"),
                plotlyOutput("inat_line")
        )
        ),
        nav_panel("Taxa", 
        layout_columns(
                plotlyOutput("inat_bar"),
                dataTableOutput("inat_abd")
        )
        ),
        nav_panel("All observations", dataTableOutput("inat_table")),
        nav_panel("", tags$img(src='willem-dafoe-gq-style3.png', alt = "Willem Dafoe is delighted by his fancy coat", align = "center"))
    ),
    # Card for pbdb output
    navset_card_underline(
        title = "Snails that were near you",
        nav_panel("Explore", 
        layout_columns(
                plotlyOutput("pbdb_map"),
                uiOutput("clicked_pbdb_image"),
                plotlyOutput("pbdb_eras")
        )
        ),
        nav_panel("Taxa", 
        layout_columns(
                plotlyOutput("pbdb_bar"),
                dataTableOutput("pbdb_abd")     
        )
        ),
        nav_panel("All observations", dataTableOutput("pbdb_table")),
        nav_panel("", "")
    )
)

server <- function(input, output, session){
    
    ##############
    ## GET DATA ##
    ##############

    # Get longitude/latitude bounds from location once user hits Enter
    bb <- eventReactive(input$enter, {
        req(input$location)
        getbb(input$location)
    })

    # Get map features (sf)
    map_feat <- eventReactive(input$enter,{
        opq(bbox = bb()) %>% 
            add_osm_feature(key = 'boundary', value = "administrative") %>% 
            osmdata_sf()
    })
    
    # Get Natural Earth water features 
    lakes <- read_sf("ne/ne_10m_lakes/ne_10m_lakes.shp")
    rivers <- read_sf("ne/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
    coast <- read_sf("ne/ne_10m_coastline/ne_10m_coastline.shp")
    ocean <- read_sf("ne/ne_10m_ocean/ne_10m_ocean.shp")

    # Get iNaturalist data
    inat_data <- eventReactive(input$enter,{
        bounds <- bb()[c(2,1,4,3)]
        get_inat_obs(taxon_name = "Gastropoda", bounds = bounds, quality = "research", maxresults = 10000)
    })
    # Render the iNaturalist image with slider input
    output$clicked_image <- renderUI({
      point_data_inat <- event_data("plotly_click", source = "inat_map")
      req(point_data_inat)
      
      point_id_inat <- point_data_inat$pointNumber + 1
      img_url_inat <- inat_data()$image_url[point_id_inat]
      img_size_inat <- paste0(input$image_size, "px")
      
      if (!is.null(img_url_inat) && nzchar(img_url_inat)) {
        tags$img(
          src = img_url_inat, 
          alt = "Observation Image", 
          style = paste("width:", img_size_inat, "; height:", img_size_inat, "; object-fit: contain; border: 1px solid black;")
        )
      } else {
        tags$p("No image available for this observation.")
      }
    })

    # Get paleobio db data
    pbdb_data <- eventReactive(input$enter,{
        bounds <- bb()[c(2,1,4,3)]
        pbdb_occurrences(
            base_name = "Gastropoda",
            show = c("coords", "classext"),
            vocab = "pbdb",
            limit = "all",
            lngmax = bounds[4], lngmin = bounds[2], latmax = bounds[3], latmin = bounds[1]
        )
    })
    # Handle PBDB Map Click and Display GBIF Image with Slider Input 
    output$clicked_pbdb_image <- renderUI({
      point_data_pbdb <- event_data("plotly_click", source = "pbdb_map")
      req(point_data_pbdb)
      
      point_id_pbdb <- point_data_pbdb$pointNumber + 1
      genus_name_pbdb <- pbdb_data()$genus[point_id_pbdb]
      
      img_url_pbdb <- get_gbif_image(genus_name_pbdb)
      img_size_pbdb <- paste0(input$image_size, "px")
      
      if (!is.null(img_url_pbdb) && nzchar(img_url_pbdb)) {
        tags$img(
          src = img_url_pbdb, 
          alt = paste("Fossil image of", genus_name_pbdb), 
          style = paste("width:", img_size_pbdb, "; height:", img_size_pbdb, "; object-fit: contain; border: 1px solid black;")
        )
      } else {
        tags$div(
          style = "padding: 20px; border: 1px solid black; background-color: #f9f9f9;",
          tags$p(
            style = "font-size: 16px; font-weight: bold; color: #333;",
            paste("No image available for genus:", genus_name_pbdb)
          )
        )
      }
    })
    # Dynamically Populate Column Selector
    output$column_selector <- renderUI({
      req(input$data_source)
      
      # Get column names based on the selected data source
      cols <- if (input$data_source == "inat") {
        colnames(inat_data())
      } else {
        colnames(pbdb_data())
      }
      
      # Create Checkboxes for Column Selection
      checkboxGroupInput(
        "selected_columns", 
        label = "Select Columns to Download:", 
        choices = cols, 
        selected = cols
      )
    })
    
    output$download_combined <- downloadHandler(
      filename = function() {
        paste0(input$data_source, "_data_", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        req(input$data_source, input$selected_columns)
        
        # Select the appropriate dataset
        data_to_download <- if (input$data_source == "inat") {
          inat_data()
        } else {
          pbdb_data()
        }
        
        # Check if any columns are selected
        if (length(input$selected_columns) == 0) {
          showNotification("Please select at least one column to download.", type = "error")
          return(NULL)
        }
        
        # Export only selected columns
        write.csv(
          data_to_download[, input$selected_columns, drop = FALSE], 
          file, 
          row.names = FALSE
        )
      }
    )

    ###############
    # REACTIVE UI #
    ###############

    output$yearControl <- renderUI({
        min_yr <- year(min(inat_data()$observed_on))
        max_yr <- year(max(inat_data()$observed_on))
      sliderInput(
            "year",
            label = "Filter iNaturalist observations by year",
            min = min_yr,
            max = max_yr,
            value = c(min_yr, max_yr)
        )
    })

    ##################################
    # DEALING WITH ERRORS OR NO DATA #
    ##################################

    no_data_p <- ggplot()+ geom_text(aes(x = 1, y = 1, label = "No data for this location"))

    ######################
    # INATURALIST OUTPUT #
    ######################

    # Make iNaturalist map
    output$inat_map <- renderPlotly({
      if (nrow(inat_data()) > 0){
        # Create ggplot object
        p <- inat_data() %>%
          # Filter by year
          filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>%
          # Plot
          ggplot() +
          geom_point(
            aes(x = longitude, y = latitude, color = scientific_name),
            show.legend = FALSE
          ) +
          geom_sf(data = map_feat()$osm_lines) +
          geom_sf(data = lakes, fill ="deepskyblue4")+  
          geom_sf(data = ocean, fill ="deepskyblue4")+  
          geom_sf(data = coast, color ="deepskyblue4")+  
          geom_sf(data = rivers, color ="deepskyblue4")+  
          xlim(bb()[c(1,3)]) +
          ylim(bb()[c(2,4)]) +
          theme(legend.position = "none")
        
        # Convert ggplot to plotly with source defined
        ggplotly(p, source = "inat_map")
      } else {
        no_data_p
      }
    })

    # Make iNaturalist obervations over-time
    output$inat_line <- renderPlotly({
        if (nrow(inat_data()) > 0){
            inat_data() %>% 
            # filter by year
            filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>% 
            mutate(year = year(observed_on)) %>%
            add_count(year, scientific_name) %>%
            ggplot(aes(x = year, y = n, color = scientific_name))+
            geom_line()+
            geom_point()+
            ylab("Number of research grade observations")+
            labs(title = "Number of observations per species over time", caption = "Data from iNaturalist.com")+
            theme(legend.position = "none")
        } else no_data_p
    })

    # Make iNaturalist abundance bar graph
    output$inat_bar <- renderPlotly({
        if (nrow(inat_data()) > 0){
            inat_data() %>%
            # filter by year
            filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>% 
            # Get genus variable
            separate(scientific_name, into = c("genus","species"), sep = " ", remove = F) %>%
            add_count(genus) %>%
            # Order genus by abundance
            mutate(genus = fct_reorder(genus, -n)) %>%
            # Plot
            ggplot(aes(x = genus, fill = scientific_name))+
            geom_bar() +
            theme(
                legend.position = "none",
                axis.text.x = element_text(angle = 60, hjust = 1)
            )
        } else no_data_p
    })

    # Make iNaturalist abundance data table
    output$inat_abd <- renderDataTable({
        inat_data() %>%
        # filter by year
        filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>%
        # add genus so they can sort the table with it
        separate(scientific_name, into = c("genus","species"), sep = " ", remove = F) %>%
        mutate(species = replace_na(species, "sp."))%>%
        add_count(scientific_name) %>%
        distinct(scientific_name, genus, species, common_name, n)
    })

    # Make iNaturalist observation data table
    output$inat_table <- renderDataTable({
        inat_data() %>%
        # filter by year
        filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>%
        # add genus so they can sort the table with it
        separate(scientific_name, into = c("genus","species"), sep = " ", remove = F) %>%
        mutate(
            species = replace_na(species, "sp."),
            # round coordinates for ease of display
            latitude = round(latitude, 5),
            longitude = round(longitude, 5)
            )
    })
    
    ###############
    # PBDB OUTPUT #
    ###############

    # Make paleobio db map
    output$pbdb_map <- renderPlotly({
      if (nrow(pbdb_data()) > 0){
        # Create ggplot object
        p <- pbdb_data() %>%
          # Plot
          ggplot() +
          # Use geom_jitter to avoid overlapping points
          geom_jitter(
            aes(x = lng, y = lat, color = min_ma),
            show.legend = FALSE
          ) +
          geom_sf(data = map_feat()$osm_lines) +
          geom_sf(data = lakes, fill ="deepskyblue4")+  
          geom_sf(data = ocean, fill ="deepskyblue4")+  
          geom_sf(data = coast, color ="deepskyblue4")+  
          geom_sf(data = rivers, color ="deepskyblue4")+  
          xlim(bb()[c(1,3)]) +
          ylim(bb()[c(2,4)]) +
          theme(legend.position = "none")+
          scale_color_viridis_c()
        
        # Convert ggplot to plotly with source defined
        ggplotly(p, source = "pbdb_map")
      } else {
        no_data_p
      }
    })
    # Make pbdb abundance bar graph
    output$pbdb_bar <- renderPlotly({
        if (nrow(pbdb_data()) > 0){
            pbdb_data() %>%
            add_count(genus) %>%
            # Order genus by abundance
            mutate(genus = fct_reorder(genus, -n)) %>%
            # Plot
            ggplot(aes(x = genus, fill = identified_name))+
            geom_bar() +
            theme(
                legend.position = "none",
                axis.text.x = element_text(angle = 60, hjust = 1)
            )
        } else no_data_p
    })

    # Make era-bars plot :)
    output$pbdb_eras <- renderPlotly({
        if (nrow(pbdb_data()) > 0){
            pbdb_data() %>%
            ggplot(aes(text =identified_name), size = 7)+
            geom_linerange(aes(y = order, xmax = max_ma, xmin = min_ma, color = early_interval ), 
                 alpha = .7, 
                 position = position_dodge((width = .25)))+
            xlim((c(max(pbdb_data()$min_ma), min(pbdb_data()$max_ma)))) +
            xlab("Million years ago")+
            ggtitle("Era Bars")
        } else no_data_p
    })

    # Make pbdb abundance data table
    output$pbdb_abd <- renderDataTable({
        pbdb_data() %>%
        count(order, family, genus)
    })

    # Make paleobio db table
    output$pbdb_table <- renderDataTable({
        pbdb_data()
    })
}

shinyApp(ui, server) 