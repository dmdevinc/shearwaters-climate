#
# Shiny web application for Shearwater Project
#

# === Packages ===
library(shiny)          # Shiny framework
library(shinydashboard) # dashboard layout (header, sidebar, body)
library(tidyverse)      # data manipulation, includes ggplot2
library(readxl)         # read Excel files
library(plotly)         # interactive plots
library(scales)         # axis formatting (commas, percentages, etc.)
library(sf)             # mapping
library(rnaturalearth)  # base maps
library(ggspatial)      # north arrow

# === Rescources ===
# Appearance: https://rstudio.github.io/shinydashboard/appearance.html#icons

# === Load Data ===
# library(here) # anchors file paths to project root instead of app subfolder
# data <- readxl::read_excel(here("data", "bird_sst.xlsx"))
# # read in separate data for mapping
# bird_df <- readxl::read_excel(here("data", "clean_bird.xlsx"))
# sst_df <- readxl::read_excel(here("data", "clean_sst.xlsx"))

data <- readxl::read_excel("data/bird_sst.xlsx")
bird_df <- readxl::read_excel("data/clean_bird.xlsx")
sst_df <- readxl::read_excel("data/clean_sst.xlsx")

data <- data %>%
  mutate(across(where(is.character), as.factor)) %>% 
  # convert all char columns to factors for filtering/leveling
  mutate(daily_max_count = as.numeric(daily_max_count)) 
  # ensure count column is numeric

# === ENSO Phases ===
# Manually define El Niño/La Niña/Neutral periods for background shading
# Source: https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/enso/roni/
enso_phases <- data.frame(
  start = as.Date(c("2015-06-01", "2016-05-01",
                    "2017-01-01", "2017-11-01",
                    "2018-06-01", "2018-10-01",
                    "2020-05-01", "2021-03-01",
                    "2023-04-01", "2023-10-01",
                    "2024-06-01")),
  end   = as.Date(c("2016-04-30", "2016-12-31",
                    "2017-10-31", "2018-05-31",
                    "2018-09-30", "2020-04-30",
                    "2021-02-28", "2023-03-31",
                    "2023-09-30", "2024-05-31",
                    "2024-12-31")),
  phase = c("El Niño", "Neutral",
            "La Niña", "Neutral",
            "El Niño", "Neutral",
            "La Niña", "La Niña",
            "Neutral", "El Niño",
            "La Niña")
)

# === Define Sidebar Checkbox Options ===
species_choices <- levels(data$common_name)

# === UI ===
ui <- dashboardPage(
  skin = "blue", 
  
  dashboardHeader(
    title = "Shearwater Watch",
    titleWidth = 230
  ),
  
  dashboardSidebar(
    sidebarMenu(
      # nav tabs — each tabName must match a tabItem below in dashboardBody
      id = "sidebar",
      menuItem("Overview",          tabName = "tab_overview", icon = icon("home")),
      menuItem("Abundance & Trends",tabName = "tab_trends",   icon = icon("chart-line")),
      menuItem("SSTA & Shearwaters", tabName = "tab_sst",      icon = icon("water")),
      menuItem("Seasonal Patterns", tabName = "tab_seasonal", icon = icon("calendar")),
      menuItem("Study Area Map", tabName = "tab_map", icon = icon("map"))
    ),
    hr(), # line
    # species filter — checkboxes
    checkboxGroupInput(
      inputId  = "species_filter",
      label    = "Species",
      choices  = species_choices,
      selected = species_choices # all selected by default
    ),
    # only on trends tab
    conditionalPanel(
      condition = "input.sidebar == 'tab_trends'",
      checkboxInput("log_scale", "Log Scale (counts)", TRUE),
      checkboxInput("show_enso", "Show ENSO Phases", TRUE)
    ),
    # only on sst tab
    conditionalPanel(
      condition = "input.sidebar == 'tab_sst'",
      checkboxInput("log_scale", "Log Scale (counts)", TRUE)
      ),
    hr() # line
  ),
  
  dashboardBody(
    tabItems(
      
      # === Tab: Overview ===
      # static informational page
      tabItem(
        tabName = "tab_overview",
        fluidRow(
          box(
            title = "Monterey Bay Shearwater Watch",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h4("About This Project"),
            p("One of the seasonal joys in the Monterey Bay Area is witnessing 
              thick rafts of shearwaters on the ocean as they migrate northward 
              from their southern breeding grounds. While living in the area, I 
              observed this phenomena for many years and was inspired to create 
              this Shiny Dashboard to investigate it."),
            br(),
            p("In this project, I aim to visualize and evaluate: 
              a) trends in annual shearwater counts in Monterey County, 
              b) when birds arrive and if this change year to year, 
              c) if there is an observable effect of sea surface temperature 
              anomalies (SSTA) on shearwater counts."),
            br(),
            h4("Shearwaters"),
            p("Shearwaters have one of the longest migrations in the animal 
            kingdom, where they travel northward after breeding in the southern 
            hemisphere to western North America and Europe. For this project, I 
            focused on two species with similar life history strategies, the 
            Sooty (SOSH) and Pink-Footed Shearwaters (PFSH). PFSH are one of the
            most abundant seabirds in the world, while SOSH are listed as 
            vunerable by the IUCN and endangered by Chile and Canada. Both of 
            these species migrate through the Monterey Bay Area in spring and 
            summer and face similar threats, including mortality from being
            caught as fishery bycatch and pressure from habitat degradation and 
            predators at their island breeding colonies.
            More information about the life history of these species can be 
            found at Birds of the World online",
              a("(Pink-footed Shearwater,",
                href = "https://birdsoftheworld.org/bow/species/pifshe/cur/introduction?login", 
                target = "_blank"),
              a("Sooty Shearwater)",
                href = "https://birdsoftheworld.org/bow/species/sooshe/cur/introduction?login", 
                target = "_blank"),
            ),
            br(),
            h4("Data Sources"),
            p("Shearwater observation data in Monterey County between 2015 and 
               2025 were sourced from ",
              a("eBird (Cornell Lab of Ornithology, Ithaca, New York, version 2025)", 
                href = "  https://ebird.org/data/download", target = "_blank"),
              ", a community science platform hosting millions of bird 
              observations worldwide. SSTA data were obtained from ",
              a("NOAA 0.25-degree Daily Optimum Interpolation Sea Surface Temperature (OISST, Version 2.1)", 
                href = " https://doi.org/10.25921/RE9P-PT57", target = "_blank"),
              ", and represents gridded weekly sea surface temperature for 
              Monterey Bay from 2020 to 2025."),
            
            br(),
            h4("Methods"),
            p("For the eBird observation dataset, I focused on traveling, 
              stationary, or pelagic observations. I removed duplicate 
              records from group observations, indicated by a group 
              identification number, since these counts represent the 
              same individual birds. To avoid double-counting birds, I also only
              evaluated the daily maximum count on days where multiple observers
              recorded the same species on the same day. I did not remove
              outlier counts since shearwaters are known for forming massive 
              rafts and excluding them could obscure these patterns. Weekly SSTA
              were aggregated to monthly averages for 
              visualization and correlation analysis. Since shearwaters have a 
              strong seasonal cycle, I chose to evaluate SSTA instead of SST. 
              To contextualize variability in Shearwater counts, El 
              Niño-Southern Oscillations (ENSO) phase annotations were created 
              following the Relative Oceanic Niño Index (RONI) classifications from ",
              a("NOAA Climate Prediction Center.", 
                href = "https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/enso/roni/", target = "_blank")),
            
            br(),
            h4("Key Findings"),
            tags$ul(
              tags$li("SOSH peak in September-October and appear in higher 
                      numbers than PFSH which tend to appear more March-April 
                      and again in Fall."),
              tags$li("SOSH counts are relatively stable across the record with 
                      some interannual variability while PFSH show a spike in 
                      2022 in a La Niña, though causality is difficult to 
                      establish visually and the sample size is low."),
              tags$li("Both SOSH and PFSH were found to be not significantly 
                      associated with SSTA 
                      (Spearman Correlation p < 0.3), which suggests that SSTA 
                      alone does not predict shearwater counts in Monterey Bay, 
                      at least at the monthly resolution."),
              tags$li("Shearwater observations are spatially clustered around 
                      the underwater canyons where upwelling occurs and there 
                      are high prey concentrates which is expected, though these
                      are areas that most pelagic tours target which could 
                      add a bias"),
              br(),
              p("Overall, seasonality seems explains more of the variation 
                ovserved in shearwater presence in Monterey Bay than SSTA.")
            ),
            
            br(),
            h4("About"),
            p("Built by Danielle Devincenzi using R and Shiny. Data processing, 
            aggregation, and visualization code available on ",
              a("GitHub.", href = "https://github.com/dmdevinc", 
                target = "_blank"),
              " For questions or collaborations please reach out via ",
              a("LinkedIn.", href = "http://www.linkedin.com/in/danielle-devincenzi", 
                target = "_blank"))
          )
        )
      ),
      
      # === Tab: Abundance & Trends ===
      # Time series of daily max counts — Monterey County, selected species
      tabItem(
        tabName = "tab_trends",
        fluidRow(
          box(
            title = "Daily Maximum Counts Over Time",
            width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_trends", height = "450px")
          )
        ),
        fluidRow(
          box(
            title = "Note",
            width = 12, status = "primary", solidHeader = FALSE,
            p("Multiple observer records for the same date and location are 
            collapsed to the daily maximum to avoid double-counting the same 
            birds, therefore each point represents the highest single count 
            recorded on a given day. Counts are displayed on a log scale by 
            default, since observations span several orders of magnitude 
            (toggle off this option on the left to view raw counts).")
          )
        )
      ),
      
      # === Tab: SST & Shearwaters ===
      # dual-axis plot of SST and bird counts for Monterey County only
      # includes Spearman correlation table
      tabItem(
        tabName = "tab_sst",
        fluidRow(
          box(
            title = "Sea Surface Temperature Anomalies (SSTA) & Shearwater Counts — Monterey County",
            width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_sst", height = "450px")
          )
        ),
        fluidRow(
          box(
            title = "Spearman Correlation — SSTA vs. Count",
            width = 6, status = "primary", solidHeader = TRUE,
            tableOutput("table_correlation")
          ),
          box(
            title = "Interpretation Note",
            width = 6, status = "primary", solidHeader = FALSE,
            p("Shearwater counts are similiarly spread across both warm and cool
              anomalies with high counts appearing on either side of the zero 
              line. This is consistent with the Spearman Correlation results 
              showing no significant correlation between SSTA and counts.")
          )
        )
      ),
      
      # === Tab: Seasonal Patterns ===
      # bubble plot showing monthly max counts by year
      tabItem(
        tabName = "tab_seasonal",
        fluidRow(
          box(
            title = "Seasonal Arrival Patterns by Month and Year",
            width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_seasonal", height = "450")
          )
        )
      ),
      
      tabItem(
        tabName = "tab_map",
        fluidRow(
          box(
            title = "Study Area — Monterey County",
            width = 12, status = "primary", solidHeader = TRUE,
            plotOutput("plot_map", height = "450")
          )
        )
      )
      
    )
  )
)


# === Server ===
server <- function(input, output, session) {
  
  # === Reactive: filtered_data ===
  # filters full dataset by species sidebar input, Monterey County only
  # used by plot_trends and plot_seasonal
  filtered_data <- reactive({
    data %>%
      filter(
        common_name %in% input$species_filter,
        as.character(county) == "Monterey"
      )
  })
  
  # === Reactive: monterey_data ===
  # filters to Monterey County + selected species + only rows with SST data
  # used by plot_sst and table_correlation
  monterey_data <- reactive({
    data %>%
      filter(
        as.character(county) == "Monterey",
        as.character(common_name) %in% input$species_filter,
        !is.na(sst_monthly_avg) # restrict to date range where SST data exists
      )
  })
  
  # === Output: plot_trends ===
  # scatter plot of daily max counts over time, colored by species
  output$plot_trends <- renderPlotly({
    
    df <- filtered_data()
    
    # aggregate to monthly max to reduce noise
    df_monthly <- df %>%
      group_by(month_date = floor_date(date, "month"), common_name) %>%
      summarise(monthly_max = max(daily_max_count, na.rm = TRUE), .groups = "drop")
    
    y_min <- if (input$log_scale) 0.1 else 0
    y_max <- max(df_monthly$monthly_max, na.rm = TRUE) * 1.1
    
    p <- ggplot() +
      # ENSO background bands
      {if (input$show_enso)
        geom_rect(data = enso_phases,
                  aes(xmin = start, xmax = end,
                      ymin = y_min, ymax = 1500,
                      fill = phase),
                  alpha = 0.15, inherit.aes = FALSE)
      } +
      # raw monthly points — small and subtle
      geom_point(data = df_monthly,
                 aes(x = month_date, y = monthly_max,
                     color = common_name,
                     text = paste0("Month: ", format(month_date, "%b %Y"),
                                   "<br>Species: ", common_name,
                                   "<br>Monthly Max: ", scales::comma(monthly_max))),
                 alpha = 0.7, size = 1.2) +
      # smoothed trend line per species
      geom_smooth(data = df_monthly,
                  aes(x = month_date, y = monthly_max,
                      color = common_name),
                  method = "loess", span = 0.3,
                  se = TRUE, alpha = 0.20, linewidth = 1) +
      scale_fill_manual(
        values = c("El Niño" = "#B5485A", "La Niña" = "#5B8DB8", "Neutral" = "gray80"),
        name = ""
      ) +
      scale_color_manual(
        values = c("Sooty Shearwater"       = "#5C6B6B",
                   "Pink-footed Shearwater" = "#B5485A"),
        name = " "
      ) +
      {if (input$log_scale) scale_y_log10(labels = scales::comma_format(accuracy = 1))
        else scale_y_continuous(labels = scales::comma_format(accuracy = 1))
      } +
      labs(x = "Date", y = "Monthly Max Count") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top"
      )
    
    pl <- ggplotly(p, tooltip = "text") %>%
      config(
        modeBarButtonsToRemove = c(
          "lasso2d",
          "select2d",
          "zoomIn2d",
          "zoomOut2d",
          "autoScale2d",
          "hoverCompareCartesian",
          "hoverClosestCartesian"
        ),
        displaylogo = FALSE
      )
    
    # clean up legend labels and remove duplicates
    seen <- c()
    for (i in seq_along(pl$x$data)) {
      pl$x$data[[i]]$name <- gsub("\\(|\\)|,1|,\\d+", "", pl$x$data[[i]]$name)
      pl$x$data[[i]]$name <- trimws(pl$x$data[[i]]$name)
      
      # hide duplicate legend entries
      if (pl$x$data[[i]]$name %in% seen) {
        pl$x$data[[i]]$showlegend <- FALSE
      } else {
        seen <- c(seen, pl$x$data[[i]]$name)
      }
    }
    
    pl
  })
  
  # === Output: plot_sst ===
  output$plot_sst <- renderPlotly({
    
    df <- monterey_data() %>%
      filter(!is.na(daily_max_count), !is.na(anom_monthly_avg)) %>%
      group_by(date, anom_monthly_avg, common_name) %>%
      summarise(total_count = sum(daily_max_count, na.rm = TRUE), .groups = "drop")
    
    # run spearman correlation
    cor_result <- cor.test(df$anom_monthly_avg, df$total_count, method = "spearman")
    cor_label <- paste0("Spearman ρ = ", round(cor_result$estimate, 2),
                        ", p = ", round(cor_result$p.value, 3))
    
    p <- ggplot(df, aes(x = anom_monthly_avg, y = total_count,
                        text = paste0("Date: ", format(date, "%b %Y"),
                                      "<br>SSTA: ", round(anom_monthly_avg, 2), "°C",
                                      "<br>Total Count: ", scales::comma(total_count)))) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
      geom_point(aes(x = anom_monthly_avg, y = total_count,
                     color = common_name,
                     text = paste0("Date: ", format(date, "%b %Y"),
                                   "<br>SSTA: ", round(anom_monthly_avg, 2), "°C",
                                   "<br>Total Count: ", scales::comma(total_count))),
                 alpha = 0.6, size = 2.5) +
      scale_color_manual(
        values = c("Sooty Shearwater"       = "#5C6B6B",
                   "Pink-footed Shearwater" = "#B5485A"),
        name = " "
      ) +      geom_smooth(method = "lm", se = TRUE, color = "#B5485A", fill = "#B5485A", alpha = 0.15) +
      annotate("text", x = Inf, y = Inf, label = cor_label,
               hjust = 1.05, vjust = 1.5, size = 4, color = "gray30") +
      {if (input$log_scale)
        scale_y_log10(labels = scales::comma_format(accuracy = 1))
        else
          scale_y_continuous(labels = scales::comma_format(accuracy = 1))
      } +
      labs(x = "Sea Surface Temperature Anomaly (°C)",
           y = "Total Count") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top"
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(
        modeBarButtonsToRemove = c("lasso2d", "select2d", "zoomIn2d",
                                   "zoomOut2d", "autoScale2d"),
        displaylogo = FALSE
      )
  })
  
  # === Output: table_correlation ===
  # Spearman correlation between monthly SST and daily max count
  # Grouped by species, Monterey County only, restricted to rows with SST data
  output$table_correlation <- renderTable({
    
    monterey_data() %>%
      filter(!is.na(anom_monthly_avg)) %>%
      group_by(common_name) %>%
      summarise(
        N       = n(),
        rho     = cor(anom_monthly_avg, daily_max_count, method = "spearman"),
        p_value = cor.test(anom_monthly_avg, daily_max_count, method = "spearman")$p.value,
        .groups = "drop"
      ) %>%
      mutate(
        rho          = round(rho, 3),
        p_value      = round(p_value, 4),
        # Add significance stars based on p-value thresholds
        Significance = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01  ~ "**",
          p_value < 0.05  ~ "*",
          TRUE            ~ "ns"
        )
      ) %>%
      rename(
        Species   = common_name,
        `Rho (ρ)` = rho,
        `P-value` = p_value
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # === Output: plot_seasonal ===
  # Bubble plot: month (x) vs year (y), bubble size = monthly max count
  # Colored by species, Monterey County only
  output$plot_seasonal <- renderPlotly({
    
    df <- filtered_data() %>%
      filter(!is.na(daily_max_count)) %>%
      # Aggregate to monthly max per year/species combination
      group_by(year, month, common_name) %>%
      summarise(monthly_max = max(daily_max_count, na.rm = TRUE),
                .groups = "drop") %>%
      # Convert numeric month to ordered abbreviation factor for correct x-axis order
      mutate(month_label = factor(month.abb[month], levels = month.abb))
    
    p <- ggplot(df, aes(x = month_label, y = factor(year),
                        size  = monthly_max,
                        color = common_name,
                        text  = paste0("Year: ", year,
                                       "<br>Month: ", month_label,
                                       "<br>Species: ", common_name,
                                       "<br>Max Count: ", scales::comma(monthly_max)))) +
      geom_point(alpha = 0.7) +
      scale_y_discrete(expand = expansion(add = c(1, 1))) +
      scale_size_continuous(
        name   = "Max Count",
        range  = c(1, 12), # min and max bubble size in pts
        labels = scales::comma
      ) +
      scale_color_manual(
        values = c("Sooty Shearwater"       = "#5C6B6B",
                   "Pink-footed Shearwater" = "#B5485A"),
        name = " "
      ) +
      labs(x = "Month", y = "Year") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major = element_line(color = "gray90"),
            legend.position  = "top",
            axis.text.x = element_text(angle = 40) # angled x labels to avoid overlap
      )
    
    # Convert to interactive plotly, select which tools to get rid of
    pl <- ggplotly(p, tooltip = "text") %>%
      config(
        modeBarButtonsToRemove = c(
          "lasso2d",
          "select2d",
          "zoomIn2d",
          "zoomOut2d",
          "autoScale2d",
          "hoverCompareCartesian",
          "hoverClosestCartesian"
        ),
        displaylogo = FALSE
      ) 
    
    # Clean up auto-generated legend labels from ggplotly
    for (i in seq_along(pl$x$data)) {
      pl$x$data[[i]]$name <- gsub("\\(|\\)|,1|,\\d+", "", pl$x$data[[i]]$name)
      pl$x$data[[i]]$name <- trimws(pl$x$data[[i]]$name)
    }
    
    pl
  })

# === Output: map ===
output$plot_map <- renderPlot({
  
  # set crs and species toggle option (since seperate df from prior graphs)
  noaa_m_sf <- sst_df %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  bird_sf <- bird_df %>%
    filter(common_name %in% input$species_filter) %>% # species toggle
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  land <- ne_download(scale = 10, type = "land", category = "physical", returnclass = "sf")
  cities <- ne_download(scale = "large", type = "populated_places", returnclass = "sf")
  coast <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")
  
  # filter to just the cities you want in your bounding box
  cities_sub <- cities %>%
    filter(NAME %in% c("Monterey", "Santa Cruz"))
  
  ggplot() +
   geom_sf(data = land, fill = "#F0EBD8") +
    geom_sf(data = coast, color = "#D4C8A8", linewidth = 1.5) +
    geom_sf(data = bird_sf, aes(color = common_name), size = 4, alpha = 0.5) +
    geom_sf(data = noaa_m_sf, aes(color = "SST Record Locations"), 
            size = 4, shape = 4, stroke = 1) +
    geom_sf_text(data = cities_sub, aes(label = NAME), size = 5, 
                 nudge_x = 0.28, nudge_y = 0.05, color = "gray20") +
    scale_color_manual(
      name = "Data Source",
      values = c(
        "Sooty Shearwater"       = "#5C6B6B",
        "Pink-footed Shearwater" = "#B5485A",
        "SST Record Locations"   = "#1B3A4B"
      )
    ) +
    coord_sf(xlim = c(-123.9, -120.5), ylim = c(35.2, 37.1)) +
    annotation_north_arrow(
      location = "tr", which_north = "true",
      style = north_arrow_orienteering(line_col = "black", fill = c("black", "black")),
                  height = unit(1.5, "cm"), width = unit(1.3, "cm")
    ) +
    annotation_scale(
      location = "br", width_hint = 0.4,
      bar_cols = c("black", "white"), text_cex = 0.75
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      axis.text = element_text(size = 14, color = "darkgray"),
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.999, 0.08),
      legend.justification = c(1, 0),
      legend.box.background = element_rect(
        fill = "white", color = "black", linewidth = 1),
      legend.text = element_text(size = 14),
      panel.background = element_rect(fill = "aliceblue", color = NA)
    )
})

} # close

# === Run ===
shinyApp(ui = ui, server = server)