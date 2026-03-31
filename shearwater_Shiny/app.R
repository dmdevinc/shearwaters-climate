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

# === Rescources ===
# Appearance: https://rstudio.github.io/shinydashboard/appearance.html#icons

# === Load Data ===
library(here) # anchors file paths to project root instead of app subfolder
data <- readxl::read_excel(here("data", "bird_sst.xlsx"))
data <- data %>%
  mutate(across(where(is.character), as.factor)) %>% 
  # convert all char columns to factors for filtering/leveling
  mutate(daily_max_count = as.numeric(daily_max_count)) 
  # ensure count column is numeric

# === ENSO Phases ===
# Manually define El Niño/La Niña/Neutral periods for background shading
# Source: https://ggweather.com/enso/
enso_phases <- data.frame(
  start = as.Date(c("2015-01-20", "2015-06-01", "2017-01-01", "2018-01-01",
                    "2018-05-01", "2020-09-01", "2023-04-01", "2023-10-01",
                    "2024-06-01")),
  end   = as.Date(c("2015-05-31", "2016-12-31", "2017-12-31", "2018-04-30",
                    "2020-08-31", "2023-03-31", "2023-09-30", "2024-05-31",
                    "2024-12-03")),
  phase = c("Neutral", "El Niño", "Neutral", "La Niña",
            "Neutral", "La Niña", "Neutral", "El Niño",
            "La Niña")
)

# === Define Sidebar Checkbox Options ===
species_choices <- levels(data$common_name)

# === Custom CSS for "Other" Checkbox Spacing ===
tags$head(
  tags$style(HTML("
    /* container spacing */
    .tight-checkbox {
      margin-top: -5px !important;  /* space between title and first checkbox */
    }
  "))
)

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
      menuItem("Overview",          tabName = "tab_overview", icon = icon("home")),
      menuItem("Abundance & Trends",tabName = "tab_trends",   icon = icon("chart-line")),
      menuItem("SST & Shearwaters", tabName = "tab_sst",      icon = icon("water")),
      menuItem("Seasonal Patterns", tabName = "tab_seasonal", icon = icon("calendar"))
    ),
    hr(), # line
    # species filter — checkboxes
    checkboxGroupInput(
      inputId  = "species_filter",
      label    = "Species",
      choices  = species_choices,
      selected = species_choices # all selected by default
    ),
    # create custom "Other" heading with css
    tags$div(
      tags$strong(
        style = "margin-left: 14px; display: block; margin-bottom: -14px;", 
        "Other"
      ),
      # toggle log scale and ENSO with custom css formatting (from above ui)
      tags$div(class = "tight-checkbox",
               checkboxInput("log_scale", "Log Scale (counts)", TRUE),
               checkboxInput("show_enso", "Show ENSO Phases", TRUE)
      )
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
            p("One of the joys of spending summers in Santa Cruz, CA, is 
              witnessing the thick rafts of floating shearwaters on the ocean as
              they migrate north from southern breeding grounds. While living in
              the area, I observed this seasonal occurrence for many years and 
              was inspired to create a Shiny Dashboard to investigate it further
              ."),
            br(),
            p("In this project, I aimed to evaluate, 
              1) How many birds are counted each year in Monterey County? 
              2) When do these birds arrive, and does this change year to year? 
            3) Do I observe any effect of sea surface temperature (SST) on 
              arrival timing in recent years?"),
            br(),
            h4("Shearwaters"),
            p("Shearwaters have one of the longest migrations in the animal 
            kingdom, moving northward after breeding in the southern hemisphere 
            to western North America and Europe. For this project, I focused on 
            Sooty Shearwaters (SOSH), one of the most common seabirds in the 
            world, and the Pink-footed Shearwater (PFSH), a less common species 
            that has been listed as vulnerable by the IUCN and endangered by 
            Chile and Canada. Both of these species are threatened by fisheries 
            where they are caught as incidental bycatch, and face pressure from 
            both habitat degradation and predators at island breeding colonies. 
            More information about the life history of these species can be 
            found at Birds of the World online (",
              a("Pink-footed Shearwater",
                href = 
                  "https://birdsoftheworld.org/bow/species/pifshe/cur/introduction?login", 
                target = "_blank"),
              ", ",
              a("Sooty Shearwater",
                href = 
                  "https://birdsoftheworld.org/bow/species/sooshe/cur/introduction?login", 
                target = "_blank"),
              ")"
            ),
            br(),
            h4("Data Sources"),
            p("Shearwater observation data from January 1, 2015, to December 31,
              2025, in Monterey County were sourced from ",
              a("eBird (Cornell Lab of Ornithology, Ithaca, New York, version 2025)", 
                href = "  https://ebird.org/data/download", target = "_blank"),
              ", a community science platform hosting millions of bird 
              observations worldwide. SST data were obtained from ",
              a("NOAA 0.25-degree Daily Optimum Interpolation Sea Surface Temperature (OISST, Version 2.1)", 
                href = " https://doi.org/10.25921/RE9P-PT57", target = "_blank"),
              ", and represents gridded weekly sea surface temperature for 
              Monterey Bay from February 2, 2020, to December 12, 2025."),
            
            br(),
            h4("Methods"),
            p("For the eBird dataset, I focused on traveling, stationary, and 
              pelagic observations. eBird checklists are sometimes recorded in 
              groups; to reduce the possibility of double-counting birds, I kept
              the first record for each group only. The single highest count per
              day was retained as the daily maximum for bird observations where 
              multiple observers recorded the same species on the same date and 
              county, to avoid double-counting. Weekly SST values were 
              aggregated to monthly averages for visualization and correlation 
              analysis. ENSO phase annotations follow the Oceanic Niño Index 
              (ONI) classifications from ",
              a("NOAA / Golden Gate Weather Services.", 
                href = "https://ggweather.com/enso/", target = "_blank")),
            
            br(),
            h4("Key Findings"),
            tags$ul(
              tags$li("1"),
              tags$li("2"),
              tags$li("3"),
              tags$li("4")
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
            recorded on a given day.")
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
            title = "Sea Surface Temperature (SST) & Shearwater Counts — Monterey County",
            width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_sst", height = "450px")
          )
        ),
        fluidRow(
          box(
            title = "Spearman Correlation — SST vs. Count",
            width = 6, status = "primary", solidHeader = TRUE,
            tableOutput("table_correlation")
          ),
          box(
            title = "Interpretation Note",
            width = 6, status = "primary", solidHeader = FALSE,
            p("SST and shearwater counts ...")
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
            plotlyOutput("plot_seasonal", height = "500px")
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
                      ymin = y_min, ymax = y_max,
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
                 alpha = 0.3, size = 1.2) +
      # smoothed trend line per species
      geom_smooth(data = df_monthly,
                  aes(x = month_date, y = monthly_max,
                      color = common_name),
                  method = "loess", span = 0.3,
                  se = TRUE, alpha = 0.15, linewidth = 1) +
      scale_fill_manual(
        values = c("El Niño" = "tomato", "La Niña" = "steelblue", "Neutral" = "gray80"),
        name = " "
      ) +
      scale_color_manual(
        values = c("Sooty Shearwater"       = "#5C6B6B",
                   "Pink-footed Shearwater" = "#B5485A"),
        name = " "
      ) +
      {if (input$log_scale) scale_y_log10(labels = scales::comma)
        else scale_y_continuous(labels = scales::comma)
      } +
      labs(x = "Date", y = "Monthly Max Count") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom"
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
  # dual-axis plot: bird counts (left) + SST line with ribbon (right)
  output$plot_sst <- renderPlotly({
    
    df <- monterey_data()
    
    sst_line <- df %>%
      filter(!is.na(sst_monthly_avg)) %>%
      distinct(month_date, sst_monthly_avg, sd_sst)
    
    pl <- plot_ly() %>%
      
      # SST ribbon (± 1 SD) on right axis (y2)
      add_trace(
        data = sst_line,
        x = ~month_date,
        y = ~sst_monthly_avg + sd_sst,
        type = "scatter", mode = "lines",
        line = list(color = "transparent"),
        showlegend = FALSE, yaxis = "y2", hoverinfo = "skip"
      ) %>%
      add_trace(
        data = sst_line,
        x = ~month_date,
        y = ~sst_monthly_avg - sd_sst,
        type = "scatter", mode = "lines",
        fill = "tonexty", fillcolor = "rgba(139,0,0,0.15)",
        line = list(color = "transparent"),
        showlegend = FALSE, yaxis = "y2", hoverinfo = "skip"
      ) %>%
      
      # SST mean line on right axis (y2)
      add_trace(
        data = sst_line,
        x = ~month_date,
        y = ~sst_monthly_avg,
        type = "scatter", mode = "lines",
        line = list(color = "darkred", width = 2),
        name = "SST", yaxis = "y2",
        hovertemplate = "SST: %{y:.2f}°C<extra></extra>"
      ) %>%
      
      # Bird count points — one trace per species
      add_trace(
        data = df %>% filter(as.character(common_name) == "Sooty Shearwater"),
        x = ~date, y = ~daily_max_count,
        type = "scatter", mode = "markers",
        marker = list(color = "#5C6B6B", opacity = 0.6, size = 5),
        name = "Sooty Shearwater",
        hovertemplate = paste("Date: %{x}<br>Species: Sooty Shearwater",
                              "<br>Count: %{y:,}<extra></extra>")
      ) %>%
      add_trace(
        data = df %>% filter(as.character(common_name) == "Pink-footed Shearwater"),
        x = ~date, y = ~daily_max_count,
        type = "scatter", mode = "markers",
        marker = list(color = "#B5485A", opacity = 0.6, size = 5),
        name = "Pink-footed Shearwater",
        hovertemplate = paste("Date: %{x}<br>Species: Pink-footed Shearwater",
                              "<br>Count: %{y:,}<extra></extra>")
      ) %>%
      
      # dummy traces for ENSO legend entries
      add_trace(
        x = c(NA), y = c(NA), type = "scatter", mode = "markers",
        marker = list(color = "rgba(255,99,71,0.5)", size = 12, symbol = "square"),
        name = "El Niño", showlegend = TRUE
      ) %>%
      add_trace(
        x = c(NA), y = c(NA), type = "scatter", mode = "markers",
        marker = list(color = "rgba(70,130,180,0.5)", size = 12, symbol = "square"),
        name = "La Niña", showlegend = TRUE
      ) %>%
      add_trace(
        x = c(NA), y = c(NA), type = "scatter", mode = "markers",
        marker = list(color = "rgba(200,200,200,0.5)", size = 12, symbol = "square"),
        name = "Neutral", showlegend = TRUE
      ) %>%
      
      layout(
        xaxis = list(
          title = "Date",
          range = c(as.character(min(sst_line$month_date)), 
                    as.character(max(sst_line$month_date))),
          dtick = "M12",
          tickformat = "%Y",
          ticklabelmode = "period"
        ),
        yaxis = list(
          title = "Daily Max Count",
          nticks = 6
        ),
        yaxis2 = list(
          title = "SST (°C)",
          overlaying = "y",
          side = "right",
          tickfont = list(color = "darkred"),
          titlefont = list(color = "darkred"),
          nticks = 6
        ),
        margin = list(r = 60),
        legend = list(
          orientation = "h",
          x = 0, y = -0.2,
          font = list(size = 13)
        ),
        hovermode = "closest"
      )
    
    # conditionally add ENSO bands as shapes
    if (input$show_enso) {
      enso_colors <- c("El Niño" = "rgba(255,99,71,0.15)", 
                       "La Niña" = "rgba(70,130,180,0.15)", 
                       "Neutral" = "rgba(200,200,200,0.15)")
      
      shapes <- lapply(1:nrow(enso_phases), function(i) {
        list(
          type = "rect",
          xref = "x", yref = "paper",
          x0 = enso_phases$start[i], x1 = enso_phases$end[i],
          y0 = 0, y1 = 1,
          fillcolor = enso_colors[enso_phases$phase[i]],
          line = list(width = 0)
        )
      })
      pl <- pl %>% layout(shapes = shapes)
    }
    
    # remove toolbar clutter and plotly logo
    pl <- pl %>%
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
    
    pl
  })
  
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
            legend.position  = "bottom",
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
  
  # === Output: table_correlation ===
  # Spearman correlation between monthly SST and daily max count
  # Grouped by species, Monterey County only, restricted to rows with SST data
  output$table_correlation <- renderTable({
    
    monterey_data() %>%
      filter(!is.na(sst_monthly_avg)) %>%
      group_by(common_name) %>%
      summarise(
        N       = n(),
        rho     = cor(sst_monthly_avg, daily_max_count, method = "spearman"),
        p_value = cor.test(sst_monthly_avg, daily_max_count, method = "spearman")$p.value,
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
  
}

# === Run ===
shinyApp(ui = ui, server = server)