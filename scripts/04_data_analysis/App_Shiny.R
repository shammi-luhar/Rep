library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# app.R
# source('/mnt/users_shared/Users/Shammi_Luhar/Generalisability/Generalisability_Shiny/Load_libraries.R')


# Load data
# --- Rename each column so it tells us what is in each
data <- read_excel("Catchment_Pyramid.xlsx") %>%
  rename(
    Trust = 1,
    Sex = 2,
    AgeGroup = 3,
    Catchment = 4,
    Patients = 5
  )

# --- Mark out the trust groupings - doesnt need to be 
# --- in alphabetical order yet
arcturis_trusts <- c(
  "Cambridge University Hospitals NHS Foundation Trust",
  "Chelsea And Westminster Hospital NHS Foundation Trust",
  "Great Ormond Street Hospital For Children NHS Foundation Trust",
  "Guy's And St Thomas' NHS Foundation Trust",
  "Hampshire Hospitals NHS Foundation Trust",
  "Leeds Teaching Hospitals NHS Trust",
  "Manchester University NHS Foundation Trust",
  "Milton Keynes University Hospital NHS Foundation Trust",
  "Oxford University Hospitals NHS Foundation Trust",
  "Royal Berkshire NHS Foundation Trust",
  "Royal Devon And Exeter NHS Foundation Trust",
  "University College London Hospitals NHS Foundation Trust"
)

# --- Sort the trusts that will go into the scrollable tickbox
all_trusts <- sort(unique(data$Trust)) # Puts all truest in alphabetical order
arcturis_trusts <- sort(intersect(arcturis_trusts, all_trusts)) # isolates the arcturis trusts
remaining_trusts <- sort(setdiff(all_trusts, arcturis_trusts)) # isolates the non-arcturis trusts

# --- Load in the ethnicity data 
ethnicity_raw <- read_excel("Ethnicity_input.xlsx") %>%
  rename(
    Trust = 1,
    White = 5,
    Black = 6,
    Asian = 7,
    Mixed = 8,
    Other = 9
  ) %>%
  select(Trust, White, Black, Asian, Mixed, Other)





############### Scotland 
scot_data <- read_excel("Catchment_Pyramid_Scotland.xlsx", sheet = 1) %>%
  rename(
    Area     = `Area Name`,
    AgeGroup = Agegroup,
    SexRaw   = Sex,
    Count    = `Rounded Census 2022`
  ) %>%
  mutate(
    # normalise Sex labels to match the rest of the app
    Sex = case_when(
      str_to_lower(SexRaw) %in% c("male", "males")   ~ "Male",
      str_to_lower(SexRaw) %in% c("female", "females") ~ "Female",
      TRUE ~ as.character(SexRaw)
    )
  ) %>%
  select(Area, Sex, AgeGroup, Count)

# Keep areas alphabetised for the checkbox
scot_areas <- sort(unique(scot_data$Area))

# After scot_areas <- sort(unique(scot_data$Area))
arcturis_scot_areas <- c(
  "Inverclyde",
  "Glasgow City",
  "East Dunbartonshire",
  "East Renfrewshire",
  "West Dunbartonshire",
  "Renfrewshire"
)
# Keep only names that actually exist in the data (avoids typos/mismatches)
arcturis_scot_areas <- sort(intersect(arcturis_scot_areas, scot_areas))

# Optional: make AgeGroup a factor in natural order (0-4, 5-9, ... 90+)
# This uses the order present in the file per Area; if you want a fixed set, define levels explicitly.
scot_age_levels <- scot_data %>%
  distinct(AgeGroup) %>%
  mutate(order_key = as.integer(str_extract(AgeGroup, "^[0-9]+"))) %>%
  arrange(order_key) %>%
  pull(AgeGroup)

scot_data <- scot_data %>%
  mutate(AgeGroup = factor(AgeGroup, levels = scot_age_levels))

eth_scot_raw <- read_excel("Ethnicity_input_Scotland.xlsx", sheet = 1) %>%
  # file columns: Area in first col (currently named 'Unnamed: 0'), then White, Mixed, Asian, Black, Other
  rename(Area = 1) %>%
  select(Area, White, Mixed, Asian, Black, Other)

scot_eth_areas <- sort(unique(eth_scot_raw$Area))







# UI
# --- UI is the user interface
ui <- fluidPage(
  
  # Title or Header
  titlePanel("NHS Trust Population Pyramids"),
  
  # Layout: Sidebar and Main Panel
  sidebarLayout(
    sidebarPanel(
      # Show ENGLISH trust selectors everywhere EXCEPT:
      # - Population Pyramids > Population Pyramid - Scotland
      # - Ethnicity > Ethnicity - Scotland
      conditionalPanel(
        condition = "!( (input.top_tabs === 'Population Pyramids' && input.pyr_subtabs === 'Population Pyramid - Scotland') || (input.top_tabs === 'Ethnicity' && input.eth_tabs === 'Ethnicity - Scotland') )",
        actionButton("select_all", "Select All Trusts"),
        actionButton("deselect_all", "Deselect All Trusts"),
        tags$div(
          style = "max-height: 300px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; margin-top: 10px;",
          h4("Arcturis Trusts"),
          checkboxGroupInput("selected_arcturis", NULL, choices = arcturis_trusts, selected = arcturis_trusts),
          h4("Remaining Trusts"),
          checkboxGroupInput("selected_remaining", NULL, choices = remaining_trusts)
        )
      ),
      
      # Show SCOTLAND area selectors for the POPULATION PYRAMID – SCOTLAND subtab
      conditionalPanel(
        condition = "input.top_tabs === 'Population Pyramids' && input.pyr_subtabs === 'Population Pyramid - Scotland'",
        tags$div(style="display:flex; gap:8px; flex-wrap:wrap; margin-bottom:8px;",
                 actionButton("scot_select_all","Select All Areas"),
                 actionButton("scot_deselect_all","Deselect All Areas"),
                 actionButton("scot_select_arcturis","Select Arcturis Areas")
        ),
        tags$div(
          style="max-height: 260px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; margin-bottom: 12px;",
          checkboxGroupInput("scot_selected_areas", "Select area(s):", choices = scot_areas, selected = arcturis_scot_areas)
        )
      ),
      
      # Show SCOTLAND area selectors for the ETHNICITY – SCOTLAND subtab
      conditionalPanel(
        condition = "input.top_tabs === 'Ethnicity' && input.eth_tabs === 'Ethnicity - Scotland'",
        tags$div(style="display:flex; gap:8px; flex-wrap:wrap; margin-bottom:8px;",
                 actionButton("eth_scot_select_all", "Select All Areas"),
                 actionButton("eth_scot_deselect_all", "Deselect All Areas"),
                 actionButton("eth_scot_select_arcturis", "Select Arcturis Areas")
        ),
        tags$div(
          style="max-height: 260px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; margin-bottom: 12px;",
          checkboxGroupInput(
            "eth_scot_selected_areas", "Select area(s):",
            choices  = scot_eth_areas,
            selected = arcturis_scot_areas
          )
        )
      ),
      
      # Global toggle (visible on all tabs)
      checkboxInput("show_background", "Show national comparison", value = TRUE)),
    
    # Panel for outputs (main content area)
    mainPanel(
      tabsetPanel( id = "top_tabs",
                   # ---- TOP-LEVEL TAB 1 ----
                   tabPanel("Population Pyramids",
                            tabsetPanel(id = "pyr_subtabs",
                                        tabPanel("Catchment Pyramid - England",  plotOutput("pyramid_catchment"), br(), strong("Underlying counts (selected trusts)"), tableOutput("pyr_catchment_table")),
                                        tabPanel("Patients Pyramid - England",   plotOutput("pyramid_patients"),  br(), strong("Underlying counts (selected trusts)"), tableOutput("pyr_patients_table")),
                                        tabPanel("Population Pyramid - Scotland",
                                                 plotOutput("pyramid_scotland"),
                                                 br(), strong("Underlying counts (selected areas)"),
                                                 tableOutput("pyr_scotland_table")
                                        )
                            )),
                   
                   # ---- TOP-LEVEL TAB 2 ----
                   tabPanel("Ethnicity",
                            tabsetPanel(
                              id = "eth_tabs",
                              
                              # Combined subtab for England
                              tabPanel("Ethnicity - England",
                                       h4("Breakdown (counts and %)"),
                                       tableOutput("ethnicity_table"),
                                       br(),
                                       h4("Composition (%)"),
                                       plotOutput("ethnicity_bar", height = "420px")
                              ),
                              
                              # Scotland subtab (area controls now in sidebar)
                              tabPanel("Ethnicity - Scotland",
                                       h4("Breakdown (counts and %)"),
                                       tableOutput("ethnicity_scotland_table"),
                                       br(),
                                       h4("Composition (%)"),
                                       plotOutput("ethnicity_scotland_bar")
                              )
                            ))
                   
      )
    )
  )
)

# Server
# --- This part contains the logic for your app: what happens when the user interacts with the UI.
server <- function(input, output, session) {
  # ---  trigger some code when a specific input changes or an event occurs, like a button click or a change in a slider
  # --- i.e here whhat happens when the 'select_all' button is selected
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_arcturis", selected = arcturis_trusts)
    updateCheckboxGroupInput(session, "selected_remaining", selected = remaining_trusts)
  })
  
  # --- i.e here whhat happens when the 'deselect_all' button is selected
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_arcturis", selected = character(0))
    updateCheckboxGroupInput(session, "selected_remaining", selected = character(0))
  })
  
  selected_trusts <- reactive({
    c(input$selected_arcturis, input$selected_remaining)
  })
  
  observeEvent(input$scot_select_all, {
    updateCheckboxGroupInput(session, "scot_selected_areas", selected = arcturis_scot_areas)
  })
  
  observeEvent(input$scot_deselect_all, {
    updateCheckboxGroupInput(session, "scot_selected_areas", selected = character(0))
  })
  
  observeEvent(input$scot_select_arcturis, {
    updateCheckboxGroupInput(session, "scot_selected_areas", selected = arcturis_scot_areas)
  })
  
  #Create a reactive expression	- what happens when 'selected_trusts' 
  filtered_data <- reactive({
    data %>%
      filter(Trust %in% selected_trusts()) %>%
      group_by(Sex, AgeGroup) %>%
      summarise(
        Catchment = sum(Catchment, na.rm = TRUE),
        Patients = sum(Patients, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # So using theis filtered data, it can be inputted in to the population pyramid
  output$pyramid_catchment <- renderPlot({
    df_selected <- filtered_data()
    total_selected <- sum(df_selected$Catchment, na.rm = TRUE)
    
    df_selected <- df_selected %>%
      mutate(
        Percentage = 100 * Catchment / total_selected,
        Percentage = ifelse(Sex == "Male", -Percentage, Percentage)
      )
    
    p <- ggplot() +
      geom_bar(data = df_selected, aes(x = AgeGroup, y = Percentage, fill = Sex),
               stat = "identity", position = "identity") +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
      labs(
        title = "Catchment Population Pyramid",
        x = "Age Group",
        y = "Percentage",
        fill = "Sex"
      ) +
      scale_fill_manual(values = c("Male" = "#4B9CD3", "Female" = "#D36E70")) +
      theme_minimal()
    
    if (input$show_background) {
      df_all <- data %>%
        group_by(Sex, AgeGroup) %>%
        summarise(Catchment = sum(Catchment, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          Percentage = 100 * Catchment / sum(Catchment, na.rm = TRUE),
          Percentage = ifelse(Sex == "Male", -Percentage, Percentage)
        )
      
      p <- p +
        geom_bar(data = df_all, aes(x = AgeGroup, y = Percentage),
                 stat = "identity", fill = NA, color = "black", size = 1.1)
    }
    
    p
  })
  
  output$pyramid_patients <- renderPlot({
    df_selected <- filtered_data()
    total_selected <- sum(df_selected$Patients, na.rm = TRUE)
    
    df_selected <- df_selected %>%
      mutate(
        Percentage = 100 * Patients / total_selected,
        Percentage = ifelse(Sex == "Male", -Percentage, Percentage)
      )
    
    p <- ggplot() +
      geom_bar(data = df_selected, aes(x = AgeGroup, y = Percentage, fill = Sex),
               stat = "identity", position = "identity") +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
      labs(
        title = "Patients Population Pyramid",
        x = "Age Group",
        y = "Percentage",
        fill = "Sex"
      ) +
      scale_fill_manual(values = c("Male" = "#4B9CD3", "Female" = "#D36E70")) +
      theme_minimal()
    
    if (input$show_background) {
      df_all <- data %>%
        group_by(Sex, AgeGroup) %>%
        summarise(Patients = sum(Patients, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          Percentage = 100 * Patients / sum(Patients, na.rm = TRUE),
          Percentage = ifelse(Sex == "Male", -Percentage, Percentage)
        )
      
      p <- p +
        geom_bar(data = df_all, aes(x = AgeGroup, y = Percentage),
                 stat = "identity", fill = NA, color = "black", size = 1.1)
    }
    
    p
  })
  
  # Aggregate selected Scotland areas
  scot_filtered <- reactive({
    # Fallback to all areas if the input doesn't exist yet (because the sidebar
    # is hidden by conditionalPanel) or nothing is selected.
    selected <- input$scot_selected_areas
    if (is.null(selected) || length(selected) == 0) selected <- scot_areas
    
    scot_data %>%
      filter(Area %in% selected) %>%
      group_by(Sex, AgeGroup) %>%
      summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
  })
  
  output$pyramid_scotland <- renderPlot({
    library(ggplot2)
    
    df_sel <- scot_filtered()
    total_sel <- sum(df_sel$Count, na.rm = TRUE)
    
    df_sel <- df_sel %>%
      mutate(
        Percentage = 100 * Count / ifelse(total_sel == 0, NA_real_, total_sel),
        Percentage = ifelse(Sex == "Male", -Percentage, Percentage)
      )
    
    # Base: selected areas (sex colours)
    p <- ggplot() +
      geom_bar(
        data = df_sel,
        aes(x = AgeGroup, y = Percentage, fill = Sex),
        stat = "identity",
        position = "identity"
      ) +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
      labs(
        title = "Population Pyramid - Scotland (Percentage)",
        x = "Age Group",
        y = "Percentage",
        fill = "Sex"
      ) +
      scale_fill_manual(values = c("Male" = "#4B9CD3", "Female" = "#D36E70")) +
      theme_minimal()
    
    # Optional national comparison outline (on TOP)
    if (isTRUE(input$show_background)) {
      df_all <- scot_data %>%
        group_by(Sex, AgeGroup) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          Percentage = 100 * Count / sum(Count, na.rm = TRUE),
          Percentage = ifelse(Sex == "Male", -Percentage, Percentage)
        )
      
      p <- p +
        geom_bar(
          data = df_all,
          aes(x = AgeGroup, y = Percentage),
          stat = "identity",
          fill = NA,
          color = "black",
          size = 1.1
        )
    }
    
    p
  })
  
  # ---- Catchment table (England) ----
  output$pyr_catchment_table <- renderTable({
    df <- filtered_data()  # has Sex, AgeGroup, Catchment
    
    # Group fine ages into broader bands
    df <- df %>%
      mutate(
        AgeBand = case_when(
          AgeGroup %in% c("0-4", "5-9", "10-14", "15-19") ~ "0-19",
          AgeGroup %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
          AgeGroup %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
          AgeGroup %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
          TRUE ~ "80+"
        )
      )
    
    out <- df %>%
      group_by(AgeBand) %>%
      summarise(
        Male   = sum(ifelse(Sex == "Male",   Catchment, 0), na.rm = TRUE),
        Female = sum(ifelse(Sex == "Female", Catchment, 0), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Total    = Male + Female,
        `Male %` = round(100 * Male   / ifelse(Total == 0, NA_real_, Total), 1),
        `Female %` = round(100 * Female / ifelse(Total == 0, NA_real_, Total), 1)
      )
    
    # Add total row
    totals <- out %>%
      summarise(
        AgeBand = "All ages",
        Male   = sum(Male, na.rm = TRUE),
        Female = sum(Female, na.rm = TRUE),
        Total  = sum(Total, na.rm = TRUE)
      ) %>%
      mutate(
        `Male %` = round(100 * Male   / Total, 1),
        `Female %` = round(100 * Female / Total, 1)
      )
    
    bind_rows(out, totals) %>%
      mutate(
        Male   = formatC(Male,   format = "f", digits = 0, big.mark = ","),
        Female = formatC(Female, format = "f", digits = 0, big.mark = ","),
        Total  = formatC(Total,  format = "f", digits = 0, big.mark = ",")
      )
  })
  
  # ---- Patients table (England) ----
  output$pyr_patients_table <- renderTable({
    df <- filtered_data()  # has Sex, AgeGroup, Patients
    
    # Group fine ages into broader bands
    df <- df %>%
      mutate(
        AgeBand = case_when(
          AgeGroup %in% c("0-4", "5-9", "10-14", "15-19") ~ "0-19",
          AgeGroup %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
          AgeGroup %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
          AgeGroup %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
          TRUE ~ "80+"
        )
      )
    
    out <- df %>%
      group_by(AgeBand) %>%
      summarise(
        Male   = sum(ifelse(Sex == "Male",   Patients, 0), na.rm = TRUE),
        Female = sum(ifelse(Sex == "Female", Patients, 0), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Total     = Male + Female,
        `Male %`  = round(100 * Male   / ifelse(Total == 0, NA_real_, Total), 1),
        `Female %`= round(100 * Female / ifelse(Total == 0, NA_real_, Total), 1)
      )
    
    # Total row
    totals <- out %>%
      summarise(
        AgeBand = "All ages",
        Male   = sum(Male, na.rm = TRUE),
        Female = sum(Female, na.rm = TRUE),
        Total  = sum(Total, na.rm = TRUE)
      ) %>%
      mutate(
        `Male %`  = round(100 * Male   / Total, 1),
        `Female %`= round(100 * Female / Total, 1)
      )
    
    bind_rows(out, totals) %>%
      mutate(
        Male   = formatC(Male,   format = "f", digits = 0, big.mark = ","),
        Female = formatC(Female, format = "f", digits = 0, big.mark = ","),
        Total  = formatC(Total,  format = "f", digits = 0, big.mark = ",")
      )
  })
  
  
  output$pyr_scotland_table <- renderTable({
    df <- scot_filtered()
    df <- df %>%
      mutate(
        AgeBand = case_when(
          AgeGroup %in% c("0-4", "5-9", "10-14", "15-19") ~ "0-19",
          AgeGroup %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
          AgeGroup %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
          AgeGroup %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
          TRUE ~ "80+"
        )
      )
    
    out <- df %>%
      group_by(AgeBand) %>%
      summarise(
        Male   = sum(ifelse(Sex == "Male",   Count, 0), na.rm = TRUE),
        Female = sum(ifelse(Sex == "Female", Count, 0), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Total    = Male + Female,
        `Male %` = round(100 * Male   / Total, 1),
        `Female %` = round(100 * Female / Total, 1)
      )
    
    totals <- out %>%
      summarise(
        AgeBand = "All ages",
        Male   = sum(Male, na.rm = TRUE),
        Female = sum(Female, na.rm = TRUE),
        Total  = sum(Total, na.rm = TRUE)
      ) %>%
      mutate(
        `Male %` = round(100 * Male   / Total, 1),
        `Female %` = round(100 * Female / Total, 1)
      )
    
    bind_rows(out, totals) %>%
      mutate(
        Male   = formatC(Male,   format = "f", digits = 0, big.mark = ","),
        Female = formatC(Female, format = "f", digits = 0, big.mark = ","),
        Total  = formatC(Total,  format = "f", digits = 0, big.mark = ",")
      )
  })
  
  
  
  
  ########
  ethnicity_filtered <- reactive({
    req(selected_trusts())
    ethnicity_raw %>%
      filter(Trust %in% selected_trusts()) %>%
      summarise(across(c(White, Black, Asian, Mixed, Other), sum, na.rm = TRUE))
  })
  
  output$ethnicity_table <- renderTable({
    df <- ethnicity_filtered()
    total <- sum(df)
    
    count_values <- round(as.numeric(df[1, ]),0)
    percent_values <- round(100 * count_values / total, 1)
    
    data.frame(
      Ethnicity = colnames(df),
      Catchment = formatC(count_values, format = "f", digits = 0, big.mark = ","),
      Percentage = paste0(formatC(percent_values, format = "f", digits = 1), "%")
    )
  })
  
  output$ethnicity_bar <- renderPlot({
    df <- ethnicity_filtered()
    total <- sum(df)
    
    df_percent <- df %>%
      pivot_longer(everything(), names_to = "Ethnicity", values_to = "Count") %>%
      mutate(Percent = 100 * Count / total)
    
    ggplot(df_percent, aes(x = Ethnicity, y = Percent, fill = Ethnicity)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Ethnicity Composition of Selected Trusts",
        y = "Percentage",
        x = "Ethnic Group"
      ) +
      theme_minimal()
  })
  
  
  # --- Button wiring (Scotland ethnicity) ---
  observeEvent(input$eth_scot_select_all, {
    updateCheckboxGroupInput(session, "eth_scot_selected_areas", selected = scot_eth_areas)
  })
  
  observeEvent(input$eth_scot_deselect_all, {
    updateCheckboxGroupInput(session, "eth_scot_selected_areas", selected = character(0))
  })
  
  observeEvent(input$eth_scot_select_arcturis, {
    updateCheckboxGroupInput(session, "eth_scot_selected_areas", selected = arcturis_scot_areas)
  })
  
  # --- Reactive: summed counts across selected areas ---
  eth_scot_filtered <- reactive({
    selected <- input$eth_scot_selected_areas
    if (is.null(selected) || length(selected) == 0) selected <- scot_eth_areas
    
    eth_scot_raw %>%
      filter(Area %in% selected) %>%
      summarise(across(c(White, Mixed, Asian, Black, Other), ~ sum(.x, na.rm = TRUE)))
  })
  
  
  # --- Table: counts + percentages ---
  output$ethnicity_scotland_table <- renderTable({
    df <- eth_scot_filtered()                 # 1-row tibble of summed counts
    counts <- as.numeric(df[1, ])
    eth_names <- names(df)
    
    total <- sum(counts, na.rm = TRUE)
    perc  <- if (is.na(total) || total == 0) rep(NA_real_, length(counts)) else round(100 * counts / total, 1)
    
    data.frame(
      Ethnicity  = eth_names,
      Count      = formatC(counts, format = "f", digits = 0, big.mark = ","),
      Percentage = paste0(formatC(perc,   format = "f", digits = 1), "%")
    )
  })
  
  # --- Bar chart: composition (%) ---
  output$ethnicity_scotland_bar <- renderPlot({
    df <- eth_scot_filtered()
    counts <- as.numeric(df[1, ])
    eth_names <- names(df)
    total <- sum(counts, na.rm = TRUE)
    
    plot_df <- data.frame(
      Ethnicity = eth_names,
      Percent   = if (total == 0) 0 else 100 * counts / total
    )
    
    ggplot(plot_df, aes(x = Ethnicity, y = Percent, fill = Ethnicity)) +
      geom_col() +
      labs(
        title = "Ethnicity Composition – Scotland (selected areas)",
        x = "Ethnic Group",
        y = "Percentage"
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      theme_minimal()
  })
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
