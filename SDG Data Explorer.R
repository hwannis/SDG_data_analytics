library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(plotly)
library(writexl)
library(openxlsx)
library(shinydashboard)
library(lmtest) # for model fitting
library(forecast)
# Load the datase

totaldb <- as.data.frame(read.csv("data/totaldb.csv"))
centroids <- as.data.frame(read.csv("data/centroids.csv"))
esadb <- as.data.frame(read.csv("data/esadb.csv",na.strings = "", check.names=FALSE))
pop <- as.data.frame(read.csv("data/pop.csv",na.strings = "", check.names=FALSE))

ui <- navbarPage("Population & SDG data Explorer",
                 # First Tab: Stacked Line Chart
                 tabPanel("Regional Pop trends",
                          tags$div(tags$a(href="https://population.un.org/wpp/Download/Standard/MostUsed/", 
                                          target="_blank", 
                                          "Data source: UNDESA, Population Division (2022). World Population Prospects 2022, Online Edition"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"),
                          fluidRow(
                            column(6, 
                                   selectInput("selectedAgeLabelStacked", 
                                               "Select Age:", 
                                               choices = unique(pop$ageLabel), 
                                               selected = "Total"),
                                   column(3, downloadButton("downloadData_regional_pop_trends", "Download Data"))
                            ),
                            column(12, 
                                   plotlyOutput("pop_stackedLineChart", height= "900px"),
                                   tableOutput("summary_population")
                            )
                          )
                 ),
                 # second Tab: Dynamic Line Chart
                 tabPanel("Country Pop trends",
                          tags$div(tags$a(href="https://population.un.org/wpp/Download/Standard/MostUsed/", 
                                          target="_blank", 
                                          "Data source: UNDESA, Population Division (2022). World Population Prospects 2022, Online Edition"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          fluidRow(
                            column(6, 
                                   selectInput("selectedCountry", 
                                               "Select Country to Highlight:", 
                                               choices = unique(pop$location[!(pop$location %in% c("World", "Africa", "Sub-Saharan Africa"))]), 
                                               multiple = TRUE)
                            ),
                            column(6, 
                                   selectInput("selectedAgeLabel", 
                                               "Select Age:", 
                                               choices = unique(pop$ageLabel), 
                                               selected = "total")
                            )
                          ),
                          fluidRow(
                            column(12, 
                                   plotlyOutput("lineChart", height= "900px"),
                                   tags$br(), tags$br(), tags$br(), tags$br()  # Space between charts
                            )
                          ),
                          fluidRow(
                            column(12, 
                                   plotlyOutput("interactiveMap", width="100%", height= "1200px")
                            )
                          ),tags$footer(
                            tags$hr(),  # Optional: horizontal line before the disclaimer
                            tags$div("The designations and maps used do not reflect a position by UNICEF on the legal status of any country or territory or of its authorities, or the delimitation of any frontiers.", 
                                     style = "text-align: center; margin-top: 20px; font-style: italic;")
                          )
                 ),
                 tabPanel("Regional overview of Data Availability",
                          tags$div(tags$a(href="https://unstats.un.org/sdgs/dataportal/database", 
                                          target="_blank", 
                                          "Data source: UN Statistics Division SDG Database, downloaded 26092023"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          fluidPage(
                            fluidRow(
                              # column(3, selectInput("variable", "Select a variable to sum:",
                              #                       choices = names(totaldb)[sapply(totaldb, is.numeric)], width = "200px")),
                              # column(3, selectInput("crsdg_filter", "Filter by crsdg:",
                              #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              # column(3, verbatimTextOutput("num_indicators")),
                              column(3, selectInput("tab1_variable", "Select the range of years to analyse data availability:",
                                                    choices = names(totaldb[, grepl("data", colnames(totaldb),ignore.case = TRUE)]), width = "200px")),
                              column(3, selectInput("tab1_crsdg_filter", "Filter by Child-related SDG indicators:",
                                                    choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              column(3, verbatimTextOutput("num_indicators_1")),
                              column(3, downloadButton("downloadData", "Download Data"))
                            ),
                            plotlyOutput("bar_chart"),
                            plotlyOutput("map", width="100%", height= "800px"), # Add this line for the map
                            tableOutput("summary")
                          ),tags$footer(
                            tags$hr(),  # Optional: horizontal line before the disclaimer
                            tags$div("The designations and maps used do not reflect a position by UNICEF on the legal status of any country or territory or of its authorities, or the delimitation of any frontiers.", 
                                     style = "text-align: center; margin-top: 20px; font-style: italic;")
                          )
                 ),
                 tabPanel("Regional overview of Data Availability by SDG Goal",
                          tags$div(tags$a(href="https://unstats.un.org/sdgs/dataportal/database",
                                          target="_blank",
                                          "Data source: UN Statistics Division SDG Database"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          fluidPage(
                            fluidRow(
                              # column(3, selectInput("variable", "Select a variable to sum:",
                              #                       choices = names(totaldb)[sapply(totaldb, is.numeric)], width = "200px")),
                              # column(3, selectInput("crsdg_filter", "Filter by crsdg:",
                              #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              # column(3, verbatimTextOutput("num_indicators")),
                              column(3, selectInput("tab5_variable", "Select the range of years to analyse data availability:",
                                                    choices = names(totaldb[, grepl("data", colnames(totaldb),ignore.case = TRUE)]), width = "200px")),
                              column(3, selectInput("tab5_crsdg_filter", "Filter by Child-related SDG indicators:",
                                                    choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              column(3, selectInput("tab5_country_filter", "Filter by Country:",
                                                    choices = c("All", unique(totaldb$geoAreaName)), width = "200px")),
                              column(3, verbatimTextOutput("num_indicators_5")),
                              column(3, downloadButton("downloadData_goal", "Download Data"))
                            ),
                            plotlyOutput("bar_chart_goal", height="700px"),
                            # plotlyOutput("map_goal", width="100%", height= "800px"), # Add this line for the map
                            tableOutput("summary_goal")
                          )
                 ),
                 tabPanel("Data Availability by Indicators and Country",
                          tags$div(tags$a(href="https://unstats.un.org/sdgs/dataportal/database", 
                                          target="_blank", 
                                          "Data source: UN Statistics Division SDG Database"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          fluidPage(
                            fluidRow(
                              # column(3, selectInput("variable", "Select a variable to sum:",
                              #                       choices = names(totaldb)[sapply(totaldb, is.numeric)], width = "200px")),
                              # column(3, selectInput("crsdg_filter", "Filter by crsdg:",
                              #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              column(3, selectInput("tab2_variable", "Select the range of years to analyse data availability:",
                                                    choices = names(totaldb[, grepl("data", colnames(totaldb),ignore.case = TRUE)]), width = "200px")),
                              column(3, selectInput("tab2_crsdg_filter", "Filter by Child-related SDG indicators:",
                                                    choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              column(3, verbatimTextOutput("num_indicators_2")),
                              column(3, downloadButton("downloadData2", "Download Data"))
                            ),
                            DTOutput("data_av_indicator_country")
                          )
                 ),
                 tabPanel("Country Indicators Latest Values",
                          tags$div(tags$a(href="https://unstats.un.org/sdgs/dataportal/database", 
                                          target="_blank", 
                                          "Data source: UN Statistics Division SDG Database"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          fluidPage(
                            fluidRow(
                              # column(3, selectInput("variable", "Select a variable to sum:",
                              #                       choices = names(totaldb)[sapply(totaldb, is.numeric)], width = "200px")),
                              # column(3, selectInput("crsdg_filter", "Filter by crsdg:",
                              #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              # column(3, selectInput("tab3_variable", "Select the range of years to analyse data availability:",
                              #                       choices = names(totaldb[, grepl("data", colnames(totaldb),ignore.case = TRUE)]), width = "200px")),
                              column(3, selectInput("tab3_crsdg_filter", "Filter by Child-related SDG indicators:",
                                                    choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              column(3, verbatimTextOutput("num_indicators_3")),
                              column(3, downloadButton("downloadData3", "Download Data"))
                            ),
                            uiOutput("country_tabs_latest")
                          )
                 ),
                 tabPanel("Data availability by SP Goal Areas",
                          tags$div(tags$a(href="https://unstats.un.org/sdgs/dataportal/database", 
                                          target="_blank", 
                                          "Data source: UN Statistics Division SDG Database"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          fluidPage(
                            fluidRow(
                              # Add your common filters here, similar to the previous panels.
                              column(3, selectInput("tab4_variable", "Select the range of years to analyse data availability:",
                                                    choices = names(totaldb[, grepl("data", colnames(totaldb),ignore.case = TRUE)]), width = "200px")),
                              column(3, selectInput("tab4_crsdg_filter", "Filter by Child-related SDG indicators:",
                                                    choices = c("All", unique(totaldb$crsdg)), width = "200px")),
                              column(3, verbatimTextOutput("num_indicators_4")),
                              column(3, downloadButton("downloadData4", "Download Data"))
                            ),
                            plotlyOutput("bar_chart_sp_goal"), # Bar chart for the percentage of indicators by sp_goal
                            tableOutput("sptable"),
                            plotlyOutput("stacked_bar_chart_countries",width="100%", height= "800px"), # Stacked bar chart for countries by sp_goal
                            # tableOutput("sp_goal_summary") # You can add more UI elements as needed
                          )
                 ),
                 tabPanel("Country Indicator Trends",
                          tags$div(tags$a(href="https://unstats.un.org/sdgs/dataportal/database", 
                                          target="_blank", 
                                          "Data source: UN Statistics Division SDG Database"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Please ensure filters are selected correctly before proceeding.For more Information on the indicators and disaggregates refer to the UNSD website."),
                              downloadButton("downloadData5", "Download Data"),
                              selectInput("seriesDescription2", "Indicator", choices = unique(esadb$seriesDescription2), selected = 1),
                              selectInput("nature", "Nature", choices = unique(esadb$`attributes.Nature`), selected = 1),
                              selectInput("age", "Age", choices = unique(esadb$`dimensions.Age`), selected = 1),
                              selectInput("location", "Location", choices = unique(esadb$`dimensions.Location`), selected = 1),
                              selectInput("sex", "Sex", choices = unique(esadb$`dimensions.Sex`), selected = 1),
                              selectInput("education_level", "Education level", choices = unique(esadb$`dimensions.Education level`), selected = 1),
                              selectInput("quantile", "Quantile", choices = unique(esadb$`dimensions.Quantile`), selected = 1),
                              selectInput("type_of_product", "Type of product", choices = unique(esadb$`dimensions.Type of product`), selected = 1),
                              selectInput("name_of_non_communicable_disease", "Name of non-communicable disease", choices = unique(esadb$`dimensions.Name of non-communicable disease`), selected = 1),
                              selectInput("type_of_occupation", "Type of occupation", choices = unique(esadb$`dimensions.Type of occupation`), selected = 1),
                              selectInput("IHR_capacity", "IHR Capacity", choices = unique(esadb$`dimensions.IHR Capacity`), selected = 1),
                              selectInput("activity", "Activity", choices = unique(esadb$`dimensions.Activity`), selected = 1),
                              selectInput("level_status", "Level/Status", choices = unique(esadb$`dimensions.Level/Status`), selected = 1),
                              selectInput("deviation_level", "Deviation Level", choices = unique(esadb$`dimensions.Deviation Level`), selected = 1),
                              selectInput("type_of_renewable_technology", "Type of renewable technology", choices = unique(esadb$`dimensions.Type of renewable technology`), selected = 1),
                              selectInput("disability_status", "Disability status", choices = unique(esadb$`dimensions.Disability status`), selected = 1),
                              selectInput("name_of_international_institution", "Name of international institution", choices = unique(esadb$`dimensions.Name of international institution`), selected = 1),
                              selectInput("type_of_speed", "Type of speed", choices = unique(esadb$`dimensions.Type of speed`), selected = 1),
                              selectInput("type_of_skill", "Type of skill", choices = unique(esadb$`dimensions.Type of skill`), selected = 1),
                              selectInput("migratory_status", "Migratory status", choices = unique(esadb$`dimensions.Migratory status`), selected = 1),
                              selectInput("grounds_of_discrimination", "Grounds of discrimination", choices = unique(esadb$`dimensions.Grounds of discrimination`), selected = 1),
                              selectInput("population_group", "Population Group", choices = unique(esadb$`dimensions.Population Group`), selected = 1)
                            ),
                            mainPanel(
                              DT::dataTableOutput("country_indicators_trend"),
                              plotlyOutput("countriesTrendChart", width="2500px", height= "900px")
                            )
                          )
                 ),
                 tabPanel("Country Indicator Projections",
                          tags$div(tags$a(href="https://unstats.un.org/sdgs/dataportal/database", 
                                          target="_blank", 
                                          "Data source: UN Statistics Division SDG Database"),
                                   style="text-align:right; margin-top:10px; margin-bottom:10px;"
                          ),
                          fluidRow(
                            column(3, selectInput("seriesDescription2_1", "Indicator", choices = unique(esadb$seriesDescription2), selected = 1)))
                          ,
                          fluidRow(
                            column(1, selectInput("sex_1", "Sex", choices = unique(esadb$`dimensions.Sex`), selected = 1)),
                            column(1, selectInput("age_1", "Age", choices = unique(esadb$`dimensions.Age`), selected = 1)),
                            column(1, selectInput("location_1", "Location", choices = unique(esadb$`dimensions.Location`), selected = 1)),
                            column(1, selectInput("quantile_1", "Quantile", choices = unique(esadb$`dimensions.Quantile`), selected = 1))
                            ,
                            column(1, selectInput("education_level_1", "Education level", choices = unique(esadb$`dimensions.Education level`), selected = 1)),
                            column(1, selectInput("nature_1", "Nature", choices = unique(esadb$`attributes.Nature`), selected = 1)),
                            column(1, selectInput("type_of_product_1", "Type of product", choices = unique(esadb$`dimensions.Type of product`), selected = 1)),
                            column(1, selectInput("name_of_non_communicable_disease_1", "Name of non-communicable disease", choices = unique(esadb$`dimensions.Name of non-communicable disease`), selected = 1)),
                            column(1, selectInput("type_of_occupation_1", "Type of occupation", choices = unique(esadb$`dimensions.Type of occupation`), selected = 1))
                            ,
                            column(1, selectInput("activity_1", "Activity", choices = unique(esadb$`dimensions.Activity`), selected = 1)),
                            column(1, selectInput("IHR_capacity_1", "IHR Capacity", choices = unique(esadb$`dimensions.IHR Capacity`), selected = 1)),
                          ),
                          fluidRow(
                            column(1, selectInput("level_status_1", "Level/Status", choices = unique(esadb$`dimensions.Level/Status`), selected = 1)),
                            column(1, selectInput("deviation_level_1", "Deviation Level", choices = unique(esadb$`dimensions.Deviation Level`), selected = 1)),
                            column(1, selectInput("type_of_renewable_technology_1", "Type of renewable technology", choices = unique(esadb$`dimensions.Type of renewable technology`), selected = 1)),
                            column(1, selectInput("disability_status_1", "Disability status", choices = unique(esadb$`dimensions.Disability status`), selected = 1)),
                            column(1, selectInput("name_of_international_institution_1", "Name of international institution", choices = unique(esadb$`dimensions.Name of international institution`), selected = 1)),
                            column(1, selectInput("type_of_speed_1", "Type of speed", choices = unique(esadb$`dimensions.Type of speed`), selected = 1)),
                            column(1, selectInput("type_of_skill_1", "Type of skill", choices = unique(esadb$`dimensions.Type of skill`), selected = 1)),
                            column(1, selectInput("migratory_status_1", "Migratory status", choices = unique(esadb$`dimensions.Migratory status`), selected = 1)),
                            column(1, selectInput("grounds_of_discrimination_1", "Grounds of discrimination", choices = unique(esadb$`dimensions.Grounds of discrimination`), selected = 1)),
                            column(1, selectInput("population_group_1", "Population Group", choices = unique(esadb$`dimensions.Population Group`), selected = 1)),
                            column(1, downloadButton("downloadData6", "Download Data"))
                          ),
                          fluidRow(
                            column(6, helpText("Please ensure filters are selected correctly before proceeding.For more Information on the indicators and disaggregates refer to the UNSD website. To note that, these are illustrative linear projections for series with more than 1 estimate after 2009 thus highly inaccurate and at times over the bounds - thus shouldn't be used as is without deeper analyses.")),
                            
                          ),
                          mainPanel(
                            plotlyOutput("countriesTrendPredChart", width="2500px", height="900px"),
                            DT::dataTableOutput("country_indicators_proj_table"),
                            DT::dataTableOutput("model_metrics_table")
                          )
                 )
)


server <- function(input, output, session) {
  
  # # This reactive expression is used in all tabs
  # filtered_data <- reactive({
  #   if (input$crsdg_filter != "All") {
  #     return(totaldb[totaldb$crsdg == input$crsdg_filter, ])
  #   } else {
  #     return(totaldb)
  #   }
  # })
  
  # Mapping of variable names to date ranges
  date_ranges <- list(
    data3 = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-3),"-" ,as.integer(format(Sys.Date(),"%Y"))),
    data5 = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-5),"-" ,as.integer(format(Sys.Date(),"%Y"))),
    data10 = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-10),"-" ,as.integer(format(Sys.Date(),"%Y"))),
    data15 = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-15),"-" ,as.integer(format(Sys.Date(),"%Y"))),
    data20 = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-20),"-" ,as.integer(format(Sys.Date(),"%Y"))),
    datam20 = paste0(" - Before ",(as.integer(format(Sys.Date(),"%Y"))-20)),
    TotalDataAv = "All Time",
    data3C = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-3),"-" ,as.integer(format(Sys.Date(),"%Y")), " Country data"),
    data5C = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-5),"-" ,as.integer(format(Sys.Date(),"%Y")), " Country data"),
    data10C = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-10),"-" ,as.integer(format(Sys.Date(),"%Y")), " Country data"),
    data15C = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-15),"-" ,as.integer(format(Sys.Date(),"%Y")), " Country data"),
    data20C = paste0(" - Between ",(as.integer(format(Sys.Date(),"%Y"))-20),"-" ,as.integer(format(Sys.Date(),"%Y")), " Country data"),
    datam20C = paste0(" - Before ",(as.integer(format(Sys.Date(),"%Y"))-20), " Country data"),
    TotalDataAvC = " - All Time Country Data"
    
  )
  
  
  
  #### Pop tab 1
  output$pop_stackedLineChart <- renderPlotly({
    
    world_data <- subset(pop, location == "World" & ageLabel == input$selectedAgeLabelStacked)
    africa_data <- subset(pop, location == "Africa" & ageLabel == input$selectedAgeLabelStacked)
    ssa_data <- subset(pop, location == "Sub-Saharan Africa" & ageLabel == input$selectedAgeLabelStacked)
    esar_data<- subset(pop, ageLabel == input$selectedAgeLabelStacked & 
                         !(location %in% c("World", "Africa","Sub-Saharan Africa")))
    esar_data %>% group_by(timeLabel) %>% summarise(popsize=sum(popsize)) -> esar_data
    esar_data$location<- "ESAR"
    
    # Calculate "Rest of Africa" and "Rest of the World"
    rest_of_ssa_popsize <- ssa_data$popsize - esar_data$popsize
    rest_of_africa_popsize <- africa_data$popsize - ssa_data$popsize
    rest_of_world_popsize <- world_data$popsize - africa_data$popsize
    
    # Create the plotly stacked area chart
    p <- plot_ly() %>%
      add_trace(data= esar_data, x= ~timeLabel, y= ~popsize/1e6, type = 'scatter', mode = 'none', stackgroup = 'one', name = "ESA Region" , fillcolor = 'brown') %>%
      add_trace(data = ssa_data, x = ~timeLabel, y = ~rest_of_ssa_popsize/1e6, type = 'scatter', mode = 'none', stackgroup = 'one', name = 'Rest of Sub-Saharan Africa', fillcolor = 'darkred') %>%
      add_trace(data = africa_data, x = ~timeLabel, y = ~rest_of_africa_popsize/1e6, type = 'scatter', mode = 'none', stackgroup = 'one', name = 'Rest of Africa', fillcolor = 'orange') %>%
      add_trace(data = world_data, x = ~timeLabel, y = ~rest_of_world_popsize/1e6, type = 'scatter', mode = 'none', stackgroup = 'one', name = 'Rest of the World', fillcolor = 'lightblue') %>%
      layout(title = paste("Population Trend - ",input$selectedAgeLabelStacked), xaxis = list(title = "Year"), yaxis = list(title = "Population (in millions)", tickformat = ","))
    
    return(p)
  })
  
  output$summary_population <- renderTable({
    esar_data<- subset(pop, ageLabel == input$selectedAgeLabelStacked & 
                         !(location %in% c("World", "Africa","Sub-Saharan Africa")))
    esar_data %>% group_by(timeLabel) %>% summarise(popsize=sum(popsize)) -> esar_data
    esar_data$location<- "ESAR"
    download_data <- subset(pop, (location == "World" | location == "Africa" | location == "Sub-Saharan Africa") & ageLabel == input$selectedAgeLabelStacked) %>% select(location, timeLabel, popsize)
    download_data<- rbind(download_data,esar_data)
    download_data$popsize<- trunc(round(download_data$popsize,0))
    download_data<- download_data %>% pivot_wider(names_from = timeLabel, values_from = (popsize))
    
    download_data[] <- lapply(download_data, function(x) format(x, big.mark = ",", scientific = FALSE, nsmall = 0))
    return(download_data)
  }, sanitize.text.function = function(x) x)
  
  
  # Download handler for the first tab
  output$downloadData_regional_pop_trends <- downloadHandler(
    filename = function() {
      paste("ESAR pop trends ", input$selectedAgeLabelStacked," - " ,Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      esar_data<- subset(pop, ageLabel == input$selectedAgeLabelStacked & 
                           !(location %in% c("World", "Africa","Sub-Saharan Africa")))
      esar_data %>% group_by(timeLabel) %>% summarise(popsize=sum(popsize)) -> esar_data
      esar_data$location<- "ESAR"
      download_data <- subset(pop, (location == "World" | location == "Africa" | location == "Sub-Saharan Africa") & ageLabel == input$selectedAgeLabelStacked) %>% select(location, timeLabel, popsize)
      download_data<- rbind(download_data,esar_data)
      
      download_data<- download_data %>% pivot_wider(names_from = timeLabel, values_from = popsize)
      # Create the excel file
      write_xlsx(download_data, file)
    }
  )
  
  #### pop tab 2
  output$lineChart <- renderPlotly({
    filtered_data <- subset(pop, ageLabel == input$selectedAgeLabel &
                              !(location %in% c("World", "Africa","Sub-Saharan Africa")))
    
    # Determine the upper limit of popsize: if no countries are selected, use the max value of the entire dataset
    if (length(input$selectedCountry) == 0) {
      upper_limit <- max(filtered_data$popsize, na.rm = TRUE)
    } else {
      upper_limit <- max(filtered_data$popsize[filtered_data$location %in% input$selectedCountry], na.rm = TRUE)
    }
    
    p <- ggplot(filtered_data, aes(x = timeLabel, y = popsize/1e6, group = location)) +  # Scale population to millions
      geom_line(aes(color = ifelse(location %in% input$selectedCountry, "red", "grey"), 
                    size = ifelse(location %in% input$selectedCountry, .75, 0.25))) +   # Conditional color and size
      scale_color_identity() +
      scale_size_identity() +  # Allow geom_line to use the size aesthetic
      scale_y_continuous(limits = c(0, upper_limit/1e6)) +  # Set y-axis limits starting from 0 to upper limit
      labs(title = "Population Trends", x = "Year", y = "Population (in millions)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),  # Increase axis text size
            axis.title = element_text(size = 16))  # Increase axis title size
    
    ggplotly(p)
  })
  
  
  # economist style
  # output$lineChart <- renderPlotly({
  #   filtered_data <- subset(pop, ageLabel == input$selectedAgeLabel & 
  #                             !(location %in% c("World", "Africa", "Sub-Saharan Africa")))
  #   
  #   # Determine the upper limit of popsize: if no countries are selected, use the max value of the entire dataset
  #   if (length(input$selectedCountry) == 0) {
  #     upper_limit <- max(filtered_data$popsize, na.rm = TRUE)
  #   } else {
  #     upper_limit <- max(filtered_data$popsize[filtered_data$location %in% input$selectedCountry], na.rm = TRUE)
  #   }
  #   
  #   p <- ggplot(filtered_data, aes(x = timeLabel, y = popsize/1e6, group = location)) +  # Scale population to millions
  #     geom_line(aes(color = ifelse(location %in% input$selectedCountry, "red", "grey")), size = 1) +  # Thicker lines
  #     scale_color_manual(values = c("red" = "red", "grey" = "grey")) +
  #     scale_y_continuous(limits = c(0, upper_limit/1e6)) +  # Set y-axis limits starting from 0 to upper limit
  #     labs(title = "Population over Time", x = "Year", y = "Population (in millions)") +
  #     theme_minimal(base_family = "Helvetica") +  # The Economist uses Helvetica, but it may not be available on all systems
  #     theme(
  #       plot.background = element_rect(fill = "grey92"),  # Light grey background
  #       panel.background = element_rect(fill = "grey92"),
  #       panel.grid.minor = element_blank(),
  #       panel.grid.major = element_line(size = 0.2, linetype = 'solid', color = "white"),  # white major grid
  #       axis.title = element_text(size = 16, face = "bold"),
  #       axis.text = element_text(size = 14)
  #     ) +
  #     guides(color = FALSE)  # Remove legend
  #   
  #   ggplotly(p)
  # })
  
  
  
  output$interactiveMap <- renderPlotly({
    filtered_data <- subset(pop, ageLabel == input$selectedAgeLabel & 
                              !(location %in% c("World", "Africa","Sub-Saharan Africa")))
    
    filtered_data$location[filtered_data$location == "Eswatini"] <- "Swaziland"
    
    # Create title
    chart_title <- "Population by Country Over Time"
    
    # Create choropleth map with animation
    p_map <- plot_geo(filtered_data, locationmode = 'country names') %>%
      add_trace(
        type = 'choropleth',
        locations = ~location,
        z = ~popsize/1e6,
        # text = ~paste(location, ": ", popsize/1e6, " million"),
        color = ~popsize,
        colors = 'Blues',
        marker = list(line = list(width = 0.1)),
        showscale = TRUE,
        frame = ~timeLabel  # Add frame for animation based on timeLabel
      ) %>%
      add_text(
        x = ~lon, # Longitude coordinates for the labels
        y = ~lat, # Latitude coordinates for the labels
        text = ~paste(round(popsize/1e6, 1), "M"),
        showlegend = FALSE,
        frame = ~timeLabel  # Add frame for animation based on timeLabel
      ) %>%
      layout(title = chart_title,
             geo = list(
               showland = TRUE,
               projection = list(type = 'mercator'),
               lonaxis = list(range = c(9, 53)), # Adjust longitude range to focus on Eastern and Southern Africa
               lataxis = list(range = c(-36, 19)), # Adjust latitude range to focus on Eastern and Southern Africa
               landcolor = "white"
             ),
             updatemenus = list(
               list(
                 type = 'buttons',
                 showactive = FALSE,
                 buttons = list(
                   list(label = 'Pause',
                        method = 'animate',
                        args = list(NULL, 
                                    list(frame = list(duration = 0, redraw = TRUE), 
                                         mode = 'immediate',
                                         transition = list(duration = 0)
                                    )
                        )
                   )
                 )
               )
             )
      ) %>%
      animation_opts(frame = 200, redraw = TRUE) %>%
      animation_slider(currentvalue = list(prefix = "Year: "))
    
    return(p_map)
    
  })
  
  ################# tab1 start
  summary_data <- reactive({
    # Filter data based on crsdg if not "All"
    if (input$tab1_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab1_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    
    #cat("input$variable:", input$variable, "\n")
    # Summarize data
    summarized_data <- filtered_data %>%
      group_by(seriesDescription2, geoAreaName) %>%
      summarise(Sum = sum(!!sym(input$tab1_variable), na.rm = TRUE), .groups = 'drop') %>%
      spread(geoAreaName, Sum)
    # (summarized_data)
    # Calculate the sum and count of positive values for each country column
    sums <- colSums(summarized_data[, -1], na.rm = TRUE)
    positive_counts <- colSums(summarized_data[, -1] > 0, na.rm = TRUE)
    
    # Calculate total sum and total count of positive values across all countries
    total_sum <- sum(sums)
    total_positive_counts <- sum(positive_counts)
    
    # Calculate the number of unique seriesDescription2 indicators and number of countries
    num_indicators <- nrow(summarized_data)
    num_countries <- ncol(summarized_data) - 1
    
    # Calculate the percentage of positive counts
    percentage_positive <- (positive_counts / num_indicators) * 100
    total_percentage_positive <- (total_positive_counts / (num_indicators * num_countries)) * 100
    
    # calculate ration of total estimates to number of indicators with positive 
    ratio_nestimates_indicators<- round(sums/positive_counts,1)
    total_ratio_nest_ind<- total_sum/total_positive_counts
    
    # Create summary data frame
    summary_df <- data.frame(Country = names(sums), 
                             "Total number of estimates" = sums, 
                             "Number of indicators with at least 1 estimate" = positive_counts, 
                             "Percentage of indicators with at least 1 estimate" = percentage_positive,
                             "Ratio N of est. to ind" = ratio_nestimates_indicators, check.names = FALSE)
    
    # Add total sum and total count of positive values as an additional row
    summary_df <- rbind(summary_df, data.frame(Country = "Total", 
                                               "Total number of estimates" = total_sum, 
                                               "Number of indicators with at least 1 estimate" = total_positive_counts, 
                                               "Percentage of indicators with at least 1 estimate" = total_percentage_positive,
                                               "Ratio N of est. to ind" = total_ratio_nest_ind, check.names = FALSE))
    
    
    return(summary_df)
  })
  
  output$num_indicators_1 <- renderPrint({
    if (input$tab1_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab1_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    
    # Calculate the number of unique seriesDescription2 indicators
    num_indicators_1 <- length(unique(filtered_data$seriesDescription2))
    paste("Number of indicators selected:", num_indicators_1)
  })
  
  output$map <- renderPlotly({
    summary_df <- summary_data()
    summary_df<-summary_df[-nrow(summary_df),]
    #add centroids for the label positioning
    summary_df <- summary_df %>%
      left_join(centroids, by = c("Country" = "region"))
    
    # Get date range for the selected variable
    date_range <- ifelse(input$tab1_variable %in% names(date_ranges), date_ranges[[input$tab1_variable]], "")
    
    # Create title with date range
    chart_title <- paste("Total number of estimates by Country", date_range)
    
    summary_df$Country[summary_df$Country == "Eswatini"] <- "Swaziland"
    
    # Create choropleth map
    plot <- plot_geo(summary_df, locationmode = 'country names') %>%
      add_trace(
        type = 'choropleth',
        locations = ~Country,
        z = ~`Total number of estimates`,
        text = ~paste(Country, `Total number of estimates`),
        color = ~`Total number of estimates`,
        colors = 'Blues',
        # zmin = ~pop_range$min_pop,
        # zmax = ~pop_range$max_pop,
        marker = list(line = list(width = 0.1)),
        showscale = TRUE
      )%>%
      add_text(
        x = ~lon, # Longitude coordinates for the labels
        y = ~lat, # Latitude coordinates for the labels
        text = ~round(`Total number of estimates`,0),
        showlegend = FALSE
      ) %>%
      layout(title = chart_title,
             geo = list(showland = TRUE,
                        projection = list(type = 'mercator'),
                        lonaxis = list(range = c(9, 53)), # Adjust longitude range to focus on Eastern and Southern Africa
                        lataxis = list(range = c(-36, 19)), # Adjust latitude range to focus on Eastern and Southern Africa
                        landcolor = "white"
             ))
    
    return(plot)
  })
  
  output$bar_chart <- renderPlotly({
    summary_df <- summary_data()
    
    # Extract total percentage
    total_percentage <- tail(summary_df$`Percentage of indicators with at least 1 estimate`, 1)
    
    # Get date range for the selected variable
    date_range <- ifelse(input$tab1_variable %in% names(date_ranges), date_ranges[[input$tab1_variable]], "")
    
    # Create title with date range
    chart_title <- paste("Percentage of Indicators with at Least 1 Estimate by Country", date_range)
    
    df<-summary_df[-nrow(summary_df),]
    
    # Create bar chart
    plot <- plot_ly(df, x = ~Country, y = ~`Percentage of indicators with at least 1 estimate`, type = 'bar', name = 'Percentage by Country') %>%
      add_trace(y = ~total_percentage, type = 'scatter', mode = 'lines', name = 'Total Percentage', line = list(color = 'red')) %>%
      layout(yaxis = list(title = 'Percentage',range=c(0,100)), barmode = 'group', title = chart_title) %>%
      layout(colorway = c('cyan'))
    
    return(plot)
  })
  
  output$summary <- renderTable({
    summary_data()
  })
  
  # Download handler for the first tab
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Create the excel file
      write_xlsx(summary_data(), file)
    }
  )
  
  ##########################tab1 end
  
  ################# tab by SDG availability start
  
  summary_data_goal <- reactive({
    # Filter data based on crsdg if not "All"
    if (input$tab5_crsdg_filter != "All") {
      filtered_data_1 <- totaldb[totaldb$crsdg == input$tab5_crsdg_filter, ]
    } else {
      filtered_data_1 <- totaldb
    }
    
    # Filter data based on crsdg if not "All"
    if (input$tab5_country_filter != "All") {
      filtered_data <- filtered_data_1[filtered_data_1$geoAreaName == input$tab5_country_filter, ]
    } else {
      filtered_data <- filtered_data_1
    }
    
    # Calculate the number of unique seriesDescription2 indicators by goal
    filtered_data_1 %>%
      group_by(goal, seriesDescription2) %>%
      summarise(ind_cnt_goal = n(), .groups = 'drop') %>%
      spread(goal, ind_cnt_goal) -> ind_cnt_per_sdggoal
    
    n_ind_goal<- colSums(ind_cnt_per_sdggoal[, -1] > 0, na.rm = TRUE)
    
    
    #cat("input$variable:", input$variable, "\n")
    # Summarize data
    summarized_data_goal <- filtered_data %>%
      group_by(goal, geoAreaName,seriesDescription2) %>%
      summarise(Sum = sum(!!sym(input$tab5_variable), na.rm = TRUE), .groups = 'drop') %>%
      spread(goal, Sum)
    
    # (summarized_data)
    # Calculate the sum and count of positive values for each country column
    sums <- colSums(summarized_data_goal[, -1:-2], na.rm = TRUE)
    positive_counts <- colSums(summarized_data_goal[, -1:-2] > 0, na.rm = TRUE)
    
    # Calculate total sum and total count of positive values across all countries
    total_sum <- sum(sums)
    total_positive_counts <- sum(positive_counts)
    
    # Find common names (keys) in both vectors
    common_keys <- intersect(names(positive_counts), names(n_ind_goal))
    
    # Subset each vector to only include elements with common keys
    filtered_positive_counts <- positive_counts[common_keys]
    filtered_n_ind_goal <- n_ind_goal[common_keys]
    
    
    # Calculate the percentage of positive counts
    percentage_positive <- (filtered_positive_counts /( filtered_n_ind_goal*length(unique(filtered_data$geoAreaName)))) * 100
    total_percentage_positive <- (total_positive_counts / (sum(filtered_n_ind_goal) * length(unique(filtered_data$geoAreaName)))) * 100
    
    # calculate ration of total estimates to number of indicators with positive 
    ratio_nestimates_indicators<- round(sums/filtered_positive_counts,1)
    total_ratio_nest_ind<- total_sum/total_positive_counts
    
    # Create summary data frame
    summary_df <- data.frame(Goals = names(sums), 
                             "Total number of estimates" = sums, 
                             "Number of indicators with at least 1 estimate" = positive_counts,
                             "Number of Indicators" = filtered_n_ind_goal,
                             "Percentage of indicators with at least 1 estimate" = percentage_positive,
                             "Ratio N of est. to ind" = ratio_nestimates_indicators, check.names = FALSE)
    
    # Add total sum and total count of positive values as an additional row
    summary_df <- rbind(summary_df, data.frame(Goals = "Total", 
                                               "Total number of estimates" = total_sum, 
                                               "Number of indicators with at least 1 estimate" = total_positive_counts, 
                                               "Number of Indicators" = sum(filtered_n_ind_goal),
                                               "Percentage of indicators with at least 1 estimate" = total_percentage_positive,
                                               "Ratio N of est. to ind" = total_ratio_nest_ind, check.names = FALSE))
    
    
    return(summary_df)
  })
  
  output$num_indicators_5 <- renderPrint({
    if (input$tab5_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab5_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    
    # Calculate the number of unique seriesDescription2 indicators
    num_indicators_5 <- length(unique(filtered_data$seriesDescription2))
    paste("Number of indicators selected:", num_indicators_5)
  })
  
  
  output$bar_chart_goal <- renderPlotly({
    summary_df <- summary_data_goal()
    
    # Extract total percentage
    total_percentage <- tail(summary_df$`Percentage of indicators with at least 1 estimate`, 1)
    
    # Get date range for the selected variable
    date_range <- ifelse(input$tab5_variable %in% names(date_ranges), date_ranges[[input$tab5_variable]], "")
    
    # Create title with date range
    chart_title <- paste("Percentage of Indicators with at Least 1 Estimate by Country", date_range)
    
    df<-summary_df[-nrow(summary_df),]
    
    # Create bar chart
    plot <- plot_ly(df, x = ~sort(as.numeric(Goals)), y = ~`Percentage of indicators with at least 1 estimate`, type = 'bar', name = 'Percentage by Country') %>%
      add_trace(y = ~total_percentage, type = 'scatter', mode = 'lines', name = 'Total Percentage', line = list(color = 'red')) %>%
      layout(
        xaxis = list(title = "SDG Goals", tickvals = sort(unique(as.numeric(df$Goals))), ticktext = sort(unique(as.numeric(df$Goals)))),
        yaxis = list(title = 'Percentage',range=c(0,100)), barmode = 'group', title = chart_title) %>%
      layout(colorway = c('cyan'))
    
    return(plot)
  })
  
  output$summary_goal <- renderTable({
    summary_data_goal()
  })
  
  # Download handler for the first tab
  output$downloadData_goal <- downloadHandler(
    filename = function() {
      paste("data availability by SDG -", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Create the excel file
      write_xlsx(summary_data_goal(), file)
    }
  )
  
  
  ################ tab availability by SDG end
  
  
  
  #########################tab2 start
  
  output$data_av_indicator_country <- renderDT({
    # Filter data based on crsdg if not "All"
    if (input$tab2_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab2_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    
    # Summarize data
    summarized_data <- filtered_data %>%
      group_by(seriesDescription2, geoAreaName) %>%
      summarise(Sum = sum(!!sym(input$tab2_variable), na.rm = TRUE), .groups = 'drop') %>%
      spread(geoAreaName, Sum)
    
    # Render datatable
    datatable(summarized_data, options = list(autoWidth = TRUE, scrollX = TRUE, paging= FALSE,columnDefs = list(list(width = '350px', targets = 0))), rownames = FALSE)
  })
  
  output$num_indicators_2 <- renderPrint({
    if (input$tab2_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab2_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    
    # Calculate the number of unique seriesDescription2 indicators
    num_indicators_2 <- length(unique(filtered_data$seriesDescription2))
    paste("Number of indicators selected:", num_indicators_2)
  })
  
  # Download handler for the second tab
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("data Availability by Indicators and Country -", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Filter data based on crsdg if not "All"
      if (input$tab2_crsdg_filter != "All") {
        filtered_data <- totaldb[totaldb$crsdg == input$tab2_crsdg_filter, ]
      } else {
        filtered_data <- totaldb
      }
      
      # Summarize data
      summarized_data <- filtered_data %>%
        group_by(seriesDescription2, geoAreaName) %>%
        summarise(Sum = sum(!!sym(input$tab2_variable), na.rm = TRUE), .groups = 'drop') %>%
        spread(geoAreaName, Sum)
      
      # Create the excel file for indicators by country
      write_xlsx(summarized_data, file)
    }
  )
  ####################tab2 end
  
  ###################tab3 start
  
  output$country_tabs_latest <- renderUI({
    # Filter data based on crsdg if not "All"
    if (input$tab3_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab3_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    
    country_list <- unique(filtered_data$geoAreaName)
    
    tabs <- lapply(country_list, function(country) {
      tabPanel(country,
               DTOutput(paste0("table_latest_", country))
      )
    })
    
    do.call(tabsetPanel, c(id="country_tabs",tabs))
  })
  
  lapply(unique(totaldb$geoAreaName), function(country) {
    output[[paste0("table_latest_", country)]] <- renderDT({
      # Filter data based on crsdg if not "All"
      if (input$tab3_crsdg_filter != "All") {
        filtered_data <- totaldb[totaldb$crsdg == input$tab3_crsdg_filter & totaldb$geoAreaName == country, ]
      } else {
        filtered_data <- totaldb[totaldb$geoAreaName == country, ]
      }
      
      data <- filtered_data[, c("seriesDescription2", "latestvalue", "latestdate")]
      datatable(data, options = list(autoWidth = TRUE, paging = FALSE), rownames = FALSE)
    })
  })
  
  output$num_indicators_3 <- renderPrint({
    # Get the current country from the active tab
    country <- input$country_tabs
    
    if (input$tab3_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab3_crsdg_filter & totaldb$geoAreaName == country, ]
    } else {
      filtered_data <- totaldb[totaldb$geoAreaName == country, ]
    }
    
    # Calculate the number of unique seriesDescription2 indicators
    num_indicators_3 <- length(unique(filtered_data$seriesDescription2))
    paste("Number of indicators with values for",country, ":", num_indicators_3)
  })
  
  # download prepare for page 3
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("country_latest_values-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Create a new workbook
      wb <- createWorkbook()
      
      # Loop through each country to create a sheet and write data
      for (country in unique(totaldb$geoAreaName)) {
        # Filter data based on crsdg if not "All"
        if (input$tab3_crsdg_filter != "All") {
          filtered_data <- totaldb[totaldb$crsdg == input$tab3_crsdg_filter & totaldb$geoAreaName == country, ]
        } else {
          filtered_data <- totaldb[totaldb$geoAreaName == country, ]
        }
        
        data <- filtered_data[, c("seriesDescription2", "latestvalue", "latestdate")]
        
        # Add a sheet for this country
        addWorksheet(wb, sheetName = country)
        
        # Write the data to the sheet
        writeData(wb, sheet = country, x = data)
      }
      
      # Save the workbook to the file
      saveWorkbook(wb, file)
    }
  )
  ##################tab3 end
  
  #################tab4 start
  
  indicators_by_spgoal<- reactive({
    # Filter data based on crsdg if not "All"
    if (input$tab4_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab4_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    summarized_data <- filtered_data %>%
      group_by(seriesDescription2, sp_goal) %>%
      summarise(Countsp = if_else(n()>0,1,0), .groups = 'drop') 
    # %>% spread(sp_goal, Countsp)
    
    summarized_data <- summarized_data %>% group_by(sp_goal) %>% summarise(Number_of_Indicators=sum(Countsp))
    # positive_counts <- colSums(summarized_data[, -1] > 0, na.rm = TRUE)
    total_sum <- colSums(summarized_data[,-1])
    summarized_data<- rbind(summarized_data, data.frame(sp_goal="Total",
                                                        "Number_of_Indicators"=total_sum))
    summarized_data2 <- filtered_data %>%
      group_by(seriesDescription2,geoAreaName, sp_goal) %>% summarise(Sum = sum(!!sym(input$tab4_variable), na.rm = TRUE), .groups = 'drop') 
    summarized_data2 <- summarized_data2 %>% group_by(sp_goal) %>% summarise(Number_of_CountryEstimates=sum(Sum))
    total_sum2 <- colSums(summarized_data2[,-1])
    summarized_data2<- rbind(summarized_data2, data.frame(sp_goal="Total",
                                                          "Number_of_CountryEstimates"=total_sum2))
    
    summarized_data3 <- filtered_data %>%
      group_by(seriesDescription2,geoAreaName, sp_goal) %>% summarise(Sum = sum(!!sym(input$tab4_variable), na.rm = TRUE), .groups = 'drop') 
    summarized_data3 <- summarized_data3 %>% group_by(sp_goal) %>% summarise(Number_of_CountryEst_w1=sum(Sum>0))
    total_sum3 <- colSums(summarized_data3[,-1])
    summarized_data3<- rbind(summarized_data3, data.frame(sp_goal="Total",
                                                          "Number_of_CountryEst_w1"=total_sum3))
    
    summarized_data<- cbind(summarized_data,summarized_data2[,-1],summarized_data3[,-1])
    
    summarized_data <- summarized_data %>% mutate(`Percentage of indicators with at least 1 estimate` = (Number_of_CountryEst_w1 / (length(unique(filtered_data$geoAreaName)) * Number_of_Indicators)) * 100)
    
    return(summarized_data)
  })
  
  
  # output$num_indicators_4 <- renderPrint({
  #   # Get the current country from the active tab
  #   # country <- input$country_tabs
  #   
  #   if (input$tab4_crsdg_filter != "All") {
  #     filtered_data <- totaldb[totaldb$crsdg == input$tab4_crsdg_filter, ]
  #   } else {
  #     filtered_data <- totaldb
  #   }
  #   
  #   # Calculate the number of unique seriesDescription2 indicators
  #   num_indicators_4 <- length(unique(filtered_data$seriesDescription2))
  #   paste("Number of indicators with values for",country, ":", num_indicators_3)
  # })
  
  output$sptable <- renderTable({
    indicators_by_spgoal()
  })
  
  output$bar_chart_sp_goal <- renderPlotly({
    summary_df <- indicators_by_spgoal()
    
    # Extract total percentage
    total_percentage <- tail(summary_df$`Percentage of indicators with at least 1 estimate`, 1)
    
    # Get date range for the selected variable
    date_range <- ifelse(input$tab4_variable %in% names(date_ranges), date_ranges[[input$tab4_variable]], "")
    
    # Create title with date range
    chart_title <- paste("Percentage of Indicators with at Least 1 Estimate by SP Goal Area", date_range)
    
    df<-summary_df[-nrow(summary_df),]
    # Create bar chart
    plot <- plot_ly(df, x = ~sp_goal, y = ~`Percentage of indicators with at least 1 estimate`, type = 'bar', name = 'Percentage by SP Goal Area') %>%
      add_trace(y = ~total_percentage, type = 'scatter', mode = 'lines', name = 'Total Percentage', line = list(color = 'red'))  %>%
      layout(yaxis = list(title = 'Percentage',range=c(0,100)), barmode = 'group', title = chart_title) %>%
      layout(colorway = c('cyan'))
    
    return(plot)
  })
  
  
  # Stacked bar chart for countries by sp_goal
  output$stacked_bar_chart_countries <- renderPlotly({
    if (input$tab4_crsdg_filter != "All") {
      filtered_data <- totaldb[totaldb$crsdg == input$tab4_crsdg_filter, ]
    } else {
      filtered_data <- totaldb
    }
    summarized_data3 <- filtered_data %>%
      group_by(geoAreaName,seriesDescription2, sp_goal) %>% summarise(Sum = sum(!!sym(input$tab4_variable), na.rm = TRUE), .groups = 'drop') 
    summarized_data3 <- summarized_data3 %>% group_by(geoAreaName,sp_goal) %>% summarise(Number_of_CountryEst_w1=sum(Sum>0)) %>%
      arrange(geoAreaName, sp_goal) %>%
      group_by(geoAreaName) %>%
      mutate(total_bar = sum(Number_of_CountryEst_w1),
             cumulative_sum = total_bar - cumsum(Number_of_CountryEst_w1) + 0.5 * Number_of_CountryEst_w1)
    
    # Get date range for the selected variable
    date_range <- ifelse(input$tab4_variable %in% names(date_ranges), date_ranges[[input$tab4_variable]], "")
    
    # Create title with date range
    chart_title <- paste("Number of Indicators with at Least 1 Estimate by SP Goal Area", date_range)
    
    plot <- ggplot(summarized_data3, aes(x = geoAreaName, y = Number_of_CountryEst_w1, fill = as.character(sp_goal))) +
      geom_bar(stat = "identity") +
      geom_text(aes(y = cumulative_sum, label = Number_of_CountryEst_w1), size = 3) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
      ) +
      labs(title = chart_title,
           x = "Country",
           y = "Number of indicators",
           fill = "UNICEF SP Goals"
      )
    
    ggplotly(plot)
  })
  
  
  # download prepare for page 4
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("Data Availability by SP -", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$tab4_crsdg_filter != "All") {
        filtered_data <- totaldb[totaldb$crsdg == input$tab4_crsdg_filter, ]
      } else {
        filtered_data <- totaldb
      }
      summarized_data <- filtered_data %>%
        group_by(seriesDescription2, sp_goal) %>%
        summarise(Countsp = if_else(n()>0,1,0), .groups = 'drop') 
      # %>% spread(sp_goal, Countsp)
      
      summarized_data <- summarized_data %>% group_by(sp_goal) %>% summarise(Number_of_Indicators=sum(Countsp))
      # positive_counts <- colSums(summarized_data[, -1] > 0, na.rm = TRUE)
      total_sum <- colSums(summarized_data[,-1])
      summarized_data<- rbind(summarized_data, data.frame(sp_goal="Total",
                                                          "Number_of_Indicators"=total_sum))
      summarized_data2 <- filtered_data %>%
        group_by(seriesDescription2,geoAreaName, sp_goal) %>% summarise(Sum = sum(!!sym(input$tab4_variable), na.rm = TRUE), .groups = 'drop') 
      summarized_data2 <- summarized_data2 %>% group_by(sp_goal) %>% summarise(Number_of_CountryEstimates=sum(Sum))
      total_sum2 <- colSums(summarized_data2[,-1])
      summarized_data2<- rbind(summarized_data2, data.frame(sp_goal="Total",
                                                            "Number_of_CountryEstimates"=total_sum2))
      
      summarized_data3 <- filtered_data %>%
        group_by(seriesDescription2,geoAreaName, sp_goal) %>% summarise(Sum = sum(!!sym(input$tab4_variable), na.rm = TRUE), .groups = 'drop') 
      summarized_data3 <- summarized_data3 %>% group_by(sp_goal) %>% summarise(Number_of_CountryEst_w1=sum(Sum>0))
      total_sum3 <- colSums(summarized_data3[,-1])
      summarized_data3<- rbind(summarized_data3, data.frame(sp_goal="Total",
                                                            "Number_of_CountryEst_w1"=total_sum3))
      
      summarized_data4<- filtered_data %>%
        group_by(geoAreaName,seriesDescription2, sp_goal) %>% summarise(Sum = sum(!!sym(input$tab4_variable), na.rm = TRUE), .groups = 'drop') 
      summarized_data4 <- summarized_data4 %>% group_by(geoAreaName,sp_goal) %>% summarise(Number_of_CountryEst_w1=sum(Sum>0)) %>% pivot_wider(names_from = geoAreaName, values_from =Number_of_CountryEst_w1 )
      # 1. Calculate the sums for all columns excluding the `sp_goal` column
      sums <- colSums(summarized_data4[, -1], na.rm = TRUE)
      
      # 2. Create a new data frame row
      total_row <- data.frame(sp_goal = "Total", t(sums))
      names(total_row) <- names(summarized_data4)
      
      # 3. rbind the new row to the original data
      summarized_data4 <- rbind(summarized_data4,total_row)
      
      summarized_data<- cbind(summarized_data,summarized_data2[,-1],summarized_data3[,-1],summarized_data4[,-1])
      
      summarized_data <- summarized_data %>% mutate(`Percentage of indicators with at least 1 estimate` = (Number_of_CountryEst_w1 / (length(unique(filtered_data$geoAreaName)) * Number_of_Indicators)) * 100)
      write_xlsx(summarized_data, file)
    }
  )
  
  
  ###################tab4 end
  
  ######Code for country indicator trends
  
  # Reactive filter based on seriesDescription2
  filtered_data <- reactive({
    req(input$seriesDescription2)
    esadb %>% filter(seriesDescription2 == input$seriesDescription2)
  })
  
  # Updating other filters based on seriesDescription2 filter
  observe({
    req(filtered_data())
    # Update each selectInput
    updateSelectInput(session, "nature", choices = unique(filtered_data()$`attributes.Nature`))
    updateSelectInput(session, "age", choices = unique(filtered_data()$`dimensions.Age`))
    updateSelectInput(session, "location", choices = unique(filtered_data()$`dimensions.Location`))
    updateSelectInput(session, "sex", choices = unique(filtered_data()$`dimensions.Sex`))
    updateSelectInput(session, "quantile", choices = unique(filtered_data()$`dimensions.Quantile`))
    updateSelectInput(session, "type_of_product", choices = unique(filtered_data()$`dimensions.Type of product`))
    updateSelectInput(session, "name_of_non_communicable_disease", choices = unique(filtered_data()$`dimensions.Name of non-communicable disease`))
    updateSelectInput(session, "type_of_occupation", choices = unique(filtered_data()$`dimensions.Type of occupation`))
    updateSelectInput(session, "IHR_capacity", choices = unique(filtered_data()$`dimensions.IHR Capacity`))
    updateSelectInput(session, "education_level", choices = unique(filtered_data()$`dimensions.Education level`))
    updateSelectInput(session, "activity", choices = unique(filtered_data()$`dimensions.Activity`))
    updateSelectInput(session, "level_status", choices = unique(filtered_data()$`dimensions.Level/Status`))
    updateSelectInput(session, "deviation_level", choices = unique(filtered_data()$`dimensions.Deviation Level`))
    updateSelectInput(session, "type_of_renewable_technology", choices = unique(filtered_data()$`dimensions.Type of renewable technology`))
    updateSelectInput(session, "disability_status", choices = unique(filtered_data()$`dimensions.Disability status`))
    updateSelectInput(session, "name_of_international_institution", choices = unique(filtered_data()$`dimensions.Name of international institution`))
    updateSelectInput(session, "type_of_speed", choices = unique(filtered_data()$`dimensions.Type of speed`))
    updateSelectInput(session, "type_of_skill", choices = unique(filtered_data()$`dimensions.Type of skill`))
    updateSelectInput(session, "migratory_status", choices = unique(filtered_data()$`dimensions.Migratory status`))
    updateSelectInput(session, "grounds_of_discrimination", choices = unique(filtered_data()$`dimensions.Grounds of discrimination`))
    updateSelectInput(session, "population_group", choices = unique(filtered_data()$`dimensions.Population Group`))
  })
  # Create a reactiveValue to hold the data for the table
  # rv <- reactiveValues()
  
  # Reactive filter based on all inputs
  filtered_data_all <- reactive({
    
    req(input$nature, input$age, input$location, input$sex, input$quantile, 
        input$type_of_product, input$name_of_non_communicable_disease, input$type_of_occupation, input$IHR_capacity, 
        input$education_level, input$activity, input$level_status, input$deviation_level, input$type_of_renewable_technology, 
        input$disability_status, input$name_of_international_institution, input$type_of_speed, input$type_of_skill, 
        input$migratory_status, input$grounds_of_discrimination, input$population_group)
    filtered <- filtered_data() %>%
      filter(`attributes.Nature` == input$nature,
             `dimensions.Age` == input$age,
             `dimensions.Location` == input$location,
             `dimensions.Sex` == input$sex,
             `dimensions.Quantile` == input$quantile,
             `dimensions.Type of product` == input$type_of_product,
             `dimensions.Name of non-communicable disease` == input$name_of_non_communicable_disease,
             `dimensions.Type of occupation` == input$type_of_occupation,
             `dimensions.IHR Capacity` == input$IHR_capacity,
             `dimensions.Education level` == input$education_level,
             `dimensions.Activity` == input$activity,
             `dimensions.Level/Status` == input$level_status,
             `dimensions.Deviation Level` == input$deviation_level,
             `dimensions.Type of renewable technology` == input$type_of_renewable_technology,
             `dimensions.Disability status` == input$disability_status,
             `dimensions.Name of international institution` == input$name_of_international_institution,
             `dimensions.Type of speed` == input$type_of_speed,
             `dimensions.Type of skill` == input$type_of_skill,
             `dimensions.Migratory status` == input$migratory_status,
             `dimensions.Grounds of discrimination` == input$grounds_of_discrimination,
             `dimensions.Population Group` == input$population_group) 
    
    # Compute and sort the data
    data <- filtered %>%
      select(geoAreaName, indicator,series, seriesDescription, starts_with("timePeriodStart"), numeric_value) %>%
      mutate(numeric_value = as.numeric(numeric_value)) %>%  # Convert numeric_value to numeric
      mutate(numeric_value = ifelse(!is.na(numeric_value), round(numeric_value, 1), NA)) %>%  # Round numeric_value to 1 decimal
      distinct() %>%  # Remove duplicate rows
      pivot_wider(names_from = starts_with("timePeriodStart"), values_from = numeric_value)
    
    # Sort the columns by date
    date_cols <- grep("^\\d{4}$", names(data), value = TRUE)  # capture years in the format YYYY
    non_date_cols <- setdiff(names(data), date_cols)
    date_cols <- date_cols[order(as.numeric(date_cols))]  # Convert to numeric for sorting
    data <- data %>%
      select(non_date_cols, date_cols)
    
    return(data)
  })
  
  # # Compute the data for the table
  # observe({
  #   rv$data <- filtered_data_all() %>%
  #     select(geoAreaName, series, seriesDescription, starts_with("timePeriodStart"), numeric_value) %>%
  #     pivot_wider(names_from = starts_with("timePeriodStart"), values_from = numeric_value)
  # })
  
  # Render the datatable
  output$country_indicators_trend <- DT::renderDataTable({
    filtered_data_all ()
  })
  
  output$countriesTrendChart <- renderPlotly({
    data_to_plot <- filtered_data_all()
    
    # Keep only the first occurrence of each country
    data_to_plot <- data_to_plot %>%
      group_by(geoAreaName) %>%
      slice(1) %>%
      ungroup()
    
    # Convert the data to long format for plotting
    data_long <- data_to_plot %>%
      gather(key = "timePeriodStart", value = "numeric_value", -geoAreaName, -indicator, -series, -seriesDescription) %>%
      group_by(geoAreaName) %>%
      mutate(n_valid_points = sum(!is.na(numeric_value))) %>%
      ungroup()
    
    # Compute the minimum value in 'numeric_value'
    min_val <- min(data_long$numeric_value, na.rm = TRUE)
    # If min_val is positive, set lower limit to 0. Otherwise, use min_val.
    lower_limit <- ifelse(min_val > 0, 0, min_val)
    
    
    p <- ggplot() +
      geom_line(data = data_long %>% filter(!is.na(numeric_value)),
                aes(x = as.numeric(timePeriodStart), y = numeric_value, color = geoAreaName, group = geoAreaName),
                linetype = 1) + # Here linetype=2 sets it to dashed, you can use other numbers for other types
      geom_point(data = data_long %>% filter(!is.na(numeric_value)),
                 aes(x = as.numeric(timePeriodStart), y = numeric_value, color = geoAreaName)) +
      scale_y_continuous(limits = c(lower_limit, NA), expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste(input$seriesDescription2, " - Country Trends"), x = "Year", y = "Value") +
      theme_minimal()
    
    ggplotly(p) # Convert ggplot object to plotly for interactivity
  })
  
  # Download handler
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste(input$seriesDescription2,"-",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data_all(), file, row.names = FALSE)
    }
  )
  
  ############ tab SDG trends
  
  ########## tab SDG projections
  
  # Reactive filter based on seriesDescription2
  filtered_data_1 <- reactive({
    req(input$seriesDescription2_1)
    esadb %>% filter(seriesDescription2 == input$seriesDescription2_1) %>% filter(!is.na(numeric_value))
  })
  
  # Updating other filters based on seriesDescription2 filter
  observe({
    req(filtered_data_1())
    
    # Update each selectInput
    updateSelectInput(session, "nature_1", choices = unique(filtered_data_1()$`attributes.Nature`))
    updateSelectInput(session, "age_1", choices = unique(filtered_data_1()$`dimensions.Age`))
    updateSelectInput(session, "location_1", choices = unique(filtered_data_1()$`dimensions.Location`))
    updateSelectInput(session, "sex_1", choices = unique(filtered_data_1()$`dimensions.Sex`))
    updateSelectInput(session, "quantile_1", choices = unique(filtered_data_1()$`dimensions.Quantile`))
    updateSelectInput(session, "type_of_product_1", choices = unique(filtered_data_1()$`dimensions.Type of product`))
    updateSelectInput(session, "name_of_non_communicable_disease_1", choices = unique(filtered_data_1()$`dimensions.Name of non-communicable disease`))
    updateSelectInput(session, "type_of_occupation_1", choices = unique(filtered_data_1()$`dimensions.Type of occupation`))
    updateSelectInput(session, "IHR_capacity_1", choices = unique(filtered_data_1()$`dimensions.IHR Capacity`))
    updateSelectInput(session, "education_level_1", choices = unique(filtered_data_1()$`dimensions.Education level`))
    updateSelectInput(session, "activity_1", choices = unique(filtered_data_1()$`dimensions.Activity`))
    updateSelectInput(session, "level_status_1", choices = unique(filtered_data_1()$`dimensions.Level/Status`))
    updateSelectInput(session, "deviation_level_1", choices = unique(filtered_data_1()$`dimensions.Deviation Level`))
    updateSelectInput(session, "type_of_renewable_technology_1", choices = unique(filtered_data_1()$`dimensions.Type of renewable technology`))
    updateSelectInput(session, "disability_status_1", choices = unique(filtered_data_1()$`dimensions.Disability status`))
    updateSelectInput(session, "name_of_international_institution_1", choices = unique(filtered_data_1()$`dimensions.Name of international institution`))
    updateSelectInput(session, "type_of_speed_1", choices = unique(filtered_data_1()$`dimensions.Type of speed`))
    updateSelectInput(session, "type_of_skill_1", choices = unique(filtered_data_1()$`dimensions.Type of skill`))
    updateSelectInput(session, "migratory_status_1", choices = unique(filtered_data_1()$`dimensions.Migratory status`))
    updateSelectInput(session, "grounds_of_discrimination_1", choices = unique(filtered_data_1()$`dimensions.Grounds of discrimination`))
    updateSelectInput(session, "population_group_1", choices = unique(filtered_data_1()$`dimensions.Population Group`))
  })
  # Create a reactiveValue to hold the data for the table
  # rv <- reactiveValues()
  
  # Reactive filter based on all inputs
  filtered_data_all_1 <- reactive({
    req(input$nature_1, input$age_1, input$location_1, input$sex_1, input$quantile_1, 
        input$type_of_product_1, input$name_of_non_communicable_disease_1, input$type_of_occupation_1, input$IHR_capacity_1, 
        input$education_level_1, input$activity_1, input$level_status_1, input$deviation_level_1, input$type_of_renewable_technology_1, 
        input$disability_status_1, input$name_of_international_institution_1, input$type_of_speed_1, input$type_of_skill_1, 
        input$migratory_status_1, input$grounds_of_discrimination_1, input$population_group_1)
    filtered <- filtered_data_1() %>%
      filter(`attributes.Nature` == input$nature_1,
             `dimensions.Age` == input$age_1,
             `dimensions.Location` == input$location_1,
             `dimensions.Sex` == input$sex_1,
             `dimensions.Quantile` == input$quantile_1,
             `dimensions.Type of product` == input$type_of_product_1,
             `dimensions.Name of non-communicable disease` == input$name_of_non_communicable_disease_1,
             `dimensions.Type of occupation` == input$type_of_occupation_1,
             `dimensions.IHR Capacity` == input$IHR_capacity_1,
             `dimensions.Education level` == input$education_level_1,
             `dimensions.Activity` == input$activity_1,
             `dimensions.Level/Status` == input$level_status_1,
             `dimensions.Deviation Level` == input$deviation_level_1,
             `dimensions.Type of renewable technology` == input$type_of_renewable_technology_1,
             `dimensions.Disability status` == input$disability_status_1,
             `dimensions.Name of international institution` == input$name_of_international_institution_1,
             `dimensions.Type of speed` == input$type_of_speed_1,
             `dimensions.Type of skill` == input$type_of_skill_1,
             `dimensions.Migratory status` == input$migratory_status_1,
             `dimensions.Grounds of discrimination` == input$grounds_of_discrimination_1,
             `dimensions.Population Group` == input$population_group_1) 
    
    # Compute and sort the data
    data <- filtered %>%
      select(geoAreaName, indicator,series, seriesDescription, starts_with("timePeriodStart"), numeric_value) %>%
      mutate(numeric_value = as.numeric(numeric_value)) %>%  # Convert numeric_value to numeric
      mutate(numeric_value = ifelse(!is.na(numeric_value), round(numeric_value, 1), NA)) %>%  # Round numeric_value to 1 decimal
      distinct() %>%  # Remove duplicate rows
      pivot_wider(names_from = starts_with("timePeriodStart"), values_from = numeric_value)
    
    # Sort the columns by date
    date_cols <- grep("^\\d{4}$", names(data), value = TRUE)  # capture years in the format YYYY
    non_date_cols <- setdiff(names(data), date_cols)
    date_cols <- date_cols[order(as.numeric(date_cols))]  # Convert to numeric for sorting
    data <- data %>%
      select(non_date_cols, date_cols)
    
    return(data)
  })
  
  # countriesTrendPredChart_data<- reactive({
  #   data_to_plot <- filtered_data_all_1()
  #   
  #   # Convert data to long format for modeling and plotting
  #   data_long <- data_to_plot %>% 
  #     gather(key = "timePeriodStart", value = "numeric_value", -geoAreaName, -indicator, -series, -seriesDescription) %>%
  #     filter(as.numeric(timePeriodStart) >= 2010)%>%
  #     na.omit()  # remove rows with NAs
  #   
  # # Fit models for countries with more than 2 estimates and predict till 2030
  #   predictions <- data_long %>%
  #     group_by(geoAreaName) %>%
  #     filter(n() > 1) %>%
  #     do({
  #       country_data = .
  #       
  #       if (nrow(country_data) > 1) {
  #         model <- lm(numeric_value ~ as.numeric(timePeriodStart), data = country_data)
  #         pred_data <- data.frame(timePeriodStart = as.character(2023:2030))
  #         pred_data$numeric_value <- predict(model, newdata = pred_data)
  #         pred_data
  #       } else {
  #         return(NULL)
  #       }
  #     }) %>%
  #     ungroup()
  # 
  #   data_long <- mutate(data_long, DataType = "Actual")
  #   predictions <- mutate(predictions, DataType = "Projected")
  #   
  #   
  #   # Bind the original data with the predictions
  #   all_data <- bind_rows(data_long, predictions)
  #   
  #   return((all_data))
  #   
  # })
  
  
  #### forecast package version ZZZ model
  
  countriesTrendPredChart_data <- reactive({
    data_to_plot <- filtered_data_all_1()
    
    # Convert data to long format for modeling and plotting
    data_long <- data_to_plot %>%
      gather(key = "timePeriodStart", value = "numeric_value", -geoAreaName, -indicator, -series, -seriesDescription) %>%
      filter(as.numeric(timePeriodStart) >= 2005) %>%
      na.omit()  # remove rows with NAs
    
    # Initialize a list to store model metrics
    model_metrics <- list()
    
    # Fit adaptive ETS models for countries with more than 2 estimates and predict till 2030
    predictions <- data_long %>%
      group_by(geoAreaName) %>%
      filter(n() > 1) %>%
      do({
        country_data = .
        
        last_year <- max(as.numeric(country_data$timePeriodStart)) # Get the maximum year for each country
        
        if (nrow(country_data) > 1) {
          model <- ets(country_data$numeric_value, model = "ZZZ") # adaptive ETS model
          pred_years <- as.character((last_year + 1):2030) # Start predicting from the year after the maximum year
          pred_data <- data.frame(timePeriodStart = pred_years)
          forecasted <- forecast(model, h = length(pred_data$timePeriodStart))
          pred_data$numeric_value <- forecasted$mean
          
          # Check if model$aic is NULL, and replace with NA if so
          aic_value <- ifelse(is.null(model$aic), NA, model$aic)
          
          # Compute the MAE
          MAE <- mean(abs(fitted(model) - country_data$numeric_value))
          model_info <- data.frame(Country=unique(country_data$geoAreaName),
                                   ETS=model$method,
                                   AIC=aic_value,
                                   MAE=MAE)
          #                         
          # Capture the model components, parameter values, MAE, and AIC
          # model_info <- data.frame(
          # print(unique(country_data$geoAreaName))
          # print(model$method)
          # print(model$aic)
          
          model_metrics[[unique(country_data$geoAreaName)]] <<- model_info
          
          pred_data
        } else {
          return(NULL)
        }
      }) %>%
      ungroup()
    
    # Convert the list of model metrics to a dataframe
    metrics_df <- do.call("rbind", model_metrics)
    
    
    data_long <- mutate(data_long, DataType = "Actual")
    predictions <- mutate(predictions, DataType = "Projected")
    
    # Bind the original data with the predictions
    all_data <- bind_rows(data_long, predictions)
    
    return(list(all_data = all_data, metrics_df = metrics_df))
  })
  
  
  
  output$countriesTrendPredChart <- renderPlotly({
    temp_data<- countriesTrendPredChart_data()$all_data
    # data_to_plot <- filtered_data_all_1()
    
    # Convert data to long format for modeling and plotting
    # data_long <- data_to_plot %>%
    #   gather(key = "timePeriodStart", value = "numeric_value", -geoAreaName, -indicator, -series, -seriesDescription) %>% filter(as.numeric(timePeriodStart) >= 2010)
    # Plot
    p <- ggplot(data= temp_data, aes(x = as.numeric(timePeriodStart), y = numeric_value, color = geoAreaName, group = geoAreaName)) +
      geom_line(linetype=5) +
      geom_point(data = temp_data, aes(x = as.numeric(timePeriodStart), y = numeric_value, color = geoAreaName), size = 1) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Starts at 0 and allows for a 10% expansion at the top.
      labs(title = paste(input$seriesDescription2_1," -  Projections"), x = "Time Period Start", y = "Value") +
      scale_color_discrete(name = "Countries") + # Set legend title to "Countries"
      theme_minimal()
    
    ggplotly(p) # Convert ggplot object to plotly for interactivity
    
  })
  
  
  output$country_indicators_proj_table <- DT::renderDataTable({
    data_temp<-countriesTrendPredChart_data()$all_data
    
    data_temp <- data_temp %>%
      group_by(geoAreaName) %>%  # Group by country
      fill(indicator, series, seriesDescription, .direction = "downup") %>%  # Fill NAs down and then up
      ungroup()
    
    data_temp <- data_temp %>%
      select(geoAreaName, indicator,series, seriesDescription, starts_with("timePeriodStart"), numeric_value) %>%
      mutate(numeric_value = as.numeric(numeric_value)) %>%  # Convert numeric_value to numeric
      mutate(numeric_value = ifelse(!is.na(numeric_value), round(numeric_value, 1), NA)) %>%  # Round numeric_value to 1 decimal
      distinct() %>%  # Remove duplicate rows
      pivot_wider(names_from = starts_with("timePeriodStart"), values_from = numeric_value)
    
    # Sort the columns by date
    date_cols <- grep("^\\d{4}$", names(data_temp), value = TRUE)  # capture years in the format YYYY
    non_date_cols <- setdiff(names(data_temp), date_cols)
    date_cols <- date_cols[order(as.numeric(date_cols))]  # Convert to numeric for sorting
    data_temp <- data_temp %>%
      select(non_date_cols, date_cols)
    
    return(data_temp)
  })
  
  # You can render the metrics_df as a table using DT or other Shiny-compatible table rendering packages:
  output$model_metrics_table <- DT::renderDataTable({
    req(countriesTrendPredChart_data()$metrics_df)
    DT::datatable(countriesTrendPredChart_data()$metrics_df)
  })
  
  # output$countriesTrendPredChart <- renderPlotly({
  #   data_to_plot <- filtered_data_all()
  #   
  #   # Convert data to long format for modeling and plotting
  #   data_long <- data_to_plot %>%
  #     gather(key = "timePeriodStart", value = "numeric_value", -geoAreaName, -indicator, -series, -seriesDescription)
  #   
  #   # Fit models for countries with more than 2 estimates and predict till 2030
  #   predictions <- data_long %>%
  #     group_by(geoAreaName) %>%
  #     do({
  #       country_data = .
  #       
  #       if (nrow(country_data) > 2) {
  #         model <- lm(numeric_value ~ as.numeric(timePeriodStart), data = country_data)
  #         pred_data <- data.frame(timePeriodStart = as.character(2023:2030))
  #         pred_data$numeric_value <- predict(model, newdata = pred_data)
  #         pred_data
  #       } else {
  #         return(NULL)
  #       }
  #     }) %>%
  #     ungroup()
  #   
  #   # Bind the original data with the predictions
  #   all_data <- bind_rows(data_long, predictions)
  #   
  #   # Plot
  #   p <- ggplot(all_data, aes(x = as.numeric(timePeriodStart), y = numeric_value, color = geoAreaName, group = geoAreaName)) +
  #     geom_line() +
  #     geom_point(data = data_long, aes(x = as.numeric(timePeriodStart), y = numeric_value, color = geoAreaName), size = 2) +
  #     labs(title = "Country Series with Projections till 2030", x = "Time Period Start", y = "Numeric Value") +
  #     theme_minimal()
  #   
  #   ggplotly(p) # Convert ggplot object to plotly for interactivity
  # })
  
  # Download handler
  output$downloadData6 <- downloadHandler(
    filename = function() {
      paste(input$seriesDescription2,"-",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data_temp<-countriesTrendPredChart_data()$all_data
      
      data_temp <- data_temp %>%
        group_by(geoAreaName) %>%  # Group by country
        fill(indicator, series, seriesDescription, .direction = "downup") %>%  # Fill NAs down and then up
        ungroup()
      
      data_temp <- data_temp %>%
        select(geoAreaName, indicator,series, seriesDescription, starts_with("timePeriodStart"), numeric_value) %>%
        mutate(numeric_value = as.numeric(numeric_value)) %>%  # Convert numeric_value to numeric
        mutate(numeric_value = ifelse(!is.na(numeric_value), round(numeric_value, 1), NA)) %>%  # Round numeric_value to 1 decimal
        distinct() %>%  # Remove duplicate rows
        pivot_wider(names_from = starts_with("timePeriodStart"), values_from = numeric_value)
      
      # Sort the columns by date
      date_cols <- grep("^\\d{4}$", names(data_temp), value = TRUE)  # capture years in the format YYYY
      non_date_cols <- setdiff(names(data_temp), date_cols)
      date_cols <- date_cols[order(as.numeric(date_cols))]  # Convert to numeric for sorting
      data_temp <- data_temp %>%
        select(non_date_cols, date_cols)
      
      
      write.csv(data_temp, file, row.names = FALSE)
    }
  )
  ##### code end
  
}

shinyApp(ui = ui, server = server)
