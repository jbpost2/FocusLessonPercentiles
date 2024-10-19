library(tidyverse)
library(shiny)
library(shinydashboard)
library(tidycensus)
library(shinyvalidate)

all_rent <- readRDS("rent.rds")
#create state names with abbreviations
state_abb <- datasets::state.name

#get some default values
start_data <- all_rent[["Arkansas"]]
#drop 0 values and find mean/sd
start_data_rent <- start_data[start_data$estimate!=0, ]$estimate
start_mean <- round(mean(start_data_rent, na.rm = TRUE))
start_sd <- round(sd(start_data_rent, na.rm = TRUE))

# Define UI for application that draws a histogram
ui <- dashboardPage(
      # withMathJax(),
        dashboardHeader(title = "Census Data Activity", disable = FALSE),
        dashboardSidebar(disable = FALSE,
          sidebarMenu(
            menuItem("Median Rent Exploration", tabName = "first", icon = icon("archive")),
            menuItem("Comparing Two Distributions", tabName = "second", icon = icon("laptop"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "first",
                    titlePanel("Median Rent Exploration"),
                    sidebarLayout(
                      sidebarPanel(
                        p("Choose the state below to obtain data about the percentage of income that goes towards rent. Downloaded data is the median for each census tract within the chosen state."),
                        selectizeInput("state",
                                       "State:",
                                       choices = state_abb,
                                       selected = state_abb[1]),
                        radioButtons("adornment", "Find a Probability or Percentile?",
                                     choices = c("No", "Percentile", "Probability"),
                                     selected = "No"),
                        conditionalPanel("input.adornment == 'Percentile'",
                                         numericInput("percentile", "Percentile to find:", value = 50, min = 1, max = 99, step = 1)),
                        conditionalPanel("input.adornment == 'Probability'",
                                         radioButtons("prob_type", "Which type of Probability to Find?",
                                                      choices = c("Less Than", "Between", "Greater Than"),
                                                      selected = "Less Than"),
                                         conditionalPanel("input.prob_type == 'Less Than'",
                                                          numericInput("less_than", "Upper Value", value = start_mean)),
                                         conditionalPanel("input.prob_type == 'Between'",
                                                          numericInput("between1", "Lower Value", value = start_mean-1*start_sd),
                                                          numericInput("between2", "Upper Value", value = start_mean+1*start_sd)),
                                         conditionalPanel("input.prob_type == 'Greater Than'",
                                                          numericInput("greater_than", "Lower Value", value = start_mean))
                        ),
                        "Data courtesy: U.S. Census Bureau. (2022). American Community Survey 5 Year Data (2019-2022). U.S. Department of Commerce. Retrieved January 16, 2024, from https://data.census.gov/",
                        br(),
                        "Values above 50 were labeled as 50+ by the census burea. These have been removed from all datasets."
                      ),
                      # Show a plot of the generated distribution
                      mainPanel(
                        fluidRow(
                          tabBox(
                                id = "first_plots",
                                tabPanel(title = "Normal Approximation",
                                         plotOutput("norm_approx")),
                                tabPanel(title = "Map View", 
                                         shinycssloaders::withSpinner(
                                           leaflet::leafletOutput("map_plot"))
                                         ),
                                width = 12
                              )
                        ),
                        fluidRow(
                          box(
                            title = uiOutput("computation_value"),
                            # conditionalPanel("input.adornment == 'Percentile'",
                            #                uiOutput("percentile_calc")),
                            # conditionalPanel("input.adornment == 'Probability'",
                            #                uiOutput("probability_calc")),
                            collapsible = TRUE,
                            collapsed = TRUE,
                            width = 12
                          )
                        )
                      )
                    )
            ),
            tabItem(tabName = "second",
                    titlePanel("Comparing Two Distributions"),
                    sidebarLayout(
                      sidebarPanel(
                        p("Choose states below to obtain data about the percentage of income that goes towards rent. Downloaded data is the median for each census tract within the chosen states."),
                        selectizeInput("state1",
                                       "First State:",
                                       choices = state_abb,
                                       selected = state_abb[1]),
                        selectizeInput("state2",
                                       "Second State:",
                                       choices = state_abb,
                                       selected = state_abb[21]),
                        fluidRow(
                          column(6,
                                 radioButtons("adornment_comp_1st", "Find a Probability or Percentile?",
                                              choices = c("No", "Percentile", "Probability"),
                                              selected = "No"),
                                 conditionalPanel("input.adornment_comp_1st == 'Percentile'",
                                                  numericInput("percentile_comp_1st", "Percentile to find:", value = 50, min = 1, max = 99, step = 1)),
                                 conditionalPanel("input.adornment_comp_1st == 'Probability'",
                                                  radioButtons("prob_type_comp_1st", "Which type of Probability to Find?",
                                                               choices = c("Less Than", "Between", "Greater Than"),
                                                               selected = "Less Than"),
                                                  conditionalPanel("input.prob_type_comp_1st == 'Less Than'",
                                                                   numericInput("less_than_comp_1st", "Upper Value", value = start_mean)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_1st == 'Between'",
                                                                   numericInput("between1_comp_1st", "Lower Value", value = start_mean-1*start_sd),
                                                                   numericInput("between2_comp_1st", "Upper Value", value = start_mean+1*start_sd)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_1st == 'Greater Than'",
                                                                   numericInput("greater_than_comp_1st", "Lower Value", value = start_mean)
                                                  )
                                 )
                          ),
                          column(6,
                                 radioButtons("adornment_comp_2nd", "Find a Probability or Percentile?",
                                              choices = c("No", "Percentile", "Probability"),
                                              selected = "No"),
                                 conditionalPanel("input.adornment_comp_2nd == 'Percentile'",
                                                  numericInput("percentile_comp_2nd", "Percentile to find:", value = 50, min = 1, max = 99, step = 1)),
                                 conditionalPanel("input.adornment_comp_2nd == 'Probability'",
                                                  radioButtons("prob_type_comp_2nd", "Which type of Probability to Find?",
                                                               choices = c("Less Than", "Between", "Greater Than"),
                                                               selected = "Less Than"),
                                                  conditionalPanel("input.prob_type_comp_2nd == 'Less Than'",
                                                                   numericInput("less_than_comp_2nd", "Upper Value", value = start_mean)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_2nd == 'Between'",
                                                                   numericInput("between1_comp_2nd", "Lower Value", value = start_mean-1*start_sd),
                                                                   numericInput("between2_comp_2nd", "Upper Value", value = start_mean+1*start_sd)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_2nd == 'Greater Than'",
                                                                   numericInput("greater_than_comp_2nd", "Lower Value", value = start_mean)
                                                  )
                                 )
                          )
                        ),
                        "Data courtesy: U.S. Census Bureau. (2022). American Community Survey 5 Year Data (2019-2022). U.S. Department of Commerce. Retrieved January 16, 2024, from https://data.census.gov/"
                      ),
                      # Show a plot of the generated distribution
                      mainPanel(
                        fluidRow(
                          tabBox(
                            id = "second_plot1",
                            width = 6,
                            tabPanel(title = "Normal Approximation",
                                     plotOutput("norm_approx_1st")),
                            tabPanel(title = "Map View", 
                                     shinycssloaders::withSpinner(
                                       leaflet::leafletOutput("map_plot_1st"))
                                     ),
                            br(),
                            fluidRow(
                              box(
                                  title = uiOutput("computation_value_1st"),
                                  # conditionalPanel("input.adornment_comp_1st == 'Percentile'",
                                  #                  uiOutput("percentile_calc_comp_1st")
                                  # ),
                                  # conditionalPanel("input.adornment_comp_1st == 'Probability'",
                                  #                  uiOutput("probability_calc_comp_1st")
                                  # ),
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  width = 12
                                )
                            )
                          ),
                          
                          tabBox(
                            id = "second_plot2",
                            width = 6,
                            tabPanel(title = "Normal Approximation",
                                     plotOutput("norm_approx_2nd")),
                            tabPanel(title = "Map View", 
                                     shinycssloaders::withSpinner(
                                       leaflet::leafletOutput("map_plot_2nd"))
                                     ),
                            br(),
                            fluidRow(
                              box(
                                title = uiOutput("computation_value_2nd"),
                                # conditionalPanel("input.adornment_comp_2nd == 'Percentile'",
                                #                  uiOutput("percentile_calc_comp_2nd")
                                # ),
                                # conditionalPanel("input.adornment_comp_2nd == 'Probability'",
                                #                  uiOutput("probability_calc_comp_2nd")
                                # ),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                width = 12
                              )
                            )
                          )
                          )
                        )
                      )
                    )
            )
          )
        )

# Define server logic required to draw a histogram
server <- function(session, input, output) {

    #################################################################################
    #First tab part
    #validator so we don't have error messages popping up..
    iv_percentile <- InputValidator$new()
    iv_percentile$add_rule("percentile", sv_between(0, 100, inclusive = c(FALSE, FALSE)))
    iv_percentile$enable()
    
    iv_prob_less <- InputValidator$new()
    iv_prob_less$add_rule("less_than", sv_numeric())
    iv_prob_less$enable()
    
    iv_prob_between <- InputValidator$new()
    iv_prob_between$add_rule("between1", sv_numeric())
    iv_prob_between$add_rule("between2", sv_numeric())
    iv_prob_between$add_rule("between1", function(value) {
        if (is.na(input$between2) | (value > input$between2)){
          "Lower limit must be smaller than upper"
        }
      } 
    )
    iv_prob_between$add_rule("between2", function(value) {
        if (is.na(input$between1) | (value < input$between1)){
          "Upper limit must be larger than lower"
        }
      } 
    )
    iv_prob_between$enable()
    
    iv_prob_greater <- InputValidator$new()
    iv_prob_greater$add_rule("greater_than", sv_numeric())
    iv_prob_greater$enable()
    
    #data for 1st tab
    filtered_data <- reactive({
      state <- input$state
      rent_data <- all_rent[[state]]
      #drop 0 values
      rent_data[rent_data$estimate!=0, ]
    })
    

    #update names on the adornment selection 1st tab
    observeEvent(c(input$adornment, input$state), {
      state <- input$state
      #separate data
      rent_data <- filtered_data()
      
      #means and sds
      mean <- round(mean(rent_data$estimate, na.rm = TRUE), 2)
      sd <- round(sd(rent_data$estimate, na.rm = TRUE), 2)
      quants <- round(qnorm(c(0.25, 0.75), mean = mean, sd = sd), 2)

      updateNumericInput(session,
                         "percentile",
                         label = paste0("Percentile (0-100) to find for ", state, ":"))#, value = 0.5)
      updateNumericInput(session,
                         "less_than",
                         label = paste0("Upper value for ", state, ":"))#, value = mean)
      updateNumericInput(session,
                         "between1",
                         label = paste0("Lower value for ", state, ":"))#, value = quants[1])
      updateNumericInput(session,
                         "between2",
                         label = paste0("Upper value for ", state, ":"))#, value = quants[2])
      updateNumericInput(session,
                         "greater_than",
                         label = paste0("Lower value for ", state, ":"))#, value = mean)
    })

 

    #create normal plot 1st tab
    output$norm_approx <- renderPlot({
      #
      mean <- mean(filtered_data()$estimate, na.rm = TRUE)
      sd <- sd(filtered_data()$estimate, na.rm = TRUE)

      lower <- mean-4*sd
      upper <- mean+4*sd
      x <- seq(lower, upper, length = 1000)
      
      temp_hist <- hist(filtered_data()$estimate, 
                        freq = FALSE)
      largest <- max(temp_hist$density)
      hist(filtered_data()$estimate, 
           freq = FALSE,            
           xlab= "",
           ylab = "Density",
           ylim = c(0, largest+0.1*largest),
           main = paste0("Median Rent Distribution: \nMean = ", round(mean, 2), " and S.D. = ", round(sd, 2))
      )
      #lines(x,
      #     dnorm(x, mean = mean, sd = sd),
      #     type = "l"
      #)
      #axis(1, at = mean+(-4:4)*sd, labels = -4:4, line = 3)
      mtext("Median Rent Values", side =1, line = 2)
      
#      if(input$adornment == "Percentile" & iv_percentile$is_valid()){
#        if(!(input$percentile <= 0 | input$percentile >= 100)){
          #per <- qnorm(input$percentile/100, mean = mean, sd = sd)
          #segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          #xseq <- seq(lower, per, length = 1000)
          #polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
#        }
#      } else if(input$adornment == "Probability"){
#        if (input$prob_type == "Less Than" & iv_prob_less$is_valid()){
#          per <- input$less_than
      #     segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(lower, per, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   } else if (input$prob_type == "Between" & iv_prob_between$is_valid()){
      #     per1 <- input$between1
      #     per2 <- input$between2
      #     segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
      #     segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(per1, per2, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   } else if (input$prob_type == "Greater Than" & iv_prob_greater$is_valid()){
      #     per <- input$greater_than
      #     segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(per, upper, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   }
      # }
    })

    ### Map stuff
    #reactiveValues object
    filtered_data_map <- reactiveValues(map_data = 0, map_data_1st = 0, map_data_2nd = 0)
    
    observeEvent(c(input$first_plots, input$state), {
      if(input$first_plots == "Map View"){
        state <- input$state
        rent_data <- all_rent[[state]]
        filtered_data_map$map_data <- rent_data
      }
    })
    
    #create map on 1st tab
    output$map_plot <- leaflet::renderLeaflet({
      my_map <- filtered_data_map$map_data |> 
        mapview::mapview(zcol = "estimate", layer.name = "Rent")
      my_map@map
    })

        
    output$computation_value <- renderUI({
      mean <- mean(filtered_data()$estimate, na.rm = TRUE)
      sd <- sd(filtered_data()$estimate, na.rm = TRUE)
      
      if(input$adornment == "Percentile"){
        req(iv_percentile$is_valid())
        #percentile
        paste0(
          scales::label_ordinal()(as.integer(input$percentile)),
          " percentile from the data is ",
          round(quantile(filtered_data()$estimate, input$percentile/100, na.rm = TRUE), 2)
        )
      } else if (input$adornment == "Probability"){
        #prob
        if (input$prob_type == "Less Than"){
          req(iv_prob_less$is_valid())
        #less than
          paste0("Probability using the data: ", round(mean(filtered_data()$estimate < input$less_than, na.rm = TRUE), 4))
        } else if (input$prob_type == "Between"){
          req(iv_prob_between$is_valid())
        #between
          paste0("Probability using the data: ", round(mean((filtered_data()$estimate > input$between1) & (filtered_data()$estimate < input$between2), na.rm = TRUE), 4))
        } else if(input$prob_type == "Greater Than"){
          req(iv_prob_greater$is_valid())
          #greater than
          paste0("Probability using the data: ", round(mean(filtered_data()$estimate > input$greater_than, na.rm = TRUE), 4))
        }
      } else {
        "No computation selected."
      }
      
    })
    
    
    #Calculate percentile 1st tab
    output$percentile_calc <- renderUI({

      if(!(input$percentile <= 0 | input$percentile >= 100 | !is.numeric(input$percentile))){
        mean <- mean(filtered_data()$estimate, na.rm = TRUE)
        sd <- sd(filtered_data()$estimate, na.rm = TRUE)
  
        tags$div(
            "Percentile using the Normal model:",
            tags$br(),
            tags$ul(
              tags$li(paste0(
                scales::label_ordinal()(as.integer(input$percentile)),
                " percentile from the standard normal is ",
                round(qnorm(input$percentile/100), 2)
              )),
              tags$li(
                "Converted to the distribution of rent we use:",
                tags$br(),
                tags$code("rent value = mean + standard deviation * quantile"),
                tags$br(),
                tags$code(
                  paste0(
                    round(qnorm(input$percentile/100)*sd(filtered_data()$estimate, na.rm = TRUE) + mean(filtered_data()$estimate, na.rm = TRUE), 2),
                    " = ",
                    round(mean, 2), " + ", round(sd, 2), " * ", round(qnorm(input$percentile/100), 2)
                  )
                )
              )
            ),
            "Percentile using the data:",
            tags$br(),
            tags$ul(
              tags$li(paste0(
                scales::label_ordinal()(as.integer(input$percentile)),
                " percentile from the data is ",
                round(quantile(filtered_data()$estimate, input$percentile/100, na.rm = TRUE), 2)
              ))
            )
        )
      } else {
          "Please select a percentile between 0 and 100 (non-inclusive)."
      }
    })

    #calculate probability 1st tab
    output$probability_calc <- renderUI({
      mean <- mean(filtered_data()$estimate, na.rm = TRUE)
      sd <- sd(filtered_data()$estimate, na.rm = TRUE)
      
      if(input$prob_type == "Less Than"){
        req(iv_prob_less$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$less_than,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$less_than - mean)/sd, 2),
                  " = (",
                  input$less_than, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are less than this value is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y < ", input$less_than, ") = P(Z < ", round((input$less_than - mean)/sd, 2), ") = ", round(pnorm(round((input$less_than - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean(filtered_data()$estimate < input$less_than, na.rm = TRUE), 4))
            )
          )
      } else if(input$prob_type == "Between"){
        req(iv_prob_between$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The values of rent, ",
              input$between1, " and ", input$between2,
              ", can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between1 - mean)/sd, 2),
                  " = (",
                  input$between1, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              ),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between2 - mean)/sd, 2),
                  " = (",
                  input$between2, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are between these two values is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(", input$between1, "< Y < ", input$between2, ") = P(Y < ", input$between2, ") - P(Y < ", input$between1, ") = P(Z < ", round((input$between2 - mean)/sd, 2), ") - P(Z < ", round((input$between1 - mean)/sd, 2), ")= ", round(pnorm(round((input$between2 - mean)/sd, 2))- pnorm(round((input$between1 - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean((filtered_data()$estimate > input$between1) & (filtered_data()$estimate < input$between2), na.rm = TRUE), 4))
          )
        )
      } else if (input$prob_type == "Greater Than"){
        req(iv_prob_greater$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$greater_than,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$greater_than - mean)/sd, 2),
                  " = (",
                  input$greater_than, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are greater than this value is then found using the complement rule and the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y > ", input$greater_than, ") = 1-P(Y < ", input$greater_than, ") = 1 - P(Z < ", round((input$greater_than - mean)/sd, 2), ") = ", round(1- pnorm(round((input$greater_than - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean(filtered_data()$estimate > input$greater_than, na.rm = TRUE), 4))
          )
        )
      }
    })

    #################################################################################
    #Second tab part
    
    #validator for inputs
    iv_percentile_2nd_tab_1st <- InputValidator$new()
    iv_percentile_2nd_tab_1st$add_rule("percentile_comp_1st", sv_between(0, 100, inclusive = c(FALSE, FALSE)))
    iv_percentile_2nd_tab_1st$enable()
    
    iv_percentile_2nd_tab_2nd <- InputValidator$new()
    iv_percentile_2nd_tab_2nd$add_rule("percentile_comp_2nd", sv_between(0, 100, inclusive = c(FALSE, FALSE)))
    iv_percentile_2nd_tab_2nd$enable()
    
    iv_prob_less_2nd_tab_1st <- InputValidator$new()
    iv_prob_less_2nd_tab_1st$add_rule("less_than_comp_1st", sv_numeric())
    iv_prob_less_2nd_tab_1st$enable()
    
    iv_prob_less_2nd_tab_2nd <- InputValidator$new()
    iv_prob_less_2nd_tab_2nd$add_rule("less_than_comp_2nd", sv_numeric())
    iv_prob_less_2nd_tab_2nd$enable()
    
    iv_prob_between_2nd_tab_1st <- InputValidator$new()
    iv_prob_between_2nd_tab_1st$add_rule("between1_comp_1st", sv_numeric())
    iv_prob_between_2nd_tab_1st$add_rule("between2_comp_1st", sv_numeric())
    iv_prob_between_2nd_tab_1st$add_rule("between1_comp_1st", function(value) {
      if (is.na(input$between2_comp_1st) | (value > input$between2_comp_1st)){
        "Lower limit must be smaller than upper"
      }
    } 
    )
    iv_prob_between_2nd_tab_1st$add_rule("between2_comp_1st", function(value) {
      if (is.na(input$between1_comp_1st) | (value < input$between1_comp_1st)){
        "Upper limit must be larger than lower"
      }
    } 
    )
    iv_prob_between_2nd_tab_1st$enable()
    
    
    iv_prob_between_2nd_tab_2nd <- InputValidator$new()
    iv_prob_between_2nd_tab_2nd$add_rule("between1_comp_2nd", sv_numeric())
    iv_prob_between_2nd_tab_2nd$add_rule("between2_comp_2nd", sv_numeric())
    iv_prob_between_2nd_tab_2nd$add_rule("between1_comp_2nd", function(value) {
      if (is.na(input$between2_comp_2nd) | (value > input$between2_comp_2nd)){
        "Lower limit must be smaller than upper"
      }
    } 
    )
    iv_prob_between_2nd_tab_2nd$add_rule("between2_comp_2nd", function(value) {
      if (is.na(input$between1_comp_2nd) | (value < input$between1_comp_2nd)){
        "Upper limit must be larger than lower"
      }
    } 
    )
    iv_prob_between_2nd_tab_2nd$enable()
    
    iv_prob_greater_2nd_tab_1st <- InputValidator$new()
    iv_prob_greater_2nd_tab_1st$add_rule("greater_than_comp_1st", sv_numeric())
    iv_prob_greater_2nd_tab_1st$enable()
    
    iv_prob_greater_2nd_tab_2nd <- InputValidator$new()
    iv_prob_greater_2nd_tab_2nd$add_rule("greater_than_comp_2nd", sv_numeric())
    iv_prob_greater_2nd_tab_2nd$enable()
    
    
    #data for 2nd tab
    filtered_data_2nd <- reactive({
      state1 <- input$state1
      state2 <- input$state2

      #grab data
      rent_data1 <- all_rent[[state1]]
      #drop 0 values
      rent_data1 <- rent_data1[rent_data1$estimate!=0, ]
      #grab state 2 data
      rent_data2 <- all_rent[[state2]]
      #drop 0 values
      rent_data2 <- rent_data2[rent_data2$estimate!=0, ]
      
      #combine
      rent_data1$state <- state1
      rent_data2$state <- state2
      rbind(rent_data1, rent_data2)
    })  
    
    #update the choices for state on the 2nd tab
    observeEvent(c(input$state1, input$state2), {
      state1 <- input$state1
      state2 <- input$state2
      choices <- state_abb
      if (state1 == state2){
        choices <- choices[-which(choices == state1)]
        updateSelectizeInput(session,
                             "state2",
                             choices = choices)
      }
    })
    
    
    #update names on the adornment selectiosn 2nd tab
    observeEvent(c(input$state1, input$state2),{
      updateRadioButtons(session,
                         "adornment_comp_1st",
                         paste0("Find a Probability or Percentile for ", input$state1, "?"))
      updateRadioButtons(session,
                         "adornment_comp_2nd",
                         paste0("Find a Probability or Percentile for ", input$state2, "?"))
    })
    
    #update input names for 2nd tab
    observeEvent(c(input$adornment_comp_1st, input$adornment_comp_2nd, input$state1, input$state2), {
      state1 <- input$state1
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean1 <- round(mean(state1_data$estimate, na.rm = TRUE), 2)
      sd1 <- round(sd(state1_data$estimate, na.rm = TRUE), 2)
      mean2 <- round(mean(state2_data$estimate, na.rm = TRUE), 2)
      sd2 <- round(sd(state2_data$estimate, na.rm = TRUE), 2)
      quants1 <- round(qnorm(c(0.25, 0.75), mean = mean1, sd = sd1), 2)
      quants2 <- round(qnorm(c(0.25, 0.75), mean = mean2, sd = sd2), 2)
      
      updateNumericInput(session,
                         "percentile_comp_1st",
                         label = paste0("Percentile (0-100 to find for ", state1, ":"))#, value = 0.5)
      updateNumericInput(session,
                         "percentile_comp_2nd",
                         label = paste0("Percentile (0-100) to find for ", state2, ":"))#, value = 0.5)
      updateNumericInput(session,
                         "less_than_comp_1st",
                         label = paste0("Upper value for ", state1, ":"))#, value = mean1)
      updateNumericInput(session,
                         "less_than_comp_2nd",
                         label = paste0("Upper value for ", state2, ":"))#, value = mean2)
      updateNumericInput(session,
                         "between1_comp_1st",
                         label = paste0("Lower value for ", state1, ":"))#, value = quants1[1])
      updateNumericInput(session,
                         "between2_comp_1st",
                         label = paste0("Upper value for ", state1, ":"))#, value = quants1[2])
      updateNumericInput(session,
                         "between1_comp_2nd",
                         label = paste0("Lower value for ", state2, ":"))#, value = quants2[1])
      updateNumericInput(session,
                         "between2_comp_2nd",
                         label = paste0("Upper value for ", state2, ":"))#, value = quants2[2])
      updateNumericInput(session,
                         "greater_than_comp_1st",
                         label = paste0("Lower value for ", state1, ":"))#, value = mean1)
      updateNumericInput(session,
                         "greater_than_comp_2nd",
                         label = paste0("Lower value for ", state2, ":"))#, value = mean2)
    })
    

    #Create plot for 1st state, 2nd tab
    output$norm_approx_1st <- renderPlot({
      state1 <- input$state1
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean1 <- mean(state1_data$estimate, na.rm = TRUE)
      sd1 <- sd(state1_data$estimate, na.rm = TRUE)
      mean2 <- mean(state2_data$estimate, na.rm = TRUE)
      sd2 <- sd(state2_data$estimate, na.rm = TRUE)
      
      #plotting sequence
      lower <- min(mean1-4*sd1, mean2-4*sd2)
      upper <- max(mean1+4*sd1, mean2+4*sd2)
      x <- seq(lower, upper, length = 5000)
      
      #temp histogram to update max ylim
      temp_hist <- hist(state1_data$estimate, 
                        freq = FALSE)
      largest <- max(temp_hist$density)
      hist(state1_data$estimate, 
           freq = FALSE,            
           xlab= "",
           ylab = "Density",
           ylim = c(0, largest+0.1*largest),
           main = paste0("Rent Distribution for ", input$state1, "\nMean = ", round(mean1, 2), " and S.D. = ", round(sd1, 2))
      )
      # lines(x,
      #       dnorm(x, mean = mean1, sd = sd1),
      #       type = "l"
      # )
      # axis(1, at = mean1+(-4:4)*sd1, labels = -4:4, line = 3)
       mtext("Median Rent Values", side =1, line = 2)
      # 
      # mean <- mean1
      # sd <- sd1
      # if(input$adornment_comp_1st == "Percentile" & iv_percentile_2nd_tab_1st$is_valid()){
      #   if(!(input$percentile <= 0 | input$percentile >= 100)){
      #   per <- qnorm(input$percentile_comp_1st/100, mean = mean, sd = sd)
      #   segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #   xseq <- seq(lower, per, length = 1000)
      #   polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   }
      # } else if(input$adornment_comp_1st == "Probability"){
      #   if ((input$prob_type_comp_1st == "Less Than") & (iv_prob_less_2nd_tab_1st$is_valid())){
      #     per <- input$less_than_comp_1st
      #     segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(lower, per, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   } else if ((input$prob_type_comp_1st == "Between") & (iv_prob_between_2nd_tab_1st$is_valid())){
      #     per1 <- input$between1_comp_1st
      #     per2 <- input$between2_comp_1st
      #     segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
      #     segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(per1, per2, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   } else if ((input$prob_type_comp_1st == "Greater Than") & (iv_prob_greater_2nd_tab_1st$is_valid())){
      #     per <- input$greater_than_comp_1st
      #     segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(per, upper, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   }
      # }
    })
    
    #create plot for 2nd school, 2nd tab
    output$norm_approx_2nd <- renderPlot({
      state1 <- input$state1
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean1 <- mean(state1_data$estimate, na.rm = TRUE)
      sd1 <- sd(state1_data$estimate, na.rm = TRUE)
      mean2 <- mean(state2_data$estimate, na.rm = TRUE)
      sd2 <- sd(state2_data$estimate, na.rm = TRUE)
      
      #plotting sequence
      lower <- min(mean1-4*sd1, mean2-4*sd2)
      upper <- max(mean1+4*sd1, mean2+4*sd2)
      x <- seq(lower, upper, length = 5000)
      
      #temp histogram to update max ylim
      temp_hist <- hist(state2_data$estimate, 
                        freq = FALSE)
      largest <- max(temp_hist$density)
      hist(state2_data$estimate, 
           freq = FALSE,            
           xlab= "",
           ylab = "Density",
           ylim = c(0, largest+0.1*largest),
           main = paste0("Rent Distribution for ", input$state2, "\nMean = ", round(mean2, 2), " and S.D. = ", round(sd2, 2))
      )
      # lines(x,
      #       dnorm(x, mean = mean2, sd = sd2),
      #       type = "l"
      # )
      # axis(1, at = mean2+(-4:4)*sd2, labels = -4:4, line = 3)
      mtext("Median Rent Values", side =1, line = 2)
      # 
      # mean <- mean2
      # sd <- sd2
      # if(input$adornment_comp_2nd == "Percentile" & iv_percentile_2nd_tab_2nd$is_valid()){
      #   if(!(input$percentile <= 0 | input$percentile >= 100)){
      #   per <- qnorm(input$percentile_comp_2nd/100, mean = mean, sd = sd)
      #   segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #   xseq <- seq(lower, per, length = 1000)
      #   polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   }
      # } else if(input$adornment_comp_2nd == "Probability"){
      #   if (input$prob_type_comp_2nd == "Less Than" & iv_prob_less_2nd_tab_2nd$is_valid()){
      #     per <- input$less_than_comp_2nd
      #     segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(lower, per, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   } else if (input$prob_type_comp_2nd == "Between" & iv_prob_between_2nd_tab_2nd$is_valid()){
      #     per1 <- input$between1_comp_2nd
      #     per2 <- input$between2_comp_2nd
      #     segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
      #     segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(per1, per2, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   } else if (input$prob_type_comp_2nd == "Greater Than" & iv_prob_greater_2nd_tab_2nd$is_valid()){
      #     per <- input$greater_than_comp_2nd
      #     segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
      #     xseq <- seq(per, upper, length = 1000)
      #     polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      #   }
      # }
    })
    
    
    ## map plots
    observeEvent(c(input$second_plot1, input$state1), {
      if(input$second_plot1 == "Map View"){
        state <- input$state1
        rent_data <- all_rent[[state]]
        filtered_data_map$map_data_1st <- rent_data
      } 
    })
    
    observeEvent(c(input$second_plot2, input$state2), {
      if(input$second_plot2 == "Map View"){
        state <- input$state2
        rent_data <- all_rent[[state]]
        filtered_data_map$map_data_2nd <- rent_data
      } 
    })
    
    #create maps on 2nd tab
    output$map_plot_1st <- leaflet::renderLeaflet({
      my_map <- filtered_data_map$map_data_1st |> 
        mapview::mapview(zcol = "estimate", layer.name = "Rent")
      my_map@map
    })
    
    output$map_plot_2nd <- leaflet::renderLeaflet({
      my_map <- filtered_data_map$map_data_2nd |> 
        mapview::mapview(zcol = "estimate", layer.name = "Rent")
      my_map@map
    })
    
    
    #calculate percentile for 1st plot, 2nd tab
    output$percentile_calc_comp_1st <- renderUI({
      if(!(input$percentile_comp_1st <= 0 | input$percentile_comp_1st >= 100 | !is.numeric(input$percentile_comp_1st))){
        state1 <- input$state1
        #separate data
        state1_data <- filtered_data_2nd() %>%
          filter(state == state1)
        
        #means and sds
        mean <- mean(state1_data$estimate, na.rm = TRUE)
        sd <- sd(state1_data$estimate, na.rm = TRUE)
        
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              scales::label_ordinal()(as.integer(input$percentile_comp_1st)),
              " percentile from the standard normal is ",
              round(qnorm(input$percentile_comp_1st/100), 2)
            )),
            tags$li(
              "Converted to the distribution of rent we use:",
              tags$br(),
              tags$code("rent value = mean + standard deviation * quantile"),
              tags$br(),
              tags$code(
                paste0(
                  round(qnorm(input$percentile_comp_1st/100)*sd + mean, 2),
                  " = ",
                  round(mean, 2), " + ", round(sd, 2), " * ", round(qnorm(input$percentile_comp_1st/100), 2)
                )
              )
            )
          ),
          "Percentile using the data:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              scales::label_ordinal()(as.integer(input$percentile_comp_1st)),
              " percentile from the data is ",
              round(quantile(state1_data$estimate, input$percentile_comp_1st/100, na.rm = TRUE), 2)
            ))
          )
        )
      } else {
        "Please select a percentile between 0 and 100 (non-inclusive)."
      }

    })
    
    #calculate percentile for 2nd plot, 2nd tab
    output$percentile_calc_comp_2nd <- renderUI({
      if(!(input$percentile_comp_1st <= 0 | input$percentile_comp_1st >= 100 | !is.numeric(input$percentile_comp_1st))){
        state2 <- input$state2
        #separate data
        state1_data <- filtered_data_2nd() %>%
          filter(state == state2)
        
        #means and sds
        mean <- mean(state1_data$estimate, na.rm = TRUE)
        sd <- sd(state1_data$estimate, na.rm = TRUE)
        
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              scales::label_ordinal()(as.integer(input$percentile_comp_2nd)),
              " percentile from the standard normal is ",
              round(qnorm(input$percentile_comp_2nd/100), 2)
            )),
            tags$li(
              "Converted to the distribution of rent we use:",
              tags$br(),
              tags$code("rent value = mean + standard deviation * quantile"),
              tags$br(),
              tags$code(
                paste0(
                  round(qnorm(input$percentile_comp_2nd/100)*sd + mean, 2),
                  " = ",
                  round(mean, 2), " + ", round(sd, 2), " * ", round(qnorm(input$percentile_comp_2nd/100), 2)
                )
              )
            )
          ),
          "Percentile using the data:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              scales::label_ordinal()(as.integer(input$percentile_comp_2nd)),
              " percentile from the data is ",
              round(quantile(state1_data$estimate, input$percentile_comp_2nd/100, na.rm = TRUE), 2)
            ))
          )
        )
      } else {
        "Please select a percentile between 0 and 100 (non-inclusive)."
      }
    })
    
    #calculate probability for 1st plot, 2nd tab
    output$probability_calc_comp_1st <- renderUI({
      state1 <- input$state1
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      
      #means and sds
      mean <- mean(state1_data$estimate, na.rm = TRUE)
      sd <- sd(state1_data$estimate, na.rm = TRUE)
      
      if(input$prob_type_comp_1st == "Less Than"){
        req(iv_prob_less_2nd_tab_1st$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$less_than_comp_1st,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$less_than_comp_1st - mean)/sd, 2),
                  " = (",
                  input$less_than_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are less than this value is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y < ", input$less_than_comp_1st, ") = P(Z < ", round((input$less_than_comp_1st - mean)/sd, 2), ") = ", round(pnorm(round((input$less_than_comp_1st - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean((state1_data$estimate < input$less_than_comp_1st), na.rm = TRUE), 4))
          )
        )
      } else if(input$prob_type_comp_1st == "Between"){
        req(iv_prob_between_2nd_tab_1st$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The values of rent, ",
              input$between1_comp_1st, " and ", input$between2_comp_1st,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between1_comp_1st - mean)/sd, 2),
                  " = (",
                  input$between1_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              ),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between2_comp_1st - mean)/sd, 2),
                  " = (",
                  input$between2_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are between these two values is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(", input$between1_comp_1st, "< Y < ", input$between2_comp_1st, ") = P(Y < ", input$between2_comp_1st, ") - P(Y < ", input$between1_comp_1st, ") = P(Z < ", round((input$between2_comp_1st - mean)/sd, 2), ") - P(Z < ", round((input$between1_comp_1st - mean)/sd, 2), ")= ", round(pnorm(round((input$between2_comp_1st - mean)/sd, 2))- pnorm(round((input$between1_comp_1st - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean((state1_data$estimate > input$between1_comp_1st) & (state1_data$estimate < input$between2_comp_1st), na.rm = TRUE), 4))
          )
        )
      } else if (input$prob_type_comp_1st == "Greater Than"){
        req(iv_prob_greater_2nd_tab_1st$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$greater_than_comp_1st,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$greater_than_comp_1st - mean)/sd, 2),
                  " = (",
                  input$greater_than_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are greater than this value is then found using the complement rule and the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y > ", input$greater_than_comp_1st, ") = 1-P(Y < ", input$greater_than_comp_1st, ") = 1 - P(Z < ", round((input$greater_than_comp_1st - mean)/sd, 2), ") = ", round(1- pnorm(round((input$greater_than_comp_1st - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean((state1_data$estimate > input$greater_than_comp_1st), na.rm = TRUE), 4))
          )
        )
      }
    })
    
    #calculate probability for 2nd plot, 2nd tab
    output$probability_calc_comp_2nd <- renderUI({
      state2 <- input$state2
      #separate data
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean <- mean(state2_data$estimate, na.rm = TRUE)
      sd <- sd(state2_data$estimate, na.rm = TRUE)
      
      if(input$prob_type_comp_2nd == "Less Than"){
        req(iv_prob_less_2nd_tab_2nd$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$less_than_comp_2nd,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$less_than_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$less_than_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are less than this value is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y < ", input$less_than_comp_2nd, ") = P(Z < ", round((input$less_than_comp_2nd - mean)/sd, 2), ") = ", round(pnorm(round((input$less_than_comp_2nd - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean((state2_data$estimate < input$less_than_comp_2nd), na.rm = TRUE), 4))
          )
        )
      } else if(input$prob_type_comp_2nd == "Between"){
        req(iv_prob_between_2nd_tab_2nd$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The values of rent, ",
              input$between1_comp_2nd, " and ", input$between2_comp_2nd,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between1_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$between1_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              ),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between2_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$between2_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are between these two values is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(", input$between1_comp_2nd, "< Y < ", input$between2_comp_2nd, ") = P(Y < ", input$between2_comp_2nd, ") - P(Y < ", input$between1_comp_2nd, ") = P(Z < ", round((input$between2_comp_2nd - mean)/sd, 2), ") - P(Z < ", round((input$between1_comp_2nd - mean)/sd, 2), ")= ", round(pnorm(round((input$between2_comp_2nd - mean)/sd, 2))- pnorm(round((input$between1_comp_2nd - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean((state2_data$estimate > input$between1_comp_2nd) & (state2_data$estimate < input$between2_comp_2nd), na.rm = TRUE), 4))
          )
        )
      } else if (input$prob_type_comp_2nd == "Greater Than"){
        req(iv_prob_greater_2nd_tab_2nd$is_valid())
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$greater_than_comp_2nd,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("z = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$greater_than_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$greater_than_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are greater than this value is then found using the complement rule and the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y > ", input$greater_than_comp_2nd, ") = 1-P(Y < ", input$greater_than_comp_2nd, ") = 1 - P(Z < ", round((input$greater_than_comp_2nd - mean)/sd, 2), ") = ", round(1- pnorm(round((input$greater_than_comp_2nd - mean)/sd, 2)), 4)))
            )
          ),
          "Probability using the data:",
          tags$br(),
          tags$ul(
            tags$li(round(mean((state2_data$estimate > input$greater_than_comp_2nd), na.rm = TRUE), 4))
          )
        )
      }
    })
    
    output$computation_value_1st <- renderUI({
      state1 <- input$state1
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      
      mean <- mean(state1_data$estimate, na.rm = TRUE)
      sd <- sd(state1_data$estimate, na.rm = TRUE)
      
      if(input$adornment_comp_1st == "Percentile"){
        req(iv_percentile_2nd_tab_1st$is_valid())
        #percentile
        paste0(
          scales::label_ordinal()(as.integer(input$percentile_comp_1st)),
          " percentile from the data is ",
          round(quantile(state1_data$estimate, input$percentile_comp_1st/100, na.rm = TRUE), 2)
        )
      } else if (input$adornment_comp_1st == "Probability"){
        #prob
        if (input$prob_type_comp_1st == "Less Than"){
          req(iv_prob_less_2nd_tab_1st$is_valid())
          #less than
          paste0("Probability = ", round(mean((state1_data$estimate < input$less_than_comp_1st), na.rm = TRUE), 4))
        } else if (input$prob_type_comp_1st == "Between"){
          req(iv_prob_between_2nd_tab_1st$is_valid())
          #between
          paste0("Probability = ", round(mean((state1_data$estimate > input$between1_comp_1st) & (state1_data$estimate < input$between2_comp_1st), na.rm = TRUE), 4))
        } else if(input$prob_type_comp_1st == "Greater Than"){
          req(iv_prob_greater_2nd_tab_1st$is_valid())
          #greater than
          paste0("Probability = ", round(mean((state1_data$estimate > input$greater_than_comp_1st), na.rm = TRUE), 4))
        }
      } else {
        "No computation selected."
      }
      
    })
    
    output$computation_value_2nd <- renderUI({
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state2)
      state2_data <- state1_data
#      mean <- mean(state2_data$estimate, na.rm = TRUE)
#      sd <- sd(state2_data$estimate, na.rm = TRUE)
      
      if(input$adornment_comp_2nd == "Percentile"){
        req(iv_percentile_2nd_tab_2nd$is_valid())
        #percentile
        paste0( scales::label_ordinal()(as.integer(input$percentile_comp_2nd)),
                " percentile from the data: ",
               round(quantile(state1_data$estimate, input$percentile_comp_2nd/100, na.rm = TRUE), 2)
        )
      } else if (input$adornment_comp_2nd == "Probability"){
        #prob
        if (input$prob_type_comp_2nd == "Less Than"){
          req(iv_prob_less_2nd_tab_2nd$is_valid())
          #less than
          paste0("Probability = ", round(mean((state2_data$estimate < input$less_than_comp_2nd), na.rm = TRUE), 4))
        } else if (input$prob_type_comp_2nd == "Between"){
          req(iv_prob_between_2nd_tab_2nd$is_valid())
          #between
          paste0("Probability = ", round(mean((state2_data$estimate > input$between1_comp_2nd) & (state2_data$estimate < input$between2_comp_2nd), na.rm = TRUE), 4))
        } else if(input$prob_type_comp_2nd == "Greater Than"){
          req(iv_prob_greater_2nd_tab_2nd$is_valid())
          #greater than
          paste0("Probability = ", round(mean((state2_data$estimate > input$greater_than_comp_2nd), na.rm = TRUE), 4))
        }
      } else {
        "No computation selected."
      }
      
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
