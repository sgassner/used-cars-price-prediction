#------------------------------------------------------------------------------#
# Big Data Analytics: Group Project
# Team: Abnormal Distribution
# Created On: 26 March 2022
# Data: 
# - used_cars_lasso_model.rds: 
#   Lasso-Model created with used_cars_code.R
# - shiny_prediction_input.csv: 
#   empty data frame to store car parameter inputs for prediction in shiny
#------------------------------------------------------------------------------#

# Import libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(bigmemory)
library(biganalytics)
library(biglasso)

# set working directory
# setwd("~/Documents/R/BigData/project/shiny")

# read in the lasso model
model <- readRDS("used_cars_lasso_model.rds")

# get lasso coefficient names
lasso_coef <- model[["fit"]][["beta"]]@Dimnames[[1]]
lasso_coef <- as.data.frame(lasso_coef)

# prepare list with car brands
make_names_lasso <- lasso_coef[237:335,]
make_names_lasso_sub <- gsub("make_name_", "", make_names_lasso)
make_names_list <- as.list(make_names_lasso)
names(make_names_list) <- make_names_lasso_sub

# prepare list with model names
model_names_lasso <- lasso_coef[396:1821,]
model_names_lasso <- sort(model_names_lasso)
model_names_lasso_sub <- gsub("model_name_", "", model_names_lasso)
model_names_list <- as.list(model_names_lasso)
names(model_names_list) <- model_names_lasso_sub

#------------------------------------------------------------------------------#
# User Interface (UI)
#------------------------------------------------------------------------------#

ui <- shinyUI(fluidPage(
  
  titlePanel("Car App by Abnormal Distribution"),
  theme = shinytheme("superhero"),
  
  fluidRow(
    
    # left column
    column(4,
           tags$label(h3('Car Parameters (1/2)')),
           pickerInput(inputId = "make_name",
                       label = "Car Brand:",
                       choices = make_names_list,
                       selected = "Ford",
                       options = list(`actions-box` = TRUE,
                                      multiple = FALSE,
                                      `live-search`=TRUE,
                                      style = rep(("color: black; background: lightgrey; font-weight: bold;"),10))),
           pickerInput(inputId = "model_name",
                       label = "Model Name:",
                       choices = model_names_list,
                       selected = "Focus",
                       options = list(`actions-box` = TRUE,
                                      multiple = FALSE,
                                      `live-search`=TRUE,
                                      style = rep(("color: black; background: lightgrey; font-weight: bold;"),10))),
           numericInput("year", 
                        label = "Year:", 
                        value = 2021),
           numericInput("mileage_km", 
                        label = "Mileage (km):", 
                        value = 10000),
           numericInput("horsepower", 
                        label = "Horsepower (HP):", 
                        value = 100),
           numericInput("owner_count", 
                        label = "Owner Count:", 
                        value = 1),
           numericInput("maximum_seating", 
                        label = "Maximum Seating:", 
                        value = 5)),
    # middle column
    column(4,
           tags$label(h3('Car Parameters (2/2)')),
           selectInput("body_type", 
                       label = "Body Type:", 
                       choices = list("Coupe" = "body_type_Coupe", 
                                      "Hatchback" = "body_type_Hatchback", 
                                      "Minivan" = "body_type_Minivan",
                                      "Pickup_Truck" = "body_type_Pickup_Truck",
                                      "Sedan" = "body_type_Sedan",
                                      "SUV / Crossover" = "body_type_SUV___Crossover",
                                      "Van" = "body_type_Van",
                                      "Wagon" = "body_type_Wagon"), 
                       selected = "Coupe"),
           selectInput("fuel_type", 
                       label = "Fuel Type:", 
                       choices = list("Gasoline" = "fuel_type_Gasoline",
                                      "Natural Gas" = "fuel_type_Compressed_Natural_Gas", 
                                      "Diesel" = "fuel_type_Diesel", 
                                      "Electric" = "fuel_type_Electric",
                                      "Flex Fuel" = "fuel_type_Flex_Fuel_Vehicle",
                                      "Hybrid" = "fuel_type_Hybrid",
                                      "Propane" = "fuel_type_Propane"), 
                       selected = "Gasoline"),
           selectInput("listing_color", 
                       label = "Listing Color:", 
                       choices = list("black" = "listing_color_BLACK",
                                      "blue" = "listing_color_BLUE", 
                                      "brown" = "listing_color_BROWN", 
                                      "gold" = "listing_color_GOLD",
                                      "gray" = "listing_color_GRAY",
                                      "green" = "listing_color_GREEN",
                                      "orange" = "listing_color_ORANGE",
                                      "pink" = "listing_color_PINK", 
                                      "purple" = "listing_color_PURPLE", 
                                      "red" = "listing_color_RED",
                                      "silver" = "listing_color_SILVER",
                                      "teal" = "listing_color_TEAL",
                                      "white" = "listing_color_WHITE",
                                      "yellow" = "listing_color_YELLOW"), 
                       selected = "black"),
           checkboxGroupInput("options", 
                              label = "Options:",
                              # for simplicity, only the top 12 options are implemented
                              # the remaining 133 options are assumed to be 0
                              # these could also be implemented accordingly
                              choices = list("Backup Camera" = "backup_camera", 
                                             "Bluetooth" = "bluetooth", 
                                             "Wheels" = "alloy_wheels",
                                             "Heated Seats" = "heated_seats",
                                             "Navigation System" = "navigation_system",
                                             "Sunroof" = "sunroof_moonroof",
                                             "Remote Start" = "remote_start",
                                             "Android Auto" = "android_auto",
                                             "Leather Seats" = "leather_seats",
                                             "Blind Spot Monitoring" = "blind_spot_monitoring",
                                             "Parking Sensors" = "parking_sensors",
                                             "Adaptive Cruise Control" = "adaptive_cruise_control"), 
                              selected = NULL, 
                              inline = TRUE),
           
           actionButton("submitbutton", "Submit", 
                        class = "btn btn-primary")),
    
    # right column
    column(4,
           tags$label(h3('Price Prediction')),
           verbatimTextOutput('contents'),
           tableOutput('tabledata')
    )
  )
))

#------------------------------------------------------------------------------#
# Server
#------------------------------------------------------------------------------#

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # load data frame with all variables equal to 0 to store inputs
    df <- read.csv("shiny_prediction_input.csv")
    
    # store numeric inputs
    df$age <- 2021 - input$year
    df$age_squared <- (2021 - input$year)^2
    df$mileage_km <- input$mileage_km
    df$horsepower <- input$horsepower
    df$owner_count <- input$owner_count
    df$maximum_seating <- input$maximum_seating
    
    # store dummy inputs
    df[,input$make_name] <- 1
    df[,input$model_name] <- 1
    df[,input$fuel_type] <- 1
    df[,input$listing_color] <- 1
    
    # store all car options
    for (i in as.vector(input$options)){
      df[,i] <- 1
    }
    
    # for simplicity we set default values for the following variables:
    df$seller_rating <- 4.5
    df$engine_type_NA <- 1
    df$wheel_system_NA <- 1
    df$transmission_NA <- 1
    df$state_id_NA <- 1
    df$back_legroom_cm <- 96       # mean of all cars
    df$width_cm <- 199             # mean of all cars
    df$city_fuel_economy <- 4.89   # mean of all cars
    df$engine_displacement <- 2962 # mean of all cars
    
    # # for simplicity the following variables are assumed to be zero:
    # fleet
    # isCab
    # frame_damaged
    # franchise_dealer
    
    # # the following lasso coefficients are zero anyway:
    # front_legroom_cm
    # length_cm
    
    # convert input table into big.matrix for prediction
    df <- as.matrix(df)
    df.bm <- as.big.matrix(df)
    
    # store price prediction as output (exp because of log-transformation)
    Output <- exp(as.numeric(predict(model, df.bm, lambda = "lambda.min")))
    
    # print price prediction
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction complete.") 
    } else {
      return("Server is ready for prediction.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderPrint({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

#------------------------------------------------------------------------------#
# Create Shiny App
#------------------------------------------------------------------------------#

shinyApp(ui = ui, server = server)

#------------------------------------------------------------------------------#
# End of Script
#------------------------------------------------------------------------------#
