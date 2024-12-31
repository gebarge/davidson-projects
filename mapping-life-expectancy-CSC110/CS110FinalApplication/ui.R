# Genna Barge, Anna Catherine Wilson, Sarah Grace Clifton
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinythemes)
library(tidyverse)
library(shiny)
library(tidycensus)
library(tmap)
library(sf)

# Define UI for application
ui <- navbarPage("Final Project: Life Expectancy in NC", collapsible = TRUE, inverse = TRUE, theme =shinytheme("darkly"),
                 tabPanel("Home",
                          fluidPage(
                              fluidRow(
                                  p("NAMES", style = "font-size:10px"),
                                  titlePanel("Summary"),
                                  p("FILLER TEXT FOR NOW"),
                                  
                                  h1("Maps", style = "font-size:20px"),
                                  p("FILLER TEXT FOR NOW - will show the census tract maps"),
                                  h2("Scatterplot", style = "font-size:20px"),
                                  p("FILLER TEXT FOR NOW - will show a scatterplot"),
                                  p("FILLER TEXT - letting user choose variable")
                              )
                              
                          )
                 ),
                 
                 tabPanel("Maps",
                          fluidPage("Life Expectancy",
                                    fluidRow(
                                        column(width = 12,
                                               radioButtons("map",
                                                            label="Mapping Variable",
                                                            choices = c("Black", "White",
                                                                        "Hispanic")),
                                               tmapOutput(outputId = "race", height = "300px"))),
                                    fluidRow(
                                        column(width = 12,
                                               tmapOutput(outputId = "life", height = "300px"))),
                                    fluidRow(
                                        column(width = 12,
                                               tmapOutput(outputId = "income", height = "300px")))),
                 ),
                 tabPanel("Scatterplot",
                          fluidPage("Race vs. Life Expectancy"),
                          fluidRow(
                              column(width = 12, 
                                     checkboxGroupInput("scatterplot", 
                                                        label="Type of Race", 
                                                        choices = c("Black", "Hispanic", "White", "Asian"))),
                              column(width = 12,
                                     plotOutput("facet_plot"))
                          )
                 ),
                 tabPanel("Other",
                          fluidPage("Other Variables vs. Life Expectancy"),
                          fluidRow(
                              column(width = 12, 
                                     selectInput("graphs", 
                                                 label="Variables", 
                                                 choices = c("Income", "Health Insurance"))),
                              column(width = 12,
                                     plotOutput("plotgraphs")
                              )
                          )
                 )
)