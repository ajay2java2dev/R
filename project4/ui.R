## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(rsconnect)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("Recommender by Genre", tabName = "system1", icon = icon("dashboard")),
              menuItem("Recommender by Rating", tabName = "system2", icon = icon("star"))
            )
          ),
          
          dashboardBody(
            tabItems(
             tabItem(tabName = "system1",
                     includeCSS("css/movie.css"),
                     fluidRow(
                       box(width = 12, title = "Select your favorite Genre", status = "info", 
                           solidHeader = TRUE, collapsible = TRUE,
                           div(class = "rategenres",
                               selectInput("selected_genre", "select a genre from the drop down menu:",
                                           list("Action", "Adventure", "Animation", 
                                                "Children's", "Comedy", "Crime",
                                                "Documentary", "Drama", "Fantasy",
                                                "Film-Noir", "Horror", "Musical", 
                                                "Mystery", "Romance", "Sci-Fi", 
                                                "Thriller", "War", "Western")
                               )
                           )
                       )
                     ),
                     fluidRow(
                       useShinyjs(),
                       box(
                         width = 12, status = "info", solidHeader = TRUE,
                         title = "Step 2: Discover movies you might like",
                         br(),
                         withBusyIndicatorUI(
                           actionButton("genre_btn", "Click here to get your recommendations", class = "btn-warning")
                         ),
                         br(),
                         tableOutput("topPopularMoviesByGenre")
                       )
                     )
                    ),
             
             tabItem(tabName = "system2",
                     includeCSS("css/movie.css"),
                     fluidRow(
                       box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                           div(class = "rateitems",
                               uiOutput('ratings')
                           )
                       )
                     ),
                     fluidRow(
                       useShinyjs(),
                       box(
                         width = 12, status = "info", solidHeader = TRUE,
                         title = "Step 2: Discover movies you might like",
                         br(),
                         withBusyIndicatorUI(
                           actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                         ),
                         br(),
                         tableOutput("results")
                       )
                     )
                   )
            )
         )
    )
)

 