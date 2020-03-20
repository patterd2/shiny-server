library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme ="superhero.css",
    #theme = shinytheme("united"),
    # Application title
    titlePanel("Analysis of Data Science Teams"),
    # Sidebar with a slider input for the number of bins
    fluidRow(
      column(6,
             selectInput("company",label="Which company are you interested in?", 
                         choices = c("Airbnb","Amazon","Apple","Booking.com","Facebook","Google","Jawbone","Khan Academy","King",
                                     "LinkedIn","Microsoft","Netflix","Pandora","Pinterest","Quantifind","Quora","Spotify","TripAdvisor","Twitter",
                                     "Uber","Yahoo","Zalando","Zynga"),multiple = FALSE,selected = "Airbnb",selectize = TRUE)
      ),
      column(6,
             dataTableOutput("summary"),
             div(
               HTML("<a href='http://www.glassdoor.com/index.htm'>powered by <img src='https://www.glassdoor.com/static/img/api/glassdoor_logo_80.png' title='Job Search' /></a>")
             )
      )
    ),
    fluidRow(
      column(6,
             plotOutput("gender")
      ),
      column(6,
             plotOutput("colleges")
      )
    ),
    fluidRow(
      column(6,
             plotOutput("highest_degree")
      ),
      column(6,
             plotOutput("field_of_study")
      )
    ),
    fluidRow(
      column(6,
             plotOutput("skills")
      ),
      column(6,
             plotOutput("time_in_company")
      )
    )
  )
)
