#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Cars Dataset"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        selectInput("xvar","Variable for X-axis",
                    choices=list("Cylinder"="cyl","Transmission"="am","Gears"="gear"),selected = "cyl"),
        selectInput("color","Variable for color",
                    choices=list("Carburetors"="carb","Engine Type"="vs"),selected="carb"),
        selectInput("size","Variable for size",
                    choices=list("Horsepower"="hp","Weight"="wt"),selected = "hp"),
        submitButton("Submit")
        ),
    
    
    

        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("Title")),
            plotOutput("plot")
        )
    )
    ))

