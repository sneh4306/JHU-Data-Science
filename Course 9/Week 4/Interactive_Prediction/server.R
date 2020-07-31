#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    mtcars$vs<-factor(mtcars$vs,labels = c("V-shaped","Straight"))
    mtcars$carb<-as.factor(mtcars$carb)
    text<-reactive({
        paste("mpg vs",input$xvar)
    })
    output$Title<-renderText({
        text()
        })
    
    p<-reactive({
        ax<-list(
            title=input$xvar
        )
        al<-list(
            title=input$color
        )
        #j<-plot_ly(mtcars,x=~get(input$xvar),y=~mpg,size=~get(input$size),color = ~get(input$color),type="scatter",mode='markers')
        #j%>% layout(xaxis=ax)
        g<-ggplot(mtcars,aes_string(y="mpg",x=input$xvar,col=input$color))+geom_point(aes_string(size=input$size))
        g
        })
   
    output$plot<-renderPlot({
        p()
         })
})
