library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(readxl)
library(tidyr)

df1 <- read.csv("Kerala march.csv")

# UI
ui <- fluidPage(
    titlePanel("Covid Data -Kerala districts "),
    sidebarLayout(position = "left",
                  sidebarPanel("Select from the List",
                               column(
                                   12,fluidRow(column(12, selectizeInput("All", "District", multiple = T,choices = unique(df1$District), 
                                                                         options = list(maxItems = 15, placeholder = 'Choose a District:'))))
                               ),),
                  mainPanel("Plots of Corona Spread",
                            column(
                                12,fluidRow(column(12, plotlyOutput('plot'),plotlyOutput('plot1'),plotlyOutput('plot2'),plotlyOutput('plot4'))
                                )
                            ) )
    ))



# Server code
server <- function(input, output) {
    
    outVar1 <- reactive({
        df1 %>%
            filter(District %in% input$All) %>%
            arrange(Date) %>%
            droplevels()
    })
    
    
    
    
    output$plot <- renderPlotly({
        plot_ly(data=outVar1(), x=~Date,  y = ~Active.Cases,
                type = 'scatter', mode = 'lines', legendgroup = "1",
                color = ~District , colors = c("blue","pink","bisque","red","green","darkred","orange","black","chartreuse1","yellow","grey",'darkgreen',"firebrick","purple","darkslateblue")) %>%
            layout(legend = list(orientation = 'h'))         
    })
    output$plot1 <- renderPlotly({
        plot_ly(data=outVar1(), x=~Date,  y = ~Under.Observation,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~District , colors = c("blue","pink","bisque","red","green","darkred","orange","black","chartreuse1","yellow","grey",'darkgreen',"firebrick","purple","darkslateblue")) %>%
            layout(legend = list(orientation = 'h'))         
    }) 
    output$plot2 <- renderPlotly({
        plot_ly(data=outVar1(), x=~Date,  y = ~Home.Isolation,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~District , colors = c("blue","pink","bisque","red","green","darkred","orange","black","chartreuse1","yellow","grey",'darkgreen',"firebrick","purple","darkslateblue")) %>%
            layout(legend = list(orientation = 'h'))         
    })
    output$plot3 <- renderPlotly({
        plot_ly(data=outVar1(), x=~Date,  y = ~Hospitalized.Today,
                type = 'scatter', mode = 'lines', legendgroup = "2",
                color = ~District , colors = c("blue","pink","bisque","red","green","darkred","orange","black","chartreuse1","yellow","grey",'darkgreen',"firebrick","purple","darkslateblue")) %>%
            layout(legend = list(orientation = 'h'))         
    })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)