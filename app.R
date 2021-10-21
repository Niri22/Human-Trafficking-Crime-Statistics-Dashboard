#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(leaflet)
library(DT)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(maps)
library(plotly)
library(shinydashboard)


vars2 <- list("Canada", "British Columbia [59]", "Saskatchewan [47]", "Quebec [24]","Prince Edward Island [11]",
              "Nova Scotia [12]" , "Newfoundland and Labrador [10]", "New Brunswick [13]", "Manitoba [46]",  "Alberta [48]", "Ontario [35]" )

# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar( disable = TRUE),
        dashboardBody(
            tabItem(tabName = "dashboard",
                fluidRow(
                    box(title ="Crime Statistics Dashboard",selectInput(inputId = 'trend_province', 'Choose a Province', vars2, selected = vars2[[1]]), status = "success",olidHeader = TRUE,collapsible = TRUE, height = 250),
                    box(title ="Actual incidents",plotOutput("PD", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Rate per 100,000 population",plotOutput("PD2", height = 200), status = "warning", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Percentage change in rate",plotOutput("PD3", height = 200), status = "danger", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Unfounded incidents",plotOutput("PD4", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Percent unfounded",plotOutput("PD5", height = 200), status = "danger", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Total cleared",plotOutput("PD6", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Cleared by charge",plotOutput("PD7", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Cleared otherwise",plotOutput("PD8", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Total, persons charged",plotOutput("PD9", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Rate, total persons charged per 100,000 population aged 12 years and over",plotOutput("PD10", height = 200), status = "warning", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title = "Total, adult charged" ,plotOutput("PD11", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Rate, adult charged per 100,000 population aged 18 years and over",plotOutput("PD12", height = 200), status = "warning", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Total, youth charged",plotOutput("PD13", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Rate, youth charged per 100,000 population aged 12 to 17 years",plotOutput("PD14", height = 200), status = "warning", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Total, youth not charged",plotOutput("PD15", height = 200), status = "primary", solidHeader = TRUE,
                        collapsible = TRUE),
                    box(title ="Rate, youth not charged per 100,000 population aged 12 to 17 years",plotOutput("PD16", height = 200), status = "warning", solidHeader = TRUE,
                        collapsible = TRUE)
                    ))
            )))

# Define server logic required to draw a histogram
server <- function(input, output) {

    DashDisplay <- read_csv("DashDisplay.csv")
    
    ###First Tab- Tend Display###
    
    statsdata <- reactive({ subset(DashDisplay, DashDisplay$GEO == input$trend_province)})
    
    
    output$PD <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Actual incidents"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Actual incidents") + xlab("Years")+ theme_classic() + ylab("Number of Actual incidents") + 
            xlab("")})
    output$PD2 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Rate per 100,000 population" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Rate per 100,000 population") + xlab("Years")+ theme_classic() +  ylab("Rate per 100,000 population") +
            xlab("")})
    output$PD3 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Percentage change in rate"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Percentage change in rate") + xlab("Years")+ theme_classic() + ylab("Percentage change in rate") +
            xlab("")})
    output$PD4 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Unfounded incidents"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Unfounded incidents") + xlab("Years")+ theme_classic() + ylab("Unfounded incidents") +
            xlab("")})
    output$PD5 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Percent unfounded" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Percent unfounded") + xlab("Years")+ theme_classic() + ylab("Percent unfounded") +
            xlab("")})
    output$PD6 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Total cleared"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Total cleared") + xlab("Years")+ theme_classic() + ylab("Total cleared") +
            xlab("")})
    output$PD7 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Cleared by charge" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Cleared by charge" )+ xlab("Years")+ theme_classic() + ylab("Number of Cleared by charge") +
            xlab("")})
    output$PD8 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics =="Cleared otherwise" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Cleared otherwise") + xlab("Years")+ theme_classic() + ylab("Number of Cleared otherwise") +
            xlab("")
    })
    output$PD9 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Total, persons charged" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Total, persons charged") + xlab("Years")+ theme_classic() + ylab("Number of Total, persons charged") +
            xlab("")
    })
    output$PD10 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Rate, total persons charged per 100,000 population aged 12 years and over"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Rate, total persons charged per 100,000 population aged 12 years and over") + xlab("Years")+ theme_classic() 
        + ylab("Rate per 100,000 population") +
            xlab("")
    })
    output$PD11 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Total, adult charged" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Total, adult charged") + xlab("Years")+ theme_classic() + ylab("Number of Total, adult charged") +
            xlab("")
    })
    output$PD12 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Rate, adult charged per 100,000 population aged 18 years and over"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Rate, adult charged per 100,000 population aged 18 years and over") + xlab("Years")+ theme_classic() + ylab("Rate per 100,000 population") +
            xlab("")
    })
    output$PD13 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Total, youth charged" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Total, youth charged") + xlab("Years")+ theme_classic() + ylab("Number of Total, youth charged") +
            xlab("")
    })
    output$PD14 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Rate, youth charged per 100,000 population aged 12 to 17 years"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Rate, youth charged per 100,000 population aged 12 to 17 years") + xlab("Years")+ theme_classic() + ylab("Rate per 100,000 population") +
            xlab("")
    })
    output$PD15 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Total, youth not charged" ), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Total, youth not charged")+ xlab("Years")+ theme_classic() + ylab("Number of Total, youth not charged") +
            xlab("")
    })
    output$PD16 <- renderPlot({
        ggplot(subset(statsdata(), statsdata()$Statistics == "Rate, youth not charged per 100,000 population aged 12 to 17 years"), aes(x = REF_DATE, y = VALUE))+
            geom_line() + ggtitle("Rate, youth not charged per 100,000 population aged 12 to 17 years") + xlab("Years")+ theme_classic() + ylab("Rate, youth not charged per 100,000 population") +
            xlab("")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
