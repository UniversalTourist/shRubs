# This is a Shiny Web Application created for useR!2018 Data Challange by Natalia Da Silva and Hazel Kavili. 
# The app is about 10 plant species mostly observed in New South Wales and Queensland. 

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)

#source('code.R')
ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage("Australia Plants", fluid = TRUE,
                           tabPanel("Descriptions",
                             tabsetPanel(
                             tabPanel("Temperature", withSpinner(plotOutput(outputId = 'tempplot'))),
                             tabPanel("Precipitation",withSpinner(plotOutput(outputId = 'prpplot'))),
                             tabPanel("Observations by State", withSpinner(plotOutput(outputId = 'obsvplot'))),
                             tabPanel("Observations by Plants", withSpinner(plotOutput(outputId = 'obsvplot2')))
                                       )),
                           
                           tabPanel("Plants on Map",
                              sidebarPanel(width = 3,
                              selectInput('year', 'Year', 1990:2016),
                              selectInput('plant', 'Plants', c("Brachychiton", "Triodia", "Flindersia", "Livistona","Callitris", "Daviesia", "Ficus","Hakea", "Bottlebrush", "Grevillea"), selected="Brachychiton")),
                              mainPanel(withSpinner(plotOutput(outputId = 'plot1')))),
                           
                           
                           tabPanel("Sub"))
                )


server <- function(input, output, session){
  selectedData <- reactive({
    datos <- read.csv("datos.csv")
  })
  
  #Descriptives
  datosDesc <- datos %>% group_by(state, year) %>% 
    drop_na() %>% 
    filter(state != "") %>% filter(year > 1990) %>%
    mutate(precipitationAnnual = mean(precipitationAnnual, na.rm = TRUE),
           temperatureAnnualMaxMean = mean(temperatureAnnualMaxMean, na.rm = TRUE))
  
  output$prpplot <- renderPlot({
      ggplot(data = datosDesc, aes(x = year, y= precipitationAnnual)) + 
      geom_line() + geom_point() +
      facet_wrap(~state)
  })
  
  output$tempplot <- renderPlot({
    ggplot(data = datosDesc, aes(x = year, y = temperatureAnnualMaxMean)) + 
      geom_line() + geom_point() +
      facet_wrap(~state)
  })
  
  output$obsvplot <- renderPlot({
    datosDesc %>% group_by(year, state) %>% 
      summarise(total = n()) %>%
      filter(year > 1990) %>%
      ggplot(aes(x = year, y = total)) + 
      geom_point() + geom_line() + facet_wrap(~state) 
  })
  
  output$obsvplot2 <- renderPlot({
    datosDesc %>% group_by(year, plant) %>% 
      summarise(total = n()) %>%
      filter(year > 1990) %>%
      ggplot(aes(y = total, x = year, color = plant)) + 
      geom_point() + geom_line()
  })
    
  
  #Plots for Plants  
  output$plot1 <- renderPlot({
    
    ### Australian MAP
    load("aus_map.Rda")
    
    
    map <- aus_map %>%
      ggplot() +
      geom_polygon(aes(long, lat, group = group), alpha=1/3) +
      theme_bw() + coord_map() 
    
    #MAP with totals by year
    plcant <-  function(y, siz = FALSE, col = TRUE, pl = "all", dat){
      if(pl == "all"){
        dat3 <- dat %>% group_by(state, year) %>% drop_na() %>% 
          filter(state!="") %>% 
          filter(year == y) %>% 
          mutate(total = n())   
      } else{
        dat3 <- dat %>% group_by(state, year) %>% drop_na() %>% 
          filter(state!="") %>% 
          filter(year == y) %>% 
          mutate(total = n()) %>% 
          filter(plant == pl)
      }
      # xs=quantile(dat3$total)
      # datall <- dat3 %>% mutate(cattot = cut(total, breaks=c(xs[1], xs[2]), xs[3], xs[4], xs[5]))
      if(col){
        map + geom_point(data = dat3, aes(x = longitude, y = latitude, colour = total), alpha = 1/3) +
          labs(colour = "Total", title = paste(pl, y))
      } else{
        map + geom_point(data = dat3, aes(x = longitude, y = latitude, size = total), alpha = 1/3) +
          labs(sizw = "Total", title = paste(pl, y))
      }
    }
    
    plcant(y = input$year, pl = input$plant, dat = selectedData())
    
    })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

