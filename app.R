library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(covidregionaldata)
library(leaflet)
library(tidyverse)
library(scales)
library(DT)
library(plotly)
source(here::here('data.R'))
source(here::here('map.R'))



shinyApp(
  ui = dashboardPage(
    skin = 'red',
    dashboardHeader(
      title = "Covid 19 Situation in Balkan Countries",
      titleWidth = 450
    ),
    dashboardSidebar(
      # Remove the sidebar toggle element
      tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
      # Custom CSS to hide the default logout panel
      tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    width = '0px',
    disabled = TRUE),
    body = dashboardBody(
      fluidRow(
        infoBoxOutput(width = 4,"dailydeaths"),
        infoBoxOutput(width = 4,"dailyinf"),
        infoBoxOutput(width = 4,"mostdeath"),
        dateRangeInput('dateRange',
                       label = 'Select time',
                       start = "2019-01-01", end = Sys.Date(),
                       format = "dd/mm/yyyy")
      ),
      fluidRow(
        box(plotlyOutput("plot3")),
        box(plotlyOutput("plot4"))
      ),
      
      fluidRow(
        
        box(plotlyOutput("plot1")),
        box(plotlyOutput("plot2"))
      ),
      fluidRow(
        box(DT::dataTableOutput("mytable")),
        box(leafletOutput('myMap'))
      )
      
    ),
    title = "Covid Situation in Balkan"
  ),
  server <- function(input, output){
    
    output$userpanel <- renderUI({
      # session$user is non-NULL only in authenticated sessions
      if (!is.null(session$user)) {
        sidebarUserPanel(
          span("Logged in as ", session$user),
          subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
      }
    })
    
    g7_nots_plot1 <- reactive({
      g7_nots %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    })
    
    output$dailydeaths = renderInfoBox({
      infoBox(
        title = "Largest number of daily deaths per milion",
        value = deathpermilion[1,],
        icon = icon("bar-chart-o"),
        color = "blue"
      )
    })
    
    output$dailyinf = renderInfoBox({
      infoBox(
        title = "Largest number of daily infections per milion",
        value = infpermilion[1,],
        icon = icon("bar-chart-o"),
        color = "blue"
      )
    })
    
    output$mostdeath = renderInfoBox({
      infoBox(
        title = "Largest number of deaths per milion since beginning",
        value = mostdeath[1,],
        icon = icon("bar-chart-o"),
        color = "blue"
      )
    })
    
    
    
    output$plot1 <- renderPlotly({
      fig <- plot_ly(g7_nots_plot1(), type = 'scatter', color = ~ factor(country), mode = 'lines')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Slovenia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Albania')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Croatia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Serbia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Bosnia and Herzegovina')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Montenegro')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Macedonia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Greece')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Bulgaria')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Romania')%>%
        layout(title = 'Daily deaths from Covid19 in Balkan Countries since the beginning of pandemic',legend=list(title=list(text='Country')),
               xaxis = list(dtick = "M1"))
      options(warn = -1)
      fig <- fig %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')
      
      
      fig
    })
    
    g7_nots_plot2 <- reactive({
      g7_nots %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    })
    
    output$plot2 <- renderPlotly({
      fig <- plot_ly(g7_nots_plot2(), type = 'scatter', mode = 'lines', color = ~ factor(country))%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Slovenia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Albania')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Croatia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Serbia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Bosnia and Herzegovina')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Montenegro')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Macedonia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Greece')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Bulgaria')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Romania')%>%
        layout(title = 'Daily infections from Covid19 in Balkan Countries since the beginning of pandemic',legend=list(title=list(text='Country')),
               xaxis = list(dtick = "M1"))
      options(warn = -1)
      fig <- fig %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')
      
      
      fig
    })
    
    
    g7_nots_plot3 <- reactive({
      g7_nots %>% filter(date >= '2022-01-01')
    })
    
    output$plot3 <- renderPlotly({
      fig <- plot_ly(g7_nots_plot3(), type = 'scatter', mode = 'lines+markers', color = ~ factor(country))%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Slovenia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Albania')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Croatia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Serbia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Bosnia and Herzegovina')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Montenegro')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Macedonia')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Greece')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Bulgaria')%>%
        add_trace(x = ~date, y = ~cases_new, country = 'Romania')%>%
        layout(title = 'Daily infections from Covid19 in Balkan Countries during 2022',legend=list(title=list(text='Country')),
               xaxis = list(dtick = "M1"))
      options(warn = -1)
      fig <- fig %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')
      
      
      fig
    })
    
    
    g7_nots_plot4 <- reactive({
      g7_nots %>% filter(date >= '2022-01-01')
    })
    
    output$plot4 <- renderPlotly({
      fig <- plot_ly(g7_nots_plot4(), type = 'scatter', mode = 'lines+markers', color = ~ factor(country))%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Slovenia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Albania')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Croatia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Serbia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Bosnia and Herzegovina')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Montenegro')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Macedonia')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Greece')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Bulgaria')%>%
        add_trace(x = ~date, y = ~deaths_new, country = 'Romania')%>%
        layout(title = 'Daily deaths from Covid19 in Balkan Countries during 2022',legend=list(title=list(text='Country')),
               xaxis = list(dtick = "M1"))
      options(warn = -1)
      fig <- fig %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6')
      
      
      fig
    })
    
    
    output$myMap = renderLeaflet({
      m <- leaflet(deathmap) %>% 
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
        setView( lat=41.337515, lng=19.808873 , zoom=5) %>%
        addCircleMarkers(~long, ~lat, radius = deathmap$cases_new/1000, opacity = 5, popup = ~mytext, 
                         fillColor = ~mypalette(cases_new), fillOpacity = 0.7, color="white", stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), 
                                                      textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal= mypalette, values=~ cases_new, opacity=0.9, title = "Covid19 in Balkan", position = "bottomright")%>%
      
        htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('Overlay options');
            $('.leaflet-control-layers-list').prepend('Base layer options');
        }
    ")
      
      
      m
        })
    
    output$mytable <- DT::renderDataTable(g7_nots1,
                                          options = list(scrollX = TRUE),
                                          rownames = FALSE)
  }
  

  
)