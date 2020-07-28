
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import shinyMobile
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "Databrew Dashboard"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="Main",
            tabName="main"),
        
          menuItem(
            text = 'About',
            tabName = 'about')
        )),
      dashboardBody(
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        tabItems(
          tabItem(
            tabName="main",
            navbarPage(title = '',
                       collapsible = TRUE,
                       tabPanel('Main',
                                fluidPage(
                                  fluidRow(
                                    column(6,
                                           h3('Inputs'),
                                           sliderInput('n',
                                                       'Number of participants',
                                                       min = 50,
                                                       max = 5000,
                                                       value = 500,
                                                       step = 50),
                                           sliderInput('incidence',
                                                       'Weekly Covid-19 incidence per 100k',
                                                       min = 0,
                                                       max = 250,
                                                       value = 30,
                                                       step = 5),
                                           sliderInput('prob_skip_sick',
                                                       'If infected, likelihood of skipping event (due to symptoms)',
                                                       min = 0,
                                                       max = 100,
                                                       value = 0,
                                                       step = 5),
                                           sliderInput('days_infectious',
                                                       'Number of days during which a person is infectious',
                                                       min = 1, max = 20,
                                                       value = 10,
                                                       step = 1),
                                           actionButton('simulate',
                                                        'Simulate!',
                                                        icon = icon('dog'))),
                                    column(6,
                                           h3('Outputs'),
                                           plotOutput('the_plot'))
                                  )
                                ))
                       
            )
          ),
          tabItem(
            tabName = 'about',
            fluidPage(
              fluidRow(
                div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                h4('Built in partnership with ',
                   a(href = 'http://databrew.cc',
                     target='_blank', 'Databrew'),
                   align = 'center'),
                p('Empowering research and analysis through collaborative data science.', align = 'center'),
                div(a(actionButton(inputId = "email", label = "info@databrew.cc",
                                   icon = icon("envelope", lib = "font-awesome")),
                      href="mailto:info@databrew.cc",
                      align = 'center')),
                style = 'text-align:center;'
              )
            )
          )
        )
      )
    )
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'covidevent')
  )
  
  
  # share <- list(
  #   title = "Databrew's COVID-19 Data Explorer",
  #   url = "https://datacat.cc/covid19/",
  #   image = "http://www.databrew.cc/images/blog/covid2.png",
  #   description = "Comparing epidemic curves across countries",
  #   twitter_user = "data_brew"
  # )
  
  tags$head(
    
    # # Facebook OpenGraph tags
    # tags$meta(property = "og:title", content = share$title),
    # tags$meta(property = "og:type", content = "website"),
    # tags$meta(property = "og:url", content = share$url),
    # tags$meta(property = "og:image", content = share$image),
    # tags$meta(property = "og:description", content = share$description),
    # 
    # # Twitter summary cards
    # tags$meta(name = "twitter:card", content = "summary"),
    # tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:title", content = share$title),
    # tags$meta(name = "twitter:description", content = share$description),
    # tags$meta(name = "twitter:image", content = share$image),
    # 
    # # golem::activate_js(),
    # # golem::favicon(),
    # # Add here all the external resources
    # # Google analytics script
    # includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    # includeScript(system.file('app/www/script.js', package = 'covid19')),
    # includeScript(system.file('app/www/mobile.js', package = 'covid19')),
    # includeScript('inst/app/www/script.js'),
    
    # includeScript('www/google-analytics.js'),
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

##################################################
# SERVER
##################################################
#' @import shiny
#' @import dplyr
#' @import ggplot2
app_server <- function(input, output, session) {
  
  reactive_data <- reactiveValues(data = tibble())
  
  observeEvent(input$simulate,{
    message('prob skip sick is: ', input$pob_sick_skip)
    df <- calculate(n = input$n,
                    incidence = input$incidence,
                    prob_skip_sick = input$prob_skip_sick,
                    days_infectious = input$days_infectious)
    reactive_data$data <- df
  })
  
  output$the_plot <- renderPlot({
    pd <- reactive_data$data
    if(nrow(pd) > 0){
      zeros <- pd %>% filter(infectious == 0) %>% .$p
      prob_ok <- round(100 - zeros, digits = 1)
      ggplot(data = pd,
             aes(x = infectious,
                 y = p)) +
        geom_bar(stat = 'identity') +
        labs(x = 'Number of infectious people',
             y = 'Likelihood of outcome',
             title = paste0(prob_ok, '% likelihood of at least 1 infectious attendee')) +
        theme_bw() +
        geom_text(aes(label = paste0(round(p, digits = 1))),
                  nudge_y = 3,
                  alpha = 0.6,
                  size = 6) +
        scale_x_continuous(name = 'Number of infectious people',
                           breaks = pd$infectious)
    }
    
  })
  
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}