#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(mokeR)
# Return dnorm(x) for 0 < x < 2, and NA for all other x
dnorm_limit <- function(x, lower_lim = 0, upper_lim = 2) {
  y <- dnorm(x)
  y[x < lower_lim  |  x > upper_lim] <- NA
  return(y)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Measuring Error"),

  # UI Sections -------------------------------------------------------------

  navlistPanel(
    id = "tabset", 

  ## Should GoodSell Reduce Noise? -------------------------------------------
    "Should GoodSell Reduce Noise?",
    tabPanel("Measuring Noise", 

    # Sidebar with a slider input for number of bins 
    fluidPage(
      # p("The effect of consistent bias on decision-making is intuitive. We can often account for the bias because we know which direction it’s going to be in. If your friend is always late, you can tell them the party is starting 30 minutes earlier than it actually is and expect them to show up a little closer to the start time. With noise on the other hand, errors go both ways. It is less obvious that noise can be just as bad because noise can cancel out in our minds. If that same friend was 30 minutes early half the time and 30 minutes late the other half, on average they show up on time."),
          column(width = 12,
                 fluidRow(uiOutput("goodsell_intro")),
                 # fluidRow(p("A company named GoodSell asks its sales analysts to predict GoodSell's marketshare in the region. Below is the distribution of their forecasts.")),
            fluidRow(align = "center", plotOutput("distPlot", width = "60%")),
            fluidRow(uiOutput("distText")),
            # p("Because this data is normally distributed, we know that around two-thirds of the forecasts are within one standard deviation on either side of the mean."),
            p("The question now is: should GoodSell try to reduce noise now? Or should they wait to find out the actual market share first?")
          )
          
        )
    ),
    tabPanel("Measuring Bias",
             fluidPage(
               fluidRow(plotOutput("errorPlot", width = "60%"),
                        textOutput("bias")),
               fluidRow(
                 column(3, sliderInput("actual",
                                       "Actual Marketshare",
                                       min = 0,
                                       max = 100,
                                       value = 34, post = "%")),
                 column(9, p("Use the slider on the left to set the Actual market share."))),
               fluidRow(
                 p("Note that while the amount of bias depends on the true outcome, the amount of noise is the same. This is a key property of noise - you can measure it without knowing the outcome."))
             )
             ),
    "Mean Squares",
    "Error Equations",
    "The Cost of Noise"
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  # Reactive Data -----------------------------------------------------------

  
  # Create normal distrib random forecasts
  goodsell_forecasts = reactive({
    set.seed(0)
    
    tibble(
      # forecaster = 1:1000,
      forecast = rnorm(n = 10000, mean = .44, sd = .10)
    )
  })
  
  
  forecast_mean = reactive({
    mean(goodsell_forecasts()$forecast)
  })
  
  forecast_sd = reactive({
    sd(goodsell_forecasts()$forecast)
  })
  
  
  

  # Outputs -----------------------------------------------------------------


    output$distPlot <- renderPlot({

        ggplot(goodsell_forecasts(), aes(x = forecast))+
          geom_density()+
          # Actual
          # geom_vline(xintercept = input$actual, colour = viz_colours[2], linewidth = 1.5)+
          # annotate("text", x = input$actual-.03, y = 5.5, label = "Actual", size = 6, hjust = "center", vjust = "top")+
          geom_vline(xintercept = forecast_mean(), linetype = "dashed")+
          geom_vline(xintercept = forecast_mean()+forecast_sd(), linetype = "dotted")+
          scale_x_continuous(name = NULL, 
                             breaks = seq(from = forecast_mean()-3*forecast_sd(), to = forecast_mean()+3*forecast_sd(), by = forecast_sd()),
                             # breaks = scales::breaks_pretty(n = 7), 
                             labels = scales::percent_format(accuracy = 1))+
          scale_y_continuous(name = NULL, expand = expansion(), labels = NULL)+
          labs(caption = "Distribution of GoodSell's market share forecasts for one region")+
          annotate("text", x = forecast_mean()-.05, y = 6.5, label = str_wrap("Mean Forecast", width = 8), 
                   size = 5, hjust = "center", vjust = "top", fontface = "italic", family = "Noto Sans")+
          annotate("text", x = forecast_mean()+forecast_sd()-.05, y = 1.5, label = str_wrap("Noise = 1 SD", width = 7),
                   size = 5, fontface = "italic", family = "Noto Sans")+
          annotate("segment", x = forecast_mean()+.005, xend = forecast_mean()+forecast_sd()-.005, y = .5, yend = .5,
                   arrow = arrow(ends = "both", length = unit(.75, "lines")))+
          theme_moke()+
          theme(axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.grid.major = element_blank())
    }, res = 96)
  
  output$goodsell_intro = renderUI({
    p("A company named GoodSell employs forecasters to predict market share in their region. The head of forecasting wants to know how accurate her forecasters are so she decided to conduct a noise audit. ",
      "The results of the audit, plotted below, show a normally distributed set of forecasts. The average forecast is ", strong(scales::percent(forecast_mean())), 
      ", while there are some extreme optimists who forecast that GoodSell will capture up to ", 
      strong(scales::percent(max(goodsell_forecasts()$forecast))), 
      " of the market and pessimists who expect this number to be as low as ", strong(scales::percent(min(goodsell_forecasts()$forecast))),
      # ". Below is the distribution of their forecasts.",
      ". If each forecaster were accurate, the forecasts would all be identical."
    )
  })
    
    output$distText = renderUI({
      # forecast_mean = mean(isolate(goodsell_forecasts()$forecast))
      dist_description = p(#"The average forecast is ", strong(scales::percent(forecast_mean())), 
            # ", while there are some extreme optimists who forecast that GoodSell will capture up to ", 
            # strong(scales::percent(max(goodsell_forecasts()$forecast))), 
            # " of the market and pessimists who expect this number to be as low as ", strong(scales::percent(min(goodsell_forecasts()$forecast))), 
            # ". We can't see the bias of GoodSell's forecasters until we know the actual market share, but we can see the noise",
            # ". If each forecaster were accurate, the forecasts would all be identical", 
            "Noise can be measured using the standard deviation, a measure representing the typical distance of a forecast from the average, which in this case is ", strong(scales::percent(forecast_sd())),
            ". Because this data is normally distributed, we know that around two-thirds of the forecasts are within one standard deviation on either side of the mean i.e. between ",
            strong(scales::percent(forecast_mean()-forecast_sd())), " and ", strong(scales::percent(forecast_mean()+forecast_sd())), 
            ".")
      
      dist_description
      # p(dist_description)
    })
    actual_pct = reactive({input$actual/100})
    
    output$bias = renderText({
      goodsell_errors = isolate(goodsell_forecasts()) |> 
        mutate(error = forecast - actual_pct())
      scales::percent(mean(goodsell_errors$error))
    })
    
    output$errorPlot <- renderPlot({
      # forecast_mean = mean(goodsell_forecasts()$forecast)
      # forecast_sd = sd(goodsell_forecasts()$forecast)
      
      bias = forecast_mean()-actual_pct()
      browser()
      goodsell_errors = isolate(goodsell_forecasts()) |> 
        mutate(error = forecast - actual_pct(),
               bias = mean(error),
               noise = error - bias)
      # bias_calc = mean(goodsell_errors$error)
      
      # From: https://community.rstudio.com/t/conditional-or-arbitrary-fill-in-density-plots/18094/2
      dens <- density(goodsell_errors$error)
      
      goodsell_error_density <- tibble(x = dens$x, y = dens$y) %>% 
        mutate(error_type = case_when(
          (forecast_mean() > actual_pct() & x >= 0 & x < bias) ~ "Bias",
          (forecast_mean() > actual_pct() & x >= bias & x <= bias+forecast_sd()) ~ "Noise",
          (forecast_mean() <= actual_pct() & x <= 0 & x > bias) ~ "Bias",
          (forecast_mean() <= actual_pct() & x <= bias & x > bias-forecast_sd()) ~ "Noise",
          TRUE ~ NA_character_))
      
      bias_start = if_else(forecast_mean() > actual_pct(), 0+.005, 0-.005)
      bias_end = if_else(forecast_mean() > actual_pct(), bias-.005, bias+.005)
      noise_start = if_else(forecast_mean() > actual_pct(), bias+.005, bias-.005)
      noise_end = if_else(forecast_mean() > actual_pct(), bias+forecast_sd()-.005, bias-forecast_sd()+.005)
      
      ggplot(goodsell_error_density, aes(x,y)) + 
        geom_line() +
        geom_area(data = filter(goodsell_error_density, error_type == 'Bias'), fill = viz_colours[3], alpha = .8) + 
        geom_area(data = filter(goodsell_error_density, error_type == 'Noise'), fill = viz_colours[6], alpha = .8)+
        # geom_density()+
        # Zero error
        geom_vline(xintercept = 0, colour = viz_colours[2], linewidth = 1.5)+
        # Error of the mean forecast
        geom_vline(xintercept = bias, linetype = "dashed")+
        # Show SD
        geom_vline(xintercept = if_else(forecast_mean() > actual_pct(), bias+forecast_sd(), bias-forecast_sd()), linetype = "dotted")+
        # stat_density(bounds = after_stat(c(0, bias)), geom = "area", fill = viz_colours[3], alpha = 0.2, na.rm=T)+
        scale_x_continuous(name = NULL, 
                           breaks = scales::breaks_pretty(n = 7), 
                           labels = scales::percent)+
        scale_y_continuous(name = NULL, expand = expansion(), labels = NULL)+
        labs(caption = "Distribution of errors in GoodSell's forecasts for one region")+
        # Actual Marketshare
        annotate("text", x = if_else(forecast_mean() >= actual_pct(), 0-.05, 0+.05), y = 6.5, label = str_wrap("Zero Error", width = 5), 
                 size = 5, hjust = "center", vjust = "top", family = "Noto Sans")+
        # Mean Forecast
        annotate("text", x = if_else(forecast_mean() < actual_pct(), bias-.05, bias+.05), y = 5, label = str_wrap("Mean Forecast", width = 8), 
                 size = 5, hjust = "center", vjust = "top", fontface = "italic", family = "Noto Sans")+
        # Bias
        annotate("text", x = mean(c(0, bias)), y = 1.5, label = "Bias", 
                 size = 5, fontface = "italic", family = "Noto Sans")+
        annotate("segment", x = bias_start, xend = bias_end, y = .5, yend = .5,
                 arrow = arrow(ends = "both", length = unit(.75, "lines")))+
        # Noise
        annotate("text", x = mean(c(bias, noise_end)), y = 1.5, label = str_wrap("Noise = 1 SD", width = 7), 
                 size = 5, fontface = "italic", family = "Noto Sans")+
        annotate("segment", x = noise_start, xend = noise_end, y = .5, yend = .5,
                 arrow = arrow(ends = "both", length = unit(.75, "lines")))+
        theme_moke()+
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank())
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
