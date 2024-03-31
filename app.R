#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, tidyverse, timetk, plotly, modeltime, parsnip, rsample)

import <- read_csv('data/Import.csv')
export <- read_csv('data/Export.csv')
fnb <- read_csv('data/FnB.csv')
retail <- read_csv('data/Retail.csv')
gdp <- read_csv('data/GDPgrowth.csv')
import$DATE <- as.Date(paste0('01/',import$DATE), format = '%d/%m/%Y')
export$DATE <- as.Date(paste0('01/',export$DATE), format = '%d/%m/%Y')
fnb$DATE <- as.Date(paste0('01/',fnb$DATE), format = '%d/%m/%Y')
retail$DATE <- as.Date(paste0('01/',retail$DATE), format = '%d/%m/%Y')
gdp$year = substring(gdp$DATE,1,4)
gdp$quarter = substring(gdp$DATE,6,7)
gdp <- gdp %>%
  mutate(
    quarter = case_when(
      quarter == '1Q' ~ '01/03/',
      quarter == '2Q' ~ '01/06/',
      quarter == '3Q' ~ '01/09/',
      quarter == '4Q' ~ '01/12/'
    )
  ) 
gdp$DATE = as.Date(paste0(gdp$quarter,gdp$year), format = '%d/%m/%Y')
gdp <- gdp %>%
  arrange(DATE) %>%
  select(-c("year","quarter"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NOWCASTING STATE OF ECONOMIC IN SINGAPORE"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        width= 3,
        selectInput(
          "dataset",
          "Dataset:",
          choices = c("Import", "Export", "FnB", "Retail", "GDP")),
        uiOutput(
          "region_dropdown"
        ),
        uiOutput(
          "sub_button"
        ),
      ),
      mainPanel(
        tabsetPanel(
          id = "panel",
          tabPanel(
            "Historical",
            plotlyOutput(
              "main",
              height = "250px",
              width = "1000px"
            ),
            plotlyOutput(
              "stl",
              height = "600px",
              width = "1000px"
            ),
          ),
          tabPanel(
            "Diagnostic",
            plotlyOutput(
              "p&acf",
              height = "400px",
              width = "1000px"
            ),
            plotlyOutput(
              "seasonal",
              height = "400px",
              width = "1000px"
            ),
          ),
          tabPanel(
            "Nowcast",
            sidebarLayout(
              sidebarPanel(
                fluidRow(
                  selectInput(
                    "method",
                    "Method:",
                    choices = c("ETS", "Arima", "Prophet")),
                ),
                uiOutput(
                  "parameters"
                ),
                fluidRow(
                  actionButton(
                    "submit_sub", 
                    "Generate"),
                ),
                width = 12
              ),
              mainPanel(
                plotOutput(
                  "v_nowcast",
                  height = "200px",
                  width = "1000px"
                ),
                tableOutput(
                  "accuracy_nowcast"
                ),
                plotOutput(
                  "nowcast",
                  height = "200px",
                  width = "1000px"
                ),
                width = 12
              )
            )
          ),
          tabPanel(
            "Auto Nowcast",
            plotOutput(
              "validation",
              height = "300px",
              width = "1000px"
            ),
            tableOutput(
              "accuracy_table"
            ),
            plotOutput(
              "a_nowcast",
              height = "400px",
              width = "1000px"
            )
          )
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
    output$region_dropdown <- renderUI({
      if(input$dataset %in% c('Import',"Export")) {
        if (input$dataset == "Import"){
          country_list <- colnames(import)[-1]  
        } else {
          country_list <- colnames(export)[-1]  
        }
        selectInput(
          "region", 
          "Choose a region:", 
          choices = country_list,
          multiple = FALSE)
      } else if (input$dataset == "GDP"){
        cat_list <- colnames(gdp)[-1]
        selectInput(
          "category",
          "Choose a category:",
          choices = cat_list,
          multiple = FALSE
        )
      }
    })
    output$sub_button <- renderUI({
      if(input$panel %in% c('Historical',"Diagnostic","Auto Nowcast")) {
        actionButton(
          "submit", 
          "Generate")
      }
    })
    observeEvent(input$submit, {
      if (input$dataset == "Export") {
        output_data <- export %>%
          select(DATE, input$region) %>%
          rename(VALUE = input$region)
      } else if (input$dataset == "Import"){
        output_data <- import %>%
          select(DATE, input$region) %>%
          rename(VALUE = input$region)
      } else if (input$dataset == "FnB"){
        output_data <- fnb
      } else if (input$dataset == "Retail"){
        output_data <- retail
      } else if (input$dataset == "GDP"){
        output_data <- gdp %>%
          select(DATE, input$category) %>%
          rename(VALUE = input$category)
      }
      if (input$panel == "Historical"){
        output$main <- renderPlotly({
          p <- output_data %>%
            plot_time_series(
              DATE, VALUE,
              .interactive = TRUE,
              .plotly_slider = TRUE
            )
          ggplotly(p)
        })
        output$stl <- renderPlotly({
          p <- output_data %>%
            plot_stl_diagnostics(
              DATE, VALUE,
              .frequency = "auto", .trend = "auto",
              .feature_set = c("season", "trend", "remainder"),
              .interactive = TRUE)
          ggplotly(p)
        })
      } else if (input$panel == "Diagnostic") {
        output$`p&acf` <- renderPlotly({
          p <- output_data %>%
            plot_acf_diagnostics(
              DATE, VALUE,
              .interactive = TRUE,
              .plotly_slider = TRUE
            )
          ggplotly(p)
        })
        output$seasonal <- renderPlotly({
          output_data %>%
            plot_seasonal_diagnostics(
              DATE, VALUE,
              .interactive = TRUE
            )
        })
      } else if (input$panel == "Auto Nowcast"){
        
        if (input$dataset == "GDP"){
          assess = "6 months"
        } else {
          assess = "3 months"
        }
        
        splits <- time_series_split(
          output_data,
          assess = assess,
          cumulative = TRUE
        )
        
        model_arima <- arima_reg()%>%
          set_engine("auto_arima") %>% 
          fit(VALUE ~ DATE, training(splits))
        model_ets <- exp_smoothing()%>%
          set_engine("ets") %>% 
          fit(VALUE ~ DATE, training(splits))
        model_prophet <- prophet_reg()%>%
          set_engine("prophet") %>% 
          fit(VALUE ~ DATE, training(splits))
        
        models_tbl <- modeltime_table(
          model_arima,
          model_ets,
          model_prophet
        )
        
        calibration_tbl <- models_tbl %>%
          modeltime_calibrate(new_data = testing(splits))
        
        output$validation <- renderPlot({
          
          plot_data <- output_data %>% 
            filter(
              DATE > '2020-01-01'
            )
          
          calibration_tbl %>%
            modeltime_forecast(
              new_data    = testing(splits),
              actual_data = plot_data
            ) %>%
            plot_modeltime_forecast(
              .title = "Model Nowcast Validation",
              .interactive = FALSE
            ) 
        })
        output$accuracy_table <- renderTable({
          calibration_tbl %>%
            modeltime_accuracy() %>%
            table_modeltime_accuracy(
              .interactive = FALSE
            )
        })
        output$a_nowcast <- renderPlot({
          refit_tbl <- calibration_tbl %>%
            modeltime_refit(data = output_data)

          plot_data <- output_data %>%
            filter(
              DATE > '2020-01-01'
            )

          refit_tbl %>%
            modeltime_forecast(h = assess, actual_data = plot_data) %>%
            plot_modeltime_forecast(
              .title = "Model Nowcast",
              .interactive      = FALSE
            )
        })
        
      }
    }, ignoreInit = TRUE) 
    
    output$parameters <- renderUI({
      if( input$method == "ETS"){
        width = 2
        choice = c("auto","additive","multiplicative","none")
        fluidRow(
          column(
            width = width,
            numericInput(
              "period",
              "Seasonal Period:",
              value = 12
            )
          ),
          column(
            width = width,
            selectInput(
              "error",
              "Error:",
              choices = choice
            )
          ),
          column(
            width = width,
            selectInput(
              "trend",
              "Trend:",
              choices = choice
            )
          ),
          column(
            width = width,
            selectInput(
              "season",
              "Season:",
              choices = choice
            )
          ),
          column(
            width = width,
            selectInput(
              "damping",
              "Damping:",
              choices = c("auto","damped","none")
            )
          )
        )
      } else if (input$method == "Arima"){
        width = 3
        fluidRow(
          fluidRow(
            column(
              width = width,
              numericInput(
                "nsa",
                "Non Seasonal AR(p):",
                value = 3
              )
            ),
            column(
              width = width,
              numericInput(
                "nsd",
                "Non Seasonal Differences(d):",
                value = 1
              )
            ),
            column(
              width = width,
              numericInput(
                "nsm",
                "Non Seasonal MA(q):",
                value = 3
              )
            ),
            column(
              width = width,
              numericInput(
                "period",
                "Seasonal Period:",
                value = 12
              )
            ),
          ),
          fluidRow(
            column(
              width = width,
              numericInput(
                "sa",
                "Seasonal AR(P):",
                value = 1
              )
            ),
            column(
              width = width,
              numericInput(
                "sd",
                "Seasonal Differences(D):",
                value = 0
              )
            ),
            column(
              width = width,
              numericInput(
                "sm",
                "Seasonal MA(Q):",
                value = 1
              )
            ),
          )
        )
      } else if (input$method == "Prophet"){
        width = 2
        choice = c("auto","TRUE","FALSE")
        fluidRow(
          column(
            width = width,
            selectInput(
              "growth",
              "Growth:",
              choices = c("linear","logistic_cap","logistic_floor")
            )
          ),
          column(
            width = width,
            selectInput(
              "sy",
              "Seasonality Yearly:",
              choices = choice
            )
          ),
          column(
            width = width,
            selectInput(
              "sw",
              "Seasonality Weekly:",
              choices = choice
            )
          ),
          column(
            width = width,
            selectInput(
              "sd",
              "Seasonality Daily:",
              choices = choice
            )
          ),
          column(
            width = width,
            selectInput(
              "season",
              "Season:",
              choices = c("additive","multiplicative")
            )
          )
        )
      }
    })
    observeEvent(input$submit_sub,{
      if (input$dataset == "Export") {
        output_data <- export %>%
          select(DATE, input$region) %>%
          rename(VALUE = input$region)
      } else if (input$dataset == "Import"){
        output_data <- import %>%
          select(DATE, input$region) %>%
          rename(VALUE = input$region)
      } else if (input$dataset == "FnB"){
        output_data <- fnb
      } else if (input$dataset == "Retail"){
        output_data <- retail
      } else if (input$dataset == "GDP"){
        output_data <- gdp %>%
          select(DATE, input$category) %>%
          rename(VALUE = input$category)
      }
      
      if (input$dataset == "GDP"){
        assess = "6 months"
      } else {
        assess = "3 months"
      }
      
      splits <- time_series_split(
        output_data,
        assess = assess,
        cumulative = TRUE
      )
      
      if (input$method == "ETS"){
        model <- exp_smoothing(
          seasonal_period = input$period,
          error = input$error,
          trend = input$trend,
          season = input$season,
          damping = input$damping,
        ) %>%
          set_engine("ets") %>%
          fit(VALUE ~ DATE, training(splits))
      } else if (input$method == "Arima"){
        model <- arima_reg(
          seasonal_period = input$period,
          non_seasonal_ar = input$nsa,
          non_seasonal_differences = input$nsd,
          non_seasonal_ma = input$nsm,
          seasonal_ar = input$sa,
          seasonal_differences = input$sd,
          seasonal_ma = input$sm
        ) %>%
          set_engine("arima") %>%
          fit(VALUE ~ DATE, training(splits))
      } else if (input$method == "Prophet"){
        model <- prophet_reg(
          growth = input$growth,
          seasonality_yearly = input$sy,
          seasonality_weekly = input$sw,
          seasonality_daily = input$sd,
          season = input$season
        ) %>%
          set_engine("prophet") %>%
          fit(VALUE ~ DATE, training(splits))
      }
      
      models_tbl <- modeltime_table(
        model
      )
      
      calibration_tbl <- models_tbl %>%
        modeltime_calibrate(new_data = testing(splits))
      
      plot_data <- output_data %>% 
        filter(
          DATE > '2020-01-01'
        )
      
      output$v_nowcast <- renderPlot({
        calibration_tbl %>%
          modeltime_forecast(
            new_data    = testing(splits),
            actual_data = plot_data
          ) %>%
          plot_modeltime_forecast(
            .title = "Model Nowcast Validation",
            .interactive = FALSE
          ) 
      })
      
      output$accuracy_nowcast <- renderTable({
        calibration_tbl %>%
          modeltime_accuracy() %>%
          table_modeltime_accuracy(
            .interactive = FALSE
          )
      })
      refit_tbl <- calibration_tbl %>%
        modeltime_refit(data = output_data)
      
      output$nowcast <- renderPlot({
        
        p <- refit_tbl %>%
          modeltime_forecast(h = assess, actual_data = plot_data) %>%
          plot_modeltime_forecast(
            .title = "Model Nowcast",
            .interactive      = FALSE
          )
        p
      })
    }, ignoreInit = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
