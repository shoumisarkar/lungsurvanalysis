library(shiny)
library(survival)
library(DT)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("sandstone"),
  
  # App title ----
  titlePanel("Interactive Survival Analysis of the survival::lung dataset"),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # br() element to introduce extra vertical spacing ----
      paste("We present an interactive survival analysis tool to analyze the NCCTG \"lung\" data in the survival package: It contains survival data on patients with advanced lung cancer from the North Central Cancer Treatment Group."),
      br(), br(),
      paste("The variables in this dataset include:"), br(),
      HTML(paste("<b>inst</b>: institution code,")), br(),
      HTML(paste("<b>time</b>: survival time in days,")), br(),
      HTML(paste("<b>status</b>: censoring status (1=censored, 2=dead),")), br(),
      HTML(paste("<b>age</b>: age in years,")), br(),
      HTML(paste("<b>sex</b>: male=1, female=2,")), br(),
      HTML(paste("<b>ph.ecog</b>: ECOG performance score as rated by the physician. (0=asymptomatic, 1= symptomatic but completely ambulatory, 2= in bed <50% of the day, 3= in bed > 50% of the day but not bedbound, 4 = bedbound),")), br(),
      HTML(paste("<b>ph.karno</b>: Karnofsky performance score (bad=0-good=100) rated by physician,")), br(),
      HTML(paste("<b>pat.karno</b>: Karnofsky performance score as rated by patient,")), br(),
      HTML(paste("<b>meal.cal</b>: Calories consumed at meals,")), br(),
      HTML(paste("<b>wt.loss</b>: Weight loss in last six months (pounds).")), br(), br(),
      
      paste("Performance scores rate how well the patient can perform usual daily activities. We present a preview of the data, provide interactive tools to measure prevalence, build parametric (we cover exponential, Weibull and log-logistic distributions), non-parametric and semi-parametric models of our choice, assess their goodness of fit, and finally compare groups (males and females) across user-specified time intervals to see if the survivals differ by sex."),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Prevalence Measures", 
                           br(), 
                           #HTML(paste("<b>Prevalence Measures Calculators: </b>")),
                           
                           h2("A preview of the NCCTG Lung Cancer dataset"),
                           DT::dataTableOutput('mytable'),
                           br(),
                           br(),
                           
                           h2("Point Measures of Prevalence:"),
                           h3("Play around with the slider bars to get interpretations specific to your choice!"),
                           
                           br(),
                           fluidRow(
                             column(width = 5,
                                    sliderInput("pp_time", "Choose a timepoint:",
                                                min = min(lung$time), max = max((lung$time)), value = 520),
                             ),

                             column(width = 5,
                                    h3("Point Prevalence"),
                                    textOutput("pointprev"),
                             )
                             ),
                           
                           br(),

                           h2("Period Measures of Prevalence:"),
                             fluidRow(
                               column(width = 5,
                                      sliderInput("pp_rng", "Choose a time interval:", value = c(10, 20), min = min(lung$time), max = max((lung$time))),
                               ),
                               column(width = 5,
                                      h3("Period Prevalence"),
                                      textOutput("periodprev"),
                                      br(),
                                      h3("Incidence Rate Ratio"),
                                      textOutput("incirateratio"),
                                      br(), br(),
                               )
                             )
                           ),
                  tabPanel("Parametric Fitting",
                           h3("Here we fit Accelerated Failure Time (AFT) regression models to the lung data."),
                           h4("Select covariates to keep in the regression - by default, all regressors are selected (the covariates \"time\" and \"status\" are always selected as it defines the censored survival times):"),
                           varSelectInput("variables", "Choose variable(s):", lung, multiple = TRUE),
                           tableOutput("variableselectorAFT"),
                           br(),
                           
                           h4("Select parametric distribution for the AFT model:"),
                           selectInput("AFTdist", "Distribution:",
                                       c("Exponential" = "exp",
                                         "Weibull" = "weib",
                                         "Log-logistic" = "loglogistic")),
                          
                           br(),
                           verbatimTextOutput("AFTsummary"),
                           br(),
                           h3("Significant predictors in the above model"),
                           h4("(Play around with the predictor and distribution choices to see what predictors turn out to be significant!)"),
                           textOutput("AFTselected"),
                           br(),
                           h3("Diagnostics: Cox-Snell Goodness of Fit plots"),
                           h4("This refreshes for every new model that we build! The closer the plot is to the line y=x, the better the fit."),
                           plotOutput("AFT.CS.plot"),
                           br(),
                           br(),
                           ),

                  tabPanel("Non-Parametric Fitting",
                           h3("Here we find the Nelson-Aalen estimates of the cumulative hazard function and compare it with its estimated (parametric) curves."),
                           h4("The Nelson-Aalen curve is plotted below. Select distributions from the menu below to compare it with AFT regression models."),
                           br(),
                           selectInput("AFTdistNA", "Select parametric distribution(s) to compare with:",
                                       c("Exponential" = "exp",
                                         "Weibull" = "weib",
                                         "Log-logistic" = "loglogistic"),
                                       multiple = TRUE, width = "400px"),
                           br(),
                           plotOutput("NAplotWithOverlay"),
                           br(),
                           
                  ),
                  
                  tabPanel("Semiparametric (Cox PH) Fitting",
                          h3("We now implement a semiparametric approach: the Cox Proportional Hazards model."),
                          h4("Select covariates below for the Cox PH regression. By default, all covariates are selected (the covariates \"time\" and \"status\" are always selected as they define the censored survival times)."),
                          br(),
                          varSelectInput("variables.coxph", "Choose variable(s):", lung, multiple = TRUE),
                          tableOutput("variableselectorcoxph"),
                          br(),
                          h4("Of the 228 observed survival times, there are only 186 unique values, meaning that there are ties in the data. We can handle ties by the Exact method, Efron's method or Breslow's method. Choose one below!"),
                          radioButtons("ties.method", "Select method to handle ties:",
                                       c("Exact" = "exact",
                                         "Efron" = "efron",
                                         "Breslow" = "breslow")),
                          br(),
                          verbatimTextOutput("coxphsummary"),
                          br(),
                          h3("Significant predictors in the above Cox PH model"),
                          h4("(Play around with the predictor and tie-handling method choices to see what predictors turn out to be significant!)"),
                          textOutput("coxphselected"),
                          br(),
                          h3("Diagnostics: Cox-Snell Goodness of Fit plots"),
                          h4("This refreshes for every new model that we build! The closer the plot is to the line y=x, the better the fit."),
                          plotOutput("coxph.CS.plot"),
                           ),
                  
                  tabPanel("Log-Rank Test",
                  br(),
                  h3("We compare survival between females (sex=2) and males (sex=1) using the log-rank test."),
                  h4("Specify a time interval to consider for this comparison:"),
                  br(),
                  sliderInput("logranktime", "Choose a time interval:", value = c(10, 20), min = min(lung$time), max = max((lung$time))),
                  br(),
                  verbatimTextOutput("logranktest"),
                  br(),
                  textOutput("logrankpvalue"),
                  plotOutput("logrankplot"),
                  )
                  
                  )
                  
      )
    )))

