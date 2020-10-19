# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
h = 100

prior_step_size = .05
mean_step_size = 5
var_step_size = 1
lapse_step_size = .01

shinyUI(fluidPage(
  fluidRow(
    h4("Specify parameters"),
    fluidRow(
      column(3,
             h5("Prior probability of categories")),
      column(8, offset = 1, 
             h5("Category-specific cue distributions"))),
    fluidRow(
      column(3, 
             sliderInput('prior1_c1', 'p(/b/)', 
                         min=0, 
                         max=1, 
                         value=.5, 
                         step= prior_step_size, 
                         round=0)
             #             actionButton('reset', "Reset values")
      ),
      column(8, offset = 1, style = "background-color:#F8766D;",
             column(6,
                    sliderInput('mu1_c1', 'mean(/b/)', 
                                min=-100, 
                                max=90, 
                                value=0, 
                                step= mean_step_size, 
                                round=0
                    )),
             column(6,
                    sliderInput('sd1_c1', 'stdev(/b/)', 
                                min=0, 
                                max=50, 
                                value=14, 
                                step= var_step_size, 
                                round=0
                    )))),
    fluidRow(
      column(3, 
             h5("Attentional lapse"),
             fluidRow(
               column(5,
                 numericInput('lapse_rate', 'Rate', 
                          min=0, 
                          max=1, 
                          value=0, 
                          step= lapse_step_size)
               ),
               column(5,
                 htmlOutput("numericInputUI")
               ),
               column(2,
                      checkboxInput('lapse_bias_as_prior', 'Prior\nas bias?', 
                                   value= TRUE)
               )
             )
      ),
      column(8, offset = 1, style = "background-color:#00BFC4;",
             column(6,
                    sliderInput('mu1_c2', 'mean(/p/)', 
                                min=-100, 
                                max=90, 
                                value=50, 
                                step= mean_step_size, 
                                round=0
                    )),
             column(6,
                    sliderInput('sd1_c2', 'stdev(/p/)', 
                                min=0, 
                                max=50, 
                                value=14, 
                                step= var_step_size, 
                                round=0
                    ))))
  ),
  
  hr(),
  
  tabsetPanel(
    tabPanel("Categorization",
             h4("Ideal categorization for known cue likelihood and category priors"),
             
             fluidRow(
               column(4,
                      plotOutput('p.prior', height = h), 
                      plotOutput('p.means', height = h),
                      plotOutput('p.sds', height = h)
               ),
               column(8,
                      plotOutput('p.likelihood', height = h),
                      plotOutput('p.posterior', height = h),
                      plotOutput('p.categorization', height = h)
               )
             )
    ),
    tabPanel("Generate Data", 
             h4("Generate data from known category-specific cue distributions"),
             
             sidebarLayout(
                sidebarPanel(
                  numericInput('LenData', "Number of datapoints",
                               min = 1,
                               value = 10)
                ),
                mainPanel(
                  plotOutput('p.empirical.density', height = h),
                  dataTableOutput('t.data')
                )
              )
    ),
    tabPanel("Belief-updating", 
             h4("Updating beliefs and categorization based on incrementally presented data"),
             
             fluidRow(
               column(4,
                      plotOutput('p.ibbu.cat_prior', height = h), 
                      plotOutput('p.ibbu.cue_prior_mean', height = h),
                      plotOutput('p.ibbu.cue_prior_sd', height = h)
               ),
               column(8,
                      plotOutput('p.ibbu.likelihood', height = h),
                      plotOutput('p.ibbu.posterior', height = h),
                      plotOutput('p.ibbu.categorization', height = h)
               )
             )
    )
  )
))