# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("global.R")

# Input properties
h = 100
prior_step_size = .05
mean_step_size = 5
var_step_size = 1
lapse_step_size = .01

shinyUI(
  fluidPage(
    fluidRow(
      h4("Specify parameters"),
      fluidRow(
        column(3,
               h5("Prior probability of categories")),
        column(8, offset = 1, 
               h5("Category-specific cue distributions"))),
      fluidRow(
        column(3, 
               sliderInput('prior_c1', paste0('p(', c1, ')'), 
                           min=0, 
                           max=1, 
                           value=.5, 
                           step= prior_step_size, 
                           round=0)),
        column(8, offset = 1, style = "background-color:#F8766D;",
               column(6,
                      sliderInput('mu_c1', paste0('mean(', c1, ')'), 
                                  min = min_cue, 
                                  max = max_cue, 
                                  value = value_mean_c1, 
                                  step = mean_step_size, 
                                  round = 0)),
               column(6,
                      sliderInput('sd_c1', paste0('SD(', c1, ')'), 
                                  min = 0, 
                                  max = max_sd, 
                                  value = value_sd_c1, 
                                  step = var_step_size, 
                                  round = 0)))),
      fluidRow(
        column(3, NULL),
        column(8, offset = 1, style = "background-color:#00BFC4;",
               column(6,
                      sliderInput('mu_c2', paste0('mean(', c2, ')'), 
                                  min = min_cue, 
                                  max = max_cue, 
                                  value = value_mean_c2, 
                                  step = mean_step_size, 
                                  round = 0)),
               column(6,
                      sliderInput('sd_c2', paste0('SD(', c2, ')'), 
                                  min = 0, 
                                  max = max_sd, 
                                  value = value_sd_c2, 
                                  step = var_step_size, 
                                  round = 0)))),
    fluidRow(
      column(3, 
             h5("Attentional lapsing"),
             fluidRow(style = "background-color:#DDDDDD;", 
               column(4, 
                      numericInput('lapse_rate', 'Rate', 
                                   min = 0, 
                                   max = 1, 
                                   value = 0, 
                                   step = lapse_step_size)),
               column(4,
                      htmlOutput("numericInputUI")),
               column(3,
                      checkboxInput('lapse_bias_as_prior', 'Prior\nas bias?', 
                                    value = TRUE)))),
      column(8, offset = 1, 
             h5("Perceptual noise"),
             column(6,
                    sliderInput('epsilon', "Just-noticable difference (JND)", 
                                min = 0, 
                                max = 100, 
                                value = value_epsilon, 
                                step = mean_step_size, 
                                round = 0)),
             column(6, style = "background-color:#DDDDDD;",
                    sliderInput('sd_noise', paste0('SD(', cue, ')'), 
                                min = 0, 
                                max = max_sd, 
                                value = value_sd_noise, 
                                step = var_step_size, 
                                round = 0))))),
  
  hr(),
  
  tabsetPanel(
    tabPanel("Noisy perception",
             
             fluidRow(
               column(4,
                      div("This tab illustrates Bayesian inference over noisy perceptual input for both categorization 
                          (e.g., Clayards et al., 2008) and discimination (e.g,. Feldman et al., 2009)."),
                      br(),
                      plotOutput('p.prior', height = h), 
                      plotOutput('p.means', height = h),
                      plotOutput('p.sds', height = h)),
               column(8,
                      plotOutput('p.likelihood', height = h),
                      plotOutput('p.likelihood.noise', height = h),
                      plotOutput('p.posterior', height = h),
                      
                      h4("Ideal categorization"),
                      plotOutput('p.categorization', height = h),
                      
                      h4("Ideal discrimination"),
                      plotOutput('p.discrimination', height = h)))),
    tabPanel("Generate Data", 
             h4("Generate data from known category-specific cue distributions"),
             
             sidebarLayout(
               sidebarPanel(
                 numericInput('LenData', "Number of datapoints",
                              min = 1,
                              value = 10)),
               mainPanel(
                 plotOutput('p.empirical.density', height = h),
                 dataTableOutput('t.data')))),
    tabPanel("Belief-updating", 
             h4("Updating beliefs and categorization based on incrementally presented data"),
             
             fluidRow(
               column(4,
                      plotOutput('p.ibbu.cat_prior', height = h), 
                      plotOutput('p.ibbu.cue_prior_mean', height = h),
                      plotOutput('p.ibbu.cue_prior_sd', height = h)),
               column(8,
                      plotOutput('p.ibbu.likelihood', height = h),
                      plotOutput('p.ibbu.posterior', height = h),
                      plotOutput('p.ibbu.categorization', height = h)))))
))
