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
      h4("Specify beliefs"),
      fluidRow(
        column(3,
               h5("Prior probability of categories")),
        column(8, offset = 1, 
               h5("Category-specific cue distributions (likelihoods)"))),
      fluidRow(
        column(3, 
               sliderInput('prior_c1', paste0('p(', c1, ')'), 
                           min = 0, 
                           max = 1, 
                           value = value_prior_c1, 
                           step = prior_step_size, 
                           round = 0)),
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
                    NULL),
                    # sliderInput('epsilon', "Just-noticable difference (JND)", 
                    #             min = 0, 
                    #             max = 100, 
                    #             value = value_epsilon, 
                    #             step = mean_step_size, 
                    #             round = 0)),
             column(6, style = "background-color:#DDDDDD;",
                    sliderInput('sd_noise', paste0('SD(', cue, ')'), 
                                min = 0, 
                                max = max_sd, 
                                value = value_sd_noise, 
                                step = var_step_size, 
                                round = 0))))),
  
  hr(),
  
  tabsetPanel(
    # tabPanel("Debugging",
    #          fluidRow(
    #            column(12,
    #                   verbatimTextOutput("info")))
    # ),
    tabPanel("Categorization",
             
             fluidRow(
               column(4,
                      div("This tab illustrates Bayesian inference over noisy perceptual input for categorization 
                          (e.g., Clayards et al., 2008)."),
                      br(),
                      plotOutput('p.prior.categorization', height = h), 
                      plotOutput('p.means.sds.categorization', height = 2 * h)),
               column(8,
                      plotOutput('p.likelihood.categorization', height = h),
                      plotOutput('p.likelihood.noise.categorization', height = h),
                      plotOutput('p.unnormalized.posterior.categorization', height = h),
                      
                      h4("Ideal categorization"),
                      plotOutput('p.categorization', height = h))),
             fluidRow(
               column(4,
                      numericInput('n_tokens', 'Sample tokens to classify:',
                                   min = 1,
                                   value = 50,
                                   step = 1),
                      plotOutput('p.tokens.categorization', height = h)),
               column(4,
                      h4("Categorization accuracy"),
                      plotOutput('p.accuracy.categorization', height = h)))
             ),
    
    tabPanel("Discrimination",
             
             fluidRow(
               column(4,
                      div("This tab illustrates Bayesian inference over noisy perceptual input for perceptual discrimination (e.g,. Feldman et al., 2009)."),
                      br(),
                      plotOutput('p.prior.discrimination', height = h), 
                      plotOutput('p.means.sds.discrimination', height = 2 * h)),
               column(8,
                      plotOutput('p.likelihood.discrimination', height = h),
                      plotOutput('p.likelihood.noise.discrimination', height = h),
                      plotOutput('p.unnormalized.posterior.discrimination', height = h),
                      
                      h4("Ideal discrimination"),
                      plotOutput('p.warping', height = h),
                      plotOutput('p.discrimination', height = h)))),
    
    tabPanel("Specify exposure", 

             fluidRow(
               column(4,
                      div("Specify two exposure conditions. The defaults are taken from Clayards et al. (2008)."),
                      br(),
                      plotOutput('p.prior.exposure', height = h), 
                      plotOutput('p.means.sds.exposure', height = 2 * h)),
               column(8,
                      fluidRow(
                        h4("Parameters for exposure condition A"),
                        fluidRow(
                          column(3,
                                 h5("Prior probability of categories")),
                          column(8, offset = 1, 
                                 h5("Category-specific cue distributions (likelihoods)"))),
                        fluidRow(
                          column(3, 
                                 sliderInput('prior_c1_A', paste0('p(', c1, ')'), 
                                             min = 0, 
                                             max = 1, 
                                             value = value_prior_c1_A, 
                                             step = prior_step_size, 
                                             round =0)),
                          column(8, offset = 1, style = "background-color:#F8766D;",
                                 column(6,
                                        sliderInput('mu_c1_A', paste0('mean(', c1, ')'), 
                                                    min = min_cue, 
                                                    max = max_cue, 
                                                    value = value_mean_c1_A, 
                                                    step = mean_step_size, 
                                                    round = 0)),
                                 column(6,
                                        sliderInput('sd_c1_A', paste0('SD(', c1, ')'), 
                                                    min = 0, 
                                                    max = max_sd, 
                                                    value = value_sd_c1_A, 
                                                    step = var_step_size, 
                                                    round = 0)))),
                        fluidRow(
                          column(3),
                          column(8, offset = 1, style = "background-color:#00BFC4;",
                                 column(6,
                                        sliderInput('mu_c2_A', paste0('mean(', c2, ')'), 
                                                    min = min_cue, 
                                                    max = max_cue, 
                                                    value = value_mean_c2_A, 
                                                    step = mean_step_size, 
                                                    round = 0)),
                                 column(6,
                                        sliderInput('sd_c2_A', paste0('SD(', c2, ')'), 
                                                    min = 0, 
                                                    max = max_sd, 
                                                    value = value_sd_c2_A, 
                                                    step = var_step_size, 
                                                    round = 0))))),
                      fluidRow(
                        h4("Parameters for exposure condition B"),
                        fluidRow(
                          column(3,
                                 h5("Prior probability of categories")),
                          column(8, offset = 1, 
                                 h5("Category-specific cue distributions (likelihoods)"))),
                        fluidRow(
                          column(3, 
                                 sliderInput('prior_c1_B', paste0('p(', c1, ')'), 
                                             min = 0, 
                                             max = 1, 
                                             value = value_prior_c1_B, 
                                             step = prior_step_size, 
                                             round =0)),
                          column(8, offset = 1, style = "background-color:#F8766D;",
                                 column(6,
                                        sliderInput('mu_c1_B', paste0('mean(', c1, ')'), 
                                                    min = min_cue, 
                                                    max = max_cue, 
                                                    value = value_mean_c1_B, 
                                                    step = mean_step_size, 
                                                    round = 0)),
                                 column(6,
                                        sliderInput('sd_c1_B', paste0('SD(', c1, ')'), 
                                                    min = 0, 
                                                    max = max_sd, 
                                                    value = value_sd_c1_B, 
                                                    step = var_step_size, 
                                                    round = 0)))),
                        fluidRow(
                          column(3),
                          column(8, offset = 1, style = "background-color:#00BFC4;",
                                 column(6,
                                        sliderInput('mu_c2_B', paste0('mean(', c2, ')'), 
                                                    min = min_cue, 
                                                    max = max_cue, 
                                                    value = value_mean_c2_B, 
                                                    step = mean_step_size, 
                                                    round = 0)),
                                 column(6,
                                        sliderInput('sd_c2_B', paste0('SD(', c2, ')'), 
                                                    min = 0, 
                                                    max = max_sd, 
                                                    value = value_sd_c2_B, 
                                                    step = var_step_size, 
                                                    round = 0)))))),
             
             fluidRow(
               column(3, 
                 numericInput('LenData', "Number of datapoints",
                              min = 1,
                              value = value_n_exposure)),
               column(8, offset = 1,
                 plotOutput('p.exposure.density', height = h),
                 dataTableOutput('t.data'))))),
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
