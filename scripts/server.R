library(shiny)
library(ggplot2)
library(plyr)
theme_set(theme_bw())


## ------------------------------------------------------------
# To do
## ------------------------------------------------------------
# A) add option box to show text on classification plot that gives 
#   1) point of maximal ambiguity
#   2) linear term of slope in cue
#   3) quadratic term of slope in cue (can be NA for equal variance)

# B) add other choice rules.

## ------------------------------------------------------------
# Known bugs
## ------------------------------------------------------------

# 1) lapse_bias does not update when lapse_bias == prior is toggled on again.

## ------------------------------------------------------------
# Later to be sourced
## ------------------------------------------------------------

source("global.R")

# Returns the colors from ggplot
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  #Function to get ggplot colors
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

# Rescales x values between (min, max) to be between range (a,b)
rescale <- function(x, min, max, range_a, range_b){
  return(((range_b - range_a)*(x - min) / (max - min)) + range_a)
}

# Given means and variances for likelihood of c1, c2, multiply by the category prior to obtain posterior
get_categorization <- function(x, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1 = 0.5, 
                               lapse_rate = 0, lapse_bias = prior_c1) {
  prior_c2 <- 1 - prior_c1
  c1 = dnorm(x, mu_c1, sd_c1)
  c2 = dnorm(x, mu_c2, sd_c2)
  return((1-lapse_rate) *
           (c1 * prior_c1) / ((c1*prior_c1) + (c2 * prior_c2)) + 
           (lapse_rate*lapse_bias))
}

combine_likelihood_prior <- function(x, mu_c1, sd_c1, prior_c = 0.5) {
  c1 = dnorm(x, mu_c1, sd_c1)
  return(c1 * prior_c)
}


## ------------------------------------------------------------
# SERVER starts here
## ------------------------------------------------------------

shinyServer(function(input, output, session) {
  xs = seq(-100, 90, 5)
  
#  observeEvent(input$reset, {
#    updateSliderInput(session, input$prior1_c1, value = default_prior1_c1)
#  })
  
  ## ------------------------------------------------------------
  # dynamically adjusted UI input elements
  ## ------------------------------------------------------------
  output$numericInputUI <- renderUI({
    if (input$lapse_bias_as_prior == FALSE) {
      numericInput('lapse_bias', '/b/-bias', 
                   min=0, 
                   max=1, 
                   value= .5, 
                   step= .01)
    }
  })
   
  ## ------------------------------------------------------------
  # UI output elements
  ## ------------------------------------------------------------
  output$p.prior <- renderPlot({
    prior_c1 = input$prior_c1
    prior_c2 = 1 - input$prior_c1

    priors = data.frame(Category = c("/b/", "/p/"), 
                        Prior = c(prior_c1, 
                                  1-prior_c1))
    
    ggplot(priors, aes(x = Category, y = Prior, fill=Category)) +
      geom_bar(stat = "identity") + 
      ggtitle("Category priors") + 
      theme(legend.position="none")
  })
 
  output$p.means <- renderPlot({
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    
    means = data.frame(Category = c("/b/", "/p/"), 
                       VOT = c(mu_c1, 
                               mu_c2))
    
    ggplot(means, aes(x = Category, y = VOT, color=Category)) +
      geom_point() +
      ggtitle("Category means") + 
      theme(legend.position="none")
  })
   
  output$p.sds <- renderPlot({
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    
    sds = data.frame(Category = c("/b/", "/p/"), 
                     VOT_SD = c(sd_c1, 
                                sd_c2))
    
    ggplot(sds, aes(x = Category, y = VOT_SD, color=Category)) +
      geom_point() +
      ggtitle("Category SDs") + 
      theme(legend.position="none") 
  })  
  
  output$p.likelihood <- renderPlot({
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    
    ggplot(data.frame(x = xs), aes(x = xs)) + 
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c1, 
                                sd = sd_c1), 
                    aes(col = '/b/')) +
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c2, 
                                sd = sd_c2), 
                    aes(col = '/p/')) + 
      scale_colour_manual("Category", values = c('/b/' = ggplotColours(2)[1], '/p/' = ggplotColours(2)[2])) + 
      xlab("VOT (ms)") + ylab("Likelihood\n(w/o noise)") + 
      theme(axis.title.x=element_blank(), legend.position = "none")
  })  
  
  output$p.likelihood <- renderPlot({
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    sd_noise = input$sd_noise
    
    ggplot(data.frame(x = xs), aes(x = xs)) + 
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c1, 
                                sd = sqrt(sd_c1^2 + sd_noise^2)), 
                    aes(col = '/b/')) +
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c2, 
                                sd = sqrt(sd_c2^2 + sd_noise^2)), 
                    aes(col = '/p/')) + 
      scale_colour_manual("Category", values = c('/b/' = ggplotColours(2)[1], '/p/' = ggplotColours(2)[2])) + 
      xlab("VOT (ms)") + ylab("Likelihood\n(w/ noise)") + 
      theme(axis.title.x=element_blank(), legend.position = "none")
  })  

  output$p.posterior <- renderPlot({
    prior_c1 = input$prior_c1
    prior_c2 = 1 - input$prior_c1
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2

    ggplot(data.frame(x = xs), aes(x = xs)) + 
      stat_function(fun = combine_likelihood_prior, 
                    args = list(mu_c1 = mu_c1, 
                                sd_c1 = sd_c1, 
                                prior_c =  prior_c1), 
                    aes(col='/b/')) +
      stat_function(fun = combine_likelihood_prior, 
                    args = list(mu_c1 = mu_c2, 
                                sd_c1 = sd_c2, 
                                prior_c = 1-prior_c1), 
                    aes(col='/p/')) +
      scale_colour_manual("Category", values = c('/b/' = ggplotColours(2)[1], '/p/' = ggplotColours(2)[2])) + 
      xlab("VOT (ms)") + ylab("Posterior\n") + 
      theme(axis.title.x=element_blank(), legend.position = "none")
  })  
  
  output$p.categorization <- renderPlot({
    prior_c1 = input$prior_c1
    prior_c2 = 1 - input$prior_c1
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    lapse_rate = input$lapse_rate
    lapse_bias = if (!is.null(input$lapse_bias)) { input$lapse_bias } else { input$prior_c1 }

    ggplot(data.frame(xs), aes(x = xs)) + 
      stat_function(fun = get_categorization, 
                    args = list(mu_c1 = mu_c1, 
                                sd_c1 = sd_c1, 
                                mu_c2 = mu_c2, 
                                sd_c2 = sd_c2, 
                                prior_c1 = prior_c1,
                                lapse_rate = lapse_rate,
                                lapse_bias = lapse_bias)) +
      ylim(c(0,1)) +
      xlab("VOT (ms)") + ylab("Probability of\n'/b/' Response")
  })  
  
  output$t.data <- renderDataTable({
    mu = c(input$mu_c1, input$mu_c2)
    sd = c(input$sd_c1, input$sd_c2)
    
    cat_labels = rbinom(input$LenData, 1, input$prior_c1)
    cue = sapply(cat_labels, 
                FUN = function(x) {
                  x = abs(x - 2)
                  rnorm(1, mu[x], sd[x])
                })
    
    data.frame(Category = ifelse(cat_labels, "/b/", "/p/"),
               VOT = cue) #selection = list(target = 'row', selected = c(1:input$LenData)))
  }, options = list(lengthMenu = c(25, 50, 100), pageLength = 25))
  
#  output$p.empirical.density <- renderPlot({
#    ggplot(input$t.data_rows_selected, aes(x = VOT, fill=Category)) +
#      geom_density() +
#      scale_colour_manual("Category", values = c('/b/' = ggplotColours(2)[1], '/p/'=ggplotColours(2)[2])) + 
#      ylab("Density") + xlab("VOT (ms)") + theme(axis.title.x=element_blank(), legend.position = "none")
#  })
  
  
#   output$plotMyCow <- renderPlot({
#     xs = seq(-30, 80, 10)
#     prior1_c1 = input$prior1_c1
#     prior1_c2 = input$prior2_c1
#     mu1_c1 = input$mu1_c1
#     mu1_c2 = input$mu1_c2
#     sd1_c1 = input$sd1_c1
#     sd1_c2 = input$sd1_c2
#     
#     priors = data.frame(Category = c("B", "P"), 
#                         Prior = c(prior1_c1, 
#                                   1-prior1_c1))
#     
#     p.prior = ggplot(priors, aes(x = Category, y = Prior, fill=Category)) +
#       geom_bar(stat = "identity") + 
#       ggtitle("Category Prior") + 
#       theme(legend.position="none")
#     
#     #Plot means
#     means = data.frame(Category = c("B", "P"), 
#                        VOT = c(mu1_c1, 
#                                mu1_c2))
#     
#     p.means = ggplot(means, aes(x = Category, y = VOT, color=Category)) +
#       geom_point() +
#       ggtitle("Category Means") + 
#       theme(legend.position="none")
#     
#     #Plot variances
#     sds = data.frame(Category = c("B", "P"), 
#                      VOT_SD = c(sd1_c1, 
#                                 sd1_c2))
#     
#     p.sds = ggplot(sds, aes(x = Category, y = VOT_SD, color=Category)) +
#       geom_point() +
#       ggtitle("Category Standard Deviation") + 
#       theme(legend.position="none") 
#     
#     #Plot likelihood
#     p.likelihood = ggplot(data.frame(x = xs), aes(x = xs)) + 
#       stat_function(fun = dnorm, 
#                     args = list(mean = mu1_c1, 
#                                 sd = sd1_c1), 
#                     aes(col='/p/')) +
#       stat_function(fun = dnorm, 
#                     args = list(mean = mu1_c2, 
#                                 sd = sd1_c2), 
#                     aes(col='/b/')) + 
#       scale_colour_manual("Category", values = c('/p/' = ggplotColours(2)[1], '/b/'=ggplotColours(2)[2])) + 
#       ylab("Likelihood") + xlab("VOT (ms)") + theme(axis.title.x=element_blank(), legend.position = "none")
#     
#     #Plot posterior 
#     p.posterior= ggplot(data.frame(x = xs), aes(x = xs)) + 
#       stat_function(fun = combine_likelihood_prior, 
#                     args = list(mu_c1 = mu1_c1, 
#                                 sd_c1 = sd1_c1, 
#                                 prior_c =  prior1_c1), 
#                     aes(col='/b/')) +
#       stat_function(fun = combine_likelihood_prior, 
#                     args = list(mu_c1 = mu1_c2, 
#                                 sd_c1 = sd1_c2, 
#                                 prior_c = 1-prior1_c1), 
#                     aes(col='/p/')) +
#       scale_colour_manual("Category", values = c('/p/' = ggplotColours(2)[1], '/b/'=ggplotColours(2)[2])) + 
#       ylab("Posterior") + xlab("VOT (ms)") + theme(axis.title.x=element_blank(), legend.position = "none")
#     
#     #Plot categorization
#     p.categorization = ggplot(data.frame(xs), aes(x = xs)) + 
#       stat_function(fun = get_categorization, 
#                     args = list(mu_c1 = mu1_c1, 
#                                 sd_c1 = sd1_c1, 
#                                 mu_c2 = mu1_c2, 
#                                 sd_c2 = sd1_c2, 
#                                 prior_c1 = prior1_c1)) +
#       ylim(c(0,1)) +
#       xlab("VOT (ms)") + ylab("Probability of '/b/' Response")
# 
#     show(p.likelihood) 
#       
    #Combine the six plots from above
#      ggdraw() +
#         draw_plot(p.likelihood, .3, .65, .7, .35) +
#       draw_plot(p.posterior, .3, .3, .7, .35) + 
#       draw_plot(p.categorization, .3, 0, .7, .3) + 
#       draw_plot(p.prior, 0, .66, .3, .33) +
#       draw_plot(p.means, 0, .33, .3, .33) +
#       draw_plot(p.sds, 0, 0, .3, .33)
#  })  
    
# FOR TWO INPUTS  output$plotMyCow <- renderPlot({
#     xs = seq(-30, 80, 10)
#     
#     priors = data.frame(Category = c("B", "P", "B", "P"), 
#                         Prior = c(prior1_c1, 
#                                   1-prior2_c1, 
#                                   prior2_c1, 
#                                   1-prior2_c1),
#                         Likelihood = c("First", "First", "Second", "Second"))
#     
#     p.prior = ggplot(priors, aes(x = Category, y = Prior, fill=Category)) +
#       geom_bar(stat = "identity") + 
#       ggtitle("Category Prior") + 
#       theme(legend.position="none") + 
#       facet_wrap(~Likelihood)
#     
#     #Plot means
#     means = data.frame(Category = c("B", "P", "B", "P"), 
#                        VOT = c(mu1_c1, 
#                                mu1_c2, 
#                                mu2_c1, 
#                                mu2_c2),
#                        Likelihood = c("First", "First", "Second", "Second"))
#     
#     p.means = ggplot(means, aes(x = Category, y = VOT, color=Category)) +
#       geom_point() +
#       ggtitle("Category Means") + 
#       theme(legend.position="none") + 
#       facet_wrap(~Likelihood)
#     
#     #Plot variances
#     sds = data.frame(Category = c("B", "P", "B", "P"), 
#                      VOT_SD = c(sd1_c1, 
#                                 sd1_c2, 
#                                 sd2_c1, 
#                                 sd2_c2),
#                      Likelihood = c("First", "First", "Second", "Second"))
#     
#     p.sds = ggplot(sds, aes(x = Category, y = VOT_SD, color=Category)) +
#       geom_point() +
#       ggtitle("Category Standard Deviation") + 
#       theme(legend.position="none") + 
#       facet_wrap(~Likelihood)
#     
#     #Plot likelihood
#     p.likelihood = ggplot(data.frame(x = xs), aes(x = xs)) + 
#       stat_function(fun = dnorm, 
#                     args = list(mean = mu1_c1, 
#                                 sd = sd1_c1), 
#                     aes(col='/p/', linetype="First")) +
#       stat_function(fun = dnorm, 
#                     args = list(mean = mu1_c2, 
#                                 sd = sd1_c2), 
#                     aes(col='/b/', linetype="First")) + 
#       stat_function(fun = dnorm, 
#                     args = list(mean = mu2_c1, 
#                                 sd = sd2_c1), 
#                     aes(col='/p/', linetype="Second")) +
#       stat_function(fun = dnorm, 
#                     args = list(mean = mu2_c2, 
#                                 sd = sd2_c2), 
#                     aes(col='/b/', linetype="Second"))+
#       scale_colour_manual("Category", values = c('/p/' = ggplotColours(2)[1], '/b/'=ggplotColours(2)[2])) + 
#       scale_linetype_manual("Likelihood", values = c('First' = 'solid', 'Second' = 'dashed')) + 
#       ylab("Likelihood") + xlab("VOT (ms)") + theme(axis.title.x=element_blank())
#     
#     #Plot posterior 
#     p.posterior= ggplot(data.frame(x = xs), aes(x = xs)) + 
#       stat_function(fun = combine_likelihood_prior, 
#                     args = list(mu_c1 = mu1_c1, 
#                                 sd_c1 = sd1_c1, 
#                                 prior_c =  prior1_c1), 
#                     aes(col='/b/', linetype="First")) +
#       stat_function(fun = combine_likelihood_prior, 
#                     args = list(mu_c1 = mu1_c2, 
#                                 sd_c1 = sd1_c2, 
#                                 prior_c = 1-prior1_c1), 
#                     aes(col='/p/', linetype="First")) +
#       stat_function(fun = combine_likelihood_prior, 
#                     args = list(mu_c1 = mu2_c1, 
#                                 sd_c1 = sd2_c1, 
#                                 prior_c =  prior2_c1), 
#                     aes(col='/b/', linetype="Second")) +
#       stat_function(fun = combine_likelihood_prior, 
#                     args = list(mu_c1 = mu2_c2, 
#                                 sd_c1 = sd2_c2, 
#                                 prior_c = 1-prior2_c1), 
#                     aes(col='/p/', linetype="Second")) +
#       scale_colour_manual("Category", values = c('/p/' = ggplotColours(2)[1], '/b/'=ggplotColours(2)[2])) + 
#       scale_linetype_manual("Likelihood", values = c('First' = 'solid', 'Second' = 'dashed')) + 
#       ylab("Posterior") + xlab("VOT (ms)") + theme(axis.title.x=element_blank())
#     
#     #Plot categorization
#     p.categorization = ggplot(data.frame(xs), aes(x = xs)) + 
#       stat_function(fun = get_categorization, 
#                     args = list(mu_c1 = mu1_c1, 
#                                 sd_c1 = sd1_c1, 
#                                 mu_c2 = mu1_c2, 
#                                 sd_c2 = input$sd1_c2, 
#                                 prior_c1 = input$prior1_c1), 
#                     aes(linetype="First")) +
#       stat_function(fun = get_categorization, 
#                     args = list(mu_c1 = input$mu2_c1, 
#                                 sd_c1 = input$sd2_c1, 
#                                 mu_c2 = input$mu2_c2, 
#                                 sd_c2 = input$sd2_c2, 
#                                 prior_c1 = input$prior2_c1), 
#                     aes(linetype="Second")) +
#       ylim(c(0,1)) +
#       scale_linetype_manual('Likelihood', values = c('First' = 'solid', 'Second' = 'dashed')) + 
#       xlab("VOT (ms)") + ylab("Probability of '/b/' Response")
#     
#     #Combine the six plots from above
#     ggdraw() +
#       draw_plot(p.likelihood, .3, .65, .7, .35) +
#       draw_plot(p.posterior, .3, .3, .7, .35) + 
#       draw_plot(p.categorization, .3, 0, .7, .3) + 
#       draw_plot(p.prior, 0, .66, .3, .33) +
#       draw_plot(p.means, 0, .33, .3, .33) +
#       draw_plot(p.sds, 0, 0, .3, .33)
#   })  
  
#   output$p.prior <- renderPlot({   
#     priors = data.frame(Category = c("B", "P", "B", "P"), 
#                         Prior = c(input$prior1_c1, 
#                                   1-input$prior2_c1, 
#                                   input$prior2_c1, 
#                                   1-input$prior2_c1),
#                         Likelihood = c("First", "First", "Second", "Second"))
#     
#     ggplot(priors, aes(x = Category, y = Prior, fill=Category)) +
#       geom_bar(stat = "identity") + 
#       ggtitle("Prior (Category)") + 
#       theme(legend.position="none") + 
#       facet_wrap(~Likelihood)
#     })

  
})

