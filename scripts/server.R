# Libraries ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(rlang)

# Graphics
theme_set(
  theme_bw() + 
    theme(legend.position = "none"))

# 
# add noise to exposure sample


# A) add option box to show text on classification plot that gives 
#   1) point of maximal ambiguity
#   2) linear term of slope in cue
#   3) quadratic term of slope in cue (can be NA for equal variance)

# B) add other choice rules.

# Known bugs ------------------------------------------------------------


# 1) lapse_bias does not update when lapse_bias == prior is toggled on again.

# Later to be sourced -------------------------------------------------
source("global.R")

# Returns the colors from ggplot
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

# Rescales x values between (min, max) to be between range (a,b)
rescale <- function(x, min, max, range_a, range_b){
  return(((range_b - range_a)*(x - min) / (max - min)) + range_a)
}


get_unnormalized_posterior <- function(x, mu_c, sd_c, prior_c = 0.5) {
  c = dnorm(x, mu_c, sd_c)
  return(
    c * prior_c)
}

get_posterior <- function(x, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1 = 0.5) {
  c1 = dnorm(x, mu_c1, sd_c1)
  c2 = dnorm(x, mu_c2, sd_c2)
  return(
    (c1 * prior_c1) / ((c1 * prior_c1) + (c2 * (1 - prior_c1))))
}

get_categorization <- function(x, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1 = 0.5, 
                               lapse_rate = 0, lapse_bias = prior_c1) {
  prior_c2 <- 1 - prior_c1
  c1 = dnorm(x, mu_c1, sd_c1)
  c2 = dnorm(x, mu_c2, sd_c2)
  return(
    (1-lapse_rate) *
      get_posterior(x, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1) + 
      (lapse_rate * lapse_bias))
}

get_expected_target_component <- function(x, 
                                          mu_c_target, sd_c_target, 
                                          mu_c_non_target, sd_c_non_target, 
                                          prior_c_target, sd_noise) {
  return(
    get_posterior(x, mu_c_target, sd_c_target, mu_c_non_target, sd_c_non_target, prior_c_target) * 
      ((sd_c_target^2 * x + sd_noise^2 * mu_c_target) / (sd_c_target^2 + sd_noise^2)))
}

get_expected_target <- function(x, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1 = 0.5, sd_noise) {
  return(
    get_expected_target_component(x, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1, sd_noise) +
      get_expected_target_component(x, mu_c2, sd_c2, mu_c1, sd_c1, 1 - prior_c1, sd_noise))
}

get_perceptual_distance <- function(x, distance, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1 = 0.5, sd_noise) {
  return(
    abs(
      get_expected_target(x + distance/2, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1, sd_noise) -
        get_expected_target(x - distance/2, mu_c1, sd_c1, mu_c2, sd_c2, prior_c1, sd_noise)))
}


# SERVER starts here --------------------------------------------------------------

shinyServer(function(input, output, session) {
  xs = seq(min_cue, max_cue, step_cue)
  
  #  observeEvent(input$reset, {
  #    updateSliderInput(session, input$prior1_c1, value = default_prior1_c1)
  #  })
  
  # Reactive for debugging ----------------------------------------
  # output$info <- renderPrint({
  #   prior_c1 = input$prior_c1
  #   mu_c1 = input$mu_c1
  #   mu_c2 = input$mu_c2
  #   sd_c1 = input$sd_c1
  #   sd_c2 = input$sd_c2
  #   sd_noise = input$sd_noise
  #   lapse_rate = input$lapse_rate
  #   lapse_bias = if (!is.null(input$lapse_bias)) { input$lapse_bias } else { input$prior_c1 }
  # 
  #   print(
  #     currentData() %>%
  #       mutate(
  #         categorization =
  #           get_categorization(
  #               x = cue,
  #               mu_c1 = mu_c1,
  #               sd_c1 = sd_c1,
  #               mu_c2 = mu_c2,
  #               sd_c2 = sd_c2,
  #               prior_c1 = prior_c1,
  #               lapse_rate = lapse_rate,
  #               lapse_bias = lapse_bias),
  #         accuracy = ifelse(category == c1, categorization, 1 - categorization))
  #     )
  # })
  
  output$info <- renderPrint({
    req(input$token_click)
    x <- round(input$token_click$x, 2)
    y <- round(input$token_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  # Dynamically adjusted UI input elements ----------------------------------------
  output$numericInputUI <- renderUI({
    if (input$lapse_bias_as_prior == FALSE) {
      numericInput('lapse_bias', '/b/-bias', 
                   min=0, 
                   max=1, 
                   value= .5, 
                   step= .01)
    }
  })
  
  # reactives  ------------------------------------------------------------
  currentData <- reactive({
    crossing(
      token = 1:input$n_tokens,
      category = c(c1, c2)) %>%
      mutate(
        cue = rnorm(
          nrow(.), 
          mean = ifelse(category == c1, input$mu_c1, input$mu_c2),
          sd = ifelse(category == c1, input$sd_c1, input$sd_c2)))  
  })
  
  currentPrior <- reactive({
    prior_c1 = input$prior_c1
    prior_c2 = 1 - input$prior_c1
    
    priors = data.frame(Category = c(c1, c2), 
                        Prior = c(prior_c1, 
                                  1 - prior_c1))
    
    ggplot(priors, aes(x = Category, y = Prior, fill = Category)) +
      geom_bar(stat = "identity") + 
      ggtitle("Category priors") + 
      theme(axis.title.x = element_blank())
  })
  
  currentMeansSDs <- reactive({
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    
    data.frame(
      Category = c(c1, c2), 
      mean = c(mu_c1, mu_c2),
      sd = c(sd_c1, sd_c2)) %>%
      ggplot(aes(
        x = mean, 
        y = sd, 
        color = Category,
        label = Category)) +
      geom_text(fontface = "bold") +
      ggtitle("Category likelihoods") + 
      scale_x_continuous(paste("mean", cue), limits = range(xs)) +
      scale_y_continuous(paste("SD", cue), expand = c(.2,.2))
  })
  
  currentLikelihood_woNoise <- reactive({
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    
    ggplot(data.frame(x = xs), aes(x = xs)) + 
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c1, 
                                sd = sd_c1), 
                    aes(col = c1)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c2, 
                                sd = sd_c2), 
                    aes(col = c2)) + 
      scale_colour_manual(
        "Category", 
        breaks = c(c1, c2),
        values = c(ggplotColours(2)[1], ggplotColours(2)[2])) + 
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous("Likelihood\n(w/o noise)") + 
      theme(axis.title.x = element_blank())
  })
  
  currentLikelihood_wNoise <- reactive({
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    sd_noise = input$sd_noise
    
    ggplot(data.frame(x = xs), aes(x = xs)) + 
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c1, 
                                sd = sqrt(sd_c1^2 + sd_noise^2)), 
                    aes(col = c1)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mu_c2, 
                                sd = sqrt(sd_c2^2 + sd_noise^2)), 
                    aes(col = c2)) + 
      scale_colour_manual(
        "Category", 
        breaks = c(c1, c2),
        values = c(ggplotColours(2)[1], ggplotColours(2)[2])) + 
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous("Likelihood\n(w noise)") + 
      theme(axis.title.x = element_blank())
  })
  
  currentUnnormalizedPosterior <- reactive({
    prior_c1 = input$prior_c1
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    sd_noise = input$sd_noise
    
    ggplot(data.frame(x = xs), aes(x = xs)) + 
      stat_function(fun = get_unnormalized_posterior, 
                    args = list(mu_c = mu_c1, 
                                sd_c = sqrt(sd_c1^2 + sd_noise^2), 
                                prior_c =  prior_c1), 
                    aes(col = c1)) +
      stat_function(fun = get_unnormalized_posterior, 
                    args = list(mu_c = mu_c2, 
                                sd_c = sqrt(sd_c2^2 + sd_noise^2), 
                                prior_c = 1 - prior_c1), 
                    aes(col = c2)) +
      scale_colour_manual(
        "Category", 
        breaks = c(c1, c2),
        values = c(ggplotColours(2)[1], ggplotColours(2)[2])) + 
      scale_x_continuous(
        cue,
        expand = c(0,0)) +
      scale_y_continuous("Unnormalized\nposterior") 
  })
  
  # Categorization ===============================================================
  output$p.prior.categorization <- renderPlot({
    currentPrior()
  })
  
  output$p.means.sds.categorization <- renderPlot({
    currentMeansSDs()
  })  
  
  # Cue distributions ------------------------------------------------------ 
  output$p.likelihood.categorization <- renderPlot({
    currentLikelihood_woNoise()
  })  
  
  output$p.likelihood.noise.categorization <- renderPlot({
    currentLikelihood_wNoise()
  })  
  
  output$p.unnormalized.posterior.categorization <- renderPlot({
    currentUnnormalizedPosterior()
  })  
  
  output$p.categorization <- renderPlot({
    prior_c1 = input$prior_c1
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    sd_noise = input$sd_noise
    lapse_rate = input$lapse_rate
    lapse_bias = if (!is.null(input$lapse_bias)) { input$lapse_bias } else { input$prior_c1 }
    
    # get boundary
    xs_between_means <- xs[which(between(xs, mu_c1, mu_c2))]
    ys = get_categorization(
      xs_between_means, 
      mu_c1 = mu_c1, 
      sd_c1 = sqrt(sd_c1^2 + sd_noise^2), 
      mu_c2 = mu_c2, 
      sd_c2 = sqrt(sd_c2^2 + sd_noise^2), 
      prior_c1 = prior_c1,
      lapse_rate = lapse_rate,
      lapse_bias = lapse_bias)
    x_boundary <- xs_between_means[which(abs(.5 - ys) == min(abs(.5 - ys)))]
    
    ggplot(data.frame(xs), aes(x = xs)) + 
      stat_function(
        fun = get_categorization, 
        args = list(
          mu_c1 = mu_c1, 
          sd_c1 = sqrt(sd_c1^2 + sd_noise^2), 
          mu_c2 = mu_c2, 
          sd_c2 = sqrt(sd_c2^2 + sd_noise^2), 
          prior_c1 = prior_c1,
          lapse_rate = lapse_rate,
          lapse_bias = lapse_bias),
        color = ggplotColours(2)[1]) +
      { if (between(x_boundary, min(xs), max(xs)))
        list(
          geom_segment(
            x = x_boundary, xend = x_boundary,
            y = .5, yend = 0,
            linetype = 3, size = .5, color = "darkgray"),
          geom_segment(
            x = min(xs), xend = x_boundary,
            y = .5, yend = .5,
            linetype = 3, size = .5, color = "darkgray"),
          annotate(
            x = x_boundary, 
            y = 0, 
            geom = "text", 
            label = paste(round(x_boundary, 1), cue_unit, " "), 
            color = "darkgray", hjust = 1, vjust = 0)) } +
      scale_x_continuous(
        cue,
        expand = c(0,0)) +
      scale_y_continuous(
        paste0("Probability of\n", c1, "-response"),
        limits = c(0,1))
  })  
  
  output$p.tokens.categorization <- renderPlot({
    currentData() %>%
      ggplot(aes(x = cue, fill = category)) +
      geom_histogram() +
      scale_fill_manual(
        "Category", 
        breaks = c(c1, c2),
        values = c(ggplotColours(2)[1], ggplotColours(2)[2])) + 
      scale_x_continuous(cue)
  })
  
  output$p.accuracy.categorization <- renderPlot({
    prior_c1 = input$prior_c1
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    sd_noise = input$sd_noise
    lapse_rate = input$lapse_rate
    lapse_bias = if (!is.null(input$lapse_bias)) { input$lapse_bias } else { input$prior_c1 }
    
    currentData() %>%
      mutate(
        categorization = 
          get_categorization(
            x = cue, 
            mu_c1 = mu_c1, 
            sd_c1 = sd_c1, 
            mu_c2 = mu_c2, 
            sd_c2 = sd_c2, 
            prior_c1 = prior_c1, 
            lapse_rate = lapse_rate, 
            lapse_bias = lapse_bias),
    accuracy = ifelse(category == c1, categorization, 1 - categorization)) %>%
      ggplot(aes(x = category, y = accuracy, color = category)) +
      stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
      scale_color_manual(
        "Category", 
        breaks = c(c1, c2),
        values = c(ggplotColours(2)[1], ggplotColours(2)[2])) + 
      scale_x_discrete("") +
      scale_y_continuous("Average\naccuracy", limits = c(0, 1))
  })
  
  # Perceptual discrimination ===================================================== 
  output$p.prior.discrimination <- renderPlot({
    currentPrior()
  })
  
  output$p.means.sds.discrimination <- renderPlot({
    currentMeansSDs()
  })  
  
  # Cue distributions ------------------------------------------------------ 
  output$p.likelihood.discrimination <- renderPlot({
    currentLikelihood_woNoise()
  })  
  
  output$p.likelihood.noise.discrimination <- renderPlot({
    currentLikelihood_wNoise()
  })  
  
  output$p.unnormalized.posterior.discrimination <- renderPlot({
    currentUnnormalizedPosterior()
  })  
  
  output$p.warping <- renderPlot({
    prior_c1 = input$prior_c1
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    sd_noise = input$sd_noise
    
    d <-
      data.frame(xs) %>%
      mutate(
        xs_warped = get_expected_target(
          xs, 
          mu_c1 = mu_c1, 
          sd_c1 = sd_c1, 
          mu_c2 = mu_c2, 
          sd_c2 = sd_c2, 
          prior_c1 = prior_c1,
          sd_noise = sd_noise))
    
    ggplot(d, aes(x = xs)) + 
      geom_point(y = 0, aes(x = xs_warped)) +
      geom_point(y = 1) +
      geom_segment(
        aes(
          x = xs,
          xend = xs_warped,
          y = 1,
          yend = 0)) +
      scale_x_continuous(
        cue,
        expand = c(0,0)) +
      scale_y_discrete(
        "Perceptual\nspace",
        breaks = c(0,1),
        labels = c("Perceived", "Actual"),
        expand = c(.08, .05)) +
      annotate(
        x = mean(xs), 
        y = c(-.2,1.3), 
        geom = "text", 
        label = c("perceived", "actual"), 
        color = "darkgray") +
      theme(
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  })  
  
  output$p.discrimination <- renderPlot({
    prior_c1 = input$prior_c1
    mu_c1 = input$mu_c1
    mu_c2 = input$mu_c2
    sd_c1 = input$sd_c1
    sd_c2 = input$sd_c2
    sd_noise = input$sd_noise
    
    distances = c(
      (max(xs) - min(xs)) / 10,
      (max(xs) - min(xs)) / 5,
      (max(xs) - min(xs)) / 2.5
    )
    
    ggplot(data.frame(xs), aes(x = xs)) + 
      stat_function(fun = get_perceptual_distance, 
                    args = list(
                      distance = distances[1],
                      mu_c1 = mu_c1, 
                      sd_c1 = sd_c1, 
                      mu_c2 = mu_c2, 
                      sd_c2 = sd_c2, 
                      prior_c1 = prior_c1,
                      sd_noise = sd_noise),
                    linetype = 1) +
      stat_function(fun = get_perceptual_distance, 
                    args = list(
                      distance = distances[2],
                      mu_c1 = mu_c1, 
                      sd_c1 = sd_c1, 
                      mu_c2 = mu_c2, 
                      sd_c2 = sd_c2, 
                      prior_c1 = prior_c1,
                      sd_noise = sd_noise),
                    linetype = 2) +
      stat_function(fun = get_perceptual_distance, 
                    args = list(
                      distance = distances[3],
                      mu_c1 = mu_c1, 
                      sd_c1 = sd_c1, 
                      mu_c2 = mu_c2, 
                      sd_c2 = sd_c2, 
                      prior_c1 = prior_c1,
                      sd_noise = sd_noise),
                    linetype = 3) +
      scale_x_continuous(
        cue,
        expand = c(0,0)) +
      scale_y_continuous(
        paste0("Perceived\ndistance")) +
      annotate(
        x = max(xs), 
        y = c(
          get_perceptual_distance(
            max(xs),
            distances[1],
            mu_c1 = mu_c1, 
            sd_c1 = sd_c1, 
            mu_c2 = mu_c2, 
            sd_c2 = sd_c2, 
            prior_c1 = prior_c1,
            sd_noise = sd_noise),
          get_perceptual_distance(
            max(xs),
            distances[2],
            mu_c1 = mu_c1, 
            sd_c1 = sd_c1, 
            mu_c2 = mu_c2, 
            sd_c2 = sd_c2, 
            prior_c1 = prior_c1,
            sd_noise = sd_noise),
          get_perceptual_distance(
            max(xs),
            distances[3],
            mu_c1 = mu_c1, 
            sd_c1 = sd_c1, 
            mu_c2 = mu_c2, 
            sd_c2 = sd_c2, 
            prior_c1 = prior_c1,
            sd_noise = sd_noise)), 
        geom = "text", 
        label = paste(
          (max(xs) - min(xs)) / distances,
          cue_unit, " "), 
        color = "darkgray",
        hjust = 1, vjust = 0
      )
  })
  
  # Data generation --------------------------------------------------------------- 
  output$p.prior.exposure <- renderPlot({
    data.frame(
      exposure = c("prior", "prior", "exposure A", "exposure A", "exposure B", "exposure B"),
      Category = c(c1, c2, c1, c2, c1, c2), 
      prior = c(input$prior_c1, 1 - input$prior_c1,
                input$prior_c1_A, 1 - input$prior_c1_A,
                input$prior_c1_B, 1 - input$prior_c1_B)
    ) %>%
      ggplot(
        aes(x = Category, y = prior, fill = exposure)) +
      geom_bar(stat = "identity", position = position_dodge()) + 
      ggtitle("Category priors") + 
      theme(axis.title.x = element_blank())
  })
  
  output$p.means.sds.exposure <- renderPlot({
    data.frame(
      exposure = c("prior", "prior", "exposure A", "exposure A", "exposure B", "exposure B"),
      Category = c(c1, c2, c1, c2, c1, c2), 
      mean = c(input$mu_c1, input$mu_c2, input$mu_c1_A, input$mu_c2_A, input$mu_c1_B, input$mu_c2_B),
      sd = c(input$sd_c1, input$sd_c2, input$sd_c1_A, input$sd_c2_A, input$sd_c1_B, input$sd_c2_B)) %>%
      ggplot(aes(
        x = mean, 
        y = sd, 
        color = exposure,
        label = Category)) +
      geom_text(fontface = "bold") +
      ggtitle("Category likelihoods") + 
      scale_x_continuous(paste("mean", cue)) +
      scale_y_continuous(paste("SD", cue), expand = c(.2,.2))
  })  
  
  # output$p.exposure.density <- renderPlot({
  #   output$t.data %>%
  #   ggplot(aes(x = cue)) +
  #     geom_density(
  #       aes(
  #         linetype = category,
  #         fill = exposure,
  #       )) +
  #     scale_colour_manual(
  #       "Category", 
  #       breaks = c(c1, c2),
  #       values = c(ggplotColours(2)[1], ggplotColours(2)[2])) + 
  #     scale_x_continuous(cue, expand = c(0,0)) +
  #     ylab("Density") + 
  #     theme(axis.title.x = element_blank())
  # })
  # 
  
  output$t.data <- renderDataTable({
    mu_A = c(input$mu_c1_A, input$mu_c2_A)
    sd_A = c(input$sd_c1_A, input$sd_c2_A)
    cat_labels_A = rbinom(input$LenData, 1, input$prior_c1_A)
    cue_A = sapply(cat_labels_A, 
                   FUN = function(x) {
                     x = abs(x - 2)
                     rnorm(1, mu_A[x], sd_A[x])
                   })
    
    mu_B = c(input$mu_c1_B, input$mu_c2_B)
    sd_B = c(input$sd_c1_B, input$sd_c2_B)
    cat_labels_B = rbinom(input$LenData, 1, input$prior_c1_B)
    cue_B = sapply(cat_labels_B, 
                   FUN = function(x) {
                     x = abs(x - 2)
                     rnorm(1, mu_B[x], sd_B[x])
                   })
    
    data.frame(
      condition = "A",
      category = ifelse(cat_labels_A, c1, c2),
      cue = cue_A) %>%
      rbind(
        data.frame(
          condition = "B",
          category = ifelse(cat_labels_B, c1, c2),
          cue = cue_B)
      )
  }, options = list(lengthMenu = c(25, 50, 100), pageLength = 25))
  
  
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
  #       ggtitle("Category Prior")
  #     
  #     #Plot means
  #     means = data.frame(Category = c("B", "P"), 
  #                        VOT = c(mu1_c1, 
  #                                mu1_c2))
  #     
  #     p.means = ggplot(means, aes(x = Category, y = VOT, color=Category)) +
  #       geom_point() +
  #       ggtitle("Category Means")
  #     
  #     #Plot variances
  #     sds = data.frame(Category = c("B", "P"), 
  #                      VOT_SD = c(sd1_c1, 
  #                                 sd1_c2))
  #     
  #     p.sds = ggplot(sds, aes(x = Category, y = VOT_SD, color=Category)) +
  #       geom_point() +
  #       ggtitle("Category Standard Deviation")
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
  #       ylab("Likelihood") + xlab("VOT (ms)") + theme(axis.title.x=element_blank())
  #     
  #     #Plot posterior 
  #     p.posterior= ggplot(data.frame(x = xs), aes(x = xs)) + 
  #       stat_function(fun = get_posterior, 
  #                     args = list(mu_c1 = mu1_c1, 
  #                                 sd_c1 = sd1_c1, 
  #                                 prior_c =  prior1_c1), 
  #                     aes(col='/b/')) +
  #       stat_function(fun = get_posterior, 
  #                     args = list(mu_c1 = mu1_c2, 
  #                                 sd_c1 = sd1_c2, 
  #                                 prior_c = 1-prior1_c1), 
  #                     aes(col='/p/')) +
  #       scale_colour_manual("Category", values = c('/p/' = ggplotColours(2)[1], '/b/'=ggplotColours(2)[2])) + 
  #       ylab("Posterior") + xlab("VOT (ms)") + theme(axis.title.x=element_blank())
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
  #       stat_function(fun = get_posterior, 
  #                     args = list(mu_c1 = mu1_c1, 
  #                                 sd_c1 = sd1_c1, 
  #                                 prior_c =  prior1_c1), 
  #                     aes(col='/b/', linetype="First")) +
  #       stat_function(fun = get_posterior, 
  #                     args = list(mu_c1 = mu1_c2, 
  #                                 sd_c1 = sd1_c2, 
  #                                 prior_c = 1-prior1_c1), 
  #                     aes(col='/p/', linetype="First")) +
  #       stat_function(fun = get_posterior, 
  #                     args = list(mu_c1 = mu2_c1, 
  #                                 sd_c1 = sd2_c1, 
  #                                 prior_c =  prior2_c1), 
  #                     aes(col='/b/', linetype="Second")) +
  #       stat_function(fun = get_posterior, 
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
  #       facet_wrap(~Likelihood)
  #     })
  
  
})

