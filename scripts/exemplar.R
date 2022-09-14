library(tidyverse)
library(magrittr)
library(assertthat)
library(MVBeliefUpdatr)
library(gganimate)

# Load learning data
input <- 
  read_csv("data/Li_Xie_2020_L1_vowels_statistics.csv") %>%
  rename(
    category = Vowel,
    F1 = F1_gm,
    F2 = F2_gm)

# Show learning data
input %>%
  ggplot(aes(x = F2, y = F1, color = category)) +
  geom_point(alpha = .5) +
  scale_x_reverse("F2") +
  scale_y_reverse("F1") + 
  scale_color_brewer("Vowel", type = "qual") +
  theme_bw()

# Class check function
is.exemplar_model <- function(x, group = NULL, verbose = FALSE) {
  return(TRUE)
}

# Make an exemplar model
make_exemplar_model_from_data <- function(
    data,
    group = NULL,
    category = "category",
    cues,
    k = 1,     # Similarity decay rate
    r = 2,     # distance metric
    prior = NA_real_,
    lapse_rate = NA_real_,
    lapse_bias = NA_real_,
    Sigma_noise = NULL,
    add_Sigma_noise_to_category_representation = T,
    verbose = F
) {
  assert_that(any(is.data.frame(data), is_tibble(data)))
  assert_that(is_character(category, n = 1))
  assert_that(all(is_character(cues), length(cues) > 0))
  assert_that(all(c(group, category, cues) %in% names(data)))
  
  if (add_Sigma_noise_to_category_representation) message("add_Sigma_noise_to_category_representation is not yet implemented, and will be ignored.")
  
  model <- 
    data %>%
    mutate(exemplar = pmap(.l = list(!!! syms(cues)), ~ c(...))) %>%
    select(c(!!! syms(c(group, category, cues)), exemplar)) %>%
    group_by(!!! syms(c(group, category))) %>%
    nest(exemplars = c(!!! cues, exemplar))
  
  model %<>%
    mutate(
      k = k,
      r = r) %>%
    lift_likelihood_to_model(
      group = group,
      prior = prior,
      lapse_rate = lapse_rate, lapse_bias = lapse_bias,
      Sigma_noise = Sigma_noise,
      verbose = verbose)
  
  if (!is.exemplar_model(model, group = group, verbose = verbose))
    warning("Something went wrong. The returned object is not an exemplar model. Try again with verbose = T?")
  
  return(model)
}

# Test 
m <- make_exemplar_model_from_data(data = input, cues = c("F1", "F2"))

#' Distance in Minkowski r-metric
get_distance_between_exemplars <- function(e1, e2, r = 2) {
  sum(abs(e1 - e2)^r)^1/r
}

get_similarity_from_distance_between_exemplars <- function(distance, k = 1) {
  exp(-k * distance)
}

get_similarity_between_exemplars <- function(e1, e2, r = 2, k = 1, log = F) {
  assert_that(length(e1) == length(e2))
  
  sim <- get_distance_between_exemplars(e1, e2, r) * -k
  
  if (log) return(sim) else return(exp(sim))
}

get_likelihood_from_exemplar_model <- function(
    x,
    model,
    log = F,
    noise_treatment = if (is.exemplar_model(model)) "marginalize" else "no_noise",
    category = "category",
    category.label = NULL
) {
  assert_that(is.exemplar_model(model))
  assert_that(any(is.null(category.label) | is.character(category.label)))
  assert_that(any(noise_treatment == "no_noise", is.exemplar_model(model)),
              msg = 'No noise matrix Sigma_noise found. If noise_treatment is not "no_noise", then model must be an exemplar model.')
  
  if (is.null(category.label)) {
    model %<>%
      droplevels()
    
    category.label <- model %>%
      pull(!! sym(category)) %>%
      unique()
  }
  
  likelihood <-
    model %>%
    group_by(!!! syms(group), !! sym(category)) %>%
    unnest(cols = exemplars) %>%
    summarise(similarity = sum(map(exemplar, ~ get_similarity_between_exemplars(x, .x, log = log)) %>% unlist())) %>%
    mutate(activation = if (sum(similarity == 0)) 1 / length(similarity) else similarity / sum(similarity))
  
  return(likelihood)
}

get_likelihood_from_exemplar_model(x = c(1500, 2000), m)

# Categorize a set of of tokens
get_categorization_from_exemplar_model <- function(
    x,
    model,
    decision_rule,
    noise_treatment = if (decision_rule == "sampling") "sample" else "marginalize",
    lapse_treatment = if (decision_rule == "sampling") "sample" else "marginalize",
    simplify = F
) {
  assert_that(is.exemplar_model(model))
  assert_that(decision_rule  %in% c("criterion", "proportional", "sampling"),
              msg = "Decision rule must be one of: 'criterion', 'proportional', or 'sampling'.")
  assert_that(any(lapse_treatment %in% c("no_lapses", "sample", "marginalize")),
              msg = "lapse_treatment must be one of 'no_lapses', 'sample' or 'marginalize'.")
  
  posterior_probabilities <-
    get_likelihood_from_exemplar_model(x = x, model = model, log = F, noise_treatment = noise_treatment) %>%
    group_by(category) %>%
    mutate(
      observationID = 1:length(x),
      x = x,
      lapse_rate = get_lapse_rate_from_model(model),
      lapse_bias = get_lapse_biases_from_model(model, categories = category),
      prior = get_priors_from_model(model, categories = category)) %>%
    group_by(observationID) %>%
    mutate(posterior_probability = (likelihood * prior) / sum(likelihood * prior))
  
  # How should lapses be treated?
  if (lapse_treatment == "sample") {
    posterior_probabilities %<>%
      mutate(
        posterior_probability = ifelse(
          rep(
            rbinom(1, 1, lapse_rate),
            get_nlevels_of_category_labels_from_model(model)),
          lapse_bias,                 # substitute lapse probabilities for posterior
          posterior_probability))     # ... or not
  } else if (lapse_treatment == "marginalize") {
    posterior_probabilities %<>%
      mutate(posterior_probability =  lapse_rate * lapse_bias + (1 - lapse_rate) * posterior_probability)
  }
  
  # Apply decision rule
  if (decision_rule == "criterion") {
    posterior_probabilities %<>%
      mutate(
        # tie breaker in case of uniform probabilities
        posterior_probability = ifelse(
          rep(
            sum(posterior_probability == max(posterior_probability)) > 1,
            get_nlevels_of_category_labels_from_model(model)),
          posterior_probability + runif(
            get_nlevels_of_category_labels_from_model(model),
            min = 0,
            max = 0),
          posterior_probability),
        # select most probable category
        response = ifelse(posterior_probability == max(posterior_probability), 1, 0))
  } else if (decision_rule == "sampling") {
    posterior_probabilities %<>%
      mutate(response = rmultinom(1, 1, posterior_probability) %>% as.vector())
  } else if (decision_rule == "proportional") {
    posterior_probabilities %<>%
      mutate(response = posterior_probability)
  } else warning("Unsupported decision rule. This should be impossible to happen. Do not trust the results.")
  
  posterior_probabilities %<>%
    ungroup() %>%
    select(-c(likelihood, posterior_probability)) %>%
    select(observationID, x, category, response)
  
  if (simplify) {
    assert_that(decision_rule  %in% c("criterion", "sampling"),
                msg = "For simplify = T, decision rule must be either criterion or sampling.")
    return(posterior_probabilities %>%
             filter(response == 1) %>%
             select(observationID, category) %>%
             arrange(observationID) %>%
             rename(response = category) %>%
             ungroup() %>%
             pull(response))
  } else return(posterior_probabilities)
}


mog <- 
  crossing(
    eta_phi = .05,
    eta_mu = 1,
    eta_sigma = 1) %>%
  mutate(mog_id = 1:nrow(.)) %>%
  relocate(mog_id, everything()) %>%
  crossing(k = 1:K) %>%
  # Initialize
  mutate(
    phi = 1/K,
    mu = runif(K, -100, 150),
    sigma = runif(K, 0, 60)) %>%
  nest(mog = -c(mog_id))

# Update MoGs
update_mog <- function(mog, x, wta = FALSE, prune.phi_threshold = 10^-32) {
  mog %>%
    # Unnest if necessary
    { if (!("mu" %in% names(.))) unnest(., cols = c(mog)) else . } %>%
    mutate(
      unnormalized_weight = exp(log(phi) + dnorm(x, mu, sigma, log = TRUE)),
      weight = unnormalized_weight / sum(unnormalized_weight, na.rm = T),
      phi = if (!wta) phi + eta_phi * weight else { 
        # implement wta here 
      },
      phi = phi / sum(phi),
      mu = mu + eta_mu * weight  * ((x - mu) / sigma^2),
      sigma = sigma + eta_sigma * weight * ((sigma^-3) * (x - mu)^2 - (sigma^-1))) %>%
    select(-ends_with("weight")) %>%
    # Prune 
    filter(phi > prune.phi_threshold)
}

# Test updating
# mog <- update_mog(mog, 20)
# print(mog)

# Visualize a MoG
plot_mog <- function(mog, x_axis = seq(-100, 100, length.out = 301), animate_by = NULL) {
  suppressWarnings(
    mog %>%
      # Unnest if necessary
      { if (!("mu" %in% names(.))) unnest(., cols = c(mog)) else . } %>%
      # Cross with x-axis data
      crossing(x_axis = x_axis) %>%
      # Calculate y
      mutate(y = pmap(
        .l = list(x_axis, phi, mu, sigma), 
        .f = function(x_axis, phi, mu, sigma) y = phi * dnorm(x = x_axis, mean = mu, sd = sigma)) %>% unlist()) %>%
      # Plot
      ggplot(aes(x = x_axis, y = y)) +
      geom_line(aes(group = k), color = "gray") +
      stat_function(fun = ~ .5 * dnorm(.x, mean = true_mu_1, sd = true_sigma_1), color = "red") +
      stat_function(fun = ~ .5 * dnorm(.x, mean = true_mu_2, sd = true_sigma_2), color = "blue") +
      scale_x_continuous("VOT (in msec)") +
      scale_y_continuous("density") +
      { if (!is.null(animate_by)) 
        transition_states(
          states = !! sym(animate_by), 
          transition_length = 1/5, 
          state_length = 1/5) } +
      facet_wrap(~ mog_id) +
      theme_bw())
}

# Test visualization
mog %>%
  plot_mog(animate_by = "mog_id")

# Update MoG with learning data
d <- 
  crossing(
    input, 
    mog %>%
      { if ("mu" %in% names(mog)) nest(-c(mog_id)) else . }) %>%
  arrange(mog_id, n)

for (i in 1:N) {
  d[d$n == i,]$mog[[1]] <- update_mog(d[d$n == i - 1,]$mog[[1]], d[d$n == i,]$x)
}


