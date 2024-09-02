library(tidyverse)
library(animation)
library(gganimate)

# Basic parameter
true_mu_1 <- 5
true_mu_2 <- 55
true_sigma_1 <- 9
true_sigma_2 <- 16

K <- 10       # number of mixture components
N <- 10000    # number of learning observations 

# Make learning data
input <- 
  tibble(
    category = c("b", "p"),
    mu = c(true_mu_1, true_mu_2),
    sigma = c(true_sigma_1, true_sigma_2)) %>%
  crossing(n = 0:N) %>%
  mutate(x = ifelse(n == 0, NA, rnorm(2 * (N + 1), mu, sigma))) %>%
  select(-c(mu, sigma)) %>%
  group_by(n) %>%
  sample_n(1)

# Show learning data
input %>%
  ggplot(aes(x = x, fill = category)) +
  geom_histogram(alpha = .5, position = "identity") +
  scale_x_continuous("VOT (in msec)") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()

# Make a MoG
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


## Using gganimate instead of animation
d.plot <- d %>% filter(n < 10)

p <- d.plot %>%
  plot_mog(animate_by = "n") +
  geom_rug(
    data = d.plot, 
    mapping = aes(x = x, color = category), 
    inherit.aes = FALSE,
    alpha = .5) +
  scale_color_manual(values = c("red", "blue")) 


# Make a movie out of the updates
# requires Magick installation
saveGIF({
  max_n <- N
  i <- 0
  j <- 0
  
  while (i < max_n) { 
    p <- plot_mog(d[d$n == i,]$mog[[1]]) +
      geom_rug(
        data = d %>% filter(between(n, j, i)), 
        mapping = aes(x = x, color = category), 
        inherit.aes = FALSE,
        alpha = .5) +
      scale_color_manual(values = c("red", "blue")) +
      ggtitle(paste("Observations", j, "to", i))
    print(p)
    
    j <- i + 1
    i <- i + round(log2(i + 1)^2 + .51)
  }
}, interval = .5, movie.name = "mog.gif")

# requires ffmpeg installation (brew install ffmpeg)
saveVideo({
  max_n <- N
  i <- 0
  j <- 0
  
  while (i < max_n) { 
    p <- plot_mog(d[d$n == i,]$mog[[1]]) +
      geom_rug(
        data = d %>% filter(between(n, j, i)), 
        mapping = aes(x = x, color = category), 
        inherit.aes = FALSE,
        alpha = .5) +
      scale_color_manual(values = c("red", "blue")) +
      ggtitle(paste("Observations", j, "to", i))
    print(p)
    
    j <- i + 1
    i <- i + round(log2(i + 1)^2 + .51)
  }
}, interval = .5, video.name = "mog.mp4")
