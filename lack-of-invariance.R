# Code based on Xie, Jaeger, & Kurumada (2023)
# Phonetic data from Chodroff & Wilson (2018)
# Perceptual data from Burchill & Jaeger (2023)

## ------------------------------ Libraries ----------------------------
library(tidyverse)
library(magrittr)           # pipes
library(gganimate)          # animations

library(diptest)            # bimodality test
library(lme4)               # mixed-effects linear regression for normalization
library(MVBeliefUpdatr)     # for ideal observers and adaptors 

## ------------------------------ Constants ----------------------------
max.f0 <- 350            # max raw f0
min.observation.n <- 25  # min observation per stop
max.p <- .1              # cut-off for rejection of unimodality

colors.voicing = c("blue", "red")

## ------------------------------- Functions ---------------------------
apply_ccure <- function(x, data) {
  x - predict(lmer(x ~ 1 + (1 | Talker), data = data), random.only = T)
}

myGplot.defaults = function(
    type = c("paper","poster","slides")[1],
    base_size = if (type == "paper") { 10 } else if (type == "slides") { 32 } else if (type == "poster") { 36 } else { 10 }, 
    margin=c("t" = 0.6, "r" = 0.5, "b" = 0.5, "l" = 0.3),
    set_theme = T
)
{
  require(ggplot2)
  
  if (set_theme) {
    theme_set(theme_bw(base_size=base_size))
    theme_update(
      axis.text.x = element_text(size=base_size, vjust=1),
      axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
      axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"), 
      axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"), 
      strip.text = element_text(size=base_size, color = "white"),
      strip.background = element_rect(fill = "black", color = "black"),
      legend.title = element_text(size=base_size, face = "bold", hjust= 0), 
      legend.text = element_text(size=base_size),
      plot.margin = unit(margin, "lines")
    )
  } else {
    return(
      theme(
        axis.text.x = element_text(size=base_size, vjust=1),
        axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
        axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"), 
        axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"), 
        strip.text = element_text(size=base_size, color = "white"),
        strip.background = element_rect(fill = "black", color = "black"),
        legend.title = element_text(size=base_size, face = "bold", hjust= 0), 
        legend.text = element_text(size=base_size),
        plot.margin = unit(margin, "lines")))
  }
}

myGplot.defaults(type = "slides")

## ------------------------------ Data import ----------------------------
d.chodroff_wilson <- 
  read_csv("../data/Chodroff-Wilson-2018/all_observations_with_non-missing_vot_cog_f0.csv") %>%
  rename(category = stop, VOT = vot, f0 = usef0, Talker = subj, Word = word, Trial = trial, Vowel = vowel) %>%
  mutate(
    category = 
      plyr::mapvalues(
        category,
        c("B", "D", "G", "P", "T", "K"),
        c("/b/", "/d/", "/g/", "/p/", "/t/", "/k/")),
    gender = factor(
      plyr::mapvalues(
        gender, 
        c("F", "M"),
        c("female", "male")),
      levels = c("male", "female")),
    poa = factor(
      plyr::mapvalues(
        poa, 
        c("lab", "cor", "dor"),
        c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
      levels = c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
    voicing = factor(
      ifelse(category %in% c("/b/", "/d/", "/g/"), "yes", "no"),
      levels = c("yes", "no"))) %>%
  mutate(across(c(Talker, Word, gender, category), factor)) %>%
  select(Talker, Word, Trial, Vowel, gender, category, poa, voicing, VOT, f0)

# Filter f0 above 500 Hz
d.chodroff_wilson %<>%
  filter(f0 < max.f0)

# Keep only subjects with at last n.min observations for each stop
d.chodroff_wilson %<>%
  group_by(Talker, category) %>%
  mutate(n = length(category)) %>%
  group_by(Talker) %>%
  mutate(n = ifelse(any(is.na(n)), 0, min(n))) %>%
  ungroup() %>%
  filter(n > min.observation.n)

# Identify and remove talkers with bimodal f0 distributions
d.chodroff_wilson %<>%
  group_by(Talker) %>%
  mutate(f0_Mel = phonR::normMel(f0)) %>%
  group_by(Talker, category) %>%
  mutate(
    f0.multimodal = dip.test(f0)$p.value < .1,
    f0_Mel.multimodal = dip.test(f0_Mel)$p.value < .1) %>%
  filter(!f0.multimodal, !f0.multimodal) %>%
  droplevels()

# Get Mel and Semitones, then C-CuRE normalization
d.chodroff_wilson %<>%
  group_by(Talker) %>%
  mutate(
    f0_Mel = phonR::normMel(f0),
    f0_semitones = 12 * log(f0 / mean(f0)) / log(2)) %>%
  ungroup() %>%
  mutate_at(
    c("VOT", "f0", "f0_Mel", "f0_semitones"),
    list("centered" = function(x) apply_ccure(x, data = .)))

## ------------------------------ Make ideal observers ----------------------------
cues <- c("VOT_centered", "f0_Mel_centered")

items <- c("DOLLARS","TOPIC","DOES","TUNNELS","DAUGHTER","TALKING","DIED","TIME","DEFINITELY", "TELL", "DAY","TAKE", "DEAL", "TEACHER","DON'T","TOTALLY","DO","TOO")

d.chodroff_wilson.selected <- 
  d.chodroff_wilson %>%
  filter(Word %in% items) %>%
  group_by(Talker, category) %>%
  mutate(n = n()) %>%
  group_by(Talker) %>%
  # subsample n tokens, as determined by category with fewer tokens
  mutate(
    n_min = min(n), 
    n_category = n_distinct(category)) %>%
  # select talkers with both /d/ and /t/ observations
  filter(n_category == 2) %>% 
  group_by(Talker, category) %>%
  sample_n(size = first(n_min)) %>%
  ungroup() %>%
  mutate_at(
    c("VOT", "f0", "f0_Mel", "f0_semitones"),
    list("centered" = function(x) apply_ccure(x, data = .)))

prior_marginal_VOT_f0_stats <- 
  d.chodroff_wilson.selected %>%
  group_by(Talker) %>%
  summarise(across(c(VOT, f0_Mel), mean)) %>%
  ungroup() %>%
  summarise(
    x_mean = list(c(VOT = mean(VOT), f0 = mean(f0_Mel))),
    x_var_VOT = var(VOT),
    x_var_f0 = var(f0_Mel),
    x_cov = list(cov(cbind(VOT, f0_Mel))))

m.VOT_f0_MVG <- 
  make_MVG_from_data(
    data = d.chodroff_wilson.selected, # sample a balanced set of voiced and voiceless tokens for each place of articulation; the specific number is determined by the min number of tokens per category for that contrast,
    category = "category",
    cues = cues) 

## ------------------------------ Visualizing lack of invariance ----------------------------
n.subj <- 10
seed <- 59876

talkers <- sample(unique(d.chodroff_wilson$Talker), n.subj, replace = F) 

p.talkers <- 
  d.chodroff_wilson %>% 
  # Keep random subject of subjects
  ungroup() %>%
  filter(
    !is.na(VOT),
    !is.na(f0_Mel),
    Talker %in% talkers) %>%
  mutate(across(c(Talker, voicing, poa), factor)) %>%
  group_by(Talker, voicing, poa, .drop = FALSE) %>%
  mutate(n = length(n)) %>%
  group_by(Talker) %>%
  mutate(min_n = min(n)) %>%
  ungroup() %>%
  filter(min_n > 20) %>%
  droplevels() %>%
  ggplot(aes(x = VOT, y = f0_Mel, color = voicing, shape = gender)) +
  geom_point(alpha = .1) +
  stat_ellipse() +
  scale_x_continuous(expression("VOT (ms)")) +
  scale_y_continuous(expression("f0 (Mel)")) +
  scale_color_manual("Voiced", breaks = c("yes", "no"), values = colors.voicing) +
  scale_shape_discrete("Gender") +
  facet_grid(Talker ~ poa) 

p.talkers +
  geom_line(
    data = . %>% 
      group_by(Talker, gender, category, poa, voicing) %>% 
      summarise_at(
        vars(VOT, f0_Mel),
        mean),
    mapping = aes(group = Talker),
    alpha = .9, size = .3, color = "gray") +
  geom_point(
    data = . %>% 
      group_by(Talker, gender, category, poa, voicing) %>% 
      summarise_at(
        vars(VOT, f0_Mel),
        mean),
    alpha = .9)

p.means <-
  d.chodroff_wilson %>% 
  ggplot(aes(x = VOT, y = f0_Mel, color = voicing)) +
  stat_ellipse() +
  scale_x_continuous(expression("VOT (ms)")) +
  scale_y_continuous(expression("f0 (Mel)")) +
  scale_color_manual("Voiced", breaks = c("yes", "no"), values = colors.voicing) +
  scale_shape_discrete("Gender") +
  scale_size_continuous("n", range = c(.1, 3)) +
  facet_grid(. ~ poa)

p.means +
  geom_line(
    data = . %>% 
      group_by(Talker, gender, category, poa, voicing) %>% 
      summarise_at(
        vars(VOT, f0_Mel),
        mean),
    mapping = aes(group = Talker),
    alpha = .1, size = .3, color = "gray") +
  geom_point(
    data = . %>% 
      group_by(Talker, gender, category, poa, voicing) %>% 
      summarise_at(
        vars(VOT, f0_Mel),
        mean),
    mapping = aes(shape = gender),
    alpha = .3)
