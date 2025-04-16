#install.packages("tictoc")

library(tidyverse)
library(broom)
library(gt)
library(patchwork) 
library(tictoc)
library(MASS)
library(dunnr)

# Load my R package and set the ggplot theme
library(dunnr)
extrafont::loadfonts(device = "win", quiet = TRUE)
theme_set(theme_td())
set_geom_fonts()
set_palette()

#-------------------------------------------------------------------------------------------#

default <- ISLR2::Default
glimpse(default)

#-------------------------------------------------------------------------------------------#

mu1 <- -1.25
mu2 <-  1.25
sigma1 <- 1
sigma2 <- 1

bayes_boundary <- (mu1 + mu2) / 2
p1 <- ggplot(data = tibble(x = seq(-4, 4, 0.1)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = mu1, sd = sigma1),
                geom = "line", linewidth = 1.5, color = td_colors$nice$emerald) +
  stat_function(fun = dnorm, args = list(mean = mu2, sd = sigma2),
                geom = "line", linewidth = 1.5, color = td_colors$nice$opera_mauve) +
  geom_vline(xintercept = bayes_boundary, lty = 2, size = 1.5) +
  remove_axis("y")

set.seed(42)
d <- tribble(
  ~class, ~x,
  1, rnorm(20, mean = mu1, sd = sigma1),
  2, rnorm(20, mean = mu2, sd = sigma2)
) %>%
  unnest(x)
lda_boundary <-
  (mean(filter(d, class == 1)$x) + mean(filter(d, class == 2)$x)) / 2

p2 <- d %>%
  ggplot(aes(x, fill = factor(class), color = factor(class))) +
  geom_histogram(bins = 13, alpha = 0.5, position = "identity") +
  geom_vline(xintercept = bayes_boundary, lty = 2, size = 1.5) +
  geom_vline(xintercept = lda_boundary, lty = 1, size = 1.5) +
  scale_fill_manual(values = c(td_colors$nice$emerald,
                               td_colors$nice$opera_mauve)) +
  scale_color_manual(values = c(td_colors$nice$emerald,
                                td_colors$nice$opera_mauve)) +
  theme(legend.position = "none")
p1 | p2


#-------------------------------------------------------------------------------
set.seed(2021)

d <- tribble(
  ~class, ~x,
  1, rnorm(1e3, mean = mu1, sd = sigma1),
  2, rnorm(1e3, mean = mu2, sd = sigma2)
) %>%
  unnest(x)

# The LDA boundary must be recomputed with the new data
lda_boundary <-
  (mean(filter(d, class == 1)$x) + mean(filter(d, class == 2)$x)) / 2

d %>%
  mutate(
    bayes_class = ifelse(x > bayes_boundary, 1, 2),
    lda_class = ifelse(x > lda_boundary, 1, 2)
  ) %>%
  summarise(
    `Bayes error rate` = mean(class == bayes_class),
    `LDA error rate` = mean(class == lda_class)
  )

#---------------------------------------------------------------------------------

d <- crossing(x1 = seq(-2, 2, 0.1), x2 = seq(-2, 2, 0.1))
d1 <- d %>%
  bind_cols(
    prob = mvtnorm::dmvnorm(
      x = as.matrix(d),
      mean = c(0, 0), sigma = matrix(c(1, 0, 0, 1), nrow = 2)
    )
  )
d2 <- d %>%
  bind_cols(
    prob = mvtnorm::dmvnorm(
      x = as.matrix(d),
      mean = c(0, 0), sigma = matrix(c(1, 0.7, 0.7, 1), nrow = 2)
    )
  )
p1 <- d1 %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_tile(aes(fill = prob)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")
p2 <- d2 %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_tile(aes(fill = prob)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")
p1 | p2

#----------------------------------------------------------------------------------
select <- dplyr::select

lda_default_balance_student <-
  MASS::lda(default ~ balance + student, data = default)
lda_default_balance_student

lda_pred <- 
  bind_cols(
    pred_default = predict(lda_default_balance_student, 
                           newdata = default)$class, default 
  ) 

lda_posterior <- predict(lda_default_balance_student, newdata = default)$posterior

addmargins(table(pred=lda_pred$pred_default,true=lda_pred$default),FUN=sum)


lda_pred_20 <- bind_cols(
  default,
  posterior_prob_default = lda_posterior[,2]
) %>%
  mutate(
    pred_default = ifelse(posterior_prob_default > 0.2, "Yes", "No")
  )
addmargins(table(pred=lda_pred_20$pred_default,true=lda_pred$default),FUN=sum)

# ROC courve
lda_roc <-
  yardstick::roc_curve(
    lda_pred_20,
    posterior_prob_default, truth = default,
    event_level = "second"
  )
autoplot(lda_roc)

yardstick::roc_auc(
  lda_pred_20,
  posterior_prob_default, truth = default,
  event_level = "second"
)


















