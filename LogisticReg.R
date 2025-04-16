install.packages("tictoc")

library(tidyverse)
library(broom)
library(gt)
library(patchwork) 
library(tictoc) 
library(Epi) 
library(dunnr)

#-------------------------------------------------------------------------------------------#

tidy_custom <- function(mod, coef_round = 3, se_round = 4, t_round = 2) {
  tidy(mod) %>%
    transmute(
      term,
      estimate = round(estimate, coef_round),
      s.e. = round(std.error, se_round),
      `t-stat` = round(statistic, t_round),
      `p-value` = scales::pvalue(p.value)
    )
}

#-------------------------------------------------------------------------------------------#

default <- ISLR2::Default
head(default)

p1 <- default %>%
  ggplot(aes(x = balance, y = income)) +
  geom_point(aes(color = default, shape = default),
             alpha = 0.5, show.legend = FALSE)
p2 <- default %>%
  ggplot(aes(x = default, y = balance)) +
  geom_boxplot(aes(fill = default), show.legend = FALSE)
p3 <- default %>%
  ggplot(aes(x = default, y = income)) +
  geom_boxplot(aes(fill = default), show.legend = FALSE)
p1 | (p2 | p3)




#--------------------------------------------------------------------------

lm_default_balance <-
  lm(
    default ~ balance,
    # Turn the factor levels into 0 and 1
    data = default %>% mutate(default = as.numeric(default) - 1)
  )
glm_default_balance <-
  glm(default ~ balance, data = default,
      family = binomial(link = "logit"))
# Plot the data
p <- default %>%
  ggplot(aes(x = balance)) +
  geom_point(aes(y = as.numeric(default) - 1), color = td_colors$nice$soft_orange, alpha = 0.5)
# Plot the linear model
p1 <- p +
  geom_abline(slope = coef(lm_default_balance)["balance"],
              intercept = coef(lm_default_balance)["(Intercept)"],
              size = 1.5, color = td_colors$nice$strong_blue) +
  labs(y = "Probability of default")
# Plot the logistic model
p2 <- p +
  geom_line(
    aes(y = pred_default),
    data = tibble(balance = seq(0, 2700, 1)) %>%
      mutate(
        sum_beta = coef(glm_default_balance)["(Intercept)"] +
          balance * coef(glm_default_balance)["balance"],
        pred_default = plogis(sum_beta)
      ),
    size = 1.5, color = td_colors$nice$strong_blue
  ) +
  labs(y = NULL)
p1 | p2


#----------------------------------------------------------------------

  predict(lm_default_balance, newdata = default) %>%
  mean()
  predict(glm_default_balance, newdata = default) %>%
  plogis() %>%
  mean()

  table(default$default)
  
  glm_default_student <-
    glm(default ~ student, data = default,
        # Note: don't need to specify binomial(link = "logit") because it is the
        #  default link
        family = binomial)
  tidy_custom(glm_default_student) %>% gt()
  
  summary(glm_default_student)
  
#------------------------------------------------------------------------------#
  
  glm_default_all <-
    glm(default ~ .,
        data = default %>% mutate(income = income / 1000),
        family = binomial)
  tidy_custom(glm_default_all) %>% gt()
  
  summary(glm_default_all)
  
  
  balance_breaks <- seq(0, 2700, by = 270)
  balance_midpoints <-
    (balance_breaks[1:(length(balance_breaks) - 1)] +
       balance_breaks[2:length(balance_breaks)]) / 2
  p1 <- default %>%
    mutate(
      balance_binned = cut(balance, breaks = balance_breaks,
                           include.lowest = TRUE, labels = balance_midpoints),
      balance_binned = as.numeric(as.character(balance_binned))
    ) %>%
    group_by(student, balance_binned) %>%
    summarise(p_default = mean(default == "Yes"), .groups = "drop") %>%
    ggplot(aes(x = balance_binned, y = p_default, color = student)) +
    geom_line(size = 1.5) +
    geom_hline(
      data = default %>%
        group_by(student) %>%
        summarise(p_mean_default = mean(default == "Yes"),
                  .groups = "drop"),
      aes(yintercept = p_mean_default, color = student), lty = 2, size = 1
    ) +
    scale_color_manual(values = c(td_colors$nice$strong_blue,
                                  td_colors$nice$strong_red)) +
    theme(legend.position = c(0.2, 0.7))
  p2 <- default %>%
    ggplot(aes(x = student, y = balance)) +
    geom_boxplot(aes(fill = student)) +
    scale_fill_manual(values = c(td_colors$nice$strong_blue,
                                 td_colors$nice$strong_red)) +
    theme(legend.position = "none")
  p1 | p2
  
#---------------------------------------------------------------------------------  

idx   <- sample(1:nrow(default), size=nrow(default)*0.5)
train <- default[ idx, ]
test  <- default[-idx, ]

glm_default_train <- glm(default ~balance, data = train %>% mutate(income = income / 1000), family = binomial)  
summary(glm_default_train)  
  
yhat <- ifelse(glm_default_train$fitted.values>=0.5, "Yes", "No")
addmargins(table(real=train$default, pred=yhat))  
  
yhat_test <- predict(glm_default_train,test,type="response")

ROC(test=yhat_test, stat=test$default, plot="ROC", AUC=T, main="Logistic Regression")
  

  
  
  
  
  
  
  
  
  
  
    

  
