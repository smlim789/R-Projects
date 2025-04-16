
#install.packages("ISLR2")
#install.packages("tidyverse")
#install.packages("remotes")
#remotes::install_github("taylordunn/dunnr")

#install.packages("patchwork")
#install.packages("ggplot2")
#install.packages("ggpubr")

library(tidyverse)
library(dplyr)
library(ggpubr)
library(dunnr) 
library(patchwork) 
library(ISLR2)

wage <- ISLR2::Wage
head(wage)

  p1 <- wage %>%
  ggplot(aes(x = age, y = wage)) +
  geom_point(color = "lightgrey") +
  geom_smooth(color = "blue")+
  theme_bw()

  p2 <- wage %>%
  ggplot(aes(x = year, y = wage)) +
  geom_point(color = "lightgrey") +
  geom_smooth(method = "lm", color = "blue")+
  theme_bw()

p3 <- wage %>%
  mutate(education = fct_relabel(education, ~str_extract(., "\\d"))) %>%
  ggplot(aes(x = education, y = wage)) +
  geom_boxplot(aes(fill = education),show.legend = FALSE) +
  theme(legend.position = "none")+
  theme_bw()

p1+p2+p3






