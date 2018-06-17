# comparing raw data with back-transformed values
# raw data are depth integrated
# code here can be used to get observed values at each depth that are not integrated

library(tidyverse)
library(glmulti)
library(MuMIn)
library(gridExtra)
library(Hmisc)
library(grid)
library(scales)
library(plotly)
source('R/funcs.R')

data(crbs)
data(envdat)

# exp vars to use in reg mods
kpvars <- c('pH', 'Temperature', 'Aragonite')

## checking depth integration

# exp vars to use in reg mods
kpvars <- c('pH', 'Temperature', 'Aragonite')

toplo <- envdat %>%
  dplyr::select_(.dots = c('depth', 'CTD', kpvars)) %>%
  gather('var', 'val', -depth, -CTD) %>%
  group_by(CTD, var) %>%
  arrange(CTD, var, depth) %>%
  mutate(
    valbck = val * 1:n(),
    valbck = c(val[1], diff(valbck))
  ) %>%
  ungroup %>%
  gather('trns', 'val', val, valbck) %>%
  filter(CTD == 64)

ggplot(toplo, aes(x = val, y = depth)) +
  geom_path() +
  geom_point() +
  facet_wrap(trns ~ var, scales = 'free_x') +
  scale_y_reverse()

# example data
ex <- data.frame(
  val = c(1:10),
  depth = c(1:10)
) %>%
  mutate(
    valint = cumsum(val) / 1:nrow(.),
    valbck = valint * 1:nrow(.),
    valbck = c(valint[1], diff(valbck))
  )

