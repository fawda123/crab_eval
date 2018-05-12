library(R.matlab)
library(tidyverse)
library(readxl)
library(tibble)

# matlab matrix data
raw <- readMat('raw/DataForNina.mat')

# really small dissolution dataset
crbdisraw <- read_excel('raw/crab_diss.xlsx')

##
# crab data

# names of crab variables, unknown might be 'night'
crnm <- c('CTD', 'unk', 'abundances', 'Average CL', 'Average CW', 
          'Average R-DCS', 'Average TL', 'pa')

crbs <- raw$Crabs %>% 
  as.tibble %>% 
  rename_all(funs(make.names(crnm))) %>% 
  dplyr::select(CTD, abundances, pa, Average.CL)

# dissolution data, seven CTD stations, true zeroes
crbdis <- crbdisraw %>% 
  rename(CTD = stations)
crbs <- crbs %>% 
  left_join(crbdis, by = 'CTD')

save(crbs, file = 'data/crbs.RData', compress = 'xz')

##
# variable names

vrnm <- raw$VarList %>% 
  unlist %>% 
  make.names %>% 
  c('depth', 'CTD', .)

##
# environmental data

# convert array to lis
envdat <- raw$Data %>% 
  array_tree(margin = 1) %>% 
  enframe('depth') %>% 
  mutate(
    depth = 10 * depth, 
    value = map(value, ~ data.frame(CTD = crbs$CTD, .))
  ) %>% 
  unnest %>% 
  rename_all(funs(c(vrnm)))

save(envdat, file = 'data/envdat.RData', compress = 'xz')
