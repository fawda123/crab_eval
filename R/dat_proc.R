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

# dissolution data, seven CTD stations, true zeroes, recode levels
crbdis <- crbdisraw %>% 
  rename(diss = `Outside carapace`) %>% 
  arrange(CTD)
  
# join dissolution with other crb data
crbs <- crbs %>% 
  full_join(crbdis, by = 'CTD') %>% 
  arrange(CTD)

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
    value = map(value, ~ data.frame(CTD = raw$Crabs[,1], .))
  ) %>% 
  unnest %>% 
  rename_all(funs(c(vrnm)))

# manually add data from Nina
mandat <- tibble(
  CTD = c(109, 128),
  Temperature = c(7.59, 10.34),
  Aragonite = c(0.89, 1.73), 
  depth = c(100, 100)
)

# use rbind.fill to combine
envdat <- plyr::rbind.fill(envdat, mandat) %>%
  arrange(depth, CTD)
  
save(envdat, file = 'data/envdat.RData', compress = 'xz')
