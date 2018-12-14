######
# for crab_explr.Rmd, results.Rnw

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

######
# for disseval.Rmd

library(tidyverse)
library(readxl)

##
# dissolution data
dissdat <- read_excel('raw/Copy of SEM crab scoring sheet_renewed analyses_finalised.xlsx', skip = 2)
nms <- names(dissdat)
nms <- case_when(
  nms %in% c('X__3') ~ 'diss_pore',
  nms %in% c('X__6') ~ 'diss_ridg', 
  nms %in% c('X__9') ~ 'diss_crys', 
  nms == 'CTD station' ~ 'CTD',
  nms == 'Body part' ~ 'prt',
  T ~ nms
)
names(dissdat) <- nms
dissdat <- dissdat %>% 
  select(CTD, prt, diss_pore, diss_ridg, diss_crys) %>% 
  .[-1, ] %>% 
  fill_('CTD') %>% 
  mutate(prt = gsub('^Body part$', 'body part', prt)) %>% 
  mutate_if(!names(.) %in% 'prt', as.numeric) %>% 
  gather('distyp', 'disval', -CTD, -prt)

save(dissdat, file = 'data/dissdat.RData', compress = 'xz')

##
# length data, all
lengdatall <- read_excel('raw/Long deliniated lengths.xlsx') %>% 
  rename(
    shoreloc = `onshore = 2; offshore 1`
  ) %>% 
  mutate(
    shoreloc = case_when(
      shoreloc == 2 ~ 'onshore', 
      shoreloc == 1 ~ 'offshore'
    ),
    CL = ifelse(CL > 9, NA, CL)
  )

# length dat, summarized
lengdat <- lengdatall %>% 
  gather('lenvar', 'lenval', -CTD, -shoreloc) %>%
  group_by(CTD, shoreloc, lenvar) %>%
  summarise(
    avelen = mean(lenval, na.rm = T),
    stelen = 1.96 * sd(lenval, na.rm = T)/(length(na.omit(lenval)))
  )

save(lengdat, file = 'data/lengdat.RData', compress = 'xz')

######
# estimate delta values for selected environmental variables
# first, gam is created to predict estimated env var at zero depth
# then, delt value is estimated

data(envdat)

envdatdelt <- envdat %>% 
  select(CTD, depth, Aragonite, Temperature, pH) %>% 
  gather('var', 'val', -CTD, -depth) %>% 
  filter(!CTD %in% c(109, 128)) %>% 
  group_by(CTD, var) %>% 
  nest %>% 
  mutate(
    zeroval = map(data, function(x){
      
      # extrapolate env variable at zero depth
      mod <- gam(val ~ s(depth, bs = 'cs'), data = x)
      predict(mod, newdata = data.frame(depth = 0))
      
    })
  ) %>% 
  unnest(zeroval) %>% 
  unnest %>% 
  mutate(delt = zeroval - val) %>% 
  select(-zeroval)

save(envdatdelt, file = 'data/envdatdelt.RData', compress = 'xz')
