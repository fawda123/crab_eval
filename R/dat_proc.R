######
# for crab_explr.Rmd, results.Rnw

library(R.matlab)
library(tidyverse)
library(readxl)
library(tibble)
library(lubridate)

source('R/funcs.R')

# matlab matrix data
raw <- readMat('data/raw/DataForNina.mat')

# really small dissolution dataset
crbdisraw <- read_excel('data/raw/crab_diss.xlsx')

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

######
# for disseval.Rmd

library(tidyverse)
library(readxl)

##
# dissolution data
dissdat <- read_excel('data/raw/Copy of SEM crab scoring sheet_renewed analyses_finalised.xlsx', skip = 2)
nms <- names(dissdat)
nms <- case_when(
  nms %in% c('...7') ~ 'diss_pore',
  nms %in% c('...11') ~ 'diss_ridg', 
  nms %in% c('...15') ~ 'diss_crys', 
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
  gather('distyp', 'disval', -CTD, -prt) %>% 
  mutate(disval = disval / 2) # severity of relative dissolution

# combine dissolution measures 
# average ridges, crystals for body part legs
# average ridges, crystals, pore for internal
dissdat <- dissdat %>% 
  filter(!(distyp %in% 'diss_pore')) %>% 
  group_by(CTD, prt) %>% 
  summarise(disval = mean(disval, na.rm = T)) %>% 
  ungroup
  
save(dissdat, file = 'data/dissdat.RData', compress = 'xz')

##
# length data, all
lengdatall <- read_excel('data/raw/Long deliniated lengths.xlsx') %>% 
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
  ) %>% 
  ungroup

save(lengdat, file = 'data/lengdat.RData', compress = 'xz')

######
# envdatdelt

# estimate delta values for selected environmental variables
# first, gam is created to predict estimated env var at zero depth
# then, delt value is estimated

CTD.df <- read.csv('data/raw/CTD_data.csv', stringsAsFactors = FALSE )

envdatdelt <- crossing(
  var = c('pH.TOT', 'Arag', 'Calc', 'CTD.T', 'DIC', 'pCO2', 'CTD.O2'),
  depth = seq(10, 200, by = 10),
  Stn = unique(CTD.df$Sta[!is.na(CTD.df$Sta)])
) %>% 
  rowwise() %>% 
  mutate(
    val = calc.1.V(data.frame(Stn = Stn), CTD.df, var, depth, 0, c(0, depth), depth)[[2]],
    delt = calc.1.V(data.frame(Stn = Stn), CTD.df, var, depth, 0, c(0, depth), depth)[[1]],
    var = case_when(
      var %in% 'Arag' ~ 'Aragonite',
      var %in% 'Calc' ~ 'Calcite', 
      var %in% 'pH.TOT' ~ 'pH', 
      var %in% 'CTD.T' ~ 'Temperature', 
      var %in% 'pCO2' ~ 'pCO2',
      var %in% 'CTD.O2' ~ 'Oxygen', 
      var %in% 'DIC' ~ 'DIC'
    )
  ) %>% 
  ungroup %>% 
  rename(
    CTD = Stn
  ) %>% 
  select(CTD, var, depth, val, delt) %>% 
  arrange(CTD, var, depth)

##
# chl data, only discrete surface samples

chldat <- read_excel('data/raw/WCOA16_data_12-20-2017.xlsx') %>%
  select(STATION_NO, YEAR_UTC, MONTH_UTC, DAY_UTC, CHL_A_UG_L_GFF) %>% 
  unite('date', YEAR_UTC, MONTH_UTC, DAY_UTC, sep = '-') %>% 
  mutate(date = ymd(date)) %>% 
  rename(
    CTD = STATION_NO, 
    Chla = CHL_A_UG_L_GFF
  ) %>% 
  filter(CTD %in% envdatdelt$CTD) %>% 
  filter(Chla != -999) %>% 
  group_by(CTD) %>% 
  summarise(Chla = mean(Chla))

# add dummy depths to chldat so it works with shiny
dumdep <- crossing(
  CTD = unique(chldat$CTD), 
  depth = unique(envdatdelt$depth)
)

chldat <- dumdep %>% 
  left_join(chldat, by = 'CTD') %>% 
  mutate(
    var = 'Chla',
    delt = NA
    ) %>% 
  rename(val = Chla) %>% 
  select(CTD, var, depth, val, delt)

# combine with envdatdalt
envdatdelt <- envdatdelt %>% 
  rbind(chldat) %>% 
  arrange(var, depth)

save(envdatdelt, file = 'data/envdatdelt.RData', compress = 'xz')

######
# get off/onshore designations
# add 109, 128 manually

data(lengdat)

# onshore, offshore
shoreloc <- lengdat %>% 
  select(CTD, shoreloc) %>% 
  unique

toadd <- tibble(
  CTD = c(109, 128), 
  shoreloc = c('onshore', 'offshore')
)

shoreloc <- bind_rows(shoreloc, toadd) %>% 
  arrange(CTD)

save(shoreloc, file = 'data/shoreloc.RData', compress = 'xz')
