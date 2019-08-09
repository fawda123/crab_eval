library(tidyverse)

data(dissdat)
data(lengdat)
data(envdatdelt)
data(shoreloc)

lng <- lengdat %>% 
  filter(lenvar %in% 'CW') %>% 
  select(-stelen, -shoreloc) %>% 
  spread(lenvar, avelen)

dissdat <- dissdat %>% 
  spread(prt, disval)

# station locatins
statloc <- read.csv('data/raw/CTD_data.csv', stringsAsFactors = FALSE ) %>% 
  select(Sta, Long, Lat) %>% 
  rename(CTD = Sta) %>% 
  unique %>% 
  na.omit()

# get all values and only ph delt
envdatdelt <- envdatdelt %>% 
  filter(!var %in% 'DIC') %>% 
  gather('envtyp', 'envval', val, delt) %>% 
  unite('var', var, envtyp, sep = '') %>% 
  filter(grepl('val$|^pHdelt$', var)) %>% 
  mutate(var = gsub('\\val$', '', var)) %>% 
  rename(val = envval) %>% 
  mutate(
    val = ifelse(var %in% 'Chla', log10(val), val)
  ) 

alldat <- envdatdelt %>% 
  filter(depth == 60) %>% 
  filter(var %in% c('pH', 'pHdelt', 'Chla')) %>% 
  select(-depth) %>% 
  spread(var, val) %>% 
  full_join(dissdat, by = c('CTD')) %>% 
  mutate(
    external = (`body part` + legs) / 2
  ) %>% 
  full_join(lng, by = 'CTD') %>% 
  full_join(shoreloc, by = 'CTD') %>% 
  left_join(statloc, by = 'CTD')

# chl plot
toplo <- alldat %>% 
  select(CTD, Long, Lat, Chla) %>% 
  filter(Lat > 45.5) %>% 
  na.omit
ggplot(toplo, aes(x = Long, y = Lat)) + 
  geom_point(aes(fill = 10^Chla), pch = 21, size = 3) + 
  coord_map() +
  scale_fill_distiller(type = 'seq', palette = "RdYlBu", direction = 1)

# internal

tomod1 <- alldat %>% 
  select(Chla, pH, internal, CTD) %>% 
  na.omit

vifv <- lm(Chla ~ pH, tomod1)
rsq <- vifv %>% 
  summary %>% 
  .$r.squared
vifv <- 1 / (1 - rsq)

mod1 <- lm(internal ~ pH + Chla, data = tomod1)
step(mod1)

# external

tomod2 <- alldat %>% 
  select(Chla, pHdelt, external, CTD) %>% 
  na.omit

vifv <- lm(Chla ~ pHdelt, tomod2)
rsq <- vifv %>% 
  summary %>% 
  .$r.squared
vifv <- 1 / (1 - rsq)
mod2 <- lm(external ~ pHdelt + Chla, data = tomod2)
step(mod2)

# CW

tomod3 <- alldat %>% 
  select(Chla, external, CW, CTD, shoreloc) %>% 
  filter(shoreloc %in% 'onshore') %>% 
  na.omit

vifv <- lm(Chla ~ external, tomod3)
rsq <- vifv %>% 
  summary %>% 
  .$r.squared
vifv <- 1 / (1 - rsq)
mod3 <- lm(CW ~ external + Chla, data = tomod3)
step(mod3)
