
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
  full_join(shoreloc, by = 'CTD')

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
