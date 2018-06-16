
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

abus <- crbs %>% 
  select(CTD, abundances) %>% 
  na.omit %>% 
  mutate(abundances = log(1 + abundances))
        
resopt <- crossing(
  dep = seq(10, 200, length = 20), 
  kpvars = kpvars
  ) %>% 
  group_by(dep, kpvars) %>% 
  nest %>% 
  mutate(
    est = pmap(list(dep, kpvars), function(dep, kpvars){
      
      # get depth integrated data
      depsub <- envdat %>% 
        dplyr::select_(.dots = c('depth', 'CTD', vr)) %>% 
        filter(depth >= dep) %>% 
        gather('var', 'val', -depth, -CTD) %>% 
        arrange(CTD, depth) %>% 
        group_by(CTD, var) %>% 
        summarise(val = mean(val, na.rm = T)) %>% 
        left_join(abus, by = 'CTD')
      
      avestr <- mean(depsub$val)
      sdstr <- sd(depsub$val)
      modest <- nls(abundances ~ k*exp(-1/2*(val-mu)^2/sigma^2), start=c(mu=avestr,sigma=sdstr,k=1) , data = depsub)
      
      prddat <- seq(min(depsub$val), max(depsub$val), length = 100) %>% 
        data.frame(val = .)
      fitdt <- predict(modest, newdata = prddat)
      prddat <- data.frame(prddat, abundances = fitdt)
      
      ggplot(depsub, aes(x = val, y = abundances)) + 
        geom_point() + 
        geom_line(data = prddat)
      
      coffs <- coefficients(modest)
      muval <- coffs['mu']
      sival <- coffs['sigma']
      
      out <- qnorm(c(0.05, 0.5, 0.95), muval, sival)
      
      return(out)
      
    })
  )




# valrng <- range(depsub$val)
# valbin <- seq(valrng[1], valrng[2], length = 20)
# depsub <- depsub %>% 
#   mutate(valbin = cut(val, breaks = valbin)) %>% 
#   group_by(valbin) %>% 
#   summarise(abundances = mean(abundances, na.rm = T)) %>% 
#   na.omit
# 
# 
# ggplot(depsub, aes(x = valbin, y = abundances)) + 
#   geom_bar(stat = 'identity') + 
#   geom_line(data = prddat)

avestr <- mean(depsub$val)
sdstr <- sd(depsub$val)
modest <- nls(abundances ~ k*exp(-1/2*(val-mu)^2/sigma^2), start=c(mu=avestr,sigma=sdstr,k=1) , data = depsub)

prddat <- seq(min(depsub$val), max(depsub$val), length = 100) %>% 
  data.frame(val = .)
fitdt <- predict(modest, newdata = prddat)
prddat <- data.frame(prddat, abundances = fitdt)

ggplot(depsub, aes(x = val, y = abundances)) + 
  geom_point() + 
  geom_line(data = prddat)

coffs <- coefficients(modest)
muval <- coffs['mu']
sival <- coffs['sigma']

qnorm(c(0.05, 0.5, 0.95), muval, sival
