# playing with dissolution data

library(tidyverse)
library(readxl)
library(ggord)

data(crbs)

# environment data 100m, 30m
envall<- read_excel('raw/WCOA16_depth_limited_stats.xlsx') %>% 
  select(matches(c("^CTD Station$|100 m MEAN"))) %>% 
  rename(
    CTD = `CTD Station`, 
    arag = `Arag to 100 m MEAN`,
    ph = `pH to 100 m MEAN`,
    oxy = `Oxy [umol/kg] to 100 m MEAN`, 
    temp = `T [deg C] to 100 m MEAN`,
    alk = `TA [umol/kg] to 100 m MEAN`
  ) %>% 
  mutate_if(is.character, as.numeric)

# crab data joined with env data
toeval <- crbs %>% 
  select(CTD, diss) %>% 
  na.omit %>% 
  left_join(envall, by = 'CTD') %>% 
  .[-6, ]
  
# prepped for first plot
toplo <- toeval %>% 
  gather('var', 'val', -CTD, -diss)

ggplot(toplo, aes(x = val, y = diss)) + 
  geom_point() + 
  stat_smooth(method = 'lm') +
  facet_wrap(~var, scales = 'free_x')

corests <- toeval %>% 
  gather('var', 'val', -CTD, -diss) %>% 
  group_by(var) %>% 
  nest %>% 
  mutate(
    val = map(data, function(x){
      
      tst <- cor.test(x$val, x$diss, method = 'spearman')
      est <- tst$estimate
      pvl <- p_ast(tst$p.value)
      out <- paste(round(est, 2), pvl)
      
      return(out)
      
      }
      )
    ) %>% 
  select(-data) %>% 
  unnest
