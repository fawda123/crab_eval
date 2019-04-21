# checking new delta values by stations with growth
library(tidyverse)
library(readxl)

source('R/funcs.R')

data(crbs)

# delta environmental data
deldat <- read_excel('data/raw/delta values for crabs.xlsx') %>% 
  rename(CTD = Station)

# combine env and crb data
cmbdat <- crbs %>% 
  inner_join(deldat, by = 'CTD') %>% 
  gather('var', 'val', -CTD, -abundances, -pa, -Average.CL, -diss) %>% 
  separate(var, c('delta', 'var', 'dep')) %>% 
  select(-delta)

# abundance
p1 <- ggplot(cmbdat, aes(x = val, y = abundances, fill = factor(dep), group = factor(dep), colour = factor(dep))) +
  geom_point(pch = 21, size = 4, colour = 'black') +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~var, scales = 'free_x', strip.position = 'bottom') +
  theme_bw(base_family = 'serif', base_size = 16) +
  theme(
    strip.background = element_blank(), 
    strip.placement = 'outside', 
    axis.title.x = element_blank(), 
    legend.position = 'top', 
    strip.text = element_blank()
  ) +
  scale_fill_manual('Depth', values = c("skyblue", "slateblue")) +
  scale_colour_manual('Depth', values = c("skyblue", "slateblue")) +
  scale_y_continuous('log10-abundance', trans = 'log10')

# growth as carapace length
p2 <- ggplot(cmbdat, aes(x = val, y = Average.CL, fill = factor(dep), group = factor(dep), colour = factor(dep))) +
  geom_point(pch = 21, size = 4, colour = 'black') +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~var, scales = 'free_x', strip.position = 'bottom') +
  theme_bw(base_family = 'serif', base_size = 16) +
  theme(
    strip.background = element_blank(), 
    strip.placement = 'outside', 
    axis.title.x = element_blank(), 
    legend.position = 'none', 
    strip.text = element_blank()
  ) +
  scale_fill_manual('Depth', values = c("skyblue", "slateblue")) +
  scale_colour_manual('Depth', values = c("skyblue", "slateblue")) +
  scale_y_continuous('Growth (carapace length)')

# dissolution
p3 <- ggplot(cmbdat, aes(x = val, y = diss, fill = factor(dep), group = factor(dep), colour = factor(dep))) +
  geom_point(pch = 21, size = 4, colour = 'black') +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~var, scales = 'free_x', strip.position = 'bottom') +
  theme_bw(base_family = 'serif', base_size = 16) +
  theme(
    strip.background = element_blank(), 
    strip.placement = 'outside', 
    axis.title.x = element_blank(), 
    legend.position = 'none'
  ) +
  scale_fill_manual('Depth', values = c("skyblue", "slateblue")) +
  scale_colour_manual('Depth', values = c("skyblue", "slateblue")) +
  scale_y_continuous('Dissolution')

# pdf('figs/chkdelts.pdf', height = 9, width = 9, family = 'serif')
# grid.arrange(
#   p1, p2, p3, ncol = 1, heights = c(1, 0.8, 0.9)
# )
# dev.off()

# correlation values
cordat <- cmbdat %>% 
  mutate(
    log10abu = abundances
  ) %>% 
  select(-abundances, -pa) %>% 
  gather('biovr', 'biovl', log10abu, Average.CL, diss) %>% 
  group_by(biovr, var, dep) %>% 
  nest %>% 
  mutate(
    corval = map(data, function(x){
      
      tst <- cor.test(x$val, x$biovl, method = 'pearson')
      corvl <- round(tst$estimate, 2)
      prsvl <- p_ast(tst$p.value)
      out <- paste0(corvl, prsvl)
      
      return(out)
      
    }) 
  ) %>% 
  select(-data) %>% 
  unnest