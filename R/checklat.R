library(tidyverse)
library(readxl)

data(crbs)

# station lat/lon
statloc <- read_excel('raw/WCOA16_CTD-and-MAP-Station_Numbers.xlsx') %>% 
  select(Station, Lat, Long) %>% 
  rename(CTD = Station)

crbslat <- crbs %>% 
  left_join(statloc, by = 'CTD') %>% 
  mutate(abundances = log(1 + abundances)) %>% 
  select(-pa, -diss) %>% 
  gather('var', 'val', -CTD, -Lat, -Long)

p1 <- ggplot(crbslat, aes(x = Lat, y = val)) + 
  geom_point() + 
  facet_wrap(~var) + 
  theme_bw()

p2 <- ggplot(crbslat, aes(x = Long, y = Lat, size = val)) + 
  geom_point() +
  coord_map() + 
  facet_wrap(~var) +
  theme_bw()

pdf('C:/Users/Marcus/Desktop/checklat.pdf', height = 5, width = 5, family = 'serif')
p1
p2
dev.off()
