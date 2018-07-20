# evaluating abundance by temperature and aragonite saturation

library(tidyverse)

data(envdat)
data(crbs)

depv <- c(10, 100)

toplo <- envdat %>% 
  filter(depth %in% depv) %>% 
  left_join(crbs) %>% 
  select(CTD, depth, abundances, pa, Temperature, Aragonite) %>% 
  filter(!is.na(abundances)) %>% 
  mutate(pa = factor(pa, levels = c('0', '1'), labels = c('absent', 'present')))

# # x, y values for linear interp grid, long form
# expand <- 50
# new_grd <- with(toplo,
#                 expand.grid(
#                   Temperature = seq(min(Temperature), max(Temperature), length = expand),
#                   Aragonite = seq(min(Aragonite), max(Aragonite), length = expand)
#                 )
# )
# 
# # abundance in wide form
# toint <- toplo %>% 
#   select(-depth, -CTD, -pa) %>% 
#   spread(Aragonite, abundances)
# 
# # get interped values
# int_val <- fields::interp.surface(
#   obj = list(
#     x = toint$Temperature,
#     y = as.numeric(names(toint)[-1]),
#     z = as.matrix(toint[, -1])
#   ),
#   loc = new_grd
# )
# new_grd <- data.frame(new_grd, abundances = int_val)

p <- ggplot() +
  geom_point(data = toplo, aes(x = Temperature, y = Aragonite, size = abundances, colour = pa), alpha = 0.7) +
  geom_contour(data = new_grd, aes(x = Temperature, y = Aragonite, z = abundances)) +
  facet_wrap(~depth) +
  scale_colour_manual('', values = c('grey', 'blue')) +
  theme_bw() +
  theme(
    strip.background = element_blank()
  )

pdf('C:/Users/Marcus/Desktop/crbaratmp.pdf', height = 3, width = 7, family = 'serif')
p
dev.off()
ggplot(new_grd, aes(x = Temperature, y = Aragonite, z = abundances, fill = abundances)) +
  geom_tile()
ggplot(faithfuld, aes(waiting, eruptions, z = density, fill = density)) +
  geom_contour()