A <- array(seq(24), dim = c(3,4,2))
A


library(purrr)
purrr::map(1:15, function(t) {root <- sqrt(t)})

as.list(sqrt(1:15))

apply(datasets::anscombe, 2, mean)

lapply(datasets::anscombe, var)

remotes::install_github("allisonhorst/palmerpenguins")

library(palmerpenguins)
aa <- palmerpenguins::penguins_raw
palmerpenguins::penguins_raw %>%
  select(Species, Island , `Culmen Depth (mm)`, `Flipper Length (mm)`, `Culmen Length (mm)`) %>%
  filter(`Flipper Length (mm)` >= mean(`Flipper Length (mm)`, na.rm = T)) %>%
  mutate(surface = `Culmen Depth (mm)`*`Culmen Length (mm)`) %>%
  arrange(desc(surface))


