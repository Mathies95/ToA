#module 3
install.packages("devtools")
library(devtools)

library(tidyverse)
mtcars %>% 
  select(am, gear, hp, mpg) %>% 
  filter(gear < 4 & mpg<20)

mtcars %>% 
  select(am, gear, hp, mpg) %>% 
  filter(gear == 4 | mpg<20)
str(mtcars)
mtcars %>% 
  filter(gear == 4 , mpg<20)%>%
  select(vs,wt)
  