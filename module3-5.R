#module 3

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

#3.10.4
dir.create("subfolder", showWarnings = FALSE) 
write_file("Some text in a file", path = "test1.txt")
write_file("Some other text in a file", path = "subfolder/test2.txt")
read_file("test1.txt")
read_file("test2.txt")

n<-100
n*(n+1)/2

n<-1000
n*(n+1)/2

n <- 1000
x <- seq(1, n)
sum(x)

set.seed(123)
v <- sample.int(100,30)
v
sum(v)
mean(v)
sd(v)
v[c(1,4,6,15)]
v[v>50]
v[v>50|v<75]
