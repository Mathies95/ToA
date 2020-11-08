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
v[v==43]
v[is.na(v)]
which(v>75|v<25)

#
m1 <- matrix(c(37, 8, 51, NA, 50, 97, 86, NA, 84, 46, 17, 62L), nrow = 3)
m2 <- matrix(c(37, 8, 51, NA, 50, 97, 86, NA, 84, 46, 17, 62L), nrow = 3, byrow = TRUE)
m3 <- matrix(c(37, 8, 51, NA, 50, 97, 86, NA, 84, 46, 17, 62L), ncol = 3)


rowSums(m1,na=T)
colSums(m2,na=T)
rbind(m1,c(1,2,3,4))
rbind(c(1,2,3,4),m1)
cbind(m3,c(1,2,3,4))
m1[2,4]
m1[2:3,1:2]
m1[3,c(1,3,4)]
m1[3,]

m2[is.na(m2)]
m2[m2>50]

#data frames
str(mtcars)
library(tidyverse)
mtcars

col <- c(NA, "green", "blue", "red", NA, "blue", "green", "blue", "red", "red", 
         "blue", "green", "blue", "blue", "green", "red", "red", NA, NA, "red", 
         "green", "red", "red", NA, "green", NA, "blue", "green", "green", "red", 
         "green", "blue", NA)
mtcars <- cbind(mtcars, col)
class(mtcars$col)
mtcars[mtcars$vs==0,]

#lists
lst <- list(45, "Lars", TRUE, 80.5)
lst
x <- lst[2]
y <- lst[[2]]
class(x)
class(y)
names(lst)<-c("age","name","male","weight")
lst
lst$name
lst$height <- 173  # add component
lst$name <- list(first = "Lars", last = "Nielsen")  # change the name component
lst$male <- NULL   # remove male component
lst
lst$name$last

#string
str1 <- "Business Analytics (BA) refers to the scientific process of transforming data into insight for making better decisions in business."
str2 <- 'BA can both be seen as the complete decision making process for solving a business problem or as a set of methodologies that enable the creation of business value.'
str3 <- c(str1, str2)  # vector of strings
library(tidyverse)
str4 <- str_c(str1, 
              str2, 
              "As a process it can be characterized by descriptive, predictive, and prescriptive model building using data sources.",
              sep = " ")   # join strings
str4
str_c(str3, collapse = " ")    # collapse vector to a string

str_replace(str2, "BA", "Business Analytics")  # replace first occurrence
str_replace_all(str2, "the", "a")              # replace all occurrences

str_remove(str1, " for making better decisions in business")

str_detect(str2, "BA")  # detect a pattern

str_detect(c(str1,str2),"Business")

str5<-str_replace_all(str2,"BA","Business Analytics")
str6<-str_remove(str5,"or as a set of methodologies that enable the creation of business value")
str7<-str_c(str6,"This course will focus on programming and descriptive analytics.",sep=" ")
str8<-str_replace(str7,"analytics","business analytics")


str_replace_all(str2,"BA","Business Analytics")%>%
  str_remove("or as a set of methodologies that enable the creation of business value")%>%
  str_c("This course will focus on programming and descriptive analytics.",sep=" ")%>%
  str_replace("analytics","business analytics")
  
# module 4 loops and conditions
#svært
x <- c(1,2,-3,4)
x <- c(TRUE, FALSE, TRUE, TRUE)
all(x)
any(x)
any(!x)
all(!x)

x <- 1:15
x
if_else(x<7,as.integer(0),x)
if_else(x>10|x<7,NA_integer_,x)

x <- sample(c(1:10,NA,5.5), 1)
x
if (is.na(x)) {
  y <- "missing"
} else if (x %% 2 == 0) {
  y <- "even"
} else if (x %% 2 == 1) {
  y <- "odd"
} else if (x %% 1 > 0) {
  y <- "decimal"
}
x
y
x<-rep(NA,4)
for (i in 1:4){
  x[i]<-2*i+4
}
x  
i_val<-c(2,5,6,12)
x<-rep(NA,length(i_val))
for(i in 1:length(i_val)){
  x[i]<-2*i_val[i]+4
}
x
#while
i_val <- c(2, 5, 6, 12)
x <- rep(NA, length(i_val))
idx <- 1
while (idx < 5) {
  x[idx] <- 2 * i_val[idx] + 4
  idx <- idx + 1
}
x

2*c(1,2,3,4)+4
2*c(2,5,6,12)+4

#
library(tidyverse)
library(distill)
library(tfa)   
zips

idx <- 1:5
dat <- zips[idx,]
dat
distanceMat <- matrix(NA, nrow = length(idx), ncol = length(idx))
colnames(distanceMat) <- str_c(dat$Zip[idx], dat$Area[idx], sep = " ") 
rownames(distanceMat) <- colnames(distanceMat)
distanceMat

key <- "AlUJdApmvPe8y2_IMrC4j4x8fzytbD2M0SvlmpemL09ae_CWS6-IuNSgrAtXoyeP"
url <- str_c("http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0=",
             dat$Zip[1], ",Denmark",
             "&wp.1=",
             dat$Zip[2], ",Denmark",
             "&avoid=minimizeTolls&key=", key)
library(jsonlite)
lst <- jsonlite::fromJSON(url)
dist <- lst$resourceSets$resources[[1]]$travelDistance# der er en list som vi udtrækker [[1]] og gns distance er fra traveldistance
dist
lst$statusCode
lst$statusDescription

key <- "AlUJdApmvPe8y2_IMrC4j4x8fzytbD2M0SvlmpemL09ae_CWS6-IuNSgrAtXoyeP"
for(i in 1:nrow(distanceMat)){
  for(j in 1:ncol(distanceMat)){
    if(i>j){distanceMat[i,j]<-distanceMat[i,j];next}
    if(!is.na(distanceMat[i,j]))next
    if(i==j){distanceMat[i,j]<-0;next}
    url <- str_c("http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0=",
                 dat$Zip[i], ",Denmark",
                 "&wp.1=",
                 dat$Zip[j], ",Denmark",
                 "&avoid=minimizeTolls&key=", key)
    lst <- jsonlite::fromJSON(url)
    if(lst$statusCode==200){
      distanceMat[i,j]<-lst$resourceSets$resources[[1]]$travelDistance
    }
  }
}
distanceMat

#expandgrid
ite<-expand_grid(i=1:5,j=2:3)
key <- "AlUJdApmvPe8y2_IMrC4j4x8fzytbD2M0SvlmpemL09ae_CWS6-IuNSgrAtXoyeP"
for(r in 1:nrow(ite)){
    i=ite$i[r]
    j=ite$j[r]
    if(i==j){distanceMat[i,j]<-0;next}
    url <- str_c("http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0=",
                 dat$Zip[i], ",Denmark",
                 "&wp.1=",
                 dat$Zip[j], ",Denmark",
                 "&avoid=minimizeTolls&key=", key)
    lst <- jsonlite::fromJSON(url)
    if(lst$statusCode==200){
      distanceMat[i,j]<-lst$resourceSets$resources[[1]]$travelDistance
      distanceMat[j,i]<-distanceMat[i,j]
    }
  }

distanceMat

# functions
sum_n<-function(x){
  sum(1:x)
}
sum_n(5000)

compute_s_n<-function(x){
  return(sum((1:x)^2))
}
compute_s_n(10)

s_n<-vector("numeric",25)
s_n

for(i in 1:25){
  s_n[i]<-(i*(i+1)*(2*i+1))/6
}
s_n
# with confirmation
for(i in 1:25){
  if(s_n[i]!=(i*(i+1)*(2*i+1))/6){
    cat('Error!')
    break
  }
}
s_n

biggest<-function(x,y){
  if(x>y)
    return("1")
  else
    return("0")
}
biggest(2,1)

shipping1<-function(x){
  return(x*0.1)
}
shipping(10)

shipping2<-function(x,pct=0.1){
  return(x*pct)
}
shipping(450,0.2)

gas<-function(x){
  return(shipping1(x)*0.5)
}
gas(10)

gas2<-function(x,...){
  return(shipping2(x,...)*0.5)
}
gas2(10,pct=0.2)

cost<-function(x,...){
  lst<-list(shippin=shipping2(x,...),cost=x,gasoline=gas2(x,...))
  return(lst)
}
cost(450,pct=0.15)

x <- 3
my_func <- function(y){
  x <- 5
  return(y + 5)
}
my_func(7)

x <- 3
my_func <- function(y){
  return(y + x) 
}
my_func(7)
?'<<-'

x <- 3
my_func <- function(y){
  x <- 4
  x <<- 5#undgå
  return(y + 5)
}
#job seqeuence
startup_costs <- c(27, 28, 32, 35, 26)
setup_costs <- matrix(c(
  NA, 35, 22, 44, 12,
  49, NA, 46, 38, 17,
  46, 12, NA, 29, 41,
  23, 37, 31, NA, 26,
  17, 23, 28, 34, NA), 
  byrow = T, nrow = 5)

better <- function(startup, setup) {
  jobs <- nrow(setup)
  min_col_val <- apply(rbind(startup, setup), 2, min, na.rm = T)
  startup <- startup - min_col_val
  min_mat <- matrix(rep(min_col_val, jobs), 
                    ncol = jobs, byrow = T)
  setup <- setup - min_mat
  lst <- greedy(startup,setup)
  lst$cost <- lst$cost + sum(min_col_val)
  return(lst)
}
better(startup_costs, setup_costs)

