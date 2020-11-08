library(distill)
library(tfa)
#se module6-7.RMD

#tibbles
library(tidyverse)
as.tibble()
as.tibble(airquality)
airquality

?dplyr::tibble()
as_tibble(col)

dat <- tibble(name = c("Hans", "Ole"), 
              age = c(23, 45), 
              misc = list(
                list(status = 1, comment = "To young"), 
                list(comment = "Potential candidate")))
dat

#7 exporting data

url <- "https://api.statbank.dk/v1/data/FLYV41/CSV?lang=en&LUFTHAVN=*&Tid=*&Transport=*"
cat(read_lines(url, n_max = 3), sep = "\n")#data deliminitor

dat<-read_csv2(url)

library(jsonlite)
url <- "https://api.statbank.dk/v1/tableinfo/FLYV41?lang=en"
lst <- read_json(url, simplifyVector = T)
View(lst)

info <- function(tab_id) {
  url <- str_c("https://api.statbank.dk/v1/tableinfo/FLYV41?lang=en")
  lst <- read_json(url, simplifyVector = T) 
  return(list(description = lst$description, unit = lst$unit,variables=lst$variables[,1:2]))
}
info("FLYV41")

url <- "https://api.statbank.dk/v1/tables?lang=en"
lst <- jsonlite::read_json(url, simplifyVector = T)
View(lst)

tab_id <- "FOLK1A"
url <- str_c("https://api.statbank.dk/v1/tableinfo/", tab_id, "?lang=en")
lst <- read_json(url, simplifyVector = T) 
col_id <- c(2,3,5)  # column ids in lst$variables$id
cols <- lst$variables$id[col_id]
url <- str_c("https://api.statbank.dk/v1/data/", tab_id, "/CSV?lang=en&", str_c(cols, collapse = "=*&"), "=*") %>% 
  URLencode()
url

get_data<-function(tab_id,col_id){
  url <- str_c("https://api.statbank.dk/v1/tableinfo/", tab_id, "?lang=en")
  lst <- read_json(url, simplifyVector = T) 
  cols <- lst$variables$id[col_id]
  url <- str_c("https://api.statbank.dk/v1/data/", tab_id, "/CSV?lang=en&", str_c(cols, collapse = "=*&"), "=*") %>% 
    URLencode()
}
dat<-get_data("FOLK1A",c(2,3,5))
dat
write_csv(dat,"test.csv")
install.packages("openxlsx")
library(openxlsx)
write.xlsx(dat,"test.xlsx",sheetName="folka1a") 

#write to googlesheet
install.packages("googlesheets4")
library(googlesheets4)
gs <- gs4_create("test")
write_sheet(dat, ss = gs, sheet = "FOLK1A")
gs4_browse(gs)

#OPL

file="test.dat"
write_lines("nurses = {",file)
write_lines(' <"anne",11>',file,append = T)
write_lines('};',file,append = T)
cat(read_file("test.dat"))

library(tidyverse)
nurses <- read_csv(system.file("extdata/nurses.csv", package = "tfa"))
shifts <- read_csv(system.file("extdata/shifts.csv", package = "tfa"))

nurses%>%#we add "" to all names and make a new col
  mutate(across(where(is.character),
                ~str_c('"',.x,'"')))%>%
  unite("tuple",everything(),sep=",",remove = F)%>%
  mutate(tuple=str_c('<',tuple,'>'))%>%
  pull(tuple)%>%
  str_c(collapse = ",\n")
        
#takes nurse as inpt and write aas a tuple
write_tuple<-function(dat,file){
  write_lines("nurses = {",file,sep = "\n  ")
  tuples<-dat%>%
    mutate(across(where(is.character),
                  ~str_c('"',.x,'"')))%>%
    unite("tuple",everything(),sep=",",remove = F)%>%
    mutate(tuple=str_c('<',tuple,'>'))%>%
    pull(tuple)%>%
    str_c(collapse = ",\n")
  write_lines(tuples,file,append = T)
  write_lines('};',file,append = T)
}
file<-"test.dat"
write_tuple(nurses,file)
cat(read_file("test.dat"))

#work on shifts
write_tuple<-function(dat,file,append=FALSE){
  name<-deparse(substitute(dat))
  write_lines(str_c(name,"={"),file,sep = "\n  ",append = append)
  tuples<-dat%>%
    mutate(across(where(is.character),
                  ~str_c('"',.x,'"')))%>%
    unite("tuple",everything(),sep=",",remove = F)%>%
    mutate(tuple=str_c('<',tuple,'>'))%>%
    pull(tuple)%>%
    str_c(collapse = ",\n")
  write_lines(tuples,file,append = T)
  write_lines('};',file,append = T)
}
file<-"test.dat"
write_tuple(shifts,file)
cat(read_file("test.dat"))

file<-"test.dat"
write_tuple(nurses,file)
write_tuple(shifts,file,append = T)
cat(read_file("test.dat"))

