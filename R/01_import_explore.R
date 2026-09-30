library(readxl)

#first dataset with prescription information
prescriptions <- read_excel("data/h254a.xlsx")
dim(prescriptions)

names(prescriptions)
head(prescriptions)
#list of prescription dataset with few variables that are relavant 
head(prescriptions[c("DUPERSID", "RXNAME", "RXQUANTY",
                     "RXSF24X", "RXPV24X", "RXMR24X",
                     "RXMD24X", "RXXP24X")])
#2nd dataset with information about the people, demographics
people <- read_excel("data/h256.xlsx")

dim(people)

names(people)
#list of people dataset with few variables that are relavant 
head(people[c("DUPERSID", "AGE24X", "TTLP24X",
              "POVCAT24", "INSCOV24", "PERWT24F")])
#checking peoples insurance
table(people$INSCOV24)
#checking poverty level
table(people$POVCAT24)
#checking how many unique people are in prescriptions dataset
length(unique(prescriptions$DUPERSID))
#checking if all those people are also found in people dataset
sum(unique(prescriptions$DUPERSID) %in% people$DUPERSID)
