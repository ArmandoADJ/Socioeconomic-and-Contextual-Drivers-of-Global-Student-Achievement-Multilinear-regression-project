library(haven)
library(tidyverse)
install.packages("countrycode")
library(countrycode)
df <- read_sav("C:/Users/Arman/OneDrive/Documentos/R Class/Project 7130/Data/CY08MSP_STU_QQQ.SAV")
df1 <- read_sav("C:/Users/Arman/OneDrive/Documentos/R Class/Project 7130/Data/CY08MSP_SCH_QQQ.SAV")

Merged <- df %>%
  merge(df1, by= c("CNT", "CNTSCHID"))
print(Merged)

keep <- Merged[c("CNT","CNTSCHID", "ESCS","ST004D01T", "AGE", "REPEAT", "ST255Q01JA",
                "STUDYHMW", "TARDYSD", "STRATIO", "SC061Q07TA", "OECD.x",
                 "LANGN", "IMMIG", "ST322Q02JA","ICTRES","PV1MATH", "PV2MATH", "PV3MATH", 
                 "PV4MATH", "PV5MATH", "PV6MATH", "PV7MATH", "PV8MATH", "PV9MATH", "PV10MATH",
                 "PV1READ","PV2READ", "PV3READ", "PV4READ", "PV5READ", "PV6READ",
                 "PV7READ", "PV8READ", "PV9READ","PV10READ", "PV1SCIE", 
                 "PV2SCIE", "PV3SCIE", "PV4SCIE", "PV5SCIE", "PV6SCIE" ,
                 "PV7SCIE", "PV8SCIE", "PV9SCIE", "PV10SCIE"
                 )]


print(keep)

print(colSums(is.na(keep)))

print(colSums(is.na(keep)))

keep["CONTI"] <- countrycode(sourcevar = keep$CNT, origin = "iso3c", 
                             destination = "continent")

keep$CONTI[keep$CNT == "KSV"] <- "Europe"
keep$CONTI[keep$CNT == "QAZ"] <- "Asia"
keep$CONTI[keep$CNT == "QUR"] <- "Europe"
keep$CONTI[keep$CNT == "TAP"] <- "Asia"

Route <- "C:/Users/Arman/OneDrive/Documentos/R Class/Project 7130/Data/Cleaned_Data.SAV"
write_sav(keep,Route)



























