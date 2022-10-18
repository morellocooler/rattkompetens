# load packages
library(data.table)
library(lubridate)
library(methods)
library(pxweb)
library(readxl)
library(rvest)
library(stringr)
library(tibble)
library(tidyverse)
library(writexl)
library(XML)
library(xml2)

getwd()


# Förvärvsgrad & matchad förvärvsgrad

url <- "https://www.statistikdatabasen.scb.se/sq/122844"

data_in <- data.frame(read.csv(url, skip = 2, header = TRUE, sep = ","))

df1 <- data_in %>%
      mutate(Förvärvsgrad = as.numeric(Förvärvsgrad..justerad...procent...A.B.C..E.),
             Matchad_förvärvsgrad = as.numeric(Matchad.förvärvsgrad..procent..A.E.),
             Skillnad_förvärsgrad = as.numeric(Skillnad.förvärvsgrad.och.matchad.förvärvsgrad..procentenheter),
             Antal_personer = as.numeric(Totalt.antal.personer..justerat...antal..E.)) %>%
      select(kön.ålder.födelseland, !contains(".")) %>%
      separate(region, into = c("Region_kod", "Region"), sep= " ", extra = "merge") %>%
      mutate(Region = sub(" län$", "", Region),
             Region = sub("s$", "", Region)) %>%
      rename(Kön = kön.ålder.födelseland,
             År = år) %>%
      mutate(utbildning = str_sub(utbildning, end=-18)) %>%
      separate(utbildning, into = c("Utbildningskod", "Utbildning"), sep = " ", extra = "merge") %>%
      mutate(Utbildning = replace(Utbildning, Utbildning == "gymnasiein", "gymnasieingenjör" ), 
             Utbildning = str_to_sentence(Utbildning),
             Utbildning = sub("utbildning$", "", Utbildning),
             Utbildning = sub("s$", "", Utbildning),
             Utbildning = replace(Utbildning, Utbildning =="Data-, el- och energiteknisk ", "Data-, el- och energiteknik"),
             Utbildning = replace(Utbildning, Utbildning == "Vård- och omsorgsutb.; övrig gymn. utb. i hä", "Vård & omsorg"),
             Utbildning = replace(Utbildning, Utbildning == "Vvs- och fastighet", "VVS- och fastighet")) %>%
      group_by(Region, År, Utbildning) %>%
      mutate(Könsskillnad_mf = (Matchad_förvärvsgrad - lag(Matchad_förvärvsgrad, order_by=Kön))) %>%
      arrange(desc(Kön), .by_group = TRUE) %>%
      fill(Könsskillnad_mf) %>%
      mutate(Könsskillnad_mf = replace(Könsskillnad_mf, Kön == "totalt", NA))

df2 <- df1 %>%
      summarise(Andel_män = (Antal_personer[2]/Antal_personer[1])*100)

df3 <- merge(df1, df2, by=c("Region", "År", "Utbildning"), all.x = TRUE)
             


# Jobbinflöden

url2 <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906A/RegionInd19E2AN"
query1 <- pxweb_query("./queries/scbe2a.json")
pxd <- pxweb_get(url2, query1)

pxdf <- as.data.frame(pxd, column.name.type = "text", variable.value.type = "text")

df4 <- 
      mutate_if(pxdf, is.factor, as.character) %>%
      rename(Region = region) %>%
      mutate(Region = sub(" län$", "", Region),
             Region = sub("s$", "", Region)) %>%
      rename(Utbildning = utbildning) %>%
      mutate(Utbildning = str_sub(Utbildning, end=-18)) %>%
      mutate(Utbildning = replace(Utbildning, Utbildning == "gymnasiein", "gymnasieingenjör" ), 
             Utbildning = str_to_sentence(Utbildning),
             Utbildning = sub("utbildning$", "", Utbildning),
             Utbildning = sub("s$", "", Utbildning),
             Utbildning = replace(Utbildning, Utbildning =="Data-, el- och energiteknisk ", "Data-, el- och energiteknik"),
             Utbildning = replace(Utbildning, Utbildning == "Vård- och omsorgsutb.; övrig gymn. utb. i hä", "Vård & omsorg"),
             Utbildning = replace(Utbildning, Utbildning == "Vvs- och fastighet", "VVS- och fastighet")) %>%
      rename(Kön = "kön/ålder",
            År = årsintervall) %>%
      mutate(År = str_sub(År, start=-4)) %>%
      rename("Andel nya förvärvsarbetande" = "Andel nya förvärvsarbetande, procent",
             "Andel jobbytare" = "Andel jobbytare, procent",
             "Andel totalt jobbinflöde" = "Andel totalt jobbinflöde, procent")


df5 <- merge(df3, df4, by=c("Region", "År", "Utbildning", "Kön"), all.x= TRUE, all.y= TRUE )



# Utanför Arbetsmarknaden

url3 <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906B/RegionInd19U1aN"
query2 <- pxweb_query("./queries/scbu1a.json")
pxd2 <- pxweb_get(url3, query2)

pxdf2 <- as.data.frame(pxd2, column.name.type = "text", variable.value.type = "text" )

df6 <- 
      mutate_if(pxdf2, is.factor, as.character) %>%
      rename(Region = region) %>%
      mutate(Region = sub(" län$", "", Region),
             Region = sub("s$", "", Region)) %>%
      rename(Utbildning = utbildning) %>%
      mutate(Utbildning = str_sub(Utbildning, end=-18)) %>%
      mutate(Utbildning = replace(Utbildning, Utbildning == "gymnasiein", "gymnasieingenjör" ), 
             Utbildning = str_to_sentence(Utbildning),
             Utbildning = sub("utbildning$", "", Utbildning),
             Utbildning = sub("s$", "", Utbildning),
             Utbildning = replace(Utbildning, Utbildning =="Data-, el- och energiteknisk ", "Data-, el- och energiteknik"),
             Utbildning = replace(Utbildning, Utbildning == "Vård- och omsorgsutb.; övrig gymn. utb. i hä", "Vård & omsorg"),
             Utbildning = replace(Utbildning, Utbildning == "Vvs- och fastighet", "VVS- och fastighet")) %>%
      mutate(Kön = "totalt") %>%
      rename(År = år) %>%
      rename(Arbetslösa = "Inskrivna arbetslösa på Arbetsförmedlingen (B)",
             Utanför_arbetsmarknaden = "- Varav övriga",
            Totalt = "Totalt antal personer (A+B+C)",
            Förvärvsarbetande = "Förvärvsarbetande (A)") %>%
      mutate(Andel_arbetslösa = ((Arbetslösa / Totalt)*100),
            Andel_utanför_arbetsmarknaden = (Utanför_arbetsmarknaden / Totalt)*100)
      

df <- merge(df5, df6, by=c("Region", "År", "Utbildning", "Kön"), all.x= TRUE, all.y= TRUE )
df <- rename(df, Regionskod = Region_kod)

lookup <- df %>%
      filter(År=="2020", Utbildning == "Bygg", Kön == "totalt") %>%
      select(Region, Regionskod) %>%
      rename(Region_kod = Regionskod)

dff <- left_join(df, lookup, by = "Region") 
dff <- select(dff, -Regionskod)

write.csv(dff, "./data/rattkompetens.csv")

