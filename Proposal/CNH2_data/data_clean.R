library(tidyr)
library(dplyr)
setwd("C:/Users/sophi/Documents/UNH_Docs/Proposal/CNH2_data")


salaryMF <- data.frame(read.csv("SalaryMegaData_2010_2021.csv",header=TRUE))

#To do:
# 2 trip stuff
# rename sites, months
# filter for octopus


#OCTOPUS
# HORITA <- salaryMF %>% #db5PRODUCTS %>%
#   filter(KARAZANY=="h"|KARAZANY== "H"|KARAZANY== "HORITA")
# HORITA <- HORITA %>% 
#   mutate(KARAZANY2 = rep("Octopus", nrow(HORITA)))


#NOW HOW TO ACCOUNT FOR THOSE DOUBLE TRIPS (Michael Thompson)
salaryMF$ANARANY2 <- salaryMF$ANARANY
salaryMF$TAIA2 <- salaryMF$TAIA
salaryMF$FITAOVANA2 <- salaryMF$FITAOVANA
salaryMF$SEXE2 <- salaryMF$SEXE
salaryMF$ISANY3 <- salaryMF$ISANY
salaryMF$KILO3 <- salaryMF$KILO

salaryMF$ANARANY2[salaryMF$ANARANY2 == ""] <- NA
salaryMF$TAIA2[salaryMF$TAIA2 == ""] <- NA
salaryMF$SEXE2[salaryMF$SEXE2 == ""] <- NA
salaryMF$ISANY3[salaryMF$ISANY3 == ""] <- NA
salaryMF$KILO3[salaryMF$KILO3 == ""] <- NA


salaryMF$ANARANY[salaryMF$ANARANY == ""] <- NA
salaryMF$TAIA[salaryMF$TAIA == ""] <- NA
salaryMF$SEXE[salaryMF$SEXE == ""] <- NA

# KILO3 <- as.numeric(as.character(salaryMF$KILO3))
# ISANY3 <- as.numeric(as.character(salaryMF$ISANY3))


###Michael Thompson : WORKING 
for (i in 1:length(salaryMF$TAIA2)){
  if(is.na(salaryMF$ANARANY[i]) == TRUE & is.na(salaryMF$TAIA[i]) == TRUE){
    
    salaryMF <-  salaryMF %>%
      fill(TAIA2, .direction = "down") %>%
      fill(ANARANY2, .direction = "down") %>%
      fill(SEXE2, .direction = "down")
    
    
  }
  # if(is.na(salaryMF$Full.DATE[i]) == TRUE){
  #   salaryMF <-  salaryMF %>%
  #     fill(Full.DATE, .direction = "down")
  # }
}

col_select <- salaryMF %>%
  select(c("Town.of.Collection", #TOWN
           "Date...day", #DAY
           "Date...Month", #MONTH
           "Date...Year", #YEAR
           "Full.DATE", #DATE
           "ANARANY2", #FISHER_NAME
           "SEXE2", #SEX_FISHER
           "KARAZANY", #CATCH_TYPE
           "ISANY3", #NO_CAUGHT
           "KILO3", #TOT_WT
           "Average.Size..kilo.individual.", #AVG_WT
           "VIDINY.AR", #PRICE_AR
           "USD.Price..1.USD..2167.75.ariary.", #PRICE_USD
           "Olo.firy", #NO_FISHERS
           "TAIA2", #REGION
           "FITAOVANA2", #FISHING_GEAR
           "Total.time" #FISHING_TIME
          )) %>%
  rename(TOWN = Town.of.Collection,
         DAY = Date...day,
         MONTH = Date...Month,
         YEAR = Date...Year,
         DATE = Full.DATE,
         FISHER_NAME = ANARANY2,
         CATCH_TYPE = KARAZANY,
         NO_CAUGHT = ISANY3,
         TOT_WT = KILO3,
         AVG_WT = Average.Size..kilo.individual.,
         PRICE_AR = VIDINY.AR,
         PRICE_USD = USD.Price..1.USD..2167.75.ariary.,
         NO_FISHERS = Olo.firy,
         REGION = TAIA2,
         FISHER_SEX = SEXE2,
         FISHING_GEAR = FITAOVANA2,
         FISHING_TIME = Total.time
         ) %>%
  filter(CATCH_TYPE == "h" | CATCH_TYPE == "H" | CATCH_TYPE == "HORITA")

cleanData <- col_select %>%
  mutate(TOWN = recode(TOWN, 'Salary ' = 'Salary'),
         MONTH = recode(MONTH, 'January ' = 'January', #has blanks
                        'February ' = 'February', 
                        'March ' = 'March',
                        'May ' = 'May',
                        'August ' = 'August',
                        'November ' = 'November',
                        'December ' = 'December'),
         DATE = recode(DATE, "?????" = "",#has blanks, Needs to standardize
                       "??????" = ""),
         #FISHER_NAME take out spaces
         FISHER_SEX = recode(FISHER_SEX, 'L' = 'M',
                             "LAHY" = 'M',
                             "A" = "F",
                             'V' = 'F',
                             "AMPELA" = 'F',
                             "*" = ""),
         CATCH_TYPE = recode(CATCH_TYPE, "h" = "HORITA",
                             "H" = "HORITA"),
         NO_CAUGHT = recode(NO_CAUGHT, "1 GONY" = "",
                            "1GONY" = "",
                            "MAROBE" = ""),
         TOT_WT = recode(TOT_WT, "MARO" = ""),
         PRICE_AR = recode(PRICE_AR, "*" = ""),
         PRICE_USD = recode(PRICE_USD, "#VALUE!" = ""),
         FISHING_GEAR = recode(FISHING_GEAR, "VINTAMPIA" = "Hook",
                               "VITAMPIA" = "Hook",
                               "Vintam-pia" = "Hook",
                               "Vitampia" = "Hook",
                               "HARATO" = "Net",
                               "Harato" = "Net",
                               "Harato                                          " = "Net",
                               "HARATOM-PAY" = "Net",
                               "VOLOSO" = "Spear",
                               "Voloso" = "Spear",
                               "voloso " = "Spear",
                               "voloso" = "Spear",
                               "VOloso" = "Spear",
                               "Voloso " = "Spear",
                               "Veloso" = "Spear",
                               "vint'angisy" = "SquidHook",
                               "VINTA" = "SquidHook",
                               "VINTAN'ANGISY" = "SquidHook",
                               "vin'angisy " = "SquidHook",
                               "Vint'angisy" = "SquidHook",
                               "VITANANGISY" = "SquidHook",
                               "NINTAN'ANGISY" = "SquidHook",
                               "Basimpia " = "SpearGun",
                               "basimpia " = "SpearGun",
                               "Basim-pia" = "SpearGun",
                               "BASIMPIA" = "SpearGun",
                               "B" = "SpearGun",
                               "VOLOSO/BASIMPIA" = "SpearGun",
                               "BASIMPIA/ VOLOSO" = "SpearGun"),
         REGION = recode(REGION, "*" = "",
                         "empty" = "",
                         "Danahitse" = "Danahitse",
                         "DANAHITSE"= "Danahitse",
                         "DANIAHITSE"= "Danahitse",
                         "DANOHITSE"= "Danahitse",
                         "Donahitse"= "Danahitse",
                         "Sarim-pase" = "Sorimpase",
                         "SORIMPASE" = "Sorimpase",
                         "BEMENA" = "Bemena",
                         "Bemena " = "Bemena",
                         "VAVAE AVARATSE" = "Vavalavaratse",
                         "vavaevaratse" = "Vavalavaratse",
                         "Vavalavaratse " = "Vavalavaratse",
                         "Tsimahafory" = "Tsimahafoty",
                         "Tsimahafot" = "Tsimahafoty",
                         "TSIMAHAFOTY" = "Tsimahafoty",
                         "Antandebe" = "Antaneo",
                         "Antandeo" = "Antaneo",
                         "ANtandeo" = "Antaneo",
                         "ANTANDEO" = "Antaneo",
                         "Antandea" = "Antaneo",
                         "Anandeo" = "Antaneo",
                         "Tandeo" = "Antaneo",
                         "TANDEO" = "Antaneo",
                         "TANDEOBE" = "Antaneo",
                         "Tandeope" = "Antaneo",
                         "TAMBAHY" = "Antamboho",
                         "TAMBOHO" = "Antamboho",
                         "TAMBOHY" = "Antamboho",
                         "Antamboha" = "Antamboho",
                         "Antamboha " = "Antamboho",
                         "Antamboho" = "Antamboho",
                         "AntambohO" = "Antamboho",
                         "Antamboho" = "Antamboho",
                         "ANTAMBOHO" = "Antamboho",
                         "ANTAMBOHY" = "Antamboho",
                         "ANTAMBAHY" = "Antamboho",
                         "ANtamboho" = "Antamboho",
                         "Antanimboho" = "Antamboho",
                         "ATAMBOHY" = "Antamboho",
                         "Mahasaha" = "Mazatse",
                         "MAHASAHA" = "Mazatse",
                         "Mahasaha " = "Mazatse",
                         "Mahaseha " = "Mazatse",
                         "Mahazatse" = "Mazatse",
                         "MAHAZATSE" = "Mazatse",
                         "Mahazatse " = "Mazatse",
                         "MAHAZATSY" = "Mazatse",
                         "AMBATOBE" = "Ambatobe",
                         "Batobe" = "Ambatobe",
                         "VATOBE " = "Ambatobe",
                         "AMBALANY" = "Ambalany",
                         "AMBALAVY" = "Ambalany",
                         "Ambalany " = "Ambalany",
                         "Ambana" = "Ambalany",
                         "Ambulany" = "Ambalany",
                         "Ampalany " = "Ambalany",
                         "AGNAHIPILO" = "Agnahipilo",
                         "AMBA" = "Amba",
                         "AMBATOLOAKY" = "Ambatoloaky",
                         "AMBATOVY " = "Ambatovy",
                         "AMBATOVY" = "Ambatovy",
                         "Ambatovy " = "Ambatovy",
                         "Batovy " = "Ambatovy",
                         "vatovy" = "Ambatovy",
                         "NDOHAOLO" = "Andohaolo",
                         "ANDOHAOLO" = "Andohaolo",
                         "AMBAVAE" = "Ambavae",
                         "AMBILANY" = "Ambilany",
                         "ABOE" = "Ambohoe",
                         "ABOHOE" = "Ambohoe",
                         "AMBOHOE" = "Ambohoe",
                         "Ambohoe " = "Ambohoe",
                         "AMBOHOKE" = "Ambohoe",
                         "AMBOHONY" = "Ambohoe",
                         "AMPASE" = "Ampase",
                         "ampase " = "Ampase",
                         "Ampase " = "Ampase",
                         "AMPASY" = "Ampase",
                         "AMPASILAVA" = "Ampase",
                         "AMPILAKO" ="Ampilako",
                         "TANOSE" = "Anananose",
                         "Anose" = "Anananose",
                         "ANOSE" = "Anananose",
                         "Anose " = "Anananose",
                         "ANOSY" = "Anananose",
                         "ANANANOSE" = "Anananose",
                         "ANANANOSE " = "Anananose",
                         "Anananosy" = "Anananose",
                         "ANANANOSY" = "Anananose",
                         "Ananaosy" = "Anananose",
                         "Ananosy" = "Anananose",
                         "Nanonose " = "Anananose",
                         "Nanose " = "Anananose",
                         "nananose" = "Anananose",
                         "Nananose" = "Anananose",
                         "NANANOSE" = "Anananose",
                         "nananose " = "Anananose",
                         "Nananose " = "Anananose",
                         "NANANOSY" = "Anananose",
                         "Andama" = "Andamabe",
                         "ANDAMA" = "Andamabe",
                         "Andama " = "Andamabe",
                         "ANdamabe " = "Andamabe",
                         "ANDAMABE" = "Andamabe",
                         "Anadamabe" = "Andamabe",
                         "Andalmabe" = "Andamabe",
                         "ANdamabe" =  "Andamabe",
                         "Andaniriake" = "Andaniriake",
                         "ANDANIRIAKE" = "Andaniriake",
                         "Daniriake" = "Andaniriake",
                         "DANIRIAKE" = "Andaniriake",
                         "ANDOHARIAKE" = "Andohariake",
                         "Andanieriake" = "Andohariake",
                         "Andohariake" = "Andohariake", #guess
                         "ANDRANO" = "Andrano",
                         "ANJOKOZOKO" = "Anjokozoko",
                         "JOKOZKO" = "Anjokozoko",
                         "Jokozoko" = "Anjokozoko",
                         "JOKOZOKO" = "Anjokozoko",
                         "Drokozoko" = "Anjokozoko",
                         "AKARA" = "Ankara",
                         "ANKARA" = "Ankara",
                         "ANKARA " = "Ankara",
                         "Ankara " = "Ankara",
                         "ANKATSAEPOTIKE" = "Ankatsaepotike",
                         "ANkenae" = "Ankenae",
                         "ANKEMAKE" = "Ankemake",
                         "Ankemoke" = "Ankemake",
                         "Ankenae" = "Ankemake",
                         "ANkenae " = "Ankemake",
                         "ANKENAE" = "Ankemake",
                         "Ankenae " = "Ankemake",
                         "Ankamake" = "Ankemake",
                         "ANKORA" = "Ankorake",
                         "ANKORAKE" = "Ankorake",
                         "Ankorake " = "Ankorake",
                         "ANOLOTSE" = "Anolotse",
                         "Antagie" = "Antangie",
                         "Antagie " = "Antangie",
                         "ANtangie" = "Antangie",
                         "Antangie " = "Antangie",
                         "Tangie" = "Antangie",
                         "TANGIE " = "Antangie",
                         "Tangie " = "Antangie",
                         "ANTOVY" = "Antovy",
                         "ANTSARAGNA" = "Antsaragna",
                         "BADOBE" = "Badobe",
                         "BEDABOKE" = "Bedaboke",
                         "BEHARONA" = "Beharona",
                         "Beharona  " = "Beharona",
                         "Beharona " = "Beharona",
                         "BEHARONGA" = "Beharona",
                         "belamera" = "Belamera",
                         "Belameara" = "Belamera",
                         "BELAMERA" = "Belamera",
                         "Belamera " = "Belamera",
                         "BELAMERA " = "Belamera",
                         "Belampa" = "Belapa",
                         "BELAPA" = "Belapa",
                         "BELOPA" = "Belapa",
                         "BELOFISA" = "Belofisa",
                         "BELOFISAKE" = "Belofisa",
                         "BETABAKE" = "Betabake",
                         "ABEVIKO" = "Beviko",
                         "BEVIKO " = "Beviko",
                         "BEVIKO" =  "Beviko",
                         "BOLOAKE" = "Boloake",
                         "LAMABE" = "Lamabe",
                         "Lamabe " = "Lamabe",
                         "marerano" = "Marerano",
                         "MARERANO" = "Marerano",
                         "nandoa" = "Nandoa",
                         "NANDOA" = "Nandoa",
                         "Nandoa " = "Nandoa",
                         "N ANDOA" = "Nandoa",
                         "NADOA" = "Nandoa",
                         "Nanahofa" = "Nanohofa",
                         "MANOFOHA" = "Nanohofa",
                         "Nanohfa" = "Nanohofa",
                         "nanohofa" = "Nanohofa",
                         "Nanohofa " = "Nanohofa",
                         "NANOHOFA" = "Nanohofa",
                         "Hanohofa " = "Nanohofa",
                         "Nokoabe" = "Nonokaobe",
                         "NONOKAOMBE" = "Nonokaobe",
                         "NONOKOABE" = "Nonokaobe",
                         "Nanokaobe " = "Nonokaobe",
                         "Nanokombe" = "Nonokaobe",
                         "Honokaobe" = "Nonokaobe",
                         "Anonokaombe " = "Nonokaobe",
                         "Nanokaobe" = "Nonokaobe",
                         "NOSEKARA" = "Nosekara",
                         "Nosekara"  = "Nosekara",
                         "Nosenkara" = "Nosekara",
                         "Nosekara " = "Nosekara",
                         "NOSYKARA" = "Nosekara",
                         "PASILAVA" = "Pasilava",
                         "SAREHIMA" = "Sarihima",
                         "Sarehima " = "Sarihima",
                         "SARIHIMA" = "Sarihima",
                         "Sarihina" = "Sarihima",
                         "Serehima" = "Sarihima",
                         "TAKOHY" = "Takohy",
                         "TANGIE" = "Tangie",
                         "TRANOBARAHA" = "Tranobaraha"),
         FISHING_TIME = recode(FISHING_TIME, "#VALUE!" = "")
         )
cleanData <- cleanData %>% mutate_all(na_if,"")


write.csv(cleanData,"cleanData.csv")
