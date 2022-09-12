salaryMF <- read.csv("C:\\Users\\cshan\\Desktop\\UNH\\Madagascar\\Data Round 2\\SalaryMegaData_2010_2021.csv",header=TRUE)

#has all locations filtered out 
#6 main fishing sites - 

#OCTOPUS
HORITA <- db5PRODUCTS %>%
  filter(KARAZANY=="h"|KARAZANY== "H"|KARAZANY== "HORITA")
HORITA <- HORITA %>% 
  mutate(KARAZANY2 = rep("Octopus", nrow(HORITA)))


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

KILO3 <- as.numeric(as.character(salaryMF$KILO3))
ISANY3 <- as.numeric(as.character(salaryMF$ISANY3))


###Michael Thompson : WORKING 
for (i in 1:length(salaryMF$TAIA2)){
  if(is.na(salary$ANARANY[i]) == TRUE & is.na(salary$TAIA[i]) == TRUE){
    
    salaryMF_Products <-  salaryMF%>%
      fill(TAIA2, .direction = "down") %>%
      fill(ANARANY2, .direction = "down") %>%
      fill(SEXE2, .direction = "down")
    
    
  }else{}
}

for (i in 1:length(salaryMF_Products$SEXE2)){
  if(is.na(salaryMF_Products$ANARANY[i]) == TRUE & is.na(salaryMF_Products$SEXE2[i] == TRUE)){
    
    salaryMF_Products <- salaryMF_Products%>%
      fill(SEXE2, .direction = "down")
    
    
  }else{}
}




VIEW <- salaryMF %>%
  count(SEXE2)

VIEW2 <- salaryMF_Products %>%
  count(SEXE2)

female <- salaryMF_Products %>%
  filter(SEXE == "A"|SEXE=="AMPELA"|SEXE == "V")

female$KILO3 <- as.numeric(as.character(female$KILO3))
female$ISANY3 <- as.numeric(as.character(female$ISANY3))

female_Avgs <- female%>%
  filter(is.na(KILO3) == FALSE)%>%
  filter(is.na(ISANY3) == FALSE)%>%
  filter(ISANY3 != 0)%>%
  filter(KILO3 != 0)



for (i in 1:length(female_Avgs$KILO3))(
  female_Avgs$avg_size[i] <- ( female_Avgs$KILO3[i] / female_Avgs$ISANY3[i] )
)


male <- salaryMF_Products %>%
  filter(SEXE == "L"|SEXE=="LAHY")

male$KILO3 <- as.numeric(as.character(male$KILO3))
male$ISANY3 <- as.numeric(as.character(male$ISANY3))

male_Avgs <- male%>%
  filter(is.na(ISANY3) == FALSE)%>%
  filter(is.na(KILO3) == FALSE)%>%
  filter(ISANY3 != 0)%>%
  filter(KILO3 != 0)



for (i in 1:length(male_Avgs$KILO3))(
  male_Avgs$avg_size[i] <- ( male_Avgs$KILO3[i] / male_Avgs$ISANY3[i] )
)

VIEW <- female_Avgs %>%
  count(KARAZANY)

VIEW2 <- male_Avgs %>%
  count(KARAZANY)

#female
VIEW <- female%>%
  count(KARAZANY)
VIEW <- female_Avgs%>%
  count(KARAZANY)

female_Avgs$avg_size <- as.numeric(as.character(female_Avgs$avg_size))
VIEW <- female_Avgs %>%
  filter(KARAZANY == "A")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- female_Avgs %>%
  filter(KARAZANY == "F"|KARAZANY == "FIA")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- female_Avgs %>%
  filter(KARAZANY == "H"|KARAZANY == "HORITA")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- female_Avgs %>%
  filter(KARAZANY == "L")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- female_Avgs %>%
  filter(KARAZANY == "ZANGA")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)



#male
VIEW <- male%>%
  count(KARAZANY)
VIEW <- female_Avgs%>%
  count(KARAZANY)

VIEW <- male_Avgs %>%
  filter(KARAZANY == "a"|KARAZANY == "A"|KARAZANY == "A "|
           KARAZANY == "ANGISY"|KARAZANY == "ANGY")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- male_Avgs %>%
  filter(KARAZANY == "f"|KARAZANY == "F"|KARAZANY == "FIA"|
           KARAZANY == "FIA ANGY"|KARAZANY == "FIA BANA"|
           KARAZANY == "FIA KIMBOROKE"|KARAZANY == "BODOLOHA")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- male_Avgs %>%
  filter(KARAZANY == "h"|KARAZANY == "H"|KARAZANY == "HORITA")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- male_Avgs %>%
  filter(KARAZANY == "L"|KARAZANY == "lamera"|KARAZANY == "Lamera"|KARAZANY == "LAMERA")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

VIEW <- male_Avgs %>%
  filter(KARAZANY == "ZANGA")
mean(VIEW$avg_size, 0, na.rm = TRUE)
std.error(VIEW$avg_size)

Male_v_FemaleComparison <- read.csv("C:\\Users\\cshan\\Desktop\\UNH\\Madagascar\\Into the thick of it!\\Male_v_FemaleComparison.csv",header=TRUE)
View(Male_v_FemaleComparison)
#comparison of products caught by gender
ggplot(Male_v_FemaleComparison, aes(fill=SEX, x= Product.Translate, y=TOTAL)) + 
  geom_bar(position = position_dodge(), stat="identity") +geom_line() + scale_color_brewer()+
  labs(x="Products Caught", y ="Number of Fishing Trips", fill="Gender of Fisher")+ 
  theme(axis.text.x = element_text(angle=90), axis.title.y = element_text(angle=90))+
  geom_text(aes(label=TOTAL), position=position_dodge(width=.4), vjust=-0.25)
#comparison of mean and st error of products caught by gender
ggplot(Male_v_FemaleComparison, aes(fill=SEX, x= Product.Translate, y=mean.size)) + 
  geom_bar(position = position_dodge(), stat="identity") +geom_line() + scale_color_brewer()+
  labs(x="Products Caught", y ="Average Size of Individual", fill="Gender of Fisher")+ 
  theme(axis.text.x = element_text(angle=90), axis.title.y = element_text(angle=90))+
  geom_errorbar(aes(ymin=mean.size-st.error, ymax=mean.size+st.error), width=.2,position=position_dodge(.9))+
  geom_text(aes(label=TOTAL), position=position_dodge(width=.4), vjust=-0.25)

Male_v_FemaleComparison_MaleProducts <- read.csv("C:\\Users\\cshan\\Desktop\\UNH\\Madagascar\\Into the thick of it!\\Male_v_FemaleComparison_MaleProducts.csv",header=TRUE)
ggplot(Male_v_FemaleComparison_MaleProducts, aes(x= Product.Translate, y=TOTAL)) + 
  geom_bar(position = position_dodge(), stat="identity") +geom_line() + scale_color_brewer()+
  labs(x="Products Caught", y ="Frequency of Catch", fill="Gender of Fisher")+ 
  theme(axis.text.x = element_text(angle=90), axis.title.y = element_text(angle=90))+
  geom_text(aes(label=TOTAL), position=position_dodge(width=.4), vjust=-0.25)



##havent actually done this yet####

#FIGURING OUT THE PRODUCTS THAT WERE CAUGHT AT EACH SITE
#count(KARAZANY)
#FIGURING OUT HOW MANY VISITS EACH SITE HAD 
#count(TAIA)
#FIGURING OUT WHICH YEARS
#count(Date...Year)
#FIGURING OUT WHICH MONTHS
#count(Date...Months)

female %>%
  filter(TAIA2=="Danahitse"|TAIA2== "DANAHITSE"|TAIA2== "DANIAHITSE"|
           TAIA2=="DANOHITSE"|TAIA2=="Donahitse")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Sarim-pase"|TAIA2== "Sorimpase"|TAIA2== "SORIMPASE")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Bemena"|TAIA2== "BEMENA"|TAIA2== "Bemena ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="VAVAE AVARATSE"|TAIA2== "vavaevaratse"|TAIA2== "Vavalavaratse ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Tsimahafory"|TAIA2== "Tsimahafot"|TAIA2== "Tsimahafoty"|TAIA2=="TSIMAHAFOTY")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Antandea"|TAIA2== "Antandebe"|TAIA2== "Antandeo"|TAIA2=="ANtandeo"
         |TAIA2=="ANTANDEO"|TAIA2=="Antaneo"|TAIA2=="Anandeo"|TAIA2=="Tandeo"
         |TAIA2=="TANDEO"|TAIA2=="TANDEOBE"|TAIA2=="Tandeope")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="TAMBAHY"|TAIA2== "TAMBOHO"|TAIA2== "TAMBOHY"|TAIA2=="Antamboha"
         |TAIA2=="Antamboha "|TAIA2=="Antamboho"|TAIA2=="AntambohO"|TAIA2=="Antamboho"
         |TAIA2=="ANTAMBOHO"|TAIA2=="ANTAMBOHY"|TAIA2=="ANTAMBAHY"|TAIA2=="Antanimboho"
         |TAIA2=="ATAMBOHY")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Mazatse"|TAIA2== "Mahasaha"|TAIA2== "MAHASAHA"|TAIA2=="Mahasaha "
         |TAIA2=="Mahaseha "|TAIA2=="Mahazatse"|TAIA2=="MAHAZATSE"|TAIA2=="Mahazatse "
         |TAIA2=="MAHAZATSY")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Ambatobe"|TAIA2== "AMBATOBE"| TAIA2== "Batobe"|TAIA2=="VATOBE ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AMBALANY"|TAIA2== "Ambalany "| TAIA2== "AMBALAVY"|
           TAIA2=="Ambalany"|TAIA2== "Ambana"| TAIA2== "Ambulany"|TAIA2=="Ampalany ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AGNAHIPILO")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AMBA")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Ambatotsilaky")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AMBATOLOAKY")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Ambatovy"|TAIA2== "AMBATOVY "| TAIA2== "Ambatovy "|
           TAIA2=="Batovy "|TAIA2== "vatovy")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="NDOHAOLO"|TAIA2== "Andohaolo"| TAIA2== "ANDOHAOLO")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AMBAVAE")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AMBILANY")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ABOE"|TAIA2== "ABOHOE"|TAIA2== "Ambohoe"|TAIA2=="AMBOHOE"
         |TAIA2=="Ambohoe "|TAIA2=="AMBOHOKE"|TAIA2=="AMBOHONY")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Ambolivato")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AMPASE"|TAIA2== "ampase "|TAIA2== "Ampase "|TAIA2=="AMPASY"
         |TAIA2=="AMPASILAVA")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Ampilako"|TAIA2== "AMPILAKO")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="TANOSE"|TAIA2== "Anose"|TAIA2== "ANOSE"|TAIA2=="Anose "
         |TAIA2=="ANOSY"|TAIA2=="Anananose"|TAIA2=="ANANANOSE"|TAIA2=="ANANANOSE "
         |TAIA2=="Anananosy"|TAIA2=="ANANANOSY"|TAIA2=="Ananaosy"|TAIA2=="Ananosy"
         |TAIA2=="Nanonose "|TAIA2=="Nanose "|TAIA2== "nananose"|TAIA2== "Nananose"
         |TAIA2=="NANANOSE"|TAIA2=="nananose "|TAIA2== "Nananose "|TAIA2== "NANANOSY")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Andama"|TAIA2== "ANDAMA"|TAIA2== "Andama "|TAIA2=="Andamabe"
         |TAIA2=="ANdamabe "|TAIA2=="ANDAMABE"|TAIA2=="Anadamabe"|TAIA2=="Andalmabe")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Andaniriake"|TAIA2== "Andaniriake"|TAIA2== "ANDANIRIAKE"|TAIA2=="Daniriake"
         |TAIA2=="DANIRIAKE")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ANDOHARIAKE")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Andohavato")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ANDRANO") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Anjokozoko"|TAIA2== "ANJOKOZOKO"|TAIA2== "JOKOZKO"|TAIA2=="Jokozoko"
         |TAIA2=="JOKOZOKO"|TAIA2=="Drokozoko")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="AKARA"|TAIA2== "Ankara"|TAIA2== "ANKARA "|TAIA2=="Ankara ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ANKATSAEPOTIKE") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Ankemake"|TAIA2== "ANKEMAKE"|TAIA2== "Ankemoke"|TAIA2=="Ankenae"
         |TAIA2=="ANkenae "|TAIA2=="ANKENAE"|TAIA2=="Ankenae "|TAIA2=="Ankamake")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ANKORA"|TAIA2== "ANKORAKE"|TAIA2== "Ankorake ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ANOLOTSE") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Antagie"|TAIA2== "Antagie "|TAIA2== "Antangie"|TAIA2=="ANtangie"
         |TAIA2=="Antangie "|TAIA2=="Tangie"|TAIA2=="TANGIE "|TAIA2=="Tangie ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Antendebe") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ANTOVY") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ANTSARAGNA") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Apilalo") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="BADOBE") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="BEDABOKE") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Beharona"|TAIA2== "BEHARONA"|TAIA2== "Beharona  "|TAIA2=="BEHARONGA")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Belameara"|TAIA2== "belamera"|TAIA2== "Belamera"|TAIA2=="BELAMERA"
         |TAIA2=="Belamera "|TAIA2=="BELAMERA ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Belampa"|TAIA2== "Belapa"|TAIA2== "BELAPA"|TAIA2=="BELOPA")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="BELOFISA"|TAIA2== "BELOFISAKE")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Betabake"|TAIA2== "BETABAKE")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="ABEVIKO"|TAIA2== "Beviko"|TAIA2== "BEVIKO ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="BOLOAKE") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Borimpase") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Bozike") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Lamabe"|TAIA2== "LAMABE"|TAIA2== "Lamabe ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="marerano"|TAIA2== "Marerano"|TAIA2== "MARERANO")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="nandoa"|TAIA2== "Nandoa"|TAIA2== "NANDOA"|TAIA2=="Nandoa "
         |TAIA2=="N ANDOA"|TAIA2=="NADOA")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Nanahofa"|TAIA2== "MANOFOHA"|TAIA2== "Nanohfa"|TAIA2=="nanohofa"
         |TAIA2=="Nanohofa "|TAIA2=="NANOHOFA"|TAIA2=="Nanohofa"|TAIA2=="Hanohofa ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="Nokoabe"|TAIA2== "Nonokaobe"|TAIA2== "NONOKAOMBE"|TAIA2=="NONOKOABE"
         |TAIA2=="Nanokaobe "|TAIA2=="Nanokombe"|TAIA2=="Honokaobe"|TAIA2=="Anonokaombe ")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="NOSEKARA"|TAIA2== "Nosekara "|TAIA2== "Nosenkara"|TAIA2== "NOSYKARA")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="PASILAVA") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="SAREHIMA"|TAIA2== "Sarehima "|TAIA2== "Sarihima"|TAIA2=="SARIHIMA"
         |TAIA2=="Sarihina"|TAIA2=="Serehima")%>%
  count(Date...Year)

female %>%
  filter(TAIA2=="TAKOHY") %>%
  count(Date...Year)

female %>%
  filter(TAIA2=="TRANOBARAHA") %>%
  count(Date...Year)


#male
male %>%
  filter(TAIA2=="Danahitse"|TAIA2== "DANAHITSE"|TAIA2== "DANIAHITSE"|
           TAIA2=="DANOHITSE"|TAIA2=="Donahitse")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Sarim-pase"|TAIA2== "Sorimpase"|TAIA2== "SORIMPASE")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Bemena"|TAIA2== "BEMENA"|TAIA2== "Bemena ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="VAVAE AVARATSE"|TAIA2== "vavaevaratse"|TAIA2== "Vavalavaratse ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Tsimahafory"|TAIA2== "Tsimahafot"|TAIA2== "Tsimahafoty"|TAIA2=="TSIMAHAFOTY")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Antandea"|TAIA2== "Antandebe"|TAIA2== "Antandeo"|TAIA2=="ANtandeo"
         |TAIA2=="ANTANDEO"|TAIA2=="Antaneo"|TAIA2=="Anandeo"|TAIA2=="Tandeo"
         |TAIA2=="TANDEO"|TAIA2=="TANDEOBE"|TAIA2=="Tandeope")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="TAMBAHY"|TAIA2== "TAMBOHO"|TAIA2== "TAMBOHY"|TAIA2=="Antamboha"
         |TAIA2=="Antamboha "|TAIA2=="Antamboho"|TAIA2=="AntambohO"|TAIA2=="Antamboho"
         |TAIA2=="ANTAMBOHO"|TAIA2=="ANTAMBOHY"|TAIA2=="ANTAMBAHY"|TAIA2=="Antanimboho"
         |TAIA2=="ATAMBOHY")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Mazatse"|TAIA2== "Mahasaha"|TAIA2== "MAHASAHA"|TAIA2=="Mahasaha "
         |TAIA2=="Mahaseha "|TAIA2=="Mahazatse"|TAIA2=="MAHAZATSE"|TAIA2=="Mahazatse "
         |TAIA2=="MAHAZATSY")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Ambatobe"|TAIA2== "AMBATOBE"| TAIA2== "Batobe"|TAIA2=="VATOBE ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AMBALANY"|TAIA2== "Ambalany "| TAIA2== "AMBALAVY"|
           TAIA2=="Ambalany"|TAIA2== "Ambana"| TAIA2== "Ambulany"|TAIA2=="Ampalany ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AGNAHIPILO")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AMBA")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Ambatotsilaky")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AMBATOLOAKY")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Ambatovy"|TAIA2== "AMBATOVY "| TAIA2== "Ambatovy "|
           TAIA2=="Batovy "|TAIA2== "vatovy")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="NDOHAOLO"|TAIA2== "Andohaolo"| TAIA2== "ANDOHAOLO")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AMBAVAE")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AMBILANY")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ABOE"|TAIA2== "ABOHOE"|TAIA2== "Ambohoe"|TAIA2=="AMBOHOE"
         |TAIA2=="Ambohoe "|TAIA2=="AMBOHOKE"|TAIA2=="AMBOHONY")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Ambolivato")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AMPASE"|TAIA2== "ampase "|TAIA2== "Ampase "|TAIA2=="AMPASY"
         |TAIA2=="AMPASILAVA")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Ampilako"|TAIA2== "AMPILAKO")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="TANOSE"|TAIA2== "Anose"|TAIA2== "ANOSE"|TAIA2=="Anose "
         |TAIA2=="ANOSY"|TAIA2=="Anananose"|TAIA2=="ANANANOSE"|TAIA2=="ANANANOSE "
         |TAIA2=="Anananosy"|TAIA2=="ANANANOSY"|TAIA2=="Ananaosy"|TAIA2=="Ananosy"
         |TAIA2=="Nanonose "|TAIA2=="Nanose "|TAIA2== "nananose"|TAIA2== "Nananose"
         |TAIA2=="NANANOSE"|TAIA2=="nananose "|TAIA2== "Nananose "|TAIA2== "NANANOSY")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Andama"|TAIA2== "ANDAMA"|TAIA2== "Andama "|TAIA2=="Andamabe"
         |TAIA2=="ANdamabe "|TAIA2=="ANDAMABE"|TAIA2=="Anadamabe"|TAIA2=="Andalmabe")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Andaniriake"|TAIA2== "Andaniriake"|TAIA2== "ANDANIRIAKE"|TAIA2=="Daniriake"
         |TAIA2=="DANIRIAKE")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ANDOHARIAKE")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Andohavato")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ANDRANO") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Anjokozoko"|TAIA2== "ANJOKOZOKO"|TAIA2== "JOKOZKO"|TAIA2=="Jokozoko"
         |TAIA2=="JOKOZOKO"|TAIA2=="Drokozoko")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="AKARA"|TAIA2== "Ankara"|TAIA2== "ANKARA "|TAIA2=="Ankara ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ANKATSAEPOTIKE") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Ankemake"|TAIA2== "ANKEMAKE"|TAIA2== "Ankemoke"|TAIA2=="Ankenae"
         |TAIA2=="ANkenae "|TAIA2=="ANKENAE"|TAIA2=="Ankenae "|TAIA2=="Ankamake")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ANKORA"|TAIA2== "ANKORAKE"|TAIA2== "Ankorake ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ANOLOTSE") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Antagie"|TAIA2== "Antagie "|TAIA2== "Antangie"|TAIA2=="ANtangie"
         |TAIA2=="Antangie "|TAIA2=="Tangie"|TAIA2=="TANGIE "|TAIA2=="Tangie ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Antendebe") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ANTOVY") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ANTSARAGNA") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Apilalo") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="BADOBE") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="BEDABOKE") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Beharona"|TAIA2== "BEHARONA"|TAIA2== "Beharona  "|TAIA2=="BEHARONGA")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Belameara"|TAIA2== "belamera"|TAIA2== "Belamera"|TAIA2=="BELAMERA"
         |TAIA2=="Belamera "|TAIA2=="BELAMERA ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Belampa"|TAIA2== "Belapa"|TAIA2== "BELAPA"|TAIA2=="BELOPA")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="BELOFISA"|TAIA2== "BELOFISAKE")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Betabake"|TAIA2== "BETABAKE")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="ABEVIKO"|TAIA2== "Beviko"|TAIA2== "BEVIKO ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="BOLOAKE") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Borimpase") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Bozike") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Lamabe"|TAIA2== "LAMABE"|TAIA2== "Lamabe ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="marerano"|TAIA2== "Marerano"|TAIA2== "MARERANO")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="nandoa"|TAIA2== "Nandoa"|TAIA2== "NANDOA"|TAIA2=="Nandoa "
         |TAIA2=="N ANDOA"|TAIA2=="NADOA")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Nanahofa"|TAIA2== "MANOFOHA"|TAIA2== "Nanohfa"|TAIA2=="nanohofa"
         |TAIA2=="Nanohofa "|TAIA2=="NANOHOFA"|TAIA2=="Nanohofa"|TAIA2=="Hanohofa ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="Nokoabe"|TAIA2== "Nonokaobe"|TAIA2== "NONOKAOMBE"|TAIA2=="NONOKOABE"
         |TAIA2=="Nanokaobe "|TAIA2=="Nanokombe"|TAIA2=="Honokaobe"|TAIA2=="Anonokaombe ")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="NOSEKARA"|TAIA2== "Nosekara "|TAIA2== "Nosenkara"|TAIA2== "NOSYKARA")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="PASILAVA") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="SAREHIMA"|TAIA2== "Sarehima "|TAIA2== "Sarihima"|TAIA2=="SARIHIMA"
         |TAIA2=="Sarihina"|TAIA2=="Serehima")%>%
  count(Date...Year)

male %>%
  filter(TAIA2=="TAKOHY") %>%
  count(Date...Year)

male %>%
  filter(TAIA2=="TRANOBARAHA") %>%
  count(Date...Year)

