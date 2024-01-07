
# STORNA #############################################################################################

Storna       <- TabelaSAP[TabelaSAP$Storno %in% c("1","2"),]

# usuń wiersze z przyczyną storna
TabelaSAP    <- TabelaSAP[!TabelaSAP$Storno %in% c("1","2"),]
TabelaSAP    <- TabelaSAP[!TabelaSAP$Rodzaj %in% c("ST"),] # likwidacja pozostałości.
#TabelaSAP<-s
TabelaSAP    <- TabelaSAP %>% dplyr::filter(substr(`Dok. rozl.`, 1, 2) != "98"|is.na(`Dok. rozl.`))

######################################################################################################

# KOMPENSATY WISZĄCE #################################################################################

# Kompensaty   <- TabelaSAP %>% subset(Przypisanie=="KOMPENSATA")
Kompensaty  <- TabelaSAP %>% subset(Referencja=="KOMPENSATA")

# Kompensaty   <- rbind(Kompensaty,Kompensaty2) %>% distinct() # kompensaty wybieramy po polu REFERENCJA
Kompensaty   <- Kompensaty %>% subset(Rozlicz.>Wprowadz.) # kompensaty wiszące na koncie, zawsze mają
######################################################################################################

# PRZELEWY WISZĄCE ###################################################################################
Przelewy  <- TabelaSAP %>% subset(Rodzaj %in% c("WB","WX"))
Przelewy  <- Przelewy %>% subset(Rozlicz.>`Data dok.`+days(1)) # kompensaty wiszące na koncie, zawsze mają

# usuwanie wadium
Przelewy  <-Przelewy[!grepl("WADIUM", Przelewy$Opis, ignore.case = TRUE), ]
Przelewy  <-Przelewy[!grepl("POŻYCZKA", Przelewy$Opis, ignore.case = TRUE), ]
Przelewy  <-Przelewy[!grepl("SPŁATA POŻYCZKI", Przelewy$Opis, ignore.case = TRUE), ]
Przelewy  <- subset(Przelewy, !grepl("WADIUM", Opis))
Przelewy <- subset(Przelewy, !grepl("ZNWU", Opis,ignore.case = TRUE))
######################################################################################################
# Przypisać do każdej pozycji 


# FILTR PO RODZAJACH DOKUMENTÓW ######################################################################
RodzajeDokumentow <- read_excel(here::here("sources","rodzaje_dokumentow.xlsm"), col_names = TRUE, skip = 0, na = "NA")

RodzajeDokumentowNaleznosci <- RodzajeDokumentow %>% subset(Do_wplywow=="x") %>% select("KOD") %>% unlist()

# TabelaSAP <- TabelaSAPSto
TabelaSAP <- TabelaSAP %>% subset(Rodzaj %in% RodzajeDokumentowNaleznosci)



# zostawiamy tylko te PW, które mają nr dokumentu kompensat wiszących
TabelaSAP <- TabelaSAP %>% subset(Rodzaj!="PW")
#TabelaSAP <- rbind(TabelaSAP,Kompensaty)

# zostawiamy tylko te WB i WX, które mają numery przelewów wiszących
TabelaSAP <- TabelaSAP %>% subset(Rodzaj!="WB")
#TabelaSAP <- rbind(TabelaSAP,Przelewy) 

# w tym momencie mamy tabelę Sap w miarę wyczyszczoną i możemy rozliczać kompensaty i przelewy

PrzelewyKompensaty    <- rbind(Kompensaty,Przelewy)
PrzelewyKompensatySkr <- PrzelewyKompensaty %>% select("Konto","Data dok.","Rozlicz.", "Kwota w Wkr")


DatyRozliczen<-TabelaSAP %>% select(c("Referencja","Data dok.","Rozlicz.","Rodzaj")) %>% distinct()
colnames(DatyRozliczen)[1]<-"Faktura"

DatyRozliczen<-DatyRozliczen %>% subset(startsWith(DatyRozliczen$Faktura,"A")|Rodzaj=="NO")

DatyRozliczen<-DatyRozliczen %>% tidyr::replace_na(list(Rozlicz. = as.Date("2030-01-01")))%>% distinct()

DatyRozliczen<-DatyRozliczen %>% group_by(Faktura)  %>% slice(which.max(Rozlicz.))
DatyRozliczen$Rodzaj <-NULL

#DatyRozliczen$`Data dok.` <-as.Date(DatyRozliczen$`Data dok.`)
#DatyRozliczen$Rozlicz. <-as.Date(DatyRozliczen$Rozlicz.)



TabelaBW_sklejona <- left_join(TabelaBW_do_sklejenia, DatyRozliczen, by="Faktura")
TabelaBW_sklejona <- TabelaBW_sklejona %>% subset(`WaN WK`!=0)
TabelaBW_sklejona<-TabelaBW_sklejona %>% dplyr::filter(!startsWith(Faktura,"A160")) #usuwamy pozycje zaczynające się od A160

#SPRAWDZIĆ A160!!!!
#DOBIĆ BRAKUJące FAKTURY Z 19stego grudnia!

TabelaBW_sklejonaNaPotrzeby <- TabelaBW_sklejona %>% subset(!is.na(`Data dok.`))


# określenie dni w których było saldo ########################################################

result <- data.frame()

result <- TabelaBW_sklejonaNaPotrzeby %>%
  rowwise() %>%
  mutate(dni_na_saldzie = list(seq(`Data dok.` , Rozlicz., by = "day"))) %>%
  unnest(dni_na_saldzie) %>%
  select(Klient, dni_na_saldzie) %>% 
  distinct()

result<-result %>% dplyr::filter(dni_na_saldzie<(today()+days(10)) )


# Łączenie danych
merged_df <- result %>%
  left_join(TabelaBW_sklejonaNaPotrzeby
            , by = "Klient")# %>%
  #dplyr::filter(Data_sprzedazy %in% Dni_na_saldzie)

merged_df<-merged_df %>%  subset(dni_na_saldzie>today()-days(365))

# Obliczanie średniej ważonej stawki VAT
weighted_avg_vat <- function(df) {
  weighted.mean(df$Stawka_VAT, w = df$Saldo)
}
# Tworzenie ramki danych z wynikami
result_df <- merged_df %>% dplyr::filter(dni_na_saldzie>=`Data dok.` & dni_na_saldzie<`Rozlicz.`) %>% 
  group_by(Klient, dni_na_saldzie) %>%
  summarise(Wazona_srednia_VAT = weighted.mean(StawkaVAT, w = `WaN WK`), Saldo=sum(`WaN WK`))


klient <- merged_df %>% dplyr::filter(dni_na_saldzie==as.Date("2023-12-23"), Klient=="10047599")
klient <- klient %>% dplyr::filter(dni_na_saldzie>=`Data dok.` & dni_na_saldzie<`Rozlicz.`)


result_przelewy<-data.frame()
library(tidyr)
result_przelewy <- PrzelewyKompensatySkr %>%
  rowwise() %>%
  mutate(dni_na_saldzie = list(seq(`Data dok.` , `Rozlicz.`, by = "day"))) %>%
  unnest(dni_na_saldzie) %>%
  select(Konto, dni_na_saldzie) %>% 
  distinct()

colnames(PrzelewyKompensatySkr)[1]<-"Konto"
# Łączenie danych
merged_przelewy_df <- result_przelewy %>%
  left_join(PrzelewyKompensatySkr
            , by = "Konto")# %>%
#dplyr::filter(Data_sprzedazy %in% Dni_na_saldzie)

merged_przelewy_df<-merged_przelewy_df %>%  subset(dni_na_saldzie>today()-days(365))

result_przelewy_df <- merged_przelewy_df %>% dplyr::filter(dni_na_saldzie>=`Data dok.` & dni_na_saldzie<`Rozlicz.`) %>% 
  group_by(Konto, dni_na_saldzie) %>%
  summarise(Saldo_przelewy=sum(`Kwota w Wkr`))


colnames(result_przelewy_df)[1]<-"Klient"



TabelaZbiorcza<-left_join(result_df,result_przelewy_df,by=c("Klient","dni_na_saldzie" ))

TabelaZbiorcza$KwotaPrzelewuNetto <- ifelse(!is.na(TabelaZbiorcza$Saldo_przelewy),TabelaZbiorcza$Saldo_przelewy/(1+TabelaZbiorcza$Wazona_srednia_VAT),0)
TabelaZbiorcza$SaldoNaleznosci <- TabelaZbiorcza$Saldo+TabelaZbiorcza$KwotaPrzelewuNetto
TabelaZbiorcza<-TabelaZbiorcza %>% subset(SaldoNaleznosci!=0)

Opiekunowie <- KlientDanePodstawowe %>% select(c("Klient","Opiekunowie","Klient - Grupa sprzedawców","...17")) %>% dplyr::filter(Opiekunowie!="Nieprzypisane")

TabelaZbiorcza<-left_join(TabelaZbiorcza,Opiekunowie,by="Klient")

# USTAWIĆ DOBRZE ZAKRESY DANYCH


NaleznosciAM      <- TabelaZbiorcza %>% select("Opiekunowie","dni_na_saldzie","SaldoNaleznosci") %>% group_by(Opiekunowie,dni_na_saldzie) %>% summarize(Saldo=sum(SaldoNaleznosci)) %>% group_by(Opiekunowie) %>% summarize(AverageReceivables=sum(Saldo))
NaleznosciDzienne <- TabelaZbiorcza %>% select("Opiekunowie","dni_na_saldzie","SaldoNaleznosci") %>% group_by(dni_na_saldzie,Opiekunowie,) %>% summarize(AverageReceivables=sum(SaldoNaleznosci))




sum(NaleznosciAM$AverageReceivables)


AMBES<-unique(KlientDanePodstawowe$`PRBr_AM BES`) 
AMBES<- AMBES[AMBES!="Nieprzypisane"]
AMBSP<-unique(KlientDanePodstawowe$`PRBr_AM BSP`) 
AMBSP<- AMBSP[AMBSP!="Nieprzypisane"]




# roczna sprzedaż




data_startowa = as.Date("2023-12-31")
unikalne_daty <- seq(data_startowa-days(365),data_startowa,by="day")



# Zakładając, że masz ramkę danych o nazwie 'sprzedaz' z kolumnami 'data', 'handlowiec' i 'sprzedaz'
# Zakładam, że dane są posortowane według daty w rosnącej kolejności


TabelaDoSprzedazy<-TabelaBW_sklejonaNaPotrzeby %>% select(`Data faktury`,Opiekunowie,`WaN WK`)
NazwyKol<-c('data', 'handlowiec' ,'sprzedaz')
colnames(TabelaDoSprzedazy)<-NazwyKol
dane<-TabelaDoSprzedazy
# Funkcja do obliczania rocznej sprzedaży dla danego dnia
oblicz_roczna_sprzedaz <- function(dzien) {
  dane_dnia <- subset(dane, data > dzien - days(365) & data <= dzien)
  roczna_sprzedaz <- aggregate(sprzedaz ~ handlowiec, data = dane_dnia, sum)
  return(data.frame(data = dzien, roczna_sprzedaz))
}

# Unikalne daty w ramce danych


# Użyj lapply do obliczenia rocznej sprzedaży dla każdego dnia
wyniki <- lapply(unikalne_daty[1:365], oblicz_roczna_sprzedaz)

# Konwersja wyników do ramki danych
wyniki <- do.call(rbind, wyniki)








# średnie należności




data_startowa=as.Date("2023-12-31")

unikalne_daty      <-seq(data_startowa-days(360),data_startowa,by="day")


TabelaDoNaleznosci <- as.data.frame(NaleznosciDzienne) %>% select(dni_na_saldzie,Opiekunowie,AverageReceivables)

NazwyKol<-c('data', 'handlowiec' ,'naleznosci')
colnames(TabelaDoNaleznosci)<-NazwyKol
dane<-TabelaDoNaleznosci
# Funkcja do obliczania średniego poziomu rocznych należności dla danego dnia
oblicz_roczna_sprzedaz <- function(dzien) {
  dane_dnia <- subset(dane, data > dzien - days(360) & data <= dzien)
  srednie_naleznosci <- aggregate(naleznosci ~ handlowiec, data = dane_dnia, mean)
  return(data.frame(data = dzien, srednie_naleznosci))
}

# Unikalne daty w ramce danych

wyniki_naleznosci<-data.frame()

for (i in 1:365) {
  # # Wybierz dane dla danego dnia
  # dane_dnia <- TabelaDoNaleznosci[TabelaDoNaleznosci$data == as.Date((max(TabelaDoNaleznosci$data) - i)), ]
  # 
  # # Oblicz roczną sprzedaż dla każdego handlowca
  # roczna_sprzedaz <- aggregate(sprzedaz ~ handlowiec, data = dane_dnia, mean)
  # 
  # # Dodaj dane do ramki wynikowej
  # wyniki_naleznosci <- rbind(wyniki_naleznosci, roczna_sprzedaz)
  # 
  # 
  dzien <- as.Date((max(TabelaDoNaleznosci$data) - days(i)))
  dane_dnia <- TabelaDoNaleznosci[TabelaDoNaleznosci$data == dzien, ]
  
  # Oblicz roczną sprzedaż dla każdego handlowca
  roczna_sprzedaz <- aggregate(naleznosci ~ handlowiec, data = dane_dnia, mean) %>% mutate(DataSalda=dzien)

  # Dodaj dane do ramki wynikowej
  wyniki_naleznosci <- rbind(wyniki_naleznosci, roczna_sprzedaz)
}






# Użyj lapply do obliczenia rocznej sprzedaży dla każdego dnia
wyniki_naleznosci <- lapply(unikalne_daty[1:365], oblicz_roczna_sprzedaz)

# Konwersja wyników do ramki danych
wyniki_naleznosci <- do.call(rbind, wyniki_naleznosci)


maleznosci31122022 <- wyniki_naleznosci %>% dplyr::filter(data=="2023-01-01")

sum(maleznosci31122022$sprzedaz)























# 
# 
# # faktury Wielopozycyjne, dwa różne zespoły przypisania
# FakturyWielopozycyjne <-  TabelaBW %>% 
#   select(Faktura, `Grupa sprzedawców`) %>% # wybierz kolumny
#   distinct() %>% # usuń duplikaty
#   group_by(Faktura) %>% #pogrupuj według nr faktury
#   summarize(LiczbaPozycji=n()) %>% # policz pozycje w grupach
#   dplyr::filter(LiczbaPozycji>1) %>% # wybierz tylko faktury, które mają więcej niż jedną grupę sprzedawców
#   ungroup() %>% # rozgrupuj
#   select(1) %>% # wybierz kolumnę z numerem faktury 
#   unlist() %>% # zamień na wektor
#   unname() # usuń nazwy z wektora
# 
# TabelaSAP$Wielopozycyjna <- ""
# TabelaSAP                <- TabelaSAP %>%  filter_all(any_vars(!is.na(.)))
# 
# # pierwszy krok rozliczenie wiszących przelewów i kompensat z pozycjami według proporcji, jak obliczyć proporcje
# # TabelaSAP <- TabelaSAPPierwotna
# 
# TabelaSAP   <-  TabelaSAP %>% mutate(across(where(is.character), ~tidyr::replace_na(., ""))) # zamiana wartości NA na "0" w kolumnach tekstowych
#                               # zamienia  znaki & we wszystkich kolumnach na znak pusty
# 
# TabelaSAP <- TabelaSAP %>% dplyr::filter(Opis!="VAT")
# 
# 


# 
# 
#                         
# TabelaSAP$Wielopozycyjna <- ifelse(TabelaSAP$Referencja %in% FakturyWielopozycyjne,"1","")
# FakturyWielopozycyjne    <- TabelaSAP[TabelaSAP$Wielopozycyjna=="1",]
# 
# # skrócona wersja TabeliSAP
# FakturaRozliczenie <- TabelaSAP %>% 
#                       select(Konto,Referencja,Rodzaj,`Kwota w Wkr` ,`Data dok.`,Rozlicz.) %>% 
#                       distinct() %>%  
#                       filter_all(any_vars(!is.na(.)))
# 
# # skrócona wersja tabeli Fakturywielopozycyjne
# FakturaRozliczenieWielop <- FakturyWielopozycyjne %>% 
#                             select(Konto,Referencja,Rodzaj,`Kwota w Wkr`,`Data dok.`,Rozlicz.) %>% 
#                             distinct() %>%  
#                             filter_all(any_vars(!is.na(.)))
# 
# WplatyNierozl <- Przelewy %>% 
#                  select(Konto,Referencja,Rodzaj,`Kwota w Wkr` ,`Data dok.`,Rozlicz.) %>% 
#                  distinct() %>%  
#                  filter_all(any_vars(!is.na(.)))
# 
# PoloczFakturyWplaty <- left_join(FakturaRozliczenieWielop,WplatyNierozl,by="Konto")
# PoloczFakturyWplaty <- PoloczFakturyWplaty[!is.na(PoloczFakturyWplaty$`Data dok..y`),]








colnames(FakturaRozliczenie)[1] <-"Faktura"
TabelaBW <- left_join(TabelaBW,FakturaRozliczenie,by="Faktura")







DataOstatniejTransakcji <- TabelaSAP %>% dplyr::filter(`Data dok.`== max(`Data dok.`)) %>%  dplyr::select("Data dok.") %>% unlist() %>% unname()


saveRDS(TabelaSAP,paste0("TabelaSAP","_",paste0(unlist(regmatches(Sys.time(), gregexpr("[[:digit:]]+", Sys.time()))),collapse="_"),".rds"))
require(openxlsx)
require(readxl)



# ile klientów pure BES z dywizją zaliczenia BSP
TabelaTypBESZaliczenieBSP<-TabelaBW[TabelaBW$`Klient - Typ klienta BES/BSP/DUA`=="ES Type" & TabelaBW$`Dywizja zaliczenia BES/BSP`=="BSP",]
TabelaTypBSPZaliczenieBES<-TabelaBW[TabelaBW$`Klient - Typ klienta BES/BSP/DUA`=="SP Type" & TabelaBW$`Dywizja zaliczenia BES/BSP`=="BES",]
TabelaTypDUAZaliczenieBES<-TabelaBW[TabelaBW$`Klient - Typ klienta BES/BSP/DUA`=="Dual Type" & TabelaBW$`Dywizja zaliczenia BES/BSP`=="BES",]
TabelaTypDUAZaliczenieBSP<-TabelaBW[TabelaBW$`Klient - Typ klienta BES/BSP/DUA`=="Dual Type" & TabelaBW$`Dywizja zaliczenia BES/BSP`=="BSP",]
TabelaTypBSPZaliczenieBSP<-TabelaBW[TabelaBW$`Klient - Typ klienta BES/BSP/DUA`=="SP Type" & TabelaBW$`Dywizja zaliczenia BES/BSP`=="BSP",]
TabelaTypBESZaliczenieBES<-TabelaBW[TabelaBW$`Klient - Typ klienta BES/BSP/DUA`=="ES Type" & TabelaBW$`Dywizja zaliczenia BES/BSP`=="BES",]



TabelaBW_do_sklejenia<-TabelaBW_do_sklejenia %>% select(-c("Faktura"))



dane <- data.frame(
  ID = 1:5,
  Opis = c("ABC123", "XYZ456", "DEF789", "ABCXYZ", "123456")
)

# Określony fragment w ciągu znaków
fragment_do_usuniecia <- "ABC"

# Usuwanie wierszy z określonym fragmentem w kolumnie "Opis"





