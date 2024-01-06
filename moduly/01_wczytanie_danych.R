# wczytać historię księgowań z SAP
# uzupełnianie historii księgowań z SAP

# 1. wyłuskanie listy faktur nierozliczonych na dany dzień, sprawdzić i rozliczyć faktury korekty, 

# 2. wyłuskanie listy przelewów nierozliczonych na dany dzień,
# 3. wyłuskanie przypisań przelewów do faktury,
# 4. redukcja wartości faktury (VAT!) / musimy znać wartość VAT faktury,
# 5. wyłuskanie pozycji sprzedaży z SAP BW,
# 6. sprawdzenie czy faktura wielopozycyjna,
#    a) jeśli tak:
#       - rozliczanie pozycji przelewu najpierw z naliczonym VAT i rozliczenie pozycji BW pozostałą resztą według proporcji wartości pozycji na fakturze,
#    b) jeśli nie:
#       - rozliczanie pozycji przelewu najpierw z naliczonym VAT i rozliczenie pozycji BW pozostałą resztą,
# 9. Przypisać również kompensaty, te, które mają przypisanie do konkrenej faktury, te, które nie mają do pierwszych nierozliczonych

# 7. jeśli przelew nieprzypisany, lub jeśli zostanie reszta z przypisanej faktury to rozliczyć pozycje od najstarszych
# 8. w tabeli z SAP BW stworzyć nową kolumnę dla faktur wielopozycyjnych z proporcją wartości

# potrzebne pola SAP:
# Klient, referencja_przypisanie, data faktury, data rozliczenia, data płatności, opis, rodzaj dokumentu, znacznik storna, numer dokumentu rozliczeniowego, kwota w wal krajowej



library(dplyr)
library(readxl)
require(stringr)
library(lubridate)



phz <- read_excel(here::here("sources","phz.xlsx"),  col_names = TRUE, skip = 0, na = "NA")


lista_plikow <- list.files(path = here::here("sources"), recursive = TRUE)

lista_BW  <- lista_plikow[startsWith(lista_plikow,"BW")]
lista_SAP <- lista_plikow[startsWith(lista_plikow,"plik")]

# tabela z danymi SAP
TabelaSAP <- read_xlsx(here::here("sources",lista_SAP[1]), col_names = TRUE, skip = 0, na = "NA") %>% filter_all(any_vars(!is.na(.)))

for (i in 2:length(lista_SAP)){
  
  pomocnicza <- read_xlsx(here::here("sources",lista_SAP[i]), col_names = TRUE, skip = 0, na = "NA") %>% filter_all(any_vars(!is.na(.)))
  TabelaSAP  <- rbind(TabelaSAP,pomocnicza)
  
}

TabelaSAP          <- TabelaSAP %>% distinct()



# tabela z danymi BW
TabelaBW <- read_excel(here::here("sources",lista_BW[1]), range="G15:Z65000",sheet = "Table",col_names = TRUE, skip = 0, na = "NA") %>% filter_all(any_vars(!is.na(.)))


for (i in 2:length(lista_BW)){
  
  pomocnicza <- read_excel(here::here("sources",lista_BW[i]), range="G15:Z65000",sheet = "Table", col_names = TRUE, skip = 0, na = "NA") %>% 
                filter_all(any_vars(!is.na(.)))
  TabelaBW   <- rbind(TabelaBW,pomocnicza)
  
}

TabelaBW            <- TabelaBW %>% distinct()

TabelaBW$KursFaktury <- TabelaBW$`WaN WK`/TabelaBW$WaN


# wyliczenie stawki podatku vat, aby dostosować wartość wiszących przelewów i kompensat.
# TabelaBW$StawkaVAT <- round(TabelaBW$`VAT Wartość`/TabelaBW$`WaN WK`*100,2)

TabelaBW$StawkaVAT <- ifelse(TabelaBW$KursFaktury==1,round(TabelaBW$`VAT Wartość`/TabelaBW$`WaN WK`,2),round(TabelaBW$`VAT Wartość`*TabelaBW$KursFaktury/TabelaBW$`WaN WK`,2))

TabelaSAP               <- TabelaSAP %>% subset(`Data dok.`<as.Date("2024-01-01"))

TabelaBW$`Data faktury` <- as.Date(TabelaBW$`Data faktury`,tryFormats = c("%d.%m.%Y"))
TabelaBW                <- TabelaBW %>% subset(`Data faktury` < as.Date("2024-01-01"))


# Na razie tylko Brenntag

TabelaBW <- TabelaBW[TabelaBW$`Jednostka gospodarcza`=="6913",]

TabelaSAPPierwotna <- TabelaSAP 
TabelaBWPierwotna  <- TabelaBW 

TabelaSAP    <- TabelaSAP[!startsWith(TabelaSAP$Konto,"2"),]
TabelaBW    <- TabelaBW[!startsWith(TabelaBW$Klient,"2"),]


#savaRds(df,paste0("TabelaSap","_",paste0(unlist(regmatches(Sys.time(), gregexpr("[[:digit:]]+", DataOstatniejTransakcji,collapse="_"),".xlsx"))

KlientDanePodstawowe <-read_excel(here::here("sources","klient_dane_podstawowe.xls"), range="G15:Y60000",sheet = "Table", col_names = TRUE, skip = 0, na = "NA") %>% filter_all(any_vars(!is.na(.)))
KliencizAdminami<-KlientDanePodstawowe %>% subset(`PRBr_Admin AM`!="Nieprzypisane")
KlienciDualAdminBES  <-KlientDanePodstawowe %>% subset(`PRBr_Admin AM`!="Nieprzypisane") %>% subset(`PRBr_Admin AM`==`PRBr_AM BES`) %>% distinct()
KlienciDualAdminBSP  <-KlientDanePodstawowe %>% subset(`PRBr_Admin AM`!="Nieprzypisane") %>% subset(`PRBr_Admin AM`==`PRBr_AM BSP`) %>% distinct()


KlientDanePodstawowe$Opiekunowie <-ifelse(KlientDanePodstawowe$`PRBr_AM BES`!="Nieprzypisane" & KlientDanePodstawowe$`PRBr_AM BSP`=="Nieprzypisane",KlientDanePodstawowe$`PRBr_AM BES`,
                                          ifelse(KlientDanePodstawowe$`PRBr_AM BSP`!="Nieprzypisane" & KlientDanePodstawowe$`PRBr_AM BES`=="Nieprzypisane",KlientDanePodstawowe$`PRBr_AM BSP`,KlientDanePodstawowe$`PRBr_Admin AM`))

KlienciZOpiekunami     <- KlientDanePodstawowe %>% select("Klient","Opiekunowie") %>% dplyr::filter(Opiekunowie!="Nieprzypisane") 
TabelaBW               <- left_join(TabelaBW,KlienciZOpiekunami,by="Klient")
TabelaBW_do_sklejenia  <- TabelaBW %>% select("Jednostka gospodarcza","Klient","Faktura","Data faktury","StawkaVAT","WaN WK","Opiekunowie")



                              

