# Instalowanie i załadowanie niezbędnych pakietów
install.packages("tidyverse")
library(tidyverse)

# Tworzenie przykładowej ramki danych
faktury <- data.frame(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
  NUMER_KLIENTA = c(1, 2, 1, 2),
  OPIEKUN = c("Anna", "Jan", "Anna", "Jan"),
  DATA_FAKTURY = as.Date(c("2021-01-15", "2021-02-20", "2022-03-10", "2022-04-05")),
  KWOTA_NETTO = c(500, 700, 600, 800),
  DATA_ZAPLATY = as.Date(c("2021-02-01", "2021-03-15", "2022-03-20", "2022-05-10")),
  STAWKA_VAT = c(0.23, 0.23, 0.23, 0.23)
)

# Filtruj faktury z roku 2022
faktury_2022 <- faktury %>%
  dplyr::filter(year(DATA_FAKTURY) == 2022)

# Tworzenie listy unikalnych opiekunów
opiekunowie <- unique(faktury_2022$OPIEKUN)

# Tworzenie ramki danych z dzieniami roku 2022
daty_2022 <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "days")

# Tworzenie ramki danych
ramka_danych <- expand.grid(OPIEKUN = opiekunowie, DATA = daty_2022)

# Funkcja obliczająca saldo dla danego opiekuna na daną datę
oblicz_saldo <- function(opiekun, data) {
  saldo <- faktury_2022 %>%
    dplyr::filter(OPIEKUN == opiekun, DATA_FAKTURY <= data) %>%
    summarise(SALDO = sum(KWOTA_NETTO * (1 + STAWKA_VAT) * -1))
  
  return(ifelse(is.na(saldo$SALDO), 0, saldo$SALDO))
}

# Użycie mapply do obliczania sald dla każdego opiekuna i daty
ramka_danych$SALDO <- mapply(oblicz_saldo, ramka_danych$OPIEKUN, ramka_danych$DATA)

# Dodanie wiersza z uśrednionymi saldami
srednie_sald <- ramka_danych %>%
  group_by(OPIEKUN) %>%
  summarise(SREDNIE_SALDO = mean(SALDO, na.rm = TRUE))

# Dodanie wiersza z uśrednionymi saldami do ramki danych
ramka_danych <- bind_rows(ramka_danych, srednie_sald)

# Wyświetlenie ramki danych
print(ramka_danych)

ramka_danych_szeroka <- ramka_danych %>%
  pivot_wider(names_from = OPIEKUN, values_from = SALDO)
















# Przykładowa ramka danych
df <- data.frame(
  faktura = c(1, 2, 3),
  data_dokumentu = as.Date(c("2022-10-15", "2022-10-16", "2022-10-17")),
  data_zaplaty = as.Date(c("2022-10-20", "2022-10-18", "2022-10-19"))
)

# Tworzenie nowej ramki danych
result <- data.frame()

# Iteracja po każdym wierszu w ramce danych oryginalnej
for (i in 1:nrow(df)) {
  faktura_num <- df$faktura[i]
  data_dokumentu <- df$data_dokumentu[i]
  data_zaplaty <- df$data_zaplaty[i]
  
  # Tworzenie sekwencji dat między data_dokumentu a data_zaplaty
  dni_na_saldzie <- seq(data_dokumentu, data_zaplaty, by = "day")
  
  # Dodawanie nowych wierszy do ramki danych wynikowej
  result <- rbind(result, data.frame(faktura = rep(faktura_num, length(dni_na_saldzie)), data = dni_na_saldzie))
}

# Wyświetlanie wynikowej ramki danych
print(result)




library(dplyr)

# Przykładowa ramka danych sprzedażowych
sprzedaz_df <- data.frame(
  Numer_klienta = rep(c(1, 2, 3), each = 5),
  Data_sprzedazy = rep(as.Date(c("2022-10-15", "2022-10-16", "2022-10-17", "2022-10-18", "2022-10-19")), 3),
  Saldo = runif(15, min = 100, max = 1000),
  Stawka_VAT = c(0.77, 0.08, 0.23, 0.08, 0.23, 0.08, 0.23, 0.05, 0.08, 0.08, 0.23, 0.08, 0.23, 0.05, 0.23)
)

# Przykładowa ramka danych z danymi pozyskanymi z poprzedniego kodu
saldo_df <- data.frame(
  Numer_klienta = c(1, 2, 3, 1, 2, 3),
  Dni_na_saldzie = as.Date(c("2022-10-15", "2022-10-16", "2022-10-17", "2022-10-18", "2022-10-19", "2022-10-20"))
)

# Łączenie danych
merged_df <- saldo_df %>%
  left_join(sprzedaz_df, by = "Numer_klienta") %>%
  dplyr::filter(Data_sprzedazy %in% Dni_na_saldzie)

# Obliczanie średniej ważonej stawki VAT
weighted_avg_vat <- function(df) {
  weighted.mean(df$Stawka_VAT, w = df$Saldo)
}
# Tworzenie ramki danych z wynikami
result_df <- merged_df %>%
  group_by(Numer_klienta, Dni_na_saldzie) %>%
  summarise(Wazona_srednia_VAT = weighted.mean(Stawka_VAT, w = Saldo))


# Wyświetlanie wynikowej ramki danych
print(result_df)















