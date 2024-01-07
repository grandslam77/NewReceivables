

start_date <- as.Date("2023-01-01")
end_date   <- as.Date("2024-12-31")


sales_data <- data.frame(
  Date = seq(start_date, end_date, by = "days")
)

# Dodawanie kolumn z danymi dla każdego handlowca i działu
set.seed(123)  # Ustawienie ziarna dla reprodukowalności
sprzedaz <- sales_data %>%
  mutate(
    Salesperson1 = ifelse(!weekdays(Date) %in% c("sobota", "niedziela"), runif(n(), min = 100, max = 1000), 0),
    Salesperson2 = ifelse(!weekdays(Date) %in% c("sobota", "niedziela"), runif(n(), min = 100, max = 1000), 0),
    Salesperson3 = ifelse(!weekdays(Date) %in% c("sobota", "niedziela"), runif(n(), min = 100, max = 1000), 0),
    Salesperson4 = ifelse(!weekdays(Date) %in% c("sobota", "niedziela"), runif(n(), min = 100, max = 1000), 0),
    Salesperson5 = ifelse(!weekdays(Date) %in% c("sobota", "niedziela"), runif(n(), min = 100, max = 1000), 0),
    Department = sample(c("DeptA", "DeptB"), size = n(), replace = TRUE)
  )
write.xlsx(sprzedaz,"dane.xlsx")
# tempdir()
# # [1] "C:\Users\XYZ~1\AppData\Local\Temp\Rtmp86bEoJ\Rtxt32dcef24de2"
# dir.create(tempdir())


naleznosci <- sales_data %>%
  mutate(
    Salesperson1 = runif(n(), min = 100, max = 1000),
    Salesperson2 = runif(n(), min = 100, max = 1000),
    Salesperson3 = runif(n(), min = 100, max = 1000),
    Salesperson4 = runif(n(), min = 100, max = 1000),
    Salesperson5 = runif(n(), min = 100, max = 1000),
    Department = sample(c("DeptA", "DeptB"), size = n(), replace = TRUE)
  )

write.xlsx(naleznosci,"naleznosci.xlsx")

naleznosci_dluga <- naleznosci %>%
  pivot_longer(
    cols = starts_with("Sale"),
    names_to = "handlowiec",
    values_to = "Wartosc"
  ) 

sprzedaz_dluga<-sprzedaz %>% 
  pivot_longer(
    cols = starts_with("Sale"),
    names_to = "handlowiec",
    values_to = "naleznosci"
  ) 

