# caricamento dei dati dal file csv
data = read.csv('tabella.csv', header = TRUE, sep = ";")


library(dplyr)

# Adding column based on other column:
data %>%
  mutate(Status = case_when(
    endsWith(ID, "R") ~ "Recovered",
    endsWith(ID, "S") ~ "Sick"
  ))