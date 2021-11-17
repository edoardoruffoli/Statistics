# caricamento dei dati dal file csv
data = read.csv('tabella.csv', header = TRUE, sep = ";")

playoff_column <- ifelse(RANK < 16, 1, 0)
