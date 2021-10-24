# pulizia console
cat("\014")

# importazione librerie da utilizzare
library(corrplot)
library(scatterplot3d)

#
#   DATA PREPROCESSING
#

# caricamento dei dati dal file csvs
data = read.csv('tabella.csv', header = TRUE, sep = ";")

# rimozione colonne nominali 
#data$nome_colonna <- NULL
#data$nome_colonna <- NULL
#data$nome_colonna <- NULL
#data$nome_colonna <- NULL

# recap data
summary(data)
str(data)
plot(data)

# controllo delle colonne per valori mancanti
#sum(is.na(data$nome_colonna))
#sum(is.na(data$nome_colonna))
#sum(is.na(data$nome_colonna))
#sum(is.na(data$nome_colonna))

# eliminazione delle colonne con più valori null
#data$nome_colonne <- NULL

# assegniamo nomi più leggibili alle colonne della tabella
colnames(data) <- c("TotalCores", "Rmax", "Rpeak", "ProcessorSpeed", "CoresPerSocket")

# standardizzazione tabella
st_data = data.frame(scale(data))

# visualizzazione tabella standardizzata
# la standardizzazione non varia il grafico di dispersione
# e non è stata quindi considerata nel seguito dell'analisi
plot(st_data)

# nuova stampa dei dati prima di iniziare l'analisi
summary(data)
str(data)
head(data)
plot(data)
corrplot(cor(data), method = "number")

#
#   REGRESSIONE LINEARE
#

