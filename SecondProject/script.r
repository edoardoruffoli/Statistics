# importazione librerie da utilizzare
library(corrplot)
library(caret)
library(MASS)
library(cluster)
library(ggbiplot)
library(pROC)

# caricamento dei dati dal file csv
data = read.csv('tabella.csv', header = TRUE, sep = ";")

# rimozione osservazioni con meno di 82 partite
a = c()
for (i in 1:length(data[,1])) {
  if (data$GP[i] != 82) {
    a=c(a,i)
  }
}
data = data[-a,]

# rimozione colonne non selezionate 
data$SEASON <- NULL
data$TEAM_NAME <- NULL
data$GP <- NULL
data$L <- NULL
data$W_PCT <- NULL
data$MIN <- NULL
data$FGM <- NULL
data$FG_PCT <- NULL
data$FG3M <- NULL
data$FG3_PCT <- NULL
data$FTM <- NULL
data$FT_PCT <- NULL
data$OREB <- NULL
data$DREB <- NULL
data$BLKA <- NULL
data$PF <- NULL
data$PFD <- NULL
data$PLUS_MINUS <- NULL
data$NET_RATING <- NULL
data$AST_PCT <- NULL
data$AST_TO <- NULL
data$AST_RATIO <- NULL
data$OREB_PCT <- NULL
data$DREB_PCT <- NULL
data$REB_PCT <- NULL
data$TM_TOV_PCT <- NULL
data$EFG_PCT <- NULL
data$TS_PCT <- NULL
data$PACE <- NULL
data$POSS <- NULL
data$PIE <- NULL

# dati utilizzati per l'analisi
summary(data)
str(data)
with(data, table(PLAYOFF))

# rinomina la colonna relativa alla classe
#names(data)[names(data) == 'PLAYOFF'] <- 'class'

#
#   REGRESSIONE LOGISTICA
#

data.glm = glm(PLAYOFF~., data=data, family=binomial)
data.glm.p = predict(data.glm, type="response")
sum((data.glm.p>0.5)==(data$PLAYOFF>0.5))/length(data$PLAYOFF)
source("s2_cmroc.r")
s2_confusion(data$PLAYOFF, data.glm.p)

data.glm.roc <- roc(data$PLAYOFF, data.glm.p)
plot(  data.glm.roc, col = "red", type="l",
       main = "Confronto Curve ROC",
       ylab = "Sensibilità  (tasso positivi veri)",
       xlab = "Specificità  (tasso negativi veri)",
       asp=NA)

data.glm.roc$auc

#
#   ANALISI DISCRIMINANTE LINEARE
#

library(MASS)
data.lda = lda(PLAYOFF~., data=data, CV=F)
data.lda.p = predict(data.lda)
data.lda.post = data.lda.p$posterior[,2]
sum((data.lda.post>0.5)==(data$PLAYOFF>0.5))/length(data$PLAYOFF)
source("s2_cmroc.r")
s2_confusion(data$PLAYOFF, data.lda.post)

data.lda.roc <- roc(data$PLAYOFF, data.lda.post)
plot(  data.lda.roc, col = "red", type="l",
       main = "Confronto Curve ROC",
       ylab = "Sensibilità  (tasso positivi veri)",
       xlab = "Specificità  (tasso negativi veri)",
       asp=NA)

data.lda.roc$auc





# costruzione modello LDA preliminare
lda = lda(PLAYOFF~., data=data)
lda.values = predict(lda)
plot(lda.values$x, pch=20, col = data$PLAYOFF)
legend("bottomright",
       inset = 0.02,
       levels(data$PLAYOFF),
       col = c("black", "red", "green3", "blue", "cyan", "magenta"),
       pch = 19,
       bg = "gray")

s2_roc.plot(s2_roc(data$PLAYOFF, data.glm.p))
