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
data$W <- NULL
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
fac <- factor(data$PLAYOFF, labels = c('Not Qualified', 'Qualified'))
barplot(table(fac), ylim=c(0,300), col=c("darkolivegreen1", "cyan"))

# Scatter Plot
library(ggplot2)
library(GGally)

# uso una tabella con factor per le stampe
dataf=data
dataf$PLAYOFF <- as.factor(dataf$PLAYOFF)

ggpairs(dataf %>% na.omit(), aes(colour = fac, alpha=0.5))

# OFF_RATING e DEF_RATING
ggplot(dataf, aes(OFF_RATING, DEF_RATING, col=PLAYOFF)) + geom_jitter() + 
    scale_color_discrete(labels = c("Not Qualified", "Qualified"))

#
#   ANALISI DISCRIMINANTE LINEARE
#

library(MASS)
data.lda = lda(PLAYOFF~., data=data, CV=F)
data.lda.p = predict(data.lda)

plot(data.lda.p$x, pch=20, col = data$PLAYOFF + 2, main = "Classificazione tramite Analisi Driscriminante Lineare")
legend("bottomright",
       inset = 0.02,
       legend = c("Non Qualificate ai Playoff", "Qualificate ai Playoff"),
       col = c("red", "green3"),
       pch = 19)

data.lda.post = data.lda.p$posterior[,2]
sum((data.lda.post>0.5)==(data$PLAYOFF>0.5))
sum((data.lda.post>0.5)==(data$PLAYOFF>0.5))/length(data$PLAYOFF)

# valutiamo l’errore
sum(data$PLAYOFF != data.lda.p$class)
sum(data$PLAYOFF != data.lda.p$class)/length(data$PLAYOFF)

source("s2_cmroc.r")
s2_confusion(data$PLAYOFF, data.lda.post)

library(pROC)
data.lda.roc <- roc(data$PLAYOFF, data.lda.post)
plot(  data.lda.roc, col = "red", type="l",
       main = "Curva ROC Analisi Discriminante LINEARE",
       ylab = "Sensibilità  (tasso positivi veri)",
       xlab = "Specificità  (tasso negativi veri)",
       asp=NA)

data.lda.roc$auc

#
#   ANALISI DISCRIMINANTE QUADRATICO
#

library(MASS)
data.qda = qda(PLAYOFF~., data=data, CV=F)
data.qda.p = predict(data.qda)

data.qda.post = data.qda.p$posterior[,2]
sum((data.qda.post>0.5)==(data$PLAYOFF>0.5))
sum((data.qda.post>0.5)==(data$PLAYOFF>0.5))/length(data$PLAYOFF)

source("s2_cmroc.r")
s2_confusion(data$PLAYOFF, data.qda.post)

library(pROC)
data.qda.roc <- roc(data$PLAYOFF, data.qda.post)
plot(  data.qda.roc, col = "red", type="l",
       main = "Curva ROC Analisi Discriminante Quadratico",
       ylab = "Sensibilità  (tasso positivi veri)",
       xlab = "Specificità  (tasso negativi veri)",
       asp=NA)

data.qda.roc$auc


#
#   REGRESSIONE LOGISTICA
#

data.glm = glm(PLAYOFF~., data=data, family=binomial)
summary(data.glm)

data.glm.p = predict(data.glm, type="response")
sum((data.glm.p>0.5)==(data$PLAYOFF>0.5))/length(data$PLAYOFF)
source("s2_cmroc.r")
s2_confusion(data$PLAYOFF, data.glm.p)

library(pROC)
data.glm.roc <- roc(data$PLAYOFF, data.glm.p)
plot(  data.glm.roc, col = "red", type="l",
       main = "Curva ROC Regressione Logistica",
       ylab = "Sensibilità  (tasso positivi veri)",
       xlab = "Specificità  (tasso negativi veri)",
       asp=NA)

data.glm.roc$auc


#
#  CONFRONTO MODELLI
#

plot(  data.glm.roc, col = "red",
       main = "Confronto Curve ROC",
       ylab = "Sensibilità  (tasso positivi veri)",
       xlab = "Specificità  (tasso negativi veri)",
       asp=NA
)
lines( data.lda.roc, col = "green3")
lines( data.qda.roc, col = "blue")
legend(  "bottomright",
        legend = c("glm", "lda", "qda"),
        col = c("red", "green", "blue"),
        lwd = 2
)

data.lda.roc$auc
data.qda.roc$auc
data.glm.roc$auc

#
#   AUTOVALUTAZIONE
#

#set.seed(265761)
set.seed(25761)

library(pROC)

l=length(data$PLAYOFF)
acc=matrix(0,100,3)
auc=matrix(0,100,3)
for(i in 1:100) {
  idx=sample(l,100)
  datacv=data[-idx,]
  
  data.lda=lda(PLAYOFF~., data=datacv)
  data.lda.p=predict(data.lda, data[idx,])$posterior[,2]
  acc[i,1]=sum((data.lda.p>0.5)==(data$PLAYOFF[idx]>0.5))/100
  auc[i,1]=roc(data$PLAYOFF[idx], data.lda.p)$auc
  
  data.qda=qda(PLAYOFF~., data=datacv)
  data.qda.p=predict(data.qda, data[idx,])$posterior[,2]
  acc[i,2]=sum((data.qda.p>0.5)==(data$PLAYOFF[idx]>0.5))/100
  auc[i,2]=roc(data$PLAYOFF[idx], data.qda.p)$auc
  
  data.glm=glm(PLAYOFF~., data=datacv)
  data.glm.p=predict(data.glm, data[idx,], type="response")
  acc[i,3]=sum((data.glm.p>0.5)==(data$PLAYOFF[idx]>0.5))/100
  auc[i,3]=roc(data$PLAYOFF[idx], data.glm.p)$auc
}

# lda
mean(acc[,1])
sd(acc[,1])

mean(auc[,1])
sd(auc[,1])

# qda
mean(acc[,2])
sd(acc[,2])

mean(auc[,2])
sd(auc[,2])

# glm
mean(acc[,3])
sd(acc[,3])

mean(auc[,3])
sd(auc[,3])

# risultati
boxplot(acc, main="Boxplot Accuracy", xlab="Classification Methods", ylab="Accuracy", xaxt="n")
axis(1,at=1:3,labels=c("lda", "qda", "glm"))

boxplot(auc, main="Boxplot AUC", xlab="Classification Methods", ylab="AUC", xaxt="n")
axis(1,at=1:3,labels=c("lda", "qda", "glm"))

#A very low p-value, this means that there’s a statistical difference between
#the two! So even though their means only differ by 0.000137 
#through 100.000 trails it’s a statistically significant difference.
wilcox.test(acc[,1], acc[,3])

#   TEST ROBUSTEZZA
#

set.seed(27647008)
n = nrow(data)
idxSwapped = sample(n, n)

accLdaMedia = rep(0, n)
accQdaMedia = rep(0, n)
accGlmMedia = rep(0, n)

# Progress bar
library(progress)
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = n,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar 

for (j in 1:n) {
  pb$tick()
  dataSwapped = data
  dataSwapped$PLAYOFF[idxSwapped[1:j]]= !dataSwapped$PLAYOFF[idxSwapped[1:j]]
  
  acc = matrix(0, 10, 3)

  # AUTOVALUTAZIONE
  for(i in 1:10) {
    idx=sample(l,100)
    datacv=dataSwapped[-idx,]
    
    data.lda=lda(PLAYOFF~., data=datacv)
    data.lda.p=predict(data.lda, data[idx,])$posterior[,2]
    acc[i,1]=sum((data.lda.p>0.5)==(data$PLAYOFF[idx]>0.5))/length(idx)
    
    data.qda=qda(PLAYOFF~., data=datacv)
    data.qda.p=predict(data.qda, data[idx,])$posterior[,2]
    acc[i,2]=sum((data.qda.p>0.5)==(data$PLAYOFF[idx]>0.5))/length(idx)
    data.glm=glm(PLAYOFF~., data=datacv)
    data.glm.p=predict(data.glm, data[idx,], type="response")
    acc[i,3]=sum((data.glm.p>0.5)==(data$PLAYOFF[idx]>0.5))/length(idx)
  }
 
  accLdaMedia[j] = mean(acc[,1])
  accQdaMedia[j] = mean(acc[,2]) 
  accGlmMedia[j] = mean(acc[,3])
}

plot(
  accGlmMedia,
  type = "l",
  col = "red",
  ylim = c(0,1),
  main = "Confronto Robustezza Modelli di Classificazione",
  ylab = "Accuratezza Media Autovalutazione",
  xlab = "Numero Valori Scambiati",
  lwd=2
)
lines(accLdaMedia, col = "green", lty=2, lwd=2)
lines(accQdaMedia, col = "blue", lwd=2)
legend( "bottomleft",
        legend = c("glm", "lda", "qda"),
        col = c("red", "green", "blue"),
        lwd = 2
)

#
#   PARTIZIONI
#

# lda
library(klaR)
partimat(PLAYOFF ~ OFF_RATING + DEF_RATING, data=dataf, method="lda", main="lda")

# qda
partimat(PLAYOFF ~ OFF_RATING + DEF_RATING, data=dataf, method="qda", main="qda")

