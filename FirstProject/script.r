# librerie
library(corrplot)

#
#   DATA PREPROCESSING
#

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
plot(data)

# standardizzazione tabella
st_data = data.frame(scale(data))

# la standardizzazione non varia il grafico di dispersione
# e non è stata quindi considerata nel seguito dell'analisi
plot(st_data)

# grafico correlazioni
corrplot.mixed(cor(data), 
         lower = "number",
         upper = "ellipse",
         number.cex=0.70,
         tl.pos = "lt",
         tl.cex=0.70)

#
#   MODELLO DI REGRESSIONE LINEARE
#

# costruzione e riduzione del modello di regressione lineare
r = matrix(ncol = 2, nrow = 9)

lm.1=lm(W~., data=data)
summary(lm.1)
r[1,]= c(summary(lm.1)$r.squared, summary(lm.1)$adj.r.squared)

lm.2=lm(W~.-FG3A, data=data)
summary(lm.2)
r[2,]= c(summary(lm.2)$r.squared, summary(lm.2)$adj.r.squared)

lm.3=lm(W~.-FG3A-BLK, data=data)
summary(lm.3)
r[3,]= c(summary(lm.3)$r.squared, summary(lm.3)$adj.r.squared)

lm.4=lm(W~.-FG3A-BLK-PTS, data=data)
summary(lm.4)
r[4,]= c(summary(lm.4)$r.squared, summary(lm.4)$adj.r.squared)

lm.5=lm(W~.-FG3A-BLK-PTS-FTA, data=data)
summary(lm.5)
r[5,]= c(summary(lm.5)$r.squared, summary(lm.5)$adj.r.squared)

lm.6=lm(W~.-FG3A-BLK-PTS-FTA-STL, data=data)
summary(lm.6)
r[6,]= c(summary(lm.6)$r.squared, summary(lm.6)$adj.r.squared)

lm.7=lm(W~.-FG3A-BLK-PTS-FTA-STL-REB, data=data)
summary(lm.7)
r[7,]= c(summary(lm.7)$r.squared, summary(lm.7)$adj.r.squared)

lm.8=lm(W~.-FG3A-BLK-PTS-FTA-STL-REB-TOV, data=data)
summary(lm.8)
r[8,]= c(summary(lm.8)$r.squared, summary(lm.8)$adj.r.squared)

lm.9=lm(W~.-FG3A-BLK-PTS-FTA-STL-REB-TOV-AST, data=data)
summary(lm.9)
r[9,]= c(summary(lm.9)$r.squared, summary(lm.9)$adj.r.squared)

# esaminiamo i valori degli R^2 e R^2 corretti
ymin = min(r)
ymax = max(r)
plot(r[,1], pch = 19, type = "b", col = "red", ylim = c(ymin,ymax+0.0005), 
     xlab=expression(R^2~e~R^2~Corretto~dei~9~Modelli~Ottenuti), 
     ylab = "")
axis(1 , at = 0:9)
lines(r[,2], pch = 19, type = "b", col = "blue")
legend("topright", inset = c(-0.10, 0),
       legend=c(expression(R^2), expression(R^2~Corretto)), 
       col=c("red", "blue"), lty=1:1, cex=0.9, bty="n")

# modello selezionato (miglior rapporto R^2/p-values)
lm = lm.7
summary(lm)

# analisi dei residui: grafici
lm.r=residuals(lm)
plot(fitted(lm),lm.r, pch=19)
plot(lm, which=1, lwd=2)
par(mfrow=c(1, 2))
hist(lm.r, 20, freq = FALSE, main="Istogramma dei Residui",
     ylab="Densità",xlab="Residui")
lines(density(lm.r), col = "blue")
lines(sort(lm.r), dnorm(sort(lm.r), mean(lm.r), sd(lm.r)), col="red", lwd=2)
qqnorm(lm.r)
qqline(lm.r, col="red", lwd=2)

# analisi dei residui: indicatori
skewness=mean(((lm.r-mean(lm.r))/sd(lm.r))^3)
skewness
kurtosi=mean(((lm.r-mean(lm.r))/sd(lm.r))^4)-3
kurtosi
shapiro.test(lm.r)

#
#   MODELLO DI REGRESSIONE NON LINEARE (LOGARITMICO)
#

# costruzione e riduzione del modello di regressione logaritmico
r = matrix(ncol = 2, nrow = 9)
ldata=log(data)

lm.log.1=lm(W~., data=ldata)
summary(lm.log.1)
r[1,]= c(summary(lm.log.1)$r.squared, summary(lm.log.1)$adj.r.squared)

lm.log.2=lm(W~.-FG3A, data=ldata)
summary(lm.log.2)
r[2,]= c(summary(lm.log.2)$r.squared, summary(lm.log.2)$adj.r.squared)

lm.log.3=lm(W~.-FG3A-FTA, data=ldata)
summary(lm.log.3)
r[3,]= c(summary(lm.log.3)$r.squared, summary(lm.log.3)$adj.r.squared)

lm.log.4=lm(W~.-FG3A-FTA-PTS, data=ldata)
summary(lm.log.4)
r[4,]= c(summary(lm.log.4)$r.squared, summary(lm.log.4)$adj.r.squared)

lm.log.5=lm(W~.-FG3A-FTA-PTS-BLK, data=ldata)
summary(lm.log.5)
r[5,]= c(summary(lm.log.5)$r.squared, summary(lm.log.5)$adj.r.squared)

lm.log.6=lm(W~.-FG3A-FTA-PTS-BLK-STL, data=ldata)
summary(lm.log.6)
r[6,]= c(summary(lm.log.6)$r.squared, summary(lm.log.6)$adj.r.squared)

lm.log.7=lm(W~.-FG3A-FTA-PTS-BLK-STL-REB, data=ldata)
summary(lm.log.7)
r[7,]= c(summary(lm.log.7)$r.squared, summary(lm.log.7)$adj.r.squared)

# tutti i p-value sono bassi, continuo comunque a ridurre
lm.log.8=lm(W~.-FG3A-FTA-PTS-BLK-STL-REB-AST, data=ldata)
summary(lm.log.8)
r[8,]= c(summary(lm.log.8)$r.squared, summary(lm.log.8)$adj.r.squared)

lm.log.9=lm(W~.-FG3A-FTA-PTS-BLK-STL-REB-AST-FGA, data=ldata)
summary(lm.log.9)
r[9,]= c(summary(lm.log.9)$r.squared, summary(lm.log.9)$adj.r.squared)

# esaminiamo i valori degli R^2 e R^2 corretti
ymin = min(r)
ymax = max(r)
par(mfrow=c(1,1))
plot(r[,1], pch=19, type="b", col="red", ylim=c(ymin,ymax+0.0005), 
     xlab=expression(R^2~e~R^2~Corretto~dei~10~Modelli~Ottenuti), 
     ylab = "")
axis(1 , at = 0:9)
lines(r[,2], pch = 19, type = "b", col = "blue")
legend("topright", inset = c(-0.10, 0),
       legend=c(expression(R^2), expression(R^2~Corretto)), 
       col=c("red", "blue"), lty=1:1, cex=0.9, bty="n")

# modello selezionato (migliori p-value/R^2)
lm.log=lm.log.9
summary(lm.log)

# analisi dei residui: grafici
lm.log.r=residuals(lm.log)
plot(fitted(lm.log),lm.log.r, pch=19)
plot(lm.log, which=1, lwd=2)
par(mfrow=c(1, 2))
hist(lm.log.r, 30, freq = FALSE, main="Istogramma dei Residui",
     ylab="Densità",xlab="Residui")
lines(density(lm.log.r), col = "blue")
lines(sort(lm.log.r), dnorm(sort(lm.log.r), mean(lm.log.r), sd(lm.log.r)), 
      col="red", lwd=2)
qqnorm(lm.log.r)
qqline(lm.log.r, col="red", lwd=2)

# analisi dei residui: indicatori
skewness=mean(((lm.log.r-mean(lm.log.r))/sd(lm.log.r))^3)
skewness
kurtosi=mean(((lm.log.r-mean(lm.log.r))/sd(lm.log.r))^4)-3
kurtosi
shapiro.test(lm.log.r)

# valutazione dei residui del modello di regressione non ridotto
lm.log.1.r = residuals(lm.log.1)
shapiro.test(lm.log.1.r)

# rimozione dei residui outliers
boxplot(lm.log.r, main="Boxplot residui prima della rimozione degli outliers", outcol="red")
r_outliers <- boxplot(lm.log.r, plot=FALSE)$out
r_outliers <- rev(sort(r_outliers))
r_outliers
lm.log.r<-lm.log.r[-which(lm.log.r %in% r_outliers[1:length(r_outliers)])]
boxplot(lm.log.r, main="Boxplot residui dopo la rimozione degli outliers", outcol="red")

# grafici post rimozione outliers
par(mfrow=c(1, 2))
hist(lm.log.r, 30, freq = FALSE, main="Istogramma dei Residui",
     ylab="Densità",xlab="Residui")
lines(density(lm.log.r), col = "blue")
lines(sort(lm.log.r), dnorm(sort(lm.log.r), mean(lm.log.r), sd(lm.log.r)), 
      col="red", lwd=2)
qqnorm(lm.log.r)
qqline(lm.log.r, col="red", lwd=2)

# indicatori post rimozione outliers
skewness=mean(((lm.log.r-mean(lm.log.r))/sd(lm.log.r))^3)
skewness
kurtosi=mean(((lm.log.r-mean(lm.log.r))/sd(lm.log.r))^4)-3
kurtosi
shapiro.test(lm.log.r)

#
#   AUTOVALUTAZIONE E PREVISIONE DEI MODELLI
#

# logaritmi dei dati originali
ldata = log(data)

set.seed(303)

# validazione modelli
n = 50
err_lin = rep(0,n)
err_log = rep(0,n)
for(i in 1:n){
  testset = sort(sample(534, 50))
  data_train = data[-testset,]
  data_test = data[testset,]
  ldata_train = ldata[-testset,]
  ldata_test = ldata[testset,]
  
  # costruzione modelli lineare e modello logaritmico con i training set
  data_train.lm = lm(W~.-FG3A-BLK-PTS-FTA-STL-REB, data = data_train)
  ldata_train.lm = lm(W~.-FG3A-FTA-PTS-BLK-STL-REB-AST-FGA , data = ldata_train)
  
  # calcoliamo l’errore per i due modelli
  data_train.lm.p = predict(data_train.lm, data_test)
  ldata_train.lm.p = predict(ldata_train.lm, ldata_test)
  
  # salviamo lo scarto quadratico medio per entrambi i modelli
  err_lin[i] = sqrt(mean((data_train.lm.p - data_test$W)^2))
  err_log[i] = sqrt(mean((exp(ldata_train.lm.p) - data_test$W)^2))
}

# media errori
mean(err_lin)
mean(err_log)

# deviazione standard errori
sd(err_lin)
sd(err_log)

# rappresentazione grafica errori dei due modelli
par(mfrow=c(1,1))
gmin = min(err_lin, err_log)
gmax = max(err_lin, err_log)
plot(err_lin, type="b", pch=20, col="blue", ylim=c(gmin, gmax+1),
     ylab="Errore", xlab="Iterazione")
points(err_log, type="b", pch=20, col="red")
legend("topright", inset = c(-0.10, 0), c("Lineare", "Logaritmico"),
       col = c("blue", "red"), pch = c(19,19), cex=0.7, bty="n")


# previsione modello lineare
set.seed(303)
testset = sort(sample(534, 50))
data_train = data[-testset,]
data_test = data[testset,]

data_train.lm = lm(W~.-FG3A-BLK-PTS-FTA-STL-REB, data = data_train)
data_train.lm.p = predict(data_train.lm, data_test)
data_test$W
sqrt(mean(data_train.lm.p-data_test$W)^2)

data_train.lm.ci = predict(data_train.lm, data_test, interval="confidence")
data_train.lm.pi = predict(data_train.lm, data_test, interval="prediction")

plot(
  data_test$W, pch=19, col="red",
  ylim = c(min(data_train.lm.pi[, 2]), max(data_train.lm.pi[, 3])),
  ylab = "Vittorie",
  xlab = "Indice"
)

x = 1:50

points(x-0.05, data_train.lm.ci[,1], pch=19, col="blue")
segments(x-0.05, data_train.lm.ci[,2], x-0.05,data_train.lm.ci[,3], col="blue")

points(x+0.05, data_train.lm.pi[,1], pch=19, col="green3")
segments(x+0.05, data_train.lm.pi[,2], x+0.05, data_train.lm.pi[,3], col="green3")

