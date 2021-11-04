# pulizia console
cat("\014")

# importazione librerie da utilizzare
library(corrplot)
library(scatterplot3d)

#
#   DATA PREPROCESSING
#

# caricamento dei dati dal file csvs
data = read.csv('nba_complete_team_statistics.csv', header = TRUE, sep = ";")

# rimozione colonne nominali 
data$SEASON <- NULL
data$TEAM_NAME <- NULL

# rimozione colonne non selezionate e percentuali
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

# recap data
summary(data)
str(data)
plot(data, pch=".")

# grafico correlazioni
corrplot.mixed(cor(data), 
         lower = "number",
         upper = "ellipse",
         number.cex=0.70,
         tl.pos = "lt",
         tl.cex=0.70)

#
#   REGRESSIONE LINEARE
#

# riduzione del modello di regressione lineare
r = matrix(ncol = 2, nrow = 10)

lm.1=lm(W~., data=data)
summary(lm.1)
r[1,]= c(summary(lm.1)$r.squared, summary(lm.1)$adj.r.squared)

lm.2=lm(W~.-NET_RATING, data=data)
summary(lm.2)
r[2,]= c(summary(lm.2)$r.squared, summary(lm.2)$adj.r.squared)

lm.3=lm(W~.-NET_RATING-FTA, data=data)
summary(lm.3)
r[3,]= c(summary(lm.3)$r.squared, summary(lm.3)$adj.r.squared)

lm.4=lm(W~.-NET_RATING-FTA-STL, data=data)
summary(lm.4)
r[4,]= c(summary(lm.4)$r.squared, summary(lm.4)$adj.r.squared)

lm.5=lm(W~.-NET_RATING-FTA-STL-BLK, data=data)
summary(lm.5)
r[5,]= c(summary(lm.5)$r.squared, summary(lm.5)$adj.r.squared)

lm.6=lm(W~.-NET_RATING-FTA-STL-BLK-AST, data=data)
summary(lm.6)
r[6,]= c(summary(lm.6)$r.squared, summary(lm.6)$adj.r.squared)

lm.7=lm(W~.-NET_RATING-FTA-STL-BLK-AST-REB, data=data)
summary(lm.7)
r[7,]= c(summary(lm.7)$r.squared, summary(lm.7)$adj.r.squared)

### Sono tutti p value bassi, provo comunque a eliminare i più alti
lm.8=lm(W~.-NET_RATING-FTA-STL-BLK-AST-REB-FG3A, data=data)
summary(lm.8)
r[8,]= c(summary(lm.8)$r.squared, summary(lm.8)$adj.r.squared)

lm.9=lm(W~.-NET_RATING-FTA-STL-BLK-AST-REB-FG3A-PTS, data=data)
summary(lm.9)
r[9,]= c(summary(lm.9)$r.squared, summary(lm.9)$adj.r.squared)

lm.10=lm(W~.-NET_RATING-FTA-STL-BLK-AST-REB-FG3A-PTS-TOV, data=data)
summary(lm.10)
r[10,]= c(summary(lm.10)$r.squared, summary(lm.10)$adj.r.squared)

# esaminiamo i valori degli R^2 e R^2 corretti
ymin = min(r)
ymax = max(r)
xl <- expression(R^2 ~ e ~ R^2 ~ Corretto ~ dei ~ 10 ~ Modelli ~ di ~ Regressione ~ Lineare)
plot(r[,1], pch = 19, type = "b", col = "red", ylim = c(ymin,ymax), xlab=xl, ylab = "")
axis(1 , at = 0:10)
lines(r[,2], pch = 19, type = "b", col = "blue")
legend(legend=c(r_squared, r_squared_adj), col=c("red", "blue"), lty=1:1)

# modello di regressione lineare scelto
lm = lm.10
summary(lm)

# scatterplot 3d del modello di regressione lineare
#s3d<-scatterplot3d(data$FGA, data$OFF_RATING , data$DEF_RATING , main="Scatterplot 3D Modello di Regressione Lineare", pch=16, highlight.3d=TRUE, type="p", grid = T, xlab = "FGA", ylab = "OFF_RATING", zlab = "DEF_RATING")
#s3d$plane3d(lm, draw_lines = F, draw_polygon = F)

# analisi dei residui 
lm.r=residuals(lm)
plot(fitted(lm),lm.r)



par(mfrow=c(1, 2))
hist(lm.r, 100, freq = FALSE)
lines(sort(lm.r), dnorm(sort(lm.r), mean(lm.r), sd(lm.r)), col="red", lwd=2)
qqnorm(lm.resid)
qqline(lm.resid, col="red", lwd=2)
skewness = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^3)
skewness
kurtosi = mean(((lm.resid - mean(lm.resid)) / sd(lm.resid))^4) - 3
kurtosi
shapiro.test(lm.resid)


