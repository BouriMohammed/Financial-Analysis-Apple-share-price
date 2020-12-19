#Importer les bibliothèques
library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(xts)
library(ggplot2)

#Importer les données d'un fichier déjà télécharger depuis "Yahoo Fianance"
data=getSymbols(c('AAPL'),src='yahoo', from='2015-04-01', to='2020-04-01',auto.assign=FALSE)
data=na.omit(data)
prix=data[,4]

par(mfrow=c(1,1))
plot(prix)
#Décomposition des données
data.ts=ts(prix, start=2015-04-02, frequency = 120)
data.de=  decompose(data.ts)
plot(data.de)

#Reconfiguration des données
logprix=log(prix)

plot(logprix, type='l' ,xlab='Time' , ylab='Log(prix)', main='logarithme du prix')

#Difference des données
dlogprix=diff(logprix, lag=1)
dlogprix=dlogprix[!is.na(dlogprix)]
ddata.ts=ts(dlogprix, start=2015-04-02, frequency = 120)

plot(dlogprix, type='l' ,xlab='Time' , ylab='Log(prix)')

#Dickey Fuller Test
print(adf.test(logprix))
print(adf.test(dlogprix))

#ACF & PACF
acf(dlogprix, main='ACF du logarithme des prix')
pacf(dlogprix, main='PACF du logarithme des prix')


#Prevision
realreturn=xts(0,as.Date("2020-01-01","%Y-%m-%d"))
forecastreturn= data.frame(Forcasted=numeric())

split=floor(nrow(dlogprix)*(2.9/3))
for (s in split:(nrow(dlogprix)-1)){
        
        dlogprix_training=dlogprix[1:s,]
        dlogprix_testing=dlogprix[(s+1):nrow(dlogprix),]
        
        fit=arima(dlogprix_training, order=c(1,0,17),include.mean = FALSE)
        summary(fit)
        
        arima.forecast = forecast (fit, h=1,level=99)
        summary(arima.forecast)
        
        Box.test(fit$residuals, lag=1, type='Ljung-Box')
        
        forecastreturn = rbind(forecastreturn,arima.forecast$mean[1])
        colnames(forecastreturn) = c("Forecasted")
        returnseries = dlogprix[(s+1),]
        realreturn = c(realreturn,xts(returnseries))
        rm(returnseries)
}

#Visualisation du modèle
realreturn=realreturn[-1]
forecastreturn=xts(forecastreturn,index(realreturn))
plot(realreturn,type='l',main='kkk')
lines(forecastreturn,lwd=2,col='red')

#Visualisation de la préision
plot(forecast(fit,h=120))
plot(forecast(log(data.ts),h=120))

#Validation du modèle
comparaison=merge(realreturn,forecastreturn)
comparaison$accuracy=sign(comparaison$realreturn)==sign(comparaison$Forecasted)
print(comparaison)

accuracy_pourcentage = sum(comparaison$accuracy ==1)*100/length(comparaison$accuracy)
print(accuracy_pourcentage)



