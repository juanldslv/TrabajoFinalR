library(ggplot2)
data<-read.csv("emisiones_2023.csv",header=TRUE,sep=",")

#data$Emisiones_CO2 <- as.numeric(data$Emisiones_CO2)
#data$Poblacion <- as.numeric(data$Poblacion)
archivo<-as.data.frame(data)

mean_emisiones<-mean(archivo$Emisiones_CO2)
median_emisiones<-median(archivo$Emisiones_CO2)
sd_emisiones<-sd(archivo$Emisiones_CO2)
moda_emisiones <- as.numeric(names(sort(table(data$Emisiones_CO2), decreasing = TRUE)[1]))

mean_emisiones<-mean(archivo$Poblacion)
median_poblacion<-median(archivo$Poblacion)
sd_poblacion<-sd(archivo$Poblacion)
moda_poblacion <- as.numeric(names(sort(table(data$Poblacion), decreasing = TRUE)[1]))

resultados<-data.frame(
  Variable=c("co2","poblacion"),
  Media=c(mean_emisiones,mean_emisiones),
  Mediana=c(median_emisiones,median_poblacion),
  Moda=c(moda_emisiones,moda_poblacion)
  )

write.csv(resultados, "resultados.csv", row.names = FALSE)

ggplot(data, aes(x = Pa?s, y = Emisiones_CO2)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Emisiones de CO2 por Pa?s", x = "Pa?s", y = "Emisiones de CO2 (en toneladas)")

ggplot(datos, aes(x = Emisiones_CO2, y = Poblaci?n, label = Pa?s)) +
  geom_point() +
  geom_text(hjust = 0, vjust = 0) +
  labs(title = "Relaci?n entre Emisiones de CO2 y Poblaci?n", x = "Emisiones de CO2", y = "Poblaci?n")

ggplot(datos, aes(x = Pa?s, y = Poblaci?n)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Poblaci?n por Pa?s", x = "Pa?s", y = "Poblaci?n")





