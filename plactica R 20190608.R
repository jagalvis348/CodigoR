

#codigos para visualizar datos

#Es codigo permite visualizar las 5 primeras filas de la 
#de la columna ArrDelay
df[1:5, "ArrDelay"]

#con este codigo se observa las 5 filas de las dos columnas indicadas
df[1:5, c("ArrDelay", "DepDelay")]


# crear una lista

lista <- list()
lista$objeto1 <- c(1,2,3)
lista$objeto2 <- "hola"
lista$objeto3 <- df[1:5,]

lista[[1]][2:3]
lista$objeto1


# programacion vectorial 

setwd("~/Python")

library(readr)
df <- as.data.frame(read_csv("2008.csv", n_max = 100000))

delay <- df$ArrDelay
delay <- delay[!is.na(delay)]  # borrar los NA

df$TaxiIn > df$TaxiOut  # comparar si una variable es mayor que otra

a <- delay[delay >0] *2  # multiplica por 2
b <- delay[delay >0] + delay[delay>0] # suma dos variables

a==b  # compara dato a dato si son iguales

all(a==b) # evalua si todos los datos son iguales


mean(delay)

# promedio mediante las matrices
delay %*% rep(1,length(delay))/length(delay)

df2 <-df[,c("CarrierDelay","WeatherDelay",
            "NASDelay", "SecurityDelay",
            "LateAircraftDelay")]

df2 <- df2[complete.cases(df2),]   ## trae sol filas completas

df2 <- as.matrix(df2)   ## crear una matriz

df2 %*% rep(1, ncol(df2))

resultado <- rep(1, nrow(df2)) %*% df2

resultado[5] == sum(df2[,5])



### planifica la creacion de variables

# opcion 1
start <- Sys.time()
suma <- c()
for(i in 1:nrow(df)){
  suma <- c(suma, df[i< "ArrDelay"])
}

print(Sys.time()-start)


# opcion 2
start <- Sys.time()
suma <- rep(0,nrow(df))
suma[1] <- df[1,"ArrDelay"]

for(i in 2:nrow(df)){
  suma[i] <- suma[i-1]+ df[i,"ArrDelay"]
}

print(Sys.time()-start)


# opcion 3
start <-Sys.time()
suma <- cumsum(df$ArrDelay)

print(Sys.time()-start)




#generar un mapa de calor

df<-read_csv("2008.csv")

df<-as.data.frame(df)

df$DayOfWeek <-as.factor(df$DayOfWeek)
df$Month <- as.factor(df$Month)



