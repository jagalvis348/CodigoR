
# Construya el modelo de Regresión de Knn en el conjunto de datos para predecir el ingreso de una persona.

rm(list=ls(all=TRUE))
library("bitops")
library("RCurl")
data=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth119/Datasets/master/Bank_dataset.csv"), header=T, sep=',',
                col.names = c('ID', 'age', 'exp', 'inc', 
                              'zip', 'family', 'ccavg', 'edu', 
                              'mortgage', 'loan', 'securities', 
                              'cd', 'online', 'cc'))
str(data)
# Eliminar ID, exp y zip
data=subset(data,select = -c(ID,exp,zip))

#Convirtiendo age, inc, CCavg, Mortgage, family a numerico y estandarizando
#Income como variable dependiente

dep_atr=c("inc")
num_atr=c("age","ccavg","mortgage","family")
categ_atr=setdiff(names(data),c(num_atr,"inc"))
num_data=data.frame(sapply(data[num_atr], function(x){as.numeric(x)}))

library(permute)
library(lattice)
library(vegan)
num_data=decostand(num_data,method = "range")

#Convirtiendo las variables categoricas a dummy
library(dummies)
categ_data=data.frame(sapply(data[categ_atr], as.factor))
categ_data=dummy.data.frame(categ_data,sep="_")

final_data=cbind(num_data,categ_data,"inc"=data$inc)
str(final_data)

# Divide los datos en entrenamiento, pruebas y datos de evaluación.
set.seed(123)
library(caTools)
rowids = 1:nrow(final_data)
train_index =  sample(rowids, length(rowids)*0.6)
test_index = sample(setdiff(rowids, train_index), length(rowids)*0.2)
eval_index = setdiff(rowids, c(train_index, test_index))

train_data=final_data[train_index,]
test_data=final_data[test_index,]
eval_data=final_data[eval_index,]

# Comprobación de cómo se dividen los registros con respecto al atributo de destino.
summary(final_data$inc)
summary(train_data$inc)
summary(test_data$inc)
summary(eval_data$inc)

ind_variables=setdiff(names(final_data),"inc")
# Usando el algoritmo de Knn para predecir la variable de ingreso
#install.packages("FNN")
library(FNN)

# .........k = 1........................#
#Predicción de los datos  de entretamiento

pred1_train = knn.reg(train = train_data[,ind_variables],test = train_data[,ind_variables],
                      y = train_data$inc, k = 1)

pred1_test = knn.reg(train = train_data[,ind_variables],test = test_data[,ind_variables],
                     
                     y = train_data$inc, k = 1)
#Métricas de error para regresión
library(grid)
library(DMwR)

#Train
cat("Error de las metricas de entrenamiento para k=1")
regr.eval(train_data[,"inc"], pred1_train$pred)
cat("MAPE para k=1 en el entrenamiento es",round(regr.eval(train_data[,"inc"], pred1_train$pred)[4],4)*100,"%")

#Test
cat("Error de las metricas de prueba para k=1")
regr.eval(test_data[,"inc"], pred1_test$pred)
cat("MAPE para k=1 en el prueba es",round(regr.eval(test_data[,"inc"], pred1_test$pred)[4],4)*100,"%")

# .........k = 3........................#
#Predicción de los datos  de entretamiento

pred3_train = knn.reg(train = train_data[,ind_variables],test = train_data[,ind_variables],
                      y = train_data$inc, k = 3)

pred3_test = knn.reg(train = train_data[,ind_variables],test = test_data[,ind_variables],
                     y = train_data$inc, k = 3)

#Métricas de error para regresión
#Train
cat("Error de las metricas de entrenamiento para k=3")
regr.eval(train_data[,"inc"], pred3_train$pred)
cat("MAPE para k=3 en el entrenamiento es",round(regr.eval(train_data[,"inc"], pred3_train$pred)[4],4)*100,"%")

#Test
cat("Error de las metricas de prueba para k=3")
regr.eval(test_data[,"inc"], pred3_test$pred)
cat("MAPE para k=3 en la prueba es",round(regr.eval(test_data[,"inc"], pred3_test$pred)[4],4)*100,"%")


# .........k = 5........................#
#Predicción de los datos  de entretamiento

pred5_train = knn.reg(train = train_data[,ind_variables],test = train_data[,ind_variables],
                      y = train_data$inc, k = 5)

pred5_test = knn.reg(train = train_data[,ind_variables],test = test_data[,ind_variables],
                     y = train_data$inc, k = 5)

#Métricas de error para regresión
#Train

cat("Error de las metricas de entrenamiento para k=5")
regr.eval(train_data[,"inc"], pred5_train$pred)
cat("MAPE para k=5 en el entrenamiento es",round(regr.eval(train_data[,"inc"], pred5_train$pred)[4],4)*100,"%")

#Test
cat("Error de las metricas de prueba para k=5")
regr.eval(test_data[,"inc"], pred5_test$pred)
cat("MAPE para k=5 es",round(regr.eval(test_data[,"inc"], pred5_test$pred)[4],4)*100,"%")

# .........k = 7........................#
#Predicción de los datos  de entretamiento

pred7_train = knn.reg(train = train_data[,ind_variables],test = train_data[,ind_variables],
                      y = train_data$inc, k = 7)

pred7_test = knn.reg(train = train_data[,ind_variables],test = test_data[,ind_variables],
                     y = train_data$inc, k = 7)


#Error en las metricas para regresión
#Train
cat("Error de las metricas de entrenamiento parar k=7")
regr.eval(train_data[,"inc"], pred7_train$pred)
cat("MAPE para k=7 es",round(regr.eval(train_data[,"inc"], pred7_train$pred)[4],4)*100,"%")

#Test
cat("Error de las metricas de prueba para k=7")
regr.eval(test_data[,"inc"], pred7_test$pred)
cat("MAPE para k=7 es",round(regr.eval(test_data[,"inc"], pred7_test$pred)[4],4)*100,"%")

# Prueba del rendimiento del modelo final en los datos de evaluación
pred_eval = knn.reg(train = train_data[,ind_variables],test = eval_data[,ind_variables],
                    y = train_data$inc, k = 5)

cat("Error de las metricas de prueba para k=5")
regr.eval(eval_data[,"inc"], pred_eval$pred)
cat("MAPE for k=5 is",round(regr.eval(eval_data[,"inc"], pred_eval$pred)[4],4)*100,"%")

