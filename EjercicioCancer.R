
install.packages("data.table")

#-------------------------------------------------#
# Ejercicio Cancer
#-------------------------------------------------#

library(data.table)

getwd()
setwd("C:/Compensar")

str(X1cancer)
table(X1cancer$diagnosis)

typeof(X1cancer)
base_cancer = as.data.table(X1cancer)
typeof(base_cancer)

#Variable respuesta: benigno y maligno
base_cancer[, VAR_RTA := ifelse(diagnosis == "M", 1, 0)]
table(base_cancer$VAR_RTA)
table(base_cancer$diagnosis)
typeof(base_cancer$VAR_RTA)

base_cancer$VAR_RTA = as.factor(base_cancer$VAR_RTA)
str(base_cancer)

base_cancer[, VAR_RTA2 := ifelse(diagnosis == "M", 1, 0)]
freq = table(base_cancer$VAR_RTA2)

pct <- round(freq/sum(freq)*100)
lbls = c("Benigno", "Maligno")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 


pie(freq, main="Gráfica de Tipos de Cancer: Benigno / Maligno", 
    labels = lbls,
    col = c("dark blue", "blue"))


#Nombres
setnames(base_cancer, "concave points_se", "concave_points_se")
setnames(base_cancer, "concave points_worst", "concave_points_worst")
setnames(base_cancer, "concave points_mean", "concave_points_mean")


#Partición en train y test
dim(base_cancer)
pp = 0.3
s = sample(dim(base_cancer)[1],nrow(base_cancer)*pp)
s

train = base_cancer[-s,]
nrow(train)
dim(train)

test = base_cancer[s,]
nrow(test)
dim(test)

str(train)
name_vars = as.data.table(names(train))

#Prueba var rta
is.factor(base_cancer$VAR_RTA)
unique(base_cancer$VAR_RTA)
levels(base_cancer$VAR_RTA)

#Regresión logística
str(base_cancer)
#lr = glm(VAR_RTA~.-X33-diagnosis-id, data = train, family = "binomial")

lr = glm(VAR_RTA~
         radius_mean+texture_mean+perimeter_mean+area_mean+smoothness_mean+
         compactness_mean+concavity_mean+concave_points_mean+symmetry_mean+
         fractal_dimension_mean+
         radius_se+texture_se+perimeter_se+area_se+
         smoothness_se+compactness_se+concavity_se+concave_points_se+
         symmetry_se+fractal_dimension_se+
         radius_worst+texture_worst+perimeter_worst+
         area_worst+smoothness_worst+compactness_worst+concavity_worst+
         concave_points_worst+symmetry_worst+fractal_dimension_worst,
data = train, family = "binomial")
#Binomial es porque quiero que sea clasificación
summary(lr)

confint(lr) # 95% CI for the coefficients
exp(coef(lr)) # exponentiated coefficients
exp(confint(lr)) # 95% CI for exponentiated coefficients
predict(lr, type="response") # predicted values
residuals(lr, type="deviance") # residuals


#Otros modelos con pocas variables

lr1 = glm(VAR_RTA~
           radius_mean+texture_mean+perimeter_mean+area_mean+smoothness_mean+
           compactness_mean+concavity_mean+concave_points_mean+symmetry_mean+
           fractal_dimension_mean,
           data = train, family = "binomial")
summary(lr1)
confint(lr1) # 95% CI for the coefficients

lr2 = glm(VAR_RTA~
           radius_se+texture_se+perimeter_se+area_se+
           smoothness_se+compactness_se+concavity_se+concave_points_se+
           symmetry_se+fractal_dimension_se,
         data = train, family = "binomial")
summary(lr2)
confint(lr2) # 95% CI for the coefficients

lr3 = glm(VAR_RTA~
           radius_worst+texture_worst+perimeter_worst+
           area_worst+smoothness_worst+compactness_worst+concavity_worst+
           concave_points_worst+symmetry_worst+fractal_dimension_worst,
         data = train, family = "binomial")
summary(lr3)
confint(lr3) # 95% CI for the coefficients


lr4 = glm(VAR_RTA~
            radius_mean+texture_mean+perimeter_mean+area_mean+smoothness_mean+
            compactness_mean+concavity_mean+concave_points_mean+symmetry_mean+
            fractal_dimension_mean+
            radius_worst+texture_worst+perimeter_worst+
            area_worst+smoothness_worst+compactness_worst+concavity_worst+
            concave_points_worst+symmetry_worst+fractal_dimension_worst,
          data = train, family = "binomial")
summary(lr4)
confint(lr4) # 95% CI for the coefficients


extractAIC(lr)
extractAIC(lr1)
extractAIC(lr2)
extractAIC(lr3)
extractAIC(lr4)


#Califica la base de test con el modelo elegido

modelo_elegido = lr4

predi = predict(modelo_elegido, test, type="response")
thres = 0.5 #Threshold (límite) para clasificar ("Up" es 1 o positivo)
lr_predi = rep("benigno", nrow(test))
lr_predi[predi > thres] = "maligno"  #Prediccion en Down o Up


#Matriz de confusion (a pedal) 
table(lr_predi,test$VAR_RTA)
table(lr_predi)

#Curva ROC (usando pROC)
#install.packages("pROC")
library(pROC)
rc = roc(response = test$VAR_RTA, predictor = predi) #Puntos de curva ROC
auc(rc) #Area bajo la curva
plot(rc) #Grafica de curva ROC

#Gráfica la comparación entre la realidad y el modelo de predicción
tabla0 = table(test$VAR_RTA)
tabla = table(lr_predi)
tabla2 = table(lr_predi, test$VAR_RTA)

#Pinta las probabilidades 
par(mfrow = c(1,2))

pct <- round(tabla0/sum(tabla)*100)
lbls = c("Benigno", "Maligno")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
lbls <- paste(lbls, tabla0)

barplot(tabla0, col = c("darkblue","light blue"), 
        main = "Clasificación REAL del Test",
        #xlab = "Tipo de Cancer (0=Benigno, 1=Maligno)",
        xlab = lbls)


pct <- round(tabla/sum(tabla)*100)
lbls = c("Benigno", "Maligno")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
lbls <- paste(lbls, tabla)

barplot(tabla, col = c("dark blue","light blue"), 
        main = "Clasificación PREDICHA del Test",
        xlab = lbls)

par(mfrow = c(1,1))
