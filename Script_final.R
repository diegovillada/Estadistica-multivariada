library(tidyverse)
library(summarytools)
library(readxl)
library(caret)
library(VIM)
library(reshape2)
library(reshape)
library(fastDummies)
library(e1071)
library(ipred)
library(xgboost)
library(ROSE)
library(DMwR)





### Limpieza de la base ####
setwd("C:/Users/diavi/Dropbox/Academia/Maestría UC/Metodos estadísticos/Proyecto")
base_final <- read_excel("base final.xlsx")
base_final$`NIVEL EN CURSO`<-as.factor(base_final$`NIVEL EN CURSO`)
# view(dfSummary(base_final)) 

#Variable objetivo
base_final$Posbile_Desertor<-ifelse(
    base_final$`PROMEDIO SEMESTRAL`<3 & 
      base_final$`PROMEDIO ACUMULADO`<3,1,0)

base_final$Posbile_Desertor<-as.factor(base_final$Posbile_Desertor)
table(base_final$Posbile_Desertor) # Tabla de frecuencias de la variable objetivo

Base_Modelo<- base_final[,
    c(67,1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,24,31,32,33,34,35,44,49)]
Base_Modelo<-dummy_cols(.data = Base_Modelo,remove_first_dummy = TRUE)
Base_Modelo<-as.data.frame(Base_Modelo)
Base_Modelo$Posbile_Desertor_1 <-NULL

view(dfSummary(Base_Modelo))# Vista de resumen de las variables





set.seed(1234)
index <- createDataPartition(Base_Modelo$Posbile_Desertor, p = 0.7, list = FALSE)
train_data <- Base_Modelo[index, ]
test_data  <- Base_Modelo[-index, ]

#----------------------------------- Modelo 1 Random Forest ####
##### Modelo 1####
Modelo_ARBOL1 <- train(Posbile_Desertor~`DURACIÓN DE ANOS EN PLAN DE ESTUDIO`+
                         `CRÉDITOS REQUERIDOS DEL PLAN`+
                         `CRÉDITOS CURSADOS EN EL PLAN`+
                         `CRÉDITOS HOMOLOGADOS`+
                         `CRÉDITOS SUPERADOS EN EL PLAN`+
                         `CRÉDITOS NO SUPERADA EN EL PLAN`+
                         `CREDITOS PENDIENTES EN EL PLAN`+
                         `AVANCE`+
                         `ASIGNATURAS CURSADAS EN EL PLAN`+
                         `ASIGNATURA HOMOLOGADAS EN EL PLAN`+
                         `ASIGNATURA SUPERADA EN EL PLAN`+
                         `TOTAL ASIGNATURAS NO SUPERADAS`+
                         `PROMEDIO SEMESTRAL`+
                         `PROMEDIO ACUMULADO`+
                         `EDAD`+
                         `NIVEL EN CURSO_2`+
                         `NIVEL EN CURSO_3`+
                         `NIVEL EN CURSO_4`+
                         `NIVEL EN CURSO_5`+
                         `NIVEL EN CURSO_6`+
                         `NIVEL EN CURSO_7`+
                         `NIVEL EN CURSO_8`+
                         `NIVEL EN CURSO_9`+
                         `NIVEL EN CURSO_10`+
                         `TIPO DE DOCUMENTO_Cedula de Extranjería`+
                         `TIPO DE DOCUMENTO_Pasaporte`+
                         `TIPO DE DOCUMENTO_Tarjeta de Identidad`+
                         `CARRERA_ARTE DRAMÁTICO`+
                         `CARRERA_BIOLOGÍA`+
                         `CARRERA_CINE`+
                         `CARRERA_COMUNICACIÓN SOCIAL Y PERIODISMO`+
                         `CARRERA_CONTADURÍA PÚBLICA`+
                         `CARRERA_CREACIÓN LITERARIA`+
                         `CARRERA_DERECHO`+
                         `CARRERA_ECONOMÍA`+
                         `CARRERA_ESTUDIOS MUSICALES`+
                         `CARRERA_INGENIERÍA AMBIENTAL`+
                         `CARRERA_INGENIERÍA DE SISTEMAS`+
                         `CARRERA_INGENIERÍA ELECTRÓNICA`+
                         `CARRERA_INGENIERÍA INDUSTRIAL`+
                         `CARRERA_INGENIERÍA MECÁNICA`+
                         `CARRERA_MATEMÁTICAS`+
                         `CARRERA_MERCADOLOGÍA`+
                         `CARRERA_PUBLICIDAD`+
                         `CARRERA_TRABAJO SOCIAL`+
                         `GENERO_Masculino`+
                         `ESTADO CIVIL_Divorciado(a)`+
                         `ESTADO CIVIL_Madre soltera`+
                         `ESTADO CIVIL_Separado(a)`+
                         `ESTADO CIVIL_Sin Informacion`+
                         `ESTADO CIVIL_Soltero(a) (Nunca se ha casado ni ha vivido en unión libre)`+
                         `ESTADO CIVIL_Union Libre`+
                         `ESTADO CIVIL_Viudo(a)`+
                         `Requiere algún apoyo técnico_Sí`+
                         `Situación laboral_Dueño o socio`+
                         `Situación laboral_Empleado`+
                         `Situación laboral_Independiente`+
                         `Salario_1 a 2 SMMLV`+
                         `Salario_2 a 5 SMMLV`+
                         `Salario_5 a 10 SMMLV`+
                         `Salario_Más de 10 SMMLV`+
                         `Tipo de vivienda_Casa`+
                         `Tipo de vivienda_Casa lote`+
                         `Tipo de vivienda_Finca`+
                         `Tipo de vivienda_Habitación`+
                         `Tenencia de la vivienda_Cedida`+
                         `Tenencia de la vivienda_Familiar`+
                         `Tenencia de la vivienda_Propia con pagos pendientes`+
                         `Tenencia de la vivienda_Propia totalmente pagada`+
                         `Tenencia de la vivienda_Vivienda estudiantil`+
                         `Estrato_Estrato 2`+
                         `Estrato_Estrato 3`+
                         `Estrato_Estrato 4`+
                         `Estrato_Estrato 5`+
                         `Estrato_Estrato 6`,
                          data =train_data,
                          method="rf",
                          preProcess = c("scale", "center"),
                          trControl=trainControl(method ="boot",
                                                 number =4,
                                                 verboseIter = FALSE,
                                                 sampling = "down"))
## Resultados del modelo 1 #####
Modelo_ARBOL1$finalModel
varImp(Modelo_ARBOL1)
plot(varImp(Modelo_ARBOL1,scale = TRUE),10,main="Importancia variables modelo RandomForest")
pred_test_random_forest<-predict(object = Modelo_ARBOL1,newdata = test_data,type = "raw")
Matriz_confusion_test_RandonForest<-confusionMatrix(test_data$Posbile_Desertor,pred_test_random_forest)
Matriz_confusion_test_RandonForest$table
Matriz_confusion_test_RandonForest$overall 

roc.curve(test_data$Posbile_Desertor, pred_test_random_forest)

#----------------------------------- Modelo 2 RF ####
#### Modelo 2 #####
Modelo_ARBOL2 <- train(Posbile_Desertor~`DURACIÓN DE ANOS EN PLAN DE ESTUDIO`+
                         `CRÉDITOS REQUERIDOS DEL PLAN`+
                         `CRÉDITOS CURSADOS EN EL PLAN`+
                         `CRÉDITOS HOMOLOGADOS`+
                         `CRÉDITOS SUPERADOS EN EL PLAN`+
                         `CRÉDITOS NO SUPERADA EN EL PLAN`+
                         `CREDITOS PENDIENTES EN EL PLAN`+
                         `AVANCE`+
                         `ASIGNATURAS CURSADAS EN EL PLAN`+
                         `ASIGNATURA HOMOLOGADAS EN EL PLAN`+
                         `ASIGNATURA SUPERADA EN EL PLAN`+
                         `TOTAL ASIGNATURAS NO SUPERADAS`+
                         `EDAD`+
                         `NIVEL EN CURSO_2`+
                         `NIVEL EN CURSO_3`+
                         `NIVEL EN CURSO_4`+
                         `NIVEL EN CURSO_5`+
                         `NIVEL EN CURSO_6`+
                         `NIVEL EN CURSO_7`+
                         `NIVEL EN CURSO_8`+
                         `NIVEL EN CURSO_9`+
                         `NIVEL EN CURSO_10`+
                         `TIPO DE DOCUMENTO_Cedula de Extranjería`+
                         `TIPO DE DOCUMENTO_Pasaporte`+
                         `TIPO DE DOCUMENTO_Tarjeta de Identidad`+
                         `CARRERA_ARTE DRAMÁTICO`+
                         `CARRERA_BIOLOGÍA`+
                         `CARRERA_CINE`+
                         `CARRERA_COMUNICACIÓN SOCIAL Y PERIODISMO`+
                         `CARRERA_CONTADURÍA PÚBLICA`+
                         `CARRERA_CREACIÓN LITERARIA`+
                         `CARRERA_DERECHO`+
                         `CARRERA_ECONOMÍA`+
                         `CARRERA_ESTUDIOS MUSICALES`+
                         `CARRERA_INGENIERÍA AMBIENTAL`+
                         `CARRERA_INGENIERÍA DE SISTEMAS`+
                         `CARRERA_INGENIERÍA ELECTRÓNICA`+
                         `CARRERA_INGENIERÍA INDUSTRIAL`+
                         `CARRERA_INGENIERÍA MECÁNICA`+
                         `CARRERA_MATEMÁTICAS`+
                         `CARRERA_MERCADOLOGÍA`+
                         `CARRERA_PUBLICIDAD`+
                         `CARRERA_TRABAJO SOCIAL`+
                         `GENERO_Masculino`+
                         `ESTADO CIVIL_Divorciado(a)`+
                         `ESTADO CIVIL_Madre soltera`+
                         `ESTADO CIVIL_Separado(a)`+
                         `ESTADO CIVIL_Sin Informacion`+
                         `ESTADO CIVIL_Soltero(a) (Nunca se ha casado ni ha vivido en unión libre)`+
                         `ESTADO CIVIL_Union Libre`+
                         `ESTADO CIVIL_Viudo(a)`+
                         `Requiere algún apoyo técnico_Sí`+
                         `Situación laboral_Dueño o socio`+
                         `Situación laboral_Empleado`+
                         `Situación laboral_Independiente`+
                         `Salario_1 a 2 SMMLV`+
                         `Salario_2 a 5 SMMLV`+
                         `Salario_5 a 10 SMMLV`+
                         `Salario_Más de 10 SMMLV`+
                         `Tipo de vivienda_Casa`+
                         `Tipo de vivienda_Casa lote`+
                         `Tipo de vivienda_Finca`+
                         `Tipo de vivienda_Habitación`+
                         `Tenencia de la vivienda_Cedida`+
                         `Tenencia de la vivienda_Familiar`+
                         `Tenencia de la vivienda_Propia con pagos pendientes`+
                         `Tenencia de la vivienda_Propia totalmente pagada`+
                         `Tenencia de la vivienda_Vivienda estudiantil`+
                         `Estrato_Estrato 2`+
                         `Estrato_Estrato 3`+
                         `Estrato_Estrato 4`+
                         `Estrato_Estrato 5`+
                         `Estrato_Estrato 6`,
                       data =train_data,
                       method="rf",
                       preProcess = c("scale", "center"),
                       trControl=trainControl(method ="boot",
                                              number =4,
                                              verboseIter = FALSE,
                                              sampling = "down"))
### Resultados modelo 2 ####
Modelo_ARBOL2$finalModel
varImp(Modelo_ARBOL2)
plot(varImp(Modelo_ARBOL2,scale = TRUE),10,main="Importancia variables modelo RandomForest")
pred_test_random_forest2<-predict(object = Modelo_ARBOL2,newdata = test_data,type = "raw")
Matriz_confusion_test_RandonForest2<-confusionMatrix(test_data$Posbile_Desertor,pred_test_random_forest2)
Matriz_confusion_test_RandonForest2$table
Matriz_confusion_test_RandonForest2$overall 

roc.curve(test_data$Posbile_Desertor, pred_test_random_forest2)












#--------------------------------  Modelo 1 SVM ####

### Modelo 1 ####
Modelo_SVM<-svm(Posbile_Desertor~`DURACIÓN DE ANOS EN PLAN DE ESTUDIO`+
                  `CRÉDITOS REQUERIDOS DEL PLAN`+
                  `CRÉDITOS CURSADOS EN EL PLAN`+
                  `CRÉDITOS HOMOLOGADOS`+
                  `CRÉDITOS SUPERADOS EN EL PLAN`+
                  `CRÉDITOS NO SUPERADA EN EL PLAN`+
                  `CREDITOS PENDIENTES EN EL PLAN`+
                  `AVANCE`+
                  `ASIGNATURAS CURSADAS EN EL PLAN`+
                  `ASIGNATURA HOMOLOGADAS EN EL PLAN`+
                  `ASIGNATURA SUPERADA EN EL PLAN`+
                  `TOTAL ASIGNATURAS NO SUPERADAS`+
                  `PROMEDIO SEMESTRAL`+
                  `PROMEDIO ACUMULADO`+
                  `EDAD`+
                  `NIVEL EN CURSO_2`+
                  `NIVEL EN CURSO_3`+
                  `NIVEL EN CURSO_4`+
                  `NIVEL EN CURSO_5`+
                  `NIVEL EN CURSO_6`+
                  `NIVEL EN CURSO_7`+
                  `NIVEL EN CURSO_8`+
                  `NIVEL EN CURSO_9`+
                  `NIVEL EN CURSO_10`+
                  `TIPO DE DOCUMENTO_Cedula de Extranjería`+
                  `TIPO DE DOCUMENTO_Pasaporte`+
                  `TIPO DE DOCUMENTO_Tarjeta de Identidad`+
                  `CARRERA_ARTE DRAMÁTICO`+
                  `CARRERA_BIOLOGÍA`+
                  `CARRERA_CINE`+
                  `CARRERA_COMUNICACIÓN SOCIAL Y PERIODISMO`+
                  `CARRERA_CONTADURÍA PÚBLICA`+
                  `CARRERA_CREACIÓN LITERARIA`+
                  `CARRERA_DERECHO`+
                  `CARRERA_ECONOMÍA`+
                  `CARRERA_ESTUDIOS MUSICALES`+
                  `CARRERA_INGENIERÍA AMBIENTAL`+
                  `CARRERA_INGENIERÍA DE SISTEMAS`+
                  `CARRERA_INGENIERÍA ELECTRÓNICA`+
                  `CARRERA_INGENIERÍA INDUSTRIAL`+
                  `CARRERA_INGENIERÍA MECÁNICA`+
                  `CARRERA_MATEMÁTICAS`+
                  `CARRERA_MERCADOLOGÍA`+
                  `CARRERA_PUBLICIDAD`+
                  `CARRERA_TRABAJO SOCIAL`+
                  `GENERO_Masculino`+
                  `ESTADO CIVIL_Divorciado(a)`+
                  `ESTADO CIVIL_Madre soltera`+
                  `ESTADO CIVIL_Separado(a)`+
                  `ESTADO CIVIL_Sin Informacion`+
                  `ESTADO CIVIL_Soltero(a) (Nunca se ha casado ni ha vivido en unión libre)`+
                  `ESTADO CIVIL_Union Libre`+
                  `ESTADO CIVIL_Viudo(a)`+
                  `Requiere algún apoyo técnico_Sí`+
                  `Situación laboral_Dueño o socio`+
                  `Situación laboral_Empleado`+
                  `Situación laboral_Independiente`+
                  `Salario_1 a 2 SMMLV`+
                  `Salario_2 a 5 SMMLV`+
                  `Salario_5 a 10 SMMLV`+
                  `Salario_Más de 10 SMMLV`+
                  # `Zona de procedencia   _Urbana (Centro poblado, municipio o ciudad)`+
                  `Tipo de vivienda_Casa`+
                  `Tipo de vivienda_Casa lote`+
                  `Tipo de vivienda_Finca`+
                  `Tipo de vivienda_Habitación`+
                  `Tenencia de la vivienda_Cedida`+
                  `Tenencia de la vivienda_Familiar`+
                  `Tenencia de la vivienda_Propia con pagos pendientes`+
                  `Tenencia de la vivienda_Propia totalmente pagada`+
                  `Tenencia de la vivienda_Vivienda estudiantil`+
                  `Estrato_Estrato 2`+
                  `Estrato_Estrato 3`+
                  `Estrato_Estrato 4`+
                  `Estrato_Estrato 5`+
                  `Estrato_Estrato 6`,
                data =train_data,probability = TRUE)
summary(Modelo_SVM)

### Resultados del modelo 1 svm #####
Modelo_SVM_pred_train<-predict(newdata = train_data,
                               object =Modelo_SVM,type = "class" )
confusionMatrix(train_data$Posbile_Desertor,Modelo_SVM_pred_train)

Modelo_SVM_pred_test<-predict(newdata = test_data,object =Modelo_SVM,type = "class" )
confusionMatrix(test_data$Posbile_Desertor,Modelo_SVM_pred_test)

roc.curve(test_data$Posbile_Desertor, Modelo_SVM_pred_test)

#-------------------------------  Modelo SVM 2 ####
### Modelo 2 svm ####

Modelo_SVM2<-svm(Posbile_Desertor~`DURACIÓN DE ANOS EN PLAN DE ESTUDIO`+
                  `CRÉDITOS REQUERIDOS DEL PLAN`+
                  `CRÉDITOS CURSADOS EN EL PLAN`+
                  `CRÉDITOS HOMOLOGADOS`+
                  `CRÉDITOS SUPERADOS EN EL PLAN`+
                  `CRÉDITOS NO SUPERADA EN EL PLAN`+
                  `CREDITOS PENDIENTES EN EL PLAN`+
                  `AVANCE`+
                  `ASIGNATURAS CURSADAS EN EL PLAN`+
                  `ASIGNATURA HOMOLOGADAS EN EL PLAN`+
                  `ASIGNATURA SUPERADA EN EL PLAN`+
                  `TOTAL ASIGNATURAS NO SUPERADAS`+
                  `EDAD`+
                  `NIVEL EN CURSO_2`+
                  `NIVEL EN CURSO_3`+
                  `NIVEL EN CURSO_4`+
                  `NIVEL EN CURSO_5`+
                  `NIVEL EN CURSO_6`+
                  `NIVEL EN CURSO_7`+
                  `NIVEL EN CURSO_8`+
                  `NIVEL EN CURSO_9`+
                  `NIVEL EN CURSO_10`+
                  `TIPO DE DOCUMENTO_Cedula de Extranjería`+
                  `TIPO DE DOCUMENTO_Pasaporte`+
                  `TIPO DE DOCUMENTO_Tarjeta de Identidad`+
                  `CARRERA_ARTE DRAMÁTICO`+
                  `CARRERA_BIOLOGÍA`+
                  `CARRERA_CINE`+
                  `CARRERA_COMUNICACIÓN SOCIAL Y PERIODISMO`+
                  `CARRERA_CONTADURÍA PÚBLICA`+
                  `CARRERA_CREACIÓN LITERARIA`+
                  `CARRERA_DERECHO`+
                  `CARRERA_ECONOMÍA`+
                  `CARRERA_ESTUDIOS MUSICALES`+
                  `CARRERA_INGENIERÍA AMBIENTAL`+
                  `CARRERA_INGENIERÍA DE SISTEMAS`+
                  `CARRERA_INGENIERÍA ELECTRÓNICA`+
                  `CARRERA_INGENIERÍA INDUSTRIAL`+
                  `CARRERA_INGENIERÍA MECÁNICA`+
                  `CARRERA_MATEMÁTICAS`+
                  `CARRERA_MERCADOLOGÍA`+
                  `CARRERA_PUBLICIDAD`+
                  `CARRERA_TRABAJO SOCIAL`+
                  `GENERO_Masculino`+
                  `ESTADO CIVIL_Divorciado(a)`+
                  `ESTADO CIVIL_Madre soltera`+
                  `ESTADO CIVIL_Separado(a)`+
                  `ESTADO CIVIL_Sin Informacion`+
                  `ESTADO CIVIL_Soltero(a) (Nunca se ha casado ni ha vivido en unión libre)`+
                  `ESTADO CIVIL_Union Libre`+
                  `ESTADO CIVIL_Viudo(a)`+
                  `Requiere algún apoyo técnico_Sí`+
                  `Situación laboral_Dueño o socio`+
                  `Situación laboral_Empleado`+
                  `Situación laboral_Independiente`+
                  `Salario_1 a 2 SMMLV`+
                  `Salario_2 a 5 SMMLV`+
                  `Salario_5 a 10 SMMLV`+
                  `Salario_Más de 10 SMMLV`+
                  # `Zona de procedencia   _Urbana (Centro poblado, municipio o ciudad)`+
                  `Tipo de vivienda_Casa`+
                  `Tipo de vivienda_Casa lote`+
                  `Tipo de vivienda_Finca`+
                  `Tipo de vivienda_Habitación`+
                  `Tenencia de la vivienda_Cedida`+
                  `Tenencia de la vivienda_Familiar`+
                  `Tenencia de la vivienda_Propia con pagos pendientes`+
                  `Tenencia de la vivienda_Propia totalmente pagada`+
                  `Tenencia de la vivienda_Vivienda estudiantil`+
                  `Estrato_Estrato 2`+
                  `Estrato_Estrato 3`+
                  `Estrato_Estrato 4`+
                  `Estrato_Estrato 5`+
                  `Estrato_Estrato 6`,
                data =train_data,probability = TRUE)

### Resultados Modelo 2 svm ####
summary(Modelo_SVM2)
Modelo_SVM2_pred_train<-predict(newdata = train_data,
                               object =Modelo_SVM2,type = "class" )
confusionMatrix(train_data$Posbile_Desertor,Modelo_SVM2_pred_train)

Modelo_SVM2_pred_test<-predict(newdata = test_data,object =Modelo_SVM2,type = "class" )
confusionMatrix(test_data$Posbile_Desertor,Modelo_SVM2_pred_test)

roc.curve(test_data$Posbile_Desertor, Modelo_SVM2_pred_test)











#------------------------------- Modelo Regresión Logística 1 ####
Modelo_Logistico <- glm(
  Posbile_Desertor~.,
  data = train_data,family = binomial)

# Resultados RL 1 #####
summary(Modelo_Logistico)

Pred_Logistica_Train<-predict(object = Modelo_Logistico,newdata = train_data,
                              type = "response" )
Pred_Logistica_Train<-ifelse(Pred_Logistica_Train>0.2,1,0)
Pred_Logistica_Train<-as.factor(Pred_Logistica_Train)
confusionMatrix(train_data$Posbile_Desertor,Pred_Logistica_Train)$table
confusionMatrix(train_data$Posbile_Desertor,Pred_Logistica_Train)$overall



Pred_Logistica_Test<-predict(object = Modelo_Logistico,newdata = test_data,
                              type = "response" )
Pred_Logistica_Test<-ifelse(Pred_Logistica_Test>0.2,1,0)
Pred_Logistica_Test<-as.factor(Pred_Logistica_Test)
confusionMatrix(test_data$Posbile_Desertor,Pred_Logistica_Test)$table
confusionMatrix(test_data$Posbile_Desertor,Pred_Logistica_Test)$overall

roc.curve(test_data$Posbile_Desertor, Pred_Logistica_Test)


#------------------------------- Modelo Regresión Logística 2 ####
Modelo_Logistico2 <- glm(
  Posbile_Desertor~`DURACIÓN DE ANOS EN PLAN DE ESTUDIO`+
    `CRÉDITOS REQUERIDOS DEL PLAN`+
    `CRÉDITOS CURSADOS EN EL PLAN`+
    `CRÉDITOS HOMOLOGADOS`+
    `CRÉDITOS SUPERADOS EN EL PLAN`+
    `CRÉDITOS NO SUPERADA EN EL PLAN`+
    `CREDITOS PENDIENTES EN EL PLAN`+
    `AVANCE`+
    `ASIGNATURAS CURSADAS EN EL PLAN`+
    `ASIGNATURA HOMOLOGADAS EN EL PLAN`+
    `ASIGNATURA SUPERADA EN EL PLAN`+
    `TOTAL ASIGNATURAS NO SUPERADAS`+
    `EDAD`+
    `NIVEL EN CURSO_2`+
    `NIVEL EN CURSO_3`+
    `NIVEL EN CURSO_4`+
    `NIVEL EN CURSO_5`+
    `NIVEL EN CURSO_6`+
    `NIVEL EN CURSO_7`+
    `NIVEL EN CURSO_8`+
    `NIVEL EN CURSO_9`+
    `NIVEL EN CURSO_10`+
    `TIPO DE DOCUMENTO_Cedula de Extranjería`+
    `TIPO DE DOCUMENTO_Pasaporte`+
    `TIPO DE DOCUMENTO_Tarjeta de Identidad`+
    `CARRERA_ARTE DRAMÁTICO`+
    `CARRERA_BIOLOGÍA`+
    `CARRERA_CINE`+
    `CARRERA_COMUNICACIÓN SOCIAL Y PERIODISMO`+
    `CARRERA_CONTADURÍA PÚBLICA`+
    `CARRERA_CREACIÓN LITERARIA`+
    `CARRERA_DERECHO`+
    `CARRERA_ECONOMÍA`+
    `CARRERA_ESTUDIOS MUSICALES`+
    `CARRERA_INGENIERÍA AMBIENTAL`+
    `CARRERA_INGENIERÍA DE SISTEMAS`+
    `CARRERA_INGENIERÍA ELECTRÓNICA`+
    `CARRERA_INGENIERÍA INDUSTRIAL`+
    `CARRERA_INGENIERÍA MECÁNICA`+
    `CARRERA_MATEMÁTICAS`+
    `CARRERA_MERCADOLOGÍA`+
    `CARRERA_PUBLICIDAD`+
    `CARRERA_TRABAJO SOCIAL`+
    `GENERO_Masculino`+
    `ESTADO CIVIL_Divorciado(a)`+
    `ESTADO CIVIL_Madre soltera`+
    `ESTADO CIVIL_Separado(a)`+
    `ESTADO CIVIL_Sin Informacion`+
    `ESTADO CIVIL_Soltero(a) (Nunca se ha casado ni ha vivido en unión libre)`+
    `ESTADO CIVIL_Union Libre`+
    `ESTADO CIVIL_Viudo(a)`+
    `Requiere algún apoyo técnico_Sí`+
    `Situación laboral_Dueño o socio`+
    `Situación laboral_Empleado`+
    `Situación laboral_Independiente`+
    `Salario_1 a 2 SMMLV`+
    `Salario_2 a 5 SMMLV`+
    `Salario_5 a 10 SMMLV`+
    `Salario_Más de 10 SMMLV`+
    `Tipo de vivienda_Casa`+
    `Tipo de vivienda_Casa lote`+
    `Tipo de vivienda_Finca`+
    `Tipo de vivienda_Habitación`+
    `Tenencia de la vivienda_Cedida`+
    `Tenencia de la vivienda_Familiar`+
    `Tenencia de la vivienda_Propia con pagos pendientes`+
    `Tenencia de la vivienda_Propia totalmente pagada`+
    `Tenencia de la vivienda_Vivienda estudiantil`+
    `Estrato_Estrato 2`+
    `Estrato_Estrato 3`+
    `Estrato_Estrato 4`+
    `Estrato_Estrato 5`+
    `Estrato_Estrato 6`,
  data = train_data,family = binomial)
### Modelo 2 Ajustado ####
Modelo_Logistico2 <- glm(
  Posbile_Desertor~
    `CRÉDITOS CURSADOS EN EL PLAN`+
    `CRÉDITOS HOMOLOGADOS`+
    `CRÉDITOS NO SUPERADA EN EL PLAN`+
    `ASIGNATURAS CURSADAS EN EL PLAN`+
    `ASIGNATURA HOMOLOGADAS EN EL PLAN`+
    `ASIGNATURA SUPERADA EN EL PLAN`+
    `EDAD`+
    `NIVEL EN CURSO_4`+
    `NIVEL EN CURSO_5`+
    `NIVEL EN CURSO_6`+
    `NIVEL EN CURSO_7`+
    `NIVEL EN CURSO_8`+
    `NIVEL EN CURSO_9`+
    `NIVEL EN CURSO_10`+
    `CARRERA_COMUNICACIÓN SOCIAL Y PERIODISMO`+
    `CARRERA_CONTADURÍA PÚBLICA`+
    `CARRERA_DERECHO`+
    `Estrato_Estrato 5`,
  data = train_data,family = binomial)

### Resultados modelo RL2 ####
summary(Modelo_Logistico2)

Pred_Logistica_Train2<-predict(object = Modelo_Logistico2,newdata = train_data,
                              type = "response" )
Pred_Logistica_Train2<-ifelse(Pred_Logistica_Train2>0.2,1,0)
Pred_Logistica_Train2<-as.factor(Pred_Logistica_Train2)
confusionMatrix(train_data$Posbile_Desertor,Pred_Logistica_Train2)$table
confusionMatrix(train_data$Posbile_Desertor,Pred_Logistica_Train2)$overall



Pred_Logistica_Test2<-predict(object = Modelo_Logistico2,newdata = test_data,
                             type = "response" )
Pred_Logistica_Test2<-ifelse(Pred_Logistica_Test2>0.2,1,0)
Pred_Logistica_Test2<-as.factor(Pred_Logistica_Test2)
confusionMatrix(test_data$Posbile_Desertor,Pred_Logistica_Test2)$table
confusionMatrix(test_data$Posbile_Desertor,Pred_Logistica_Test2)$overall

roc.curve(test_data$Posbile_Desertor, Pred_Logistica_Test2)







#Predición por Regresión Logística ####
Base_Modelo$Probabilidad_Logistica<-predict(Modelo_Logistico,
                                   newdata = Base_Modelo,type = "response")
Base_Modelo$Clasificacion_Logistica<-ifelse(Base_Modelo$Probabilidad_Logistica>0.2,
                                            "Posible Desertor","No Desertor")

Base_Exportar<-left_join(base_final,Base_Modelo[,c(1,91,92)])
Base_Exportar$Posbile_Desertor<-ifelse(Base_Exportar==0,"No Desertor",
                                       "Posible Desertor")
Base_Exportar$Probabilidad_Logistica<-as.numeric(Base_Exportar$Probabilidad_Logistica)


#install.packages("writexl")
library(writexl)
write_xlsx("D:/DEMOS/Proyecto fin curso DP/base_TABLEAU.xlsx",
           x =Base_Exportar )

