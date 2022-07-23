library(dplyr)
library(data.table)

train <- fread("train_data.csv")
#test <- fread("test_data.csv")
data_labels <- fread("train_labels.csv")

#NUMERO DE LINHAS DA BASE
#NUMERO DE CLIENTES
uniqueN(train$customer_ID)
#INTERVALO DE DATAS DA BASE
max(train$S_2)
min(train$S_2)
#


#######################
#TRATANDO DADOS FALTANTES
#Porcentagem de valores NAS em cada coluna
NAS <- round(colSums(is.na(train))*100/nrow(train), 2)
#VER TODOS NAS
NAS
#REMOVENDO COLUNAS COM MAIS DE 25% DE DADOS MISSING
colunas <- names(NAS[NAS>25])
train<- train %>% select(-colunas)
train <- na.omit(train)
train <- left_join(train, data_labels, by="customer_ID")


library(caret)
#Criando matriz com linhas dos dados de treino - 70%
set.seed(1)
filtro <- createDataPartition(y=train$target, p=0.1, list=FALSE)



BD <- train[1:1000,]
BD <- left_join(BD, data_labels, by= "customer_ID")





BD$D_63 <- NULL
BD$D_64 <- NULL
BD$D_73 <- NULL
BD$D_87 <- NULL
BD$D_88 <- NULL
BD$D_110 <- NULL
BD$D_111 <- NULL
BD$B_39 <- NULL

modelo <- lm(target ~ ., data = BD[,3:183])

modelo <- lm(target ~ B_2, data = BD)

modelo <- lm(target ~ ., data = train_df)




modelo <- lm(target ~ B_2 + B_18 + D_45 + B_33 + D_47 + D_51 + D_54 + S_8 + B_32 + S_15 + R_3 + R_2 + R_4 + D_74 + D_75 + B_3 + B_40, data = BD)
modelo <- lm(target ~ B_2 + B_18 + D_45 + B_33 + D_47 + D_51 + D_54 + S_8 + B_32 + S_15 + R_3 + R_2 + D_74 + D_75 + B_40, data = BD)
modelo <- lm(target ~ B_2 + B_18 + D_45 + B_33 + D_47 + D_54 + S_8 + B_32 + S_15 + R_3 + R_2 + D_74 + D_75 + B_40, data = BD)


summary(modelo)

modelo <- lm(target ~ B_2 + B_18 + D_45 + B_33 + D_47 + D_54 + S_8 + B_32 + S_15 + R_3 + R_2 + D_74 + D_75 + B_40, data = BD)



df_pca$target <- train_df$target
#Criando o modelo
set.seed(1)
modelo <- train(target ~ ., data=df_pca, method="glmnet", tuneLenght=4, trControl = trainControl(method="cv", number = 2))


modelo <- train(target ~ B_2 + B_18 + D_45 + B_33 + D_47 + D_54 + S_8 + B_32 + S_15 + R_3 + R_2 + D_74 + D_75 + B_40, data=train_df, method="glmnet", tuneLenght=4, trControl = trainControl(method="cv", number = 2))



#Precisao no modelo de treino
mean(modelo$resample$Rsquared)

#pca sem sentido (TESTE)
pca <- prcomp(train_df[,-(1)], rank=10 ,  scale = TRUE)
df_pca <- as.data.frame(pca$x)
summary(pca)











#######################
#TRATANDO DADOS FALTANTES
#Porcentagem de valores NAS em cada coluna
NAS <- round(colSums(is.na(BD))*100/nrow(BD), 2)
#VER TODOS NAS
NAS
#REMOVENDO COLUNAS COM MAIS DE 25% DE DADOS MISSING
colunas <- names(NAS[NAS>25])
BD<- BD %>% select(-colunas)

#Porcentagem de valores NAS em cada coluna
NAS <- round(colSums(is.na(BD))*100/nrow(BD), 2)
#VER TODOS NAS
NAS[NAS>0]


BD_MISSING <- data.frame(NAS[NAS>0])
BD_MISSING$COLUNA <- row.names(BD_MISSING)




BD_cor <- BD
BD_cor$D_63 <- NULL
BD_cor$D_64 <- NULL
#MODELO GLMNET COM BAIXA CORRELACAO DA VARIAVEL TARGET
library(corrplot)
correlations <- cor(BD_cor[,3:189])
correlations_target <- as.matrix(sort(correlations[,'target'], decreasing = TRUE))
correlated<- names(which(apply(correlations_target, 1, function(x) ((x > 0.2) | (x < -0.2)))))
train_df<-BD %>% select(correlated)
anyNA(train_df)

library(caret)
#Criando o modelo
set.seed(1)
modelo <- train(target ~ ., data=train_df, method="glmnet", tuneLenght=4, trControl = trainControl(method="cv", number = 2))
#Precisao no modelo de treino
mean(modelo$resample$Rsquared)

#Prevendo dados no modelo de teste
Prev <- predict(modelo, test)
#View(data.frame(teste$Diagnosis, Prev))

library(gmodels)
#Visualizando matriz de confusao
CrossTable(Prev, test$Diagnosis, dnn = c("Previsto", "Real"), prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE)

#Caret
#Verificando acuracia
confusionMatrix(Prev, test$Diagnosis, dnn = c("Previsto", "Real"))



