library(randomForest) # 랜덤 포레스트 모델1
library(RRF)  # 랜덤 포레스트 모델2
library(e1071) # 모델 튜닝
library(ROCR) # 모델 평가
library(caret) # 특징 선택
library(tidymodels)
library(evaluate)
library(rsample)
library(dplyr)


train=initial_split(NA_data, prop=0.7)  #train/test data
training=training(train)  #training data
testing=testing(train)  #testing data

rf_data = NA_data
rf_data=subset(rf_data, select=c(age, HE_wc, HE_ht, HE_BMI, HE_HbA1c,
                                 HE_chol, HE_TG, HE_sbp, HE_alt, HE_HB, HE_HCT, HE_WBC, HE_RBC, HE_Bplt, HE_UNa, HE_DM))

sectrain <- initial_split(rf_data, prop=0.7)
sectraining <- training(sectrain)
sectesting <- testing(sectrain)

formula.init <- "HE_DM ~ ."
formula.init <- as.formula(formula.init)
rf.model <- randomForest(formula=formula.init, data=sectraining, importance=T, proximity=T)  
#fulll model
print(rf.model)

rf.predictions <- predict(rf.model, sectesting, type="class")  #예측값
rf_conf = confusionMatrix(rf.predictions, sectesting$HE_DM)  #예측값과 검정 데이터 


#변수 중요도
randomForest::importance(rf.model, type=1)
randomForest::varImpPlot(rf.model, type=1)

secconf=table(sectesting$HE_DM,rf.predictions)
sum(secconf[row(secconf) == col(secconf)])/sum(secconf) #정분류율
1-sum(secconf[row(secconf) == col(secconf)])/sum(secconf) #오분류율


###
rrf.model <- RRF(HE_DM~ ., data=sectraining, type="classification", importance=TRUE)
sec.rrf.predictions <- predict(rrf.model, sectesting)
rrf_conf=confusionMatrix(data = sec.rrf.predictions, sectesting$HE_DM)

RRF::importance(rrf.model)
RRF::varImpPlot(rrf.model)-str(rrf.model)


###
create_rfplot <- function(rf, type){
  
  imp <- importance(rf, type = type, scale = F)
  featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[,1])
  
  p <- ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "green", width = 0.65) +
    coord_flip() + 
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 15, color = "black"),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(size = 15, color = "black"),
          axis.text.y  = element_text(size = 15, color = "black")) 
  return(p)
}


create_rfplot(rrf.model, type=2)


(1878+331+379)/(190+18+615+65+53+74+1878+331+379)









