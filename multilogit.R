miceraw = read.csv('C:/Users/tldus/Desktop/ITRC_개별연구/mice_data.csv')

miceraw$sex<-as.factor(miceraw$sex)
miceraw$HE_DM<-as.factor(miceraw$HE_DM)
miceraw$HE_HP<-as.factor(miceraw$HE_HP)
miceraw$HE_HCHOL<-as.factor(miceraw$HE_HCHOL)
miceraw$HE_HTG<-as.factor(miceraw$HE_HTG)

str(miceraw)

data=miceraw[,2:27] #HE_DM만 종속변수로 넣음
NA_data=na.omit(data) #HE_DM이 NA인 행 제외
NA_int=subset(NA_data, select=-c(sex,HE_glu, HE_DM))  #상관계수 도출을 위해 범주형 제외

install.packages('corrplot')
library(corrplot)

cor=cor(NA_int)
corrplot(cor, method="number")  

NA_data=subset(NA_data, select=-c(HE_glu))  #정확도에 차이 없음 >> 종속변수에 직접적으로 관련된 HE_glu만 제외

library(VGAM)

str(NA_data)


#비례오즈 O
fit=vglm(HE_DM~., family=sratio(parallel=T), data=NA_data)
summary(fit)

#비례오즈 X
fit_p=vglm(HE_DM~., family=sratio(parallel=F), data=NA_data)
summary(fit_p)

lrtest(fit_p, fit)   #pvalue<0.05 - 비례오즈 사용 불가


fit1=step4(fit_p)  #stepwise
summary(fit1)

fit2=step4vglm(fit_p, direction="backward")  #backward
summary(fit2)

fit3=step4vglm(fit_p, direction="both")  #both
summary(fit3)

#stepwise = backward = both model

fit4=step4vglm(fit_p, direction="forward")  #forward
summary(fit4)

lrtest(fit4, fit1)  #pvalue = 0.3789 > 0.05 이므로 fit1 채택

# make predictions
probabilities <- predict(fit1, NA_data, type="response") #사후확률
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(NA_data$HE_DM)[1]
predictions[which(predictions=="2")] <- levels(NA_data$HE_DM)[2]
predictions[which(predictions=="3")] <- levels(NA_data$HE_DM)[3]


# summarize accuracy
table(predictions, NA_data$HE_DM)
data.frame(NA_data$HE_DM, predictions)

#정분류율
(2663+601+539)/(745+46+311+136+23+82+2663+601+539)  #0.739
