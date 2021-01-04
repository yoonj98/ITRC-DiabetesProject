install.packages('xgboost')
install.packages('Ckmeans.1d.dp')

library(xgboost)
library(dplyr)
library(ggplot2)

xg_data = NA_data
xg_data=subset(xg_data, select=c(age, HE_wc, HE_ht, HE_BMI, HE_HbA1c,
                                 HE_chol, HE_TG, HE_sbp, HE_alt, HE_HB, HE_HCT, HE_WBC, HE_RBC, HE_Bplt, HE_UNa, HE_DM))

x =subset(xg_data, select=-c(HE_DM))
x_mat = as.matrix(as.data.frame(lapply(x, as.numeric)))
str(x_mat)

y = subset(xg_data, select=c(HE_DM))
y_mat=as.matrix(y)
y_mat=as.numeric(y_mat)
y_vec = as.vector(y_mat)
label=y_vec-1

#####
#Label conversion
HE_DM = xg_data$HE_DM
label = as.integer(xg_data$HE_DM)-1
xg_data$HE_DM=NULL

#Split the data for training and testing (75/25 split)
n = nrow(xg_data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(xg_data[train.index,])
train.label = label[train.index]
test.data = as.matrix(xg_data[-train.index,])
test.label = label[-train.index]

#Create the xgb.DMatrix objects
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

#Define the main parameters
num_class = length(levels(HE_DM))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

#Train the model
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=8000,
  n_jobs=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

xgb.fit

#Predict new outcomes
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(HE_DM)

#Identify the class with the highest probability for each prediction
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(HE_DM)[test.label+1]

#How accurate are the predictions?
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))

#variable importance
xgboost::xgb.ggplot.importance(xgb.importance(feature_names = colnames(x_mat), model=xgb.fit))

#type of variable importance?
imp=xgb.importance(model=xgb.fit)
data.frame(variable=rep(imp$Feature, 3), value=c(imp$Gain, imp$Cover, imp$Frequency), 
           Type=c(rep('Gain', nrow(imp)), rep('Cover', nrow(imp)), rep('Frequency', nrow(imp)))
) %>% ggplot(aes(variable, value, fill=variable))+
  geom_bar(stat='identity')+
  facet_grid(~Type)+
  theme_bw()+
  ggtitle('XGBoost : Customized Importance Plot')


table(xgb.pred$prediction, xgb.pred$label)

xg_pred=factor(xgb.pred$prediction)
xg_actual=factor(xgb.pred$label)

xg_conf=confusionMatrix(xg_pred, xg_actual)

