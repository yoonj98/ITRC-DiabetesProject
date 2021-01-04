rawdata = read.csv('C:/Users/tldus/Desktop/ITRC_개별연구/first_data.csv')
str(rawdata)

require(VIM)
aggr(rawdata,prop=FALSE,numbers=TRUE)

#factor형으로 변환
rawdata$sex<-as.factor(rawdata$sex)
rawdata$HE_DM<-as.factor(rawdata$HE_DM)
rawdata$HE_HP<-as.factor(rawdata$HE_HP)
rawdata$HE_HCHOL<-as.factor(rawdata$HE_HCHOL)
rawdata$HE_HTG<-as.factor(rawdata$HE_HTG)

str(rawdata)
summary(rawdata)

rawdataa=rawdata[,1:27]

rawdata1=rawdataa
rawdata1$NA_num<-apply(rawdataa,1,function(x) sum(is.na(x)))

rawdata2=rawdata1[which(rawdata1$NA_num<10),] #응답자 별 미응답변수 10개 이상시 데이터(행) 제거 - max : 28
aggr(rawdata2,prop=FALSE,numbers=TRUE)

raww=rawdata1[which(rawdata1$NA_num>0),]
hist(raww$NA_num,freq=FALSE)

summary(rawdata2)

rawdata3=rawdata2[,1:26]  #종속변수 뻄
Y =rawdata2[,27]  #종속변수

summary(rawdata3)
aggr(rawdata3,prop=FALSE,numbers=TRUE)

rawdata_Ukal = rawdata3[,1:25]  #HE_Ukal 변수의 missing data 수가 700 넘으므로 제거
aggr(rawdata_Ukal,prop=FALSE,numbers=TRUE)

#########################
install.packages('mice')
library(mice)

#Ukal 제외
raw.imp=mice(rawdata_Ukal, m=5, maxit=50, seed=500)
summary(raw.imp)

miceraw=complete(raw.imp)
miceraw=cbind(miceraw,Y)

aggr(miceraw,prop=FALSE,numbers=TRUE)

write.csv(miceraw,'C:/Users/tldus/Desktop/ITRC_개별연구/mice_data.csv')

