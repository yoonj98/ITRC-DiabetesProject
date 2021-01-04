#dot plot
Vis_DF <- data.frame(Model=c(rep("RF",5), rep("XG",5)), Measure=rep(c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV"), 2), Value=NA)   # 그래프를 그리기 위한 데이터 프레임을 만듭니다.
Vis_DF$Value[1] <- rf_conf$overall[1]   # random forest의 accuracy를 입력합니다.
Vis_DF$Value[2] <- rf_conf$byClass[1]   # random forest의 sensitivity를 입력합니다.
Vis_DF$Value[3] <- rf_conf$byClass[2]   # random forest의 speicificity를 입력합니다.
Vis_DF$Value[4] <- rf_conf$byClass[3]   # random forest의 positive predictive value를 입력합니다.
Vis_DF$Value[5] <- rf_conf$byClass[4]   # random forest의 negative predictive value를 입력합니다.
Vis_DF$Value[6] <- xg_conf$overall[1]   # C5.0의 accuracy를 입력합니다.
Vis_DF$Value[7] <- xg_conf$byClass[1]   # C5.0의 sensitivity를 입력합니다.
Vis_DF$Value[8] <- xg_conf$byClass[2]   # C5.0의 speicificity를 입력합니다.
Vis_DF$Value[9] <- xg_conf$byClass[3]   # C5.0의 positive predictive value를 입력합니다.
Vis_DF$Value[10] <- xg_conf$byClass[4]   # C5.0의 negative predictive value를 입력합니다.
Vis_DF$Measure <- factor(Vis_DF$Measure, levels = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV"))   # 성능 평가 지표의 순서를 입력해줍니다.

ggplot( Vis_DF, aes(x=Measure, y=Value, group=Model, col=Model)) +   # x축: 성능 지표, y축: 성능 지표값, 색상: 모델별로 색상을 표현
  geom_line(size = 1) +   # line graph를 추가합니다. 
  geom_point(size=3, fill="white")   # dot graph를 추가합니다.
ggsave("dot_line_plot.jpg", dpi = 300)   # ggplot를 저장합니다.





#####
#rader plot
install.packages('fmsb')
library(fmsb)   # radar graph를 그리기위한 fmsb 패키지입니다.
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))   # radar graph의 line 색상을 선정합니다.
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))   # radar graph의 내부 색상을 선정합니다.

Item_Radar <- data.frame(Accuracy=c(1,2), Sensitivity=NA, Specificity=NA, PPV=NA, NPV=NA)   # radar graph를 위한 데이터 프레임을 만듭니다.
Item_Radar[1,] <- Vis_DF[c(1:5),3]   # random forest 값을 입력합니다.
Item_Radar[2,] <- Vis_DF[c(6:10),3]   # c5.0 값을 입력합니다.
rownames(Item_Radar) <- c("RF","XG")   # row의 명칭을 입력합니다.

Radar_data <- rbind(rep(1,5) , rep(0,5) , Item_Radar)   # 성능 값의 최대인 1과 최소인 0을 입력해줍니다. 
jpeg(file = "radarchart.jpeg", width = 750, height = 702)   # radar graph 저장 설정을 입력합니다.
radarchart( Radar_data  , axistype=1 ,   # axes의 종류를 입력합니다.
            pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1, pty = 20, cglcol="grey", cglty=1, cglwd=0.8,   # 색상, 포인트, 라인 값을 입력합니다.
            axislabcol="black", caxislabels=c("0%", "25%", "50%", "75%", "100%"),   # axis label 값을 설정합니다.
            vlcex=1.1,   # font size를 설정합니다.
            title = "Performance Comparison for Each Model"   # title을 설정합니다. 
)

legend(x=0.7, y=1, legend = rownames(Radar_data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1, pt.cex=3)   # legend를 설정합니다.
dev.off()   # radar graph를 저장합니다.


















