setwd("E:/문지은/빅데이터청년캠퍼스(을지대)/추적")
f1
f1<-read.csv("FOLLOW_01_DATA.txt", header=TRUE)
f2<-read.csv("FOLLOW_02_DATA.txt", header=TRUE)
f3<-read.csv("FOLLOW_03_DATA.txt", header=TRUE)
f4<-read.csv("FOLLOW_04_DATA.txt", header=TRUE)
f5<-read.csv("FOLLOW_05_DATA.txt", header=TRUE)

library(dplyr)
#f1(baseline 기준으로 merge)
fm1 <- left_join(f1, f2)
fm2 <- left_join(fm1, f3)
fm3 <- left_join(fm2, f4)
fc <- left_join(fm3, f5)

str(fc)
head(fc)

#결측치 처리
fc[fc == 99999 | fc == 77777 | fc == 66666 | fc == 55555] <- NA

#날짜 유형 변경
str(fc$T01_EDATE)

#년월(int에서 문자로 변경 -> 년월 뒤에 01 붙이기 -> 날짜로 변경)
fc$edate1 <- as.Date(paste(as.character(fc$T01_EDATE),"01", sep=""), "%Y%m%d")
fc$edate2 <- as.Date(paste(as.character(fc$T02_EDATE),"01", sep=""), "%Y%m%d")
fc$edate3 <- as.Date(paste(as.character(fc$T03_EDATE),"01", sep=""), "%Y%m%d")
fc$edate4 <- as.Date(paste(as.character(fc$T04_EDATE),"01", sep=""), "%Y%m%d")
fc$edate5 <- as.Date(paste(as.character(fc$T05_EDATE),"01", sep=""), "%Y%m%d")

#날짜 계산(1970-01-01 기준계산)
fc$edate1_n <- as.integer(fc$edate1)
fc$edate2_n <- as.integer(fc$edate2)
fc$edate3_n <- as.integer(fc$edate3)
fc$edate4_n <- as.integer(fc$edate4)
fc$edate5_n <- as.integer(fc$edate5)

#데이터 일부 확인하기(1970.1.1 로부터 며칠 지났는지)
fc[1:10,] %>% select(T01_EDATE, edate1, edate1_n)
fc[1:5,] %>% select(T05_EDATE, edate5, edate1_n)

#당뇨병 또는 비만 관련 변수가 결측인 대상자와
#기반조사(baseline) 당시 당뇨병 유병자 제외

#당뇨병 및 비만 관련 변수 결측치 제외 #
fc1 <- fc %>% filter(! (is.na(T01_DM) & 
                          is.na(T01_GLU0)))
fc2 <- fc1 %>% filter(! (is.na(T01_HEIGHT) | 
                          is.na(T01_WEIGHT)))

#기반조사 당뇨병 유병자 제외
fc3 <- fc2 %>% mutate(DM1 = ifelse((T01_GLU0 >= 126 | 
                                      T01_DM == 2), 1, 0))
library(descr)
freq(fc3$DM1) #결측1명

#결측 있어서 다시 제외
fc4 <- fc3 %>% filter(!(is.na(DM1)))

#기반조사 당시 당뇨 유병 80명 제외
#fc4 <- fc3 %>% filter(DM1 != 1) #또는
fc4<-subset(fc3, DM1==0)

#각 추적지수별 당뇨병 유병 여부 변수 생성
fc5 <- fc4 %>% mutate(DM2 = ifelse((T02_GLU0 >= 126 | T02_DM == 2),1, 0),
                      DM3 = ifelse((T03_GLU0 >= 126 | T03_DM == 2),1, 0),
                      DM4 = ifelse((T04_GLU0 >= 126 | T04_DM == 2),1, 0),
                      DM5 = ifelse((T05_GLU0 >= 126 | T05_DM == 2),1, 0))


freq(fc5$DM2) #결측 29
freq(fc5$DM3) #결측 62
freq(fc5$DM4) #결측 95
freq(fc5$DM5) #결측 264

#결측값 모두 제외
fc6 <- fc5 %>% filter(!(is.na(DM2)|is.na(DM3)|is.na(DM4)|is.na(DM5)))
nrow(fc6) #487행

#총 추적 기간 동안 당뇨병 발생 여부 변수 생성
fc6 <- fc6 %>% mutate(dm_d = ifelse((DM2 == 1 | DM3 == 1| DM4 == 1 |
                                       DM5 ==1),1,0))

#변수 생성
fc6 <- fc6 %>% mutate(T_BMI = T01_WEIGHT / ((T01_HEIGHT / 100) ^2))
#BMI -> 저체중(<18.5), 정상(18.5~23), 과체중(23~25), 비만(>25)
fc6 <-fc6 %>% mutate(bmi_gr=ifelse(T_BMI<18.5, 1,
                            ifelse(T_BMI>=18.5 & T_BMI<23, 2,
                            ifelse(T_BMI>=23 & T_BMI<25, 3,
                            ifelse(T_BMI >= 25, 4, NA)))))

#저체중 대상자 향후 분석에서 제외
freq(fc6$bmi_gr)
fc7 <- fc6 %>% filter(bmi_gr != 1)

#관찰기간 (flow-up duration 생성)
#당뇨 미발생군 연구 종료 시점 (마지막 조사 참여시점)
fd <- fc7
fd$max_date <- apply(fd[ , c("edate2_n", "edate3_n","edate4_n",
                             "edate5_n")], 1, max, na.rm=T)
fd[80:100,] %>% select(edate2_n, edate3_n, edate4_n, edate5_n, max_date)

#당뇨 발생군 연구 종료 시점 (첫 발생 시점)
fd1 <- fd %>% mutate(end_date=ifelse(DM2==1, edate2_n,
                              ifelse(DM2==0 & DM3==1, edate3_n,
                              ifelse(DM2==0 & DM3==0 & DM4==1, edate4_n,
                              ifelse(DM2==0 & DM3==0 & DM4==0 & DM5==1, edate5_n,
                                     max_date)))))
fd1$end_date
#연구 시작 시점 정의
fd1$start <- fd1$edate1_n
#연구 종료 시점 정의
fd1$end <- fd1$end_date
#관찰기간(일) = (연구 종료 시점 - 연구 시작 시점)
fd1$f_time_d <- fd1$end - fd1$start
fd1$f_time_y <- round(fd1$f_time_d/365,1)

#생성 변수 확인인
fd1[1:10,] %>% select(end, start, f_time_d)

gmodels::CrossTable(fd1$bmi_gr, fd1$dm_d)
#install.packages("doBy")
library(doBy)

options(digits=4)
summaryBy(f_time_y ~ bmi_gr, data=fd1, FUN=c(mean, sum, min, max))
options(digits=5)
freq(fd1$bmi_gr)

#Time과 event 여부 정의
#install.packages("survival")
library(survival)
y<- Surv(fd1$f_time_y, fd1$dm_d)

fd1$bmi_gr <- as.factor(fd1$bmi_gr)

fit <- survfit(y ~ bmi_gr, data = fd1)
fit <- survfit(Surv(f_time_y, dm_d) ~ bmi_gr, data = fd1)
summary(fit)

plot(fit, main = "KM curve", ylab = "Survival", xlab = "year",
     col = 1:3, lty = 1:3)
legend("bottomleft", legend = c("정상체중", "과체중", "비만"),
       col = 1:3, lty = 1:3)

#install.packages("survminer")
library(survminer)
ggsurvplot(fit,
           risk.table = TRUE,
           break.time.by = 1,
           xlim = c(0,12),
           ylim = c(0.8,1),
           legend.labs = c("정상체중", "과체중", "비만만"),
           pval = TRUE)

#fun='event' 추가가
ggsurvplot(fit,
           fun='event',
           risk.table = TRUE,
           break.time.by = 1,
           xlim = c(0,12),
           ylim = c(0.8,1),
           legend.labs = c("정상체중", "과체중", "비만만"),
           pval = TRUE)


#log-rank test
log_rank <- survdiff(y ~ bmi_gr, data = fd1)
log_rank$pvlaue

#Cox proportional hazards model
cox1 <- coxph(y ~ bmi_gr, data=fd1)
options(digits =2)
summary(cox1) #exp(-coef) : harzards ratio

#성별, 연령 보정
fd1$T00_SEX <- as.factor(fd1$T00_SEX)
fd1$T00_SEX <- relevel(fd1$T00_SEX, ref = "2")

install.packages('moonBook')
library(moonBook)
cox2 <- coxph(y ~ bmi_gr + T01_AGE + T00_SEX, data = fd1) 
summary(cox2) 
extractHR(cox2)

# 비례 위험 가정 검토
#(1) log-log plot
y <- Surv(fd1$f_time_y, fd1$dm_d)
fit <- survfit(y ~ bmi_gr, data = fd1)
plot(fit, fun = "cloglog", main = "log-log KM curves", ylab = "log-log survival",
     xlab = "f_time_y", col = 1:3, lty=1:3)

cox1 <- survival::coxph(y ~ bmi_gr, data = fd1)
cox.zph(cox1)


