getwd()

setwd("C:/Users/kim/Desktop/을지/청년캠퍼스/230713_1반_통계실습")

d1<-read.csv("BASE_DATA1.txt")
d2<-read.csv("BASE_DATA2.txt")
d3<-read.csv("BASE_DATA3.txt")

str(d1) #데이터 세트 구조 확인
str(d2) #데이터 세트 구조 확인
str(d3) #데이터 세트 구조 확인
dim(d1) #관측값 수 (행), 변수의 수 (열) 확인
ls(d1) #변수명 확인
ls(d2)
head(d1) #데이터셋 앞부분 6행 출력
head(d2)
tail(d1) #데이터셋 뒷부분 6행 출력

#자료 결합 (merge) #
#PDF 35#

install.packages("dplyr")
library(dplyr)

#필요한 변수만 가져오기# (파이프 연산자(%>%))
#ctrl+shift+m#

s1<-d1%>% select(T_ID, T_SEX, T_AGE)
s2<-d2%>% select(T_ID, T_HTN, T_DM, 
                 T_DRINK, T_SMOKE)
s3<-d3%>% select(T_ID, T_WEIGHT, T_HEIGHT, 
                 T_BMI, T_GLU0)

m1<-left_join(d1,d2,by="T_ID")
m2<-left_join(m1,d3,by="T_ID")
dim(m2)


d4<-subset(d3, T_GLU0<99999)
d4<-subset(d3, !T_GLU0==99999)


testm1<-left_join(d1,d4)
testm1<-left_join(d4,d1)
testm2<-right_join(d1,d4)


#filter() 조건에 맞는 행 추출#
man<-m2 %>% filter(T_SEX==1)
man1<-subset(m2, T_SEX==1)
table(m2$T_SEX)

#select()  변수 추출 또는 제거#
sel<-m2 %>%  select(T_DM, T_GLU0)
dim(sel)
sel2<-m2 %>%  select(-T_DM, -T_GLU0)
dim(sel2)


#mutate() 새로운 변수 추가 #
head(m2$T_BMI)
dat<- m2 %>% 
  mutate(bmi=T_WEIGHT/((T_HEIGHT/100))^2)
str(dat)
head(dat$bmi)

# summarize() 데이터 요약
dat1<- m2 %>%  group_by(T_SEX) %>%  
  summarize(mean_glu=mean(T_GLU0), 
            sd_glu=sd(T_GLU0))
m2$T_GLU0

dat1<- m2 %>%  group_by(T_SEX) %>% 
  filter(T_GLU0<99999) %>%
  summarize(mean_glu=mean(T_GLU0), 
            sd_glu=sd(T_GLU0))

dat1<- m2 %>%  group_by(T_SEX) %>% 
  filter(!T_GLU0==99999) %>%
  summarize(mean_glu=mean(T_GLU0), 
            sd_glu=sd(T_GLU0))

summary(m2$T_HEIGHT)
summary(m2$T_DRINK)
str(m2$T_DRINK)

install.packages("descr")
library(descr)
freq(m2$T_SEX)
freq(m2$T_SMOKE)
freq(m2$T_DRINK)


#결측 처리 변수별  (%in% : 포함 연산자)#
m2null<-m2
m2null$T_SMOKE<-ifelse(m2null$T_SMOKE%in%
                         c(66666,77777,99999),
                       NA, m2null$T_SMOKE)
is.na(m2null$T_SMOKE)
sum(is.na(m2null$T_SMOKE))


#결측 처리 일괄# &
m2null[m2null==66666|m2null==77777|m2null==99999]<-NA
freq(m2null$T_SMOKE)
freq(m2null$T_DRINK)

#데이터 유형 변환 #
m3<-m2null
class(m3$T_HEIGHT)
str(m3)
m3$T_HEIGHT<-as.character(m3$T_HEIGHT)
summary(m3$T_HEIGHT)
m3$T_HEIGHT<-as.factor(m3$T_HEIGHT)
summary(m3$T_HEIGHT)
m3$T_HEIGHT<-as.numeric(m3$T_HEIGHT)
summary(m3$T_HEIGHT)

#데이터 변수 유형 파악#
class(m3$T_AGE)
class(m3$T_HEIGHT)
sapply(m3, class)

# 당뇨병 관련 변수가 모두 결측인 대상자 제외#
m4<-m3 %>% filter(!(is.na(T_DM)&is.na(T_GLU0))) #9992명
m4_1<-m3 %>% filter(!(is.na(T_DM)|is.na(T_GLU0))) #9753명

# 비만 관련 변수가 모두 결측인 대상자 제외#
m5<-m4 %>% filter(!(is.na(T_BMI)))  #9944명
#PDF 45
write.csv(m5,"C:/Users/박601-49/Desktop/거위/m5.csv")

m5<-read.csv("G:/데청캠/m5.csv")


#결측치 확인
is.na(m5) # 결측치 있을 때 함수의 리턴값은 TRUE

#결측값이 하나라도 있는 행은 모두 제거하고 부르기
m6<-na.omit(m5) 

#데이터셋 모든 변수별 결측치 개수 확인하기
colSums(is.na(m5))


#변수 생성 
# BMI -> 저체중 (BMI <18.5), 정상 (BMI 18.5~23), 
#과체중 (BMI 23 ~ 25), 비만(>25)
m_bmi<-m5 %>% mutate(bmi_gr=ifelse(T_BMI<18.5,1,
                                   ifelse(T_BMI>=18.5 & T_BMI<23,2,
                                          ifelse(T_BMI>=23 & T_BMI<25,3,
                                                 ifelse(T_BMI >= 25, 4, NA)))))

freq(m_bmi$bmi_gr)
str(m_bmi$bmi_gr)
class(m_bmi$bmi_gr)

#범주형으로 바꾸기
m_bmi$bmi_gr<-as.factor(m_bmi$bmi_gr)
class(m_bmi$bmi_gr)

#변수 생성
# 혈당 (>=126), 당뇨 진단 여부 조합 -> 
#-> 당뇨병 변수 생성
bmidm<-m_bmi %>% mutate(dm=ifelse(T_DM==2 | T_GLU0>=126,1,0))

freq(bmidm$dm) #210명 결측 발생


bd<-bmidm %>% filter(!(is.na(dm))) #9944-210 = 9734


#교차표 및 chi-square
install.packages("gmodels")
library(gmodels)
class(bd$T_SEX)
bd$T_SEX<-as.factor(bd$T_SEX)

CrossTable(bd$bmi_gr, bd$T_SEX) #교차표

ct<-chisq.test(bd$bmi_gr, bd$T_SEX)
ct$p.value


CrossTable(bd$T_SMOKE, bd$dm)
ct1<-chisq.test(bd$T_SMOKE, bd$dm)
ct1$p.value

#기술 통계
summary(bd$T_AGE)

install.packages("psych")
library(psych)
??psych
describe(bd$T_AGE)

summary(bd$T_BMI)
describe(bd$T_BMI)

#그래프
par(mfrow=c(1,1))
hist(bd$T_AGE, main="Histogram", xlab="T_AGE")
boxplot(bd$T_AGE, main="Box Plot", xlab="T_AGE")

hist(bd$T_BMI, main="Histogram", xlab="T_BMI")
boxplot(bd$T_BMI, main="Box Plot", xlab="T_BMI")

#그룹별 기술통계량
describeBy(bd$T_BMI,  bd$T_SEX)
boxplot(T_BMI ~ T_SEX,data=bd )

#정규성 검정
install.packages('ks')
library(ks)
KS <- ks.test(bd$T_BMI, pnorm)
KS
KS$p.value

bdm<-subset(bd, T_SEX==1)
bdw<-subset(bd, T_SEX==2)
KSm <- ks.test(bdm$T_BMI, pnorm)
KSw <- ks.test(bdw$T_BMI, pnorm)
KSm
KSm$p.value
KSw
KSw$p.value

hist(bd$T_GLU0)
KSm <- ks.test(bdm$T_GLU0, pnorm)
KSw <- ks.test(bdw$T_GLU0, pnorm)

#두 집단 평균 비교(t_test)
#성별에 따른 연령(bmi) 확인
describeBy(bd$T_GLU0,  bd$T_SEX)

#등분산성 가정 검토
var.test(T_BMI ~ T_SEX, data=bd)
# P-value가 0.05보다 작으므로 등분산성 만족 x 

# t-test
t.test(T_BMI ~ T_SEX, data=bd, 
       var.equal=T)
#t.test(T_BMI ~ T_SEX, data=bd, var.equal=F) : 등분산을 만족하지 않았을 때

#집단간 연속형변수 (수치형 자료) 비교 PDF 60
wilcox.test(T_GLU0 ~ T_SEX, data=bd)
describeBy(bd$T_GLU0, bd$T_SEX)

#분산 분석 (ANOVA)
#귀무가설 : 비만도에 따라 혈당 수준의 평균 차이가 없다.
#대립가설 : 비만도에 따라 혈당 수준의 평균 차이가 있다.

#비만도 집단별 혈당 
describeBy(bd$T_GLU0, bd$bmi_gr)

#등분산성 검토
install.packages("car")
library(car)
leveneTest(T_GLU0 ~ bmi_gr, data=bd)
# p-value : 0.05보다 작으면 등분산 만족 x, 크면 만족

# wanova<-oneway.test(T_GLU0 ~ bmi_gr, data=bd, var.equal=F)
wanova<-oneway.test(T_GLU0 ~ bmi_gr, data=bd, 
                    var.equal=T)
wanova

# 사후검정
install.packages("rstatix")
library(rstatix)
posthoc<- games_howell_test(data=bd,T_GLU0 ~ bmi_gr )
posthoc

#등분산 만족할 경우의 ANOVA 예시
aov<-aov(T_GLU0 ~ bmi_gr, data=bd)
summary(aov)

install.packages("agricolae")
library(agricolae)
install.packages("htmltools") 
#"네임스페이스 ‘htmltools’ 0.5.3는 로드되었으나 >= 0.5.4가 필요합니다" 의 오류 해결#
agricolae::scheffe.test(aov, "bmi_gr", group=F, console=T)
#a::b a 안세어 b를 실행

#box plot
boxplot(T_GLU0 ~bmi_gr, data=bd, xlab="비만도 집단", 
        ylab = "혈당", ylim=c(50,130)) # ylim: y축 조절
#boxplot(T_GLU0 ~bmi_gr, data=bd, xlab="비만도 집단", 
#        ylab = "혈당",)

install.packages("moonBook")
library(moonBook)

bd$T_INCOME <- as.factor(bd$T_INCOME)
bd$T_MARRY <- as.factor(bd$T_MARRY)

mytable(T_DM ~ T_AGE + T_SEX + T_INCOME + T_MARRY + T_DRINK
        + T_SMOKE + T_EXER + bmi_gr, data = bd)

mytable(T_DM ~ T_AGE + T_SEX + T_INCOME + T_MARRY + T_DRINK
        + T_SMOKE + T_EXER + bmi_gr + T_TCHL + T_HTN ,
        data = bd)

mycsv(mytable(T_DM ~ T_AGE + T_SEX + T_INCOME + T_MARRY + T_DRINK
              + T_SMOKE + T_EXER + bmi_gr + T_TCHL + T_HTN ,
              data = bd), file = 'test1.csv')

#상관관계 correlation

c <- cor.test(bd$T_GLU0, bd$T_BMI)
cp <- cor.test(bd$T_GLU0,bd$T_BMI, method="spearman")
#에러가 뜨긴 하지만 속 내용은 바뀜

c$estimate
cp$estimate
c$p.value
cp$p.value

plot(bd$T_GLU0, bd$T_BMI)

which(colnames(bd) == "T_SBP") # 해당 변수가 몇번째 인가
which(colnames(bd) == "T_TG")

str(bd[,47:63])
cor(bd[,47:63])

cc <- cor(bd[, 47:63], use = "complete")

cc

win.graph()
library(corrplot)
corrplot(cc)
corrplot(cc,method="number")

# 선형 회귀 분석
# 단순 선형 회귀분석
s_reg <- lm(T_GLU0 ~ T_BMI, data = bd)
summary(s_reg)
s_reg$coefficients

# 다중 선형 회귀분석
m_reg <- lm(T_GLU0 ~ T_BMI + T_AGE + T_SEX, data = bd)
summary(m_reg)

m_reg2 <- lm(T_GLU0 ~ T_BMI + T_AGE + T_SEX + T_SMOKE, data = bd)
summary(m_reg2)

m_reg3 <- lm(T_GLU0 ~ T_BMI + T_AGE + T_SEX + T_HEIGHT + T_WEIGHT, data = bd)
summary(m_reg3)

# 다중공선성 진단 : VIF>10이면 다중공선성
options(digit = 3)
vif(m_reg)

# 로지스틱 회귀분석

# 기준범주 변경 : relevel()

bd1 <-bd
class(bd1$bmi_gr)
bd1$bmi_gr <-  relevel(bd1$bmi_gr, ref = "2") # 기준그룹을 2로

# 단순 로지스틱 회귀분석
s_logit <- glm(dm ~ bmi_gr, data = bd, family = "binomial")
s_logit
summary(s_logit)
exp(coef(s_logit))
exp(confint.default(s_logit))

s_logit2 <- glm(dm ~ bmi_gr, data = bd1, family = "binomial")
s_logit2
summary(s_logit2)
exp(coef(s_logit2))
exp(confint.default(s_logit2))

# 다중 로지스틱 회귀분석
bd1$T_DRINK <- as.factor(bd1$T_DRINK)
bd1$T_SMOKE <- as.factor(bd1$T_SMOKE)
m_logit <- glm(dm ~ bmi_gr + T_AGE + T_SEX,
               data = bd, family = "binomial")
m_logit2 <- glm(dm ~ bmi_gr + T_AGE + T_SEX + T_DRINK 
                + T_SMOKE, data = bd, family= "binomial")
summary(m_logit)
exp(coef(m_logit))
exp(coef(m_logit2))
exp(confint.default(m_logit))
exp(confint.default(m_logit2))

# 오즈비를 별도로 출력하기
#install.packages("moonBook")
#library(moonBook)
extractOR(m_logit2, digits = 2)
