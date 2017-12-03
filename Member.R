install.packages("readxl")
library("readxl")
Member <- read_excel("C:/Users/0010173/Desktop/鑫富人生3/XFLife3.xlsx", sheet=1 ,col_names=T)
#參數sheet為要讀的excel sheet名稱或Index

#資料型態轉換
Member <- data.frame(Member)

#檢視Insurance
str(Member)
head(Member,10)
summary(Member)

#重新命名欄位名稱
colnames(Member) = c('Customer_ID','Gender','Birthday','')


#轉換欄位型態，可不用加"
Insurance$"Area_code" <- as.factor(Insurance$"Area_code")
Insurance$"Brokerage_area" <- as.factor(Insurance$"Brokerage_area")
Insurance$"Branch" <- as.factor(Insurance$"Branch")
Insurance$"Branch_code" <- as.factor(Insurance$"Branch_code")
Insurance$"Dept_no" <- as.factor(Insurance$"Dept_no")
Insurance$"HrDept_no" <- as.factor(Insurance$"HrDept_no")
Insurance$"PsDept_no" <- as.factor(Insurance$"PsDept_no")
Insurance$"Dept_code" <- as.factor(Insurance$"Dept_code")
Insurance$"Dept_name" <- as.factor(Insurance$"Dept_name")
Insurance$"Applicant_Insu_ID" <- as.factor(Insurance$"Applicant_Insu_ID")
Insurance$"Insu_code" <- as.factor(Insurance$"Insu_code")
Insurance$"Insu_name" <- as.factor(Insurance$"Insu_name")
Insurance$"type1" <- as.factor(Insurance$"type1")
Insurance$"type2" <- as.factor(Insurance$"type2")
Insurance$"type3" <- as.factor(Insurance$"type3")
Insurance$"Specialist_no" <- as.factor(Insurance$"Specialist_no")
Insurance$"Specialist_name" <- as.factor(Insurance$"Specialist_name")
Insurance$"Insured_id" <- as.factor(Insurance$"Insured_id")
Insurance$"Insured_gender" <- as.factor(Insurance$"Insured_gender")
#被保人年齡有負值，重新計算!


#年齡時間轉換
#如為數字變數且格式為yyyymmdd, 須先用as.character轉為文字再使用as.Date轉換
#日期格式設定可輸入??strtime查閱
??strtime
Insurance$Insured_BD <- as.Date(as.character(Insurance$Insured_BD), format="%Y%m%d")
today<-Sys.time()
Insurance$Insured_age <- floor((as.Date(today)-Insurance$Insured_BD)/365)
Insurance$Insured_age <- as.numeric(Insurance$Insured_age)
summary(Insurance$Insured_age)

#Insured_gender對Insured_age箱形圖
qplot(Insured_gender, Insured_age, data = Insurance, geom = "boxplot")
qplot(Insured_gender, Insured_age, data = Insurance, geom = "jitter")
#Insured_age機率密度圖(依性別疊合檢視)
qplot(x=Insured_age,                             
      data=Insurance,                     
      geom="density",        # 圖形=density
      xlab="被保人年齡",
      xlim=c(0,90),
      ylim=c(0,0.05),
      color=Insured_gender  # 以顏色標註性別，複合式的機率密度圖
)



#檢視被保人年齡分配
table(Insurance$Insured_age)
options(scipen=999)
summary(Insurance$Insured_age)
range(Insurance$Insured_age,na.rm = T)
quantile(Insurance$Insured_age,na.rm = T)
IQR(Insurance$Insured_age,na.rm = T)
sd(Insurance$Insured_age,na.rm = T)
var(Insurance$Insured_age,na.rm = T)
install.packages("moments")
library(moments)
skewness(Insurance$Insured_age,na.rm = T) #偏度(偏態值 < 0，為負偏態，分配集中在平均數以上，高分群的個體較多)
kurtosis(Insurance$Insured_age,na.rm = T) #峰度(峰態係數K>0稱為高峻峰，峰態係數K=0稱為常態峰，峰態係數K<0稱為低闊峰)
boxplot(formula = Insurance$Insured_age~ Insurance$Insured_gender,data=Insurance,xlab="性別",ylab="被保人年離",col="#f8f3c4")
title("Insurance被保人年齡與性別箱型圖")

#被保人年齡的直方圖，依性別分開檢視
install.packages("lattice")
require(lattice)
histogram(x= ~ Insured_age | Insured_gender,  # 根據性別的條件，繪製被保人年齡的直方圖
          data= Insurance,     
          xlab="被保人年齡分配",  
          layout=c(2,1))       # 以2x1的方式呈現圖表

#被保人年齡的直方圖，依性別疊合檢視
install.packages("ggplot2")
library("ggplot2")
qplot(Insured_age, data = Insurance, geom = "histogram",
      fill = Insured_gender)



#############################################################################################



#總筆數共3343筆
#要保人不重覆比數1643筆
#被保人不重覆比數1907筆
library(sqldf)
a <- sqldf("select * from Insurance Group by  Applicant_Insu_ID ")
a
nrow(a)

#要保人與被保人比對
nrow(Insurance)  #總筆數共3343筆
a <- sqldf("select * from Insurance where Applicant_Insu_ID = Insured_id")
head(a,10)
nrow(a)
#要保人=被保人共2814筆
#要保人!=被保人共529筆
#要保人=被保人，且不重覆的交易筆數共1535筆
a <- sqldf("select * from Insurance where Applicant_Insu_ID = Insured_id Group by Applicant_Insu_ID ")
a
nrow(a)



#撈出不重複的險種名稱，並匯出成CSV
a <- sqldf("
       select Insu_name , sum(Insu_name)
       from Insurance
       group by Insu_name
       ")
a

write.csv(a, "insurance_name.csv", row.names = FALSE)
