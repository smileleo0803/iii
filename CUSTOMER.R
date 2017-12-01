library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  
CUS <- sqlQuery(myconn, "select CUS010,CUS020,CUS030,CUS040,CUS050,CUS070,CUS080,CUS090,
                CUS120,CUS150,CUS250,CUS320,CUS350,CUS370,CUS380,CUS390,CUS570,CUS440,NBHNO_,NDEPT_
                from KGICS0MH_20171001 as k inner join BRHDTAH as b ON k.CUS010 = b.BHNO_ and k.CUS150 = b.DEPT_ ")


#使用完關閉資料連線
close(myconn) 
#匯出CSV檔案
write.csv(CUS_1, "CUS_1.csv", row.names = FALSE)

class(CUS)
head(CUS,10)
nrow(CUS)
str(CUS)
summary(CUS)
library("data.table")
CUS <- as.data.table(CUS)
#############################################################################################################

#CUS370轉字串
CUS$CUS370 <- as.character(CUS$CUS370)
#CUS380應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
library(stringr)
CUS$CUS380 <- str_pad(CUS$CUS380,6,"left",'0')
#CUS390轉字串
CUS$CUS390 <- as.character(CUS$CUS390)
#將CUS370、CUS380、CUS390轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
CUS$CUSTNO <- paste0(CUS$CUS370,CUS$CUS380,CUS$CUS390)
#姓名轉字串
CUS$CUS040 <- as.character(CUS$CUS040)
#ID由Factor轉字串
CUS$CUS050 <- as.character(CUS$CUS050) 
#CUS570應為3碼字串，但轉入為int型態，補0為3碼並轉成字串
CUS$CUS570 <- str_pad(CUS$CUS570,3,"left",'0')

##############################################################################################################

#新增性別變數(擷取ID第2碼)
#注意：資料包含法人、外國人，無法判斷性別，造成第二碼有非0或1的值
CUS$Gender <- substring(CUS$CUS050,2,2) 
CUS$Gender <- as.factor(CUS$Gender)

#新增年齡變數
#出生日期由int轉字串
CUS$CUS350 <- as.character(CUS$CUS350)
#把出生時間為0的改為NA
CUS$CUS350[CUS$CUS350 =="0" ] <- "NA"


#新增年齡變數(現在日期-出生日期)
#CUS350為年月日格式，轉成Date(無時分秒問題)
CUS$Age <- as.Date(CUS$CUS350, format = "%Y%m%d")
#當前日期與生日相減→天數，除365轉年→floor為無條件捨去
CUS$Age <- floor((Sys.Date()-CUS$Age)/365)
#型態由difftime改為numeric
CUS$Age <- as.numeric(CUS$Age)
CUS$CUS350 <- as.numeric(CUS$CUS350)

#CUS081最後往來日至今天數
CUS$CUS080 <- as.character(CUS$CUS080)
CUS$CUS081 <- as.Date(CUS$CUS080, format = "%Y%m%d")
CUS$CUS081 <- Sys.Date()-CUS$CUS081
CUS$CUS081 <- as.numeric(CUS$CUS081)
CUS$CUS080 <- as.numeric(CUS$CUS080)


#有無email(Y/N)
CUS441 <- ifelse(CUS$CUS440 == '', 'N' , 'Y')
CUS441 <- as.factor(CUS441)
CUS <- cbind(CUS,CUS441)
rm(CUS441)


#電子管道(Y/N)
CUS251 <- ifelse(CUS$CUS250 == 0 , 'N' , 'Y')
CUS251 <- as.factor(CUS251)
CUS <- cbind(CUS,CUS251)
rm(CUS251)

#增加一欄rep(1,nrow(CUS))讓每行都計數1
#增加一欄N_account→計算每個ID持有幾個帳號
library(dplyr)
CUS <- cbind(CUS,rep(1,nrow(CUS)))
#把rep(1,nrow(CUS))欄位名改為1
colnames(CUS)[ncol(CUS)]='1'

str(CUS)

#CUS拉出不重覆新帳戶 #2237455
CUS <- sqldf("select * from CUS group by CUSTNO ")

#############################################################################################################

#觀察cus的na數
sum(is.na(CUS$CUS010))
sum(is.na(CUS$CUS020))
sum(is.na(CUS$CUS030))#29
sum(is.na(CUS$CUS040))
sum(is.na(CUS$CUS050))
sum(is.na(CUS$CUS080))
sum(is.na(CUS$CUS090))
sum(is.na(CUS$CUS120))
sum(is.na(CUS$CUS150))#1410635
sum(is.na(CUS$CUS250))
sum(is.na(CUS$CUS320))#309
sum(is.na(CUS$CUS350))#3961
sum(is.na(CUS$CUS370))
sum(is.na(CUS$CUS380))#11
sum(is.na(CUS$CUS390))#41
sum(is.na(CUS$CUS570))#115322
sum(is.na(CUS$CUS440))#35914
sum(is.na(CUS$NBHNO_))
sum(is.na(CUS$NDEPT_))#1000374
sum(is.na(CUS$CUSTNO))
sum(is.na(CUS$Gender))
sum(is.na(CUS$Age))#4131
sum(is.na(CUS$CUS081))#470259

CUS.shape[0] - CUS.count()
#########################################################################################################

#CUS_1扣除已關戶 #1654959
library(sqldf)
CUS_1 <- sqldf("select * from CUS where CUS090 == 0 ")
CUS_1 <- as.data.table(CUS_1)
nrow(CUS_1)
nrow(CUS[which(CUS$CUS090 == 0)])

#CUS_2扣除已關戶且為自然人 #1631286
CUS_2 <- sqldf("select * from CUS_1 where CUS320 =='1' ")
CUS_2 <- as.data.table(CUS_2)
nrow(CUS_2)
nrow(CUS[which(CUS$CUS090 == 0 & CUS$CUS320 =='1')])

#CUS_3 →CUS扣除已關戶且為本國自然人 #1561388
CUS_3 <- sqldf("select * from CUS where CUS090 = 0 and CUS570=='000'" , method = "name__class")
CUS_3 <- as.data.table(CUS_3)
nrow(CUS_3)
nrow(CUS[which(CUS$CUS090 == 0 & CUS$CUS570 =='000')])

#CUS_4 →CUS_3且只取Gender為1或2 #1561384
CUS_4 <- sqldf("select * from CUS_3 where Gender=='1' or Gender =='2'" , method = "name__class")
CUS_4 <- as.data.table(CUS_4)
nrow(CUS_4)
nrow(CUS_3[which(CUS_3$Gender=='1' | CUS_3$Gender=='2')])

#CUS_5(實動戶) →CUS_3且CUS080 > 20150930(扣除已關戶且近一年實動之本國自然人) #277985
CUS_5 <- sqldf("select * from CUS_3 where CUS080 > 20160930 " , method = "name__class")
CUS_5 <- as.data.table(CUS_5)
nrow(CUS_5)
nrow(CUS_3[which(CUS_3$CUS080 > 20140930)])


##########################################################################################################

#看distinct 客戶ID
library(sqldf)
U.CUS <- sqldf("select * from CUS group by CUS050", method = "name__class")
nrow(U.CUS)#不重覆客戶共1865839

U.CUS_1 <- sqldf("select * from CUS_1 group by CUS050 ", method = "name__class")
nrow(U.CUS_1)#不重覆客戶共1508558

U.CUS_2 <- sqldf("select * from CUS_2 group by CUS050 ", method = "name__class")
nrow(U.CUS_2)#不重覆客戶共1495182

U.CUS_3 <- sqldf("select * from CUS_3 group by CUS050 ", method = "name__class")
nrow(U.CUS_3)#不重覆客戶共1450315

U.CUS_4 <- sqldf("select * from CUS_4 group by CUS050 ", method = "name__class")
nrow(U.CUS_4)#不重覆客戶共1450311

U.CUS_5 <- sqldf("select * from CUS_5 group by CUS050 ", method = "name__class")
nrow(U.CUS_5)#不重覆客戶共275891



#################################################################################################
#客戶類別比重
qplot(CUS320, data = U.CUS_1, geom = "histogram")
table(U.CUS_1$CUS320)
round(table(U.CUS_1$CUS320) / length(U.CUS_1$CUS320) *100,2)

#電子戶比重
table(CUS_3$CUS251)
round(table(CUS_3$CUS251) / length(CUS_3$CUS251) *100,2)

#自來戶比重
table(CUS_3$CUS120)
round(table(CUS_3$CUS120) / length(CUS_3$CUS120) *100,2)

#Email比重
table(CUS_3$CUS441)
round(table(CUS_3$CUS441) / length(CUS_3$CUS441) *100,2)

#檢視年齡分配
options(scipen=999)
summary(U.CUS_3$Age)
range(U.CUS_3$Age,na.rm = T)
quantile(U.CUS_3$Age,na.rm = T)
IQR(U.CUS_3$Age,na.rm = T)
sd(U.CUS_3$Age,na.rm = T)
var(U.CUS_3$Age,na.rm = T)
install.packages("moments")
library(moments)
skewness(U.CUS_3$Age,na.rm = T) #偏度(偏態值 > 0，為正偏態，分配集中在平均數以下，低分群的個體較多)
kurtosis(U.CUS_3$Age,na.rm = T) #峰度(峰態係數K>0稱為高峻峰，峰態係數K=0稱為常態峰，峰態係數K<0稱為低闊峰)
boxplot(formula = U.CUS_3$Age~ U.CUS_3$Gender,data=U.CUS_3,xlab="性別",ylab="年齡",col="#f8f3c4")
title("U.CUS_3客戶年齡與性別箱型圖")


#不重覆客戶的男女比
table(U.CUS_4$Gender)
round(table(U.CUS_4$Gender)/nrow(U.CUS_4),2)

#客戶年齡的直方圖，依性別分開檢視
install.packages("lattice")
require(lattice)
histogram(x= ~ Age | Gender,  # 根據性別的條件，繪製客戶年齡的直方圖
          data= CUS_4,     
          xlab="CUS_4客戶年齡分配",  
          layout=c(2,1))       # 以2x1的方式呈現圖表

#客戶年齡的直方圖
install.packages("ggplot2")
library("ggplot2")
qplot(Age, data = U.CUS_4, geom = "histogram")
#客戶年齡的直方圖，依性別疊合檢視
install.packages("ggplot2")
library("ggplot2")
qplot(Age, data = U.CUS_4, geom = "histogram",
      fill = Gender)

#################################################################################
#性別比重
pct = round(table(U.CUS_4$Gender) / length(U.CUS_4$Gender) *100,1)
labels = paste(names(pct),pct,"%")
pie(table(U.CUS_4$Gender), labels = labels)




#最後交易日至今天數
options(scipen=999)
qplot(CUS081, data = U.CUS_5, geom = "histogram")
