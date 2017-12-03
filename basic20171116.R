##找台股現存自然人、近一年有實動、有庫存


#一年內實動的現存自然人
cus <- sqlQuery(myconn, "select CUS010,CUS020,CUS030,CUS040,CUS050,CUS080,CUS090,CUS150,CUS250,CUS320,
                CUS570,NBHNO_,NDEPT_
                from KGICS0MH_20171001 as k inner join BRHDTAH as b ON k.CUS010 = b.BHNO_ and k.CUS150 = b.DEPT_
                where CUS080 > 20160930 and CUS570 = 0 and CUS090 = 0")

close(myconn)

class(cus)
str(cus)
nrow(cus)#326990
library("data.table")
cus <- as.data.table(cus)

#台股客戶20170930的AUM(扣除期貨庫存市值)
aum <- sqlQuery(myconn, "select IDNO_ , CUSTNO_ ,BTYPE_ ,TYPE1_ , TYPE2_ , VALUE_ 
                from KGIAUM0M 
                where TDATE_ = 20170930 ")
close(myconn)

head(aum)
summary(aum$VALUE_)
str(aum)
nrow(aum)#854897
aum <- as.data.table(aum)
class(aum)

library(sqldf)
aum1 <- sqldf("select IDNO_ , CUSTNO_ , sum(VALUE_) sum_value from aum
            where TYPE2_ == '全商品' and BTYPE_ == '台股'
            group by CUSTNO_
            order by sum_value desc")
class(aum1)
nrow(aum1)
aum1 <- as.data.table(aum1)

library(dplyr)
filter1 <-filter(aum1,sum_value==0)
nrow(filter1)
#nrow(aum1)=703878
#>0 共703681
# =0 共197
# <0 共0


#aum1(挑選出自然人)
q <- select(aum1,IDNO_) %>%
  filter(nchar(IDNO_) == 8 )
head(q)
nrow(q)

##################################################################################################

##cus資料格式轉換
#CUS010應為4碼字串，但轉入為int型態，補0為4碼並轉成字串
library(stringr)
cus$CUS010 <- str_pad(cus$CUS010,4,"left",'0')
#CUS020應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
cus$CUS020 <- str_pad(cus$CUS020,6,"left",'0')
#將CUS030轉為字串
cus$CUS030 <- as.character(cus$CUS030)
#姓名由Factor轉字串
cus$CUS040 <- as.character(cus$CUS040)
#ID由Factor轉字串
cus$CUS050 <- as.character(cus$CUS050)
#客戶類別應為1碼字串，但轉入為int型態，將其轉成字串
cus$CUS320 <- as.character(cus$CUS320)
##CUS570應為3碼字串，但轉入為int型態，補0為3碼並轉成字串
cus$CUS570 <- str_pad(cus$CUS570,3,"left",'0')

###################################################################################################
#觀察cus的na數
sum(is.na(cus$CUS010))
sum(is.na(cus$CUS020))
sum(is.na(cus$CUS030))
sum(is.na(cus$CUS040))
sum(is.na(cus$CUS050))
sum(is.na(cus$CUS080))
sum(is.na(cus$CUS090))
sum(is.na(cus$CUS150))#182371
sum(is.na(cus$CUS250))
sum(is.na(cus$CUS320))
sum(is.na(cus$CUS570))#3

###################################################################################################
#aum資料型態轉換
aum$IDNO_  <- as.character(aum$IDNO_)
aum$CUSTNO_  <- as.character(aum$CUSTNO_)
#觀察aum的na數
sum(is.na(aum$IDNO_))
sum(is.na(aum$CUSTNO_))
sum(is.na(aum$BTYPE_))
sum(is.na(aum$TYPE1_))
sum(is.na(aum$TYPE2_))
sum(is.na(aum$VALUE_))

##################################################################################################

##新增變數為mapping帳號
#將CUS020、CUS030轉為字串，用paste成新變數"CUS021"，供和aum比對
cus$CUS021 <- paste0(cus$CUS010,cus$CUS020,cus$CUS030)
str(cus)

head(aum1)

#匯出CSV檔案
write.csv(cus, "cus.csv", row.names = FALSE)
write.csv(aum1, "aum1.csv", row.names = FALSE)

#cus與aum1帳號11碼的交集(證券)
total <- sqldf("select * from cus inner join aum1 on cus.CUS021 == aum1.CUSTNO_ ")
nrow(total)
total <- as.data.table(total)

##Total2為1年內實動且有庫存的本國自然人戶數
total2 <- sqldf("select CUS010,CUS020,CUS030,CUS040,CUS050,CUS080,CUS090,CUS150,CUS250,CUS320,
                CUS570,CUS021,NBHNO_,NDEPT_,CUSTNO_,sum(sum_value) sum_aum
                from total
                group by CUS021
                having sum_aum <> 0
                order by sum_aum desc ")
total2 <- as.data.table(total2)

options(scipen=999)
str(total2)
head(total2)
nrow(total2)#279983
total2

#電子管道(Y/N)
CUS251 <- ifelse(total2$CUS250 == 0 , 'N' , 'Y')
CUS251 <- as.factor(CUS251)
total2 <- cbind(total2,CUS251)

table(total2$CUS251)/nrow(total2)

#######################################################################################################

##Total3為1年內實動且有庫存的本國自然人人數
total3 <- sqldf("select CUS010,CUS020,CUS030,CUS040,CUS050,CUS080,CUS090,CUS150,CUS250,CUS320,
                CUS570,CUS021,NBHNO_,NDEPT_,CUSTNO_,sum(sum_value) sum_aum_id
                from total
                group by CUS050
                having sum_aum_id <>0
                order by sum_aum_id desc")
total3 <- as.data.table(total3)

options(scipen=999)
str(total3)
nrow(total3)#278134
total3

##########################################################################################################

#total3 庫存次數分配表
options(scipen=999)
summary(total3$sum_aum_id)
range(total3$sum_aum_id,na.rm = T)
quantile(total3$sum_aum_id,na.rm = T)
IQR(total3$sum_aum_id,na.rm = T)
sd(total3$sum_aum_id,na.rm = T)
var(total3$sum_aum_id,na.rm = T)
library(moments)
skewness(total3$sum_aum_id,na.rm = T) #偏度(偏態值 > 0，為正偏態，分配集中在平均數以下，低分群的個體較多)
kurtosis(total3$sum_aum_id,na.rm = T) #峰度(峰態係數K>0稱為高峻峰，峰態係數K=0稱為常態峰，峰態係數K<0稱為低闊峰)


total3_1 <- subset(total3,sum_aum_id <= 127850) 
head(total3_1)
total3_2 <- subset(total3,sum_aum_id >127850 & sum_aum_id <= 454271.5)
head(total3_2)
total3_3 <- subset(total3,sum_aum_id >454271.5 & sum_aum_id <= 1361296.8)
head(total3_3)
total3_4 <- subset(total3,sum_aum_id >=1361296.8 )
head(total3_4,30)

#AUM箱型圖很醜，資料太離散
boxplot(total3$sum_aum_id,horizontal = T, col='#f8f3c4')
title("台股AUM箱型圖")

t <- subset(total3,sum_aum_id <= 600000000 & sum_aum_id > 100000000)
nrow(subset(total3,sum_aum_id <= 1000000 & sum_aum_id > 900000))

#圖形有error可試
#dev.off()
library(ggplot2)
ggplot(t, aes(sum_aum_id))+
  geom_histogram(aes(y = ..count..), binwidth = 10000000)+
  labs(title = "台股AUM1~6億次數分配直方圖" , x = "AUM")

ggplot(t, aes(sum_aum_id))+
  geom_histogram(aes(y = ..count..), binwidth = 5000000)+
  labs(title = "台股AUM1千萬~1億次數分配直方圖(5百萬為區間)" , x = "AUM")

ggplot(total3_3, aes(sum_aum_id))+
  geom_histogram(aes(y = ..count..), binwidth = 50000)+
  labs(title = "台股AUM40~140萬次數分配直方圖(5萬為區間)" , x = "AUM")

ggplot(t, aes(sum_aum_id))+
  geom_histogram(aes(y = ..count..), binwidth = 500000)+
  labs(title = "台股AUM140萬~1千萬次數分配直方圖(50萬為區間)" , x = "AUM")

ggplot(t, aes(sum_aum_id))+
  geom_histogram(aes(y = ..count..), binwidth = 50000)+
  labs(title = "台股AUM 0~1千萬次數分配直方圖(5萬為區間)" , x = "AUM")

ggplot(t, aes(sum_aum_id))+
  geom_histogram(aes(y = ..count..), binwidth = 10000)+
  labs(title = "台股AUM 0~140萬次數分配直方圖(1萬為區間)" , x = "AUM")


#########################################################################################################################
library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283") 
#近一年客戶交易次數資料
KGIMXMM <- sqlQuery(myconn, "select TDATE_ ,TERMSEQ_ ,TTYPE_ ,BHNO_ , CUSTNO_ , CUSTCKNO_
                    from KGIMXMM
                    where TDATE_ > 20160930 and TDATE_ <= 20170930")

class(KGIMXMM)
str(KGIMXMM)
nrow(KGIMXMM)
library("data.table")
KGIMXMM <- as.data.table(KGIMXMM)

KGIMXMM$TERMSEQ_ <- as.character(KGIMXMM$TERMSEQ_)
KGIMXMM$TTYPE_ <- as.factor(KGIMXMM$TTYPE_)
#BHNO_應為4碼字串，但轉入為factor型態，補0為4碼並轉成字串
library(stringr)
KGIMXMM$BHNO_ <- str_pad(KGIMXMM$BHNO_,4,"left",'0')
#CUSTNO_應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
KGIMXMM$CUSTNO_ <- str_pad(KGIMXMM$CUSTNO_,6,"left",'0')
#將CUSTCKNO_轉為字串
KGIMXMM$CUSTCKNO_ <- as.character(KGIMXMM$CUSTCKNO_)


#近一年客戶累計交易金額資料
MTSLCSDEAL <- sqlQuery(myconn, "select TMONTH_ ,BHNO_ , CUSTNO_ , CUSTCKNO_ ,AMT_ ,TSALE_ ,MKTYPE_ , ORTYPE_
                    from MTSLCSDEAL
                    where TMONTH_ > 201609 and TMONTH_ <= 201709")

close(myconn)

class(MTSLCSDEAL)
str(MTSLCSDEAL)
nrow(MTSLCSDEAL)
MTSLCSDEAL <- as.data.table(MTSLCSDEAL)

#BHNO_應為4碼字串，但轉入為factor型態，補0為4碼並轉成字串
library(stringr)
MTSLCSDEAL$BHNO_ <- str_pad(MTSLCSDEAL$BHNO_,4,"left",'0')
#CUSTNO_應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
MTSLCSDEAL$CUSTNO_ <- str_pad(MTSLCSDEAL$CUSTNO_,6,"left",'0')
#將CUSTCKNO_轉為字串
MTSLCSDEAL$CUSTCKNO_ <- as.character(MTSLCSDEAL$CUSTCKNO_)

########################################################################################

#tcount→近一年客戶交易次數
tcount <- select(KGIMXMM ,TDATE_ ,TERMSEQ_ ,TTYPE_ ,BHNO_ , CUSTNO_ , CUSTCKNO_ ) %>%   
          mutate( account_no = paste0(BHNO_ , CUSTNO_ , CUSTCKNO_ )) %>%
          group_by( account_no ) %>%
          summarise(TranCount = n()) %>%
          arrange(desc(TranCount))

head(tcount)
tcount <- as.data.table(tcount)
        
#tran →近一年客戶累計交易金額
tran <- select(MTSLCSDEAL , TMONTH_ ,BHNO_ , CUSTNO_ , CUSTCKNO_ , AMT_ ) %>% 
        mutate( account_no = paste0(BHNO_ , CUSTNO_ , CUSTCKNO_)) %>%
        group_by( account_no ) %>%
        summarise(SumTran = sum(AMT_)) %>%
        arrange(desc(SumTran))
head(tran)
str(tran)       
tran <- as.data.table(tran)

##########################################################################################################################        
#total2為1年內實動且有庫存的本國自然人戶數
#total4 → 1年內實動且有庫存的本國自然人戶數 + 客戶交易次數(Join total2 & tcount)
total4 <- sqldf("SELECT * 
          FROM total2 t LEFT JOIN tcount c ON t.CUS021 = c.account_no ")
class(total4)
total4 <- as.data.table(total4)

#total5 → 1年內實動且有庫存的本國自然人戶數 + 客戶交易次數+近一年客戶累計交易金額(Join total4 & tran)
total5 <- sqldf("SELECT * 
          FROM total4 t LEFT JOIN tran a ON t.CUS021 = a.account_no ")
class(total5)
str(total5)
total5 <- as.data.table(total5)

#########################################################################################################################
#交易次數分配
options(scipen=999)
summary(total5$TranCount)
range(total5$TranCount,na.rm = T)
quantile(total5$TranCount,na.rm = T)
IQR(total5$TranCount,na.rm = T)
sd(total5$TranCount,na.rm = T)
var(total5$TranCount,na.rm = T)
library(moments)
skewness(total5$TranCount,na.rm = T) #偏度(偏態值 > 0，為正偏態，分配集中在平均數以下，低分群的個體較多)
kurtosis(total5$TranCount,na.rm = T) #峰度(峰態係數K>0稱為高峻峰，峰態係數K=0稱為常態峰，峰態係數K<0稱為低闊峰)
table(total5$TranCount)

library(ggplot2)
ggplot(total5[which(total5$TranCount <= 30)], aes(TranCount))+
  geom_histogram(aes(y = ..count..))+
  labs(title = "2016/10-2017/09交易次數分配直方圖" , x = "一年內交易次數")



#交易總金額分配
options(scipen=999)
summary(total5$SumTran)
range(total5$SumTran,na.rm = T)
quantile(total5$SumTran,na.rm = T)
IQR(total5$SumTran,na.rm = T)
sd(total5$SumTran,na.rm = T)
var(total5$SumTran,na.rm = T)
library(moments)
skewness(total5$SumTran,na.rm = T) #偏度(偏態值 > 0，為正偏態，分配集中在平均數以下，低分群的個體較多)
kurtosis(total5$SumTran,na.rm = T) #峰度(峰態係數K>0稱為高峻峰，峰態係數K=0稱為常態峰，峰態係數K<0稱為低闊峰)
table(total5$SumTran)

library(ggplot2)
ggplot(total5[which(total5$SumTran >= 30000000 & total5$SumTran < 100000000)], aes(SumTran))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000)+
  labs(title = "2016/10-2017/09累計交易金額分配直方圖" , x = "一年內累計交易金額")


