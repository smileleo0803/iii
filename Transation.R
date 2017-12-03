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
