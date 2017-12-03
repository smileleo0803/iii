library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  
Contrib <- sqlQuery(myconn, "select TDATE_ , MARKET_ , BHNO_ , CUSTNO_ , CUSTCKNO_ , 
                    AMT_ , FEE_ , DISCOUNT_ , EXPENSE_ , NET_ , IDNO_
                from KGICONTRIB where TDATE_ > 201609 and (MARKET_ =='1' or MARKET_ == 'R') ")

close(myconn) 

class(Contrib)
head(Contrib,10)
nrow(Contrib)
str(Contrib)
summary(Contrib)
library("data.table")
Contrib <- as.data.table(Contrib)

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

CusContrib <- sqldf("select * from CUS left join Contrib on  ")
str(CusContrib)
summary(CusContrib)