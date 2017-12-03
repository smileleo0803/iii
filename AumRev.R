library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  
AUM <- sqlQuery(myconn, "select IDNO_ , CUSTNO_ , BTYPE_ , TYPE1_ , TYPE2_ , VALUE_ 
                from KGIAUM0M where TDATE_ = 20170930 ")  

REV <- sqlQuery(myconn, "select IDNO_ , CUSTNO_ , BTYPE_ , TYPE1_ , TYPE2_ , VALUE_ 
                from KGIREV0M where TDATE_ = 20170930 ") 
close(myconn) 

colnames(AUM) = c('IDNO_AUM' , 'CUSTNO_AUM' , 'BTYPE_AUM' , 'TYPE1_AUM' , 'TYPE2_AUM' , 'VALUE_AUM')
colnames(REV) = c('IDNO_REV' , 'CUSTNO_REV' , 'BTYPE_REV' , 'TYPE1_REV' , 'TYPE2_REV' , 'VALUE_REV')

install.packages("data.table")
library("data.table")
AUM <- as.data.table(AUM)
REV <- as.data.table(REV)

class(AUM)
str(AUM)



AUM$CUSTNO_ <- as.character(AUM$CUSTNO_)
REV$CUSTNO_ <- as.character(REV$CUSTNO_)


AUM$CUSTNO_ <- as.factor(AUM$CUSTNO_)
REV$CUSTNO_ <- as.factor(REV$CUSTNO_)

str(AUM)
str(REV)
head(AUM[,1:3])
