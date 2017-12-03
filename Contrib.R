library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  
Contrib <- sqlQuery(myconn, "select TDATE_ , MARKET_ , BHNO_ , CUSTNO_ , CUSTCKNO_ , 
                    AMT_ , FEE_ , DISCOUNT_ , EXPENSE_ , NET_ , IDNO_
                    from KGICONTRIB where TDATE_ > 201609 and MARKET_ =='1' or MARKET_ == 'R' ")

close(myconn) 

class(Contrib)
head(Contrib,10)
nrow(Contrib)
str(Contrib)
summary(Contrib)
library("data.table")
Contrib <- as.data.table(Contrib)

#BHNO_分支代碼轉字串
Contrib$BHNO_ <- as.character(Contrib$BHNO_)
#CUSTNO_帳號應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
library(stringr)
Contrib$CUSTNO_ <- str_pad(Contrib$CUSTNO_,"left",'0')
#CUSTCKNO_檢查碼轉字串
Contrib$CUSTCKNO_ <- as.character(Contrib$CUSTCKNO_)
#將BHNO_、CUSTNO_、CUSTCKNO_轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
Contrib$CUSTNO <- paste0(Contrib$BHNO_,Contrib$CUSTNO_,Contrib$CUSTCKNO_)
#ID由Factor轉字串
Contrib$IDNO_ <- as.character(Contrib$IDNO_)

tcount <- select(Contrib ,TDATE_ , MARKET_ , BHNO_ , CUSTNO_ , CUSTCKNO_ , 
                    AMT_ , FEE_ , DISCOUNT_ , EXPENSE_ , NET_ , IDNO_ , CUSTNO ) %>%   
          mutate( account_no = paste0(BHNO_ , CUSTNO_ , CUSTCKNO_ )) %>%
          group_by( account_no ) %>%
          summarise(SumAMT = sum(AMT_) , SumFEE = sum(FEE_) , SumDISCOUNT = sum(DISCOUNT_) , 
                    SumEXPENSE = sum(EXPENSE_) , SumNET = sum(NET_) %>%
          arrange(desc(SumNET))
#計算SUM(交易量)
#計算SUM(手續費)
#計算SUM(手續費折讓)
#計算SUM(應扣費用)
#計算SUM(淨收入)

CusContrib_4 <- sqldf("select * from CUS_4 c left join Contrib b on c.CUSTNO = b.CUSTNO")#CUS_4的完整帳號確認
CusContrib_4 <- as.data.table(CusContrib_4)
str(CusContrib_4)
summary(CusContrib_4)

#箱型圖
ggplot(CusContrib_4, aes(x = Gender, y = AMT_)) + 
  geom_boxplot() +
  labs(title = "客戶性別x交易量箱型圖" )

#客戶交易量的直方圖，依性別疊合檢視
qplot(Age, data = U.CUS_4, geom = "histogram", binwidth = 10,
      fill = Gender,main ="客戶年齡直方圖")

#客戶年齡的直方圖，依性別分開檢視
install.packages("lattice")
require(lattice)
histogram(x= ~ Age | Gender,  # 根據性別的條件，繪製客戶年齡的直方圖
          data= U.CUS_4,     
          xlab="U.CUS_4客戶年齡分配",  
          layout=c(2,1))       # 以2x1的方式呈現圖表


CusContrib_5 <- sqldf("select * from CUS_5 c left join Contrib b on c.CUSTNO = b.CUSTNO")#CUS_4的完整帳號確認
CusContrib_5 <- as.data.table(CusContrib_5)
str(CusContrib_5)
summary(CusContrib_5)
