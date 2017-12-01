#合併AUM及REV
f <- merge(x = AUM, y = REV, by.x=c("IDNO_AUM","CUSTNO_AUM"),by.y=c("IDNO_REV","CUSTNO_REV"), all = TRUE)
library(dplyr)
f <- full_join(x = AUM, y = REV,by = "CUSTNO_")