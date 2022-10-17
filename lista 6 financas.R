library(lubridate)
library(dplyr)
library(writexl)
library(readxl)
ltn<-reareadd.csv2("C:/Users/Nathan/Downloads/PrecoTaxaTesouroDireto.csv")

pre_fix<-seq(1:nrow(ltn))
pre_fix<-ltn
j=1
for (i in 1:nrow(ltn)) {
  if(ltn[i,1] =='Tesouro Prefixado'){
    pre_fix[j,]<-ltn[i,]
    j<-j+1
  }
    
  
}
pre_fix<-pre_fix[1:j,]
pre_fix<-pre_fix[order(as.Date(pre_fix$Data.Base, format="%d/%m/%Y")),]
save(pre_fix, file='pre_fix.Rdata')
write_xlsx(pre_fix,'pre_fix.xlsx')
##########



pre_fix_NEW<-pre_fix[8921:nrow(pre_fix),]

############
pre_fix_1 <- matrix(0, nrow = nrow(pre_fix_NEW) , ncol = ncol(pre_fix)) 
 pre_fix_1<-as.data.frame(pre_fix_1) 
j<-1

for(i in 1:nrow(pre_fix_NEW)){
  if(month(as.Date(pre_fix_NEW[i+1,3], format = "%d/%m/%Y"))- month(as.Date(pre_fix_NEW[i,3], format = "%d/%m/%Y"))==1|
   month(as.Date(pre_fix_NEW[i+1,3], format = "%d/%m/%Y"))- month(as.Date(pre_fix_NEW[i,3], format = "%d/%m/%Y"))== -11){
    for(k in 0:5){
      if(year(as.Date(pre_fix_NEW[i-k,2], format = "%d/%m/%Y"))- year(as.Date(pre_fix_NEW[i-k,3], format = "%d/%m/%Y"))==1 
         & month(as.Date(pre_fix_NEW[i-k,2])) == 1 ){
        pre_fix_1[j,]<-pre_fix_NEW[i-k,]
     j<-j+1
      }
        
    } 
      
    }
     
  }
pre_fix_1<-pre_fix_1[1:j-1,]
colnames(pre_fix_1)<-colnames(pre_fix)

for (k in 1:nrow(pre_fix_1)) {
  if(month(as.Date(pre_fix_1[k+1,3], format = "%d/%m/%Y"))- month(as.Date(pre_fix_1[k,3], format = "%d/%m/%Y"))== 0){
    pre_fix_1[k+1,1] <- NA
  }

}
pre_fix_1<-pre_fix_1[complete.cases(pre_fix_1),]


write_xlsx(pre_fix_1,' pre_fix_1.xlsx')
write.csv(pre_fix_1, file = 'pre_fix_1.csv')
#########
pre_fix_2<-read.csv('C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 6/pre_fix_2.csv')
pre_fix_3<-read.csv2('C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 6/pre_fix_3.csv')
pre_fix_1<-read.csv('C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 6/pre_fix_1.csv')

pre_fix_1<-pre_fix_1[1:144,]
pre_fix_3<-pre_fix_3[1:144,]
spot_<-read.csv('C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 6/curvade juros.csv')
spot<-read_excel('C:/Users/Nathan/Downloads/spot.xlsx')


#forward_2_1<-log(pre_fix_1$PU.Compra.Manha) - log(pre_fix_2$PU.Compra.Manha)
#forward_2_1
#dif_spot_2_1<-(as.numeric(spot_[8:nrow(spot_),3]) - as.numeric(spot_[7:(nrow(spot_)-1),7]))/100


write.csv(spot,file= 'curvade juros.csv')
write.csv2(spot_, file = 'curva de juros certa.csv')
as.Date(spot_[8,2]) - as.numeric(spot_[7,7])


#########
#30/09/2022
datas_month<-as.character(seq( ymd("2010/06/30"),ymd("2022/09/30"), by= "month" ) )
datas_month<-rev(datas_month)

datas_month_1<-datas_month[-13]#30/09/2021 n tem
spot_[7:nrow(spot_),2]<-datas_month_1

datas_month_2<-datas_month[-145] #30/09/2010 n tem
spot_[7:nrow(spot_),6]<-datas_month_2

data_month_3 <- datas_month[-(112:119)] #31/06/2013 a 31/11/2014 n tem
spot_[7:(146),10]<-data_month_3 # 30/09/2021 n tem

###############
pre_fix_1$Data.Base[-c(9,141)]

forward_2_1<-log(pre_fix_1$PU.Compra.Manha[-c(9,141)]) - log(pre_fix_2$PU.Compra.Manha[-c(9,141)])

dif_spot_2_1<-(as.numeric(spot_[c((16:149), (151:(nrow(spot_)-1))),3]) - as.numeric(spot_[c((16:18),(20:(nrow(spot_)-1))),7]))/100

  