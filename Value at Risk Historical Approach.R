# Tugas Portofolio 
# Author : Wahyu Antoro
# Date : 10 Agustus 2021 




install.packages("quantmod")
install.packages("tseries")
install.packages("mosaic")
install.packages("hrbrthemes")

library (tidyverse)
library (quantmod)
library (tseries)
library (mosaic)
library (hrbrthemes)

# Mengambil data dari Yahoo finance
getSymbols("BBCA.JK",scr="yahoo",from="2015-01-01")

# Membuat data frame 
BCA=as.data.frame(BBCA.JK)
BCA$date=as.Date(rownames(BCA))

BCA.filtered=BCA %>% 
  select(date,BBCA.JK.Adjusted) %>% 
  arrange(date) %>% 
  mutate(
    return=(BBCA.JK.Adjusted-lag(BBCA.JK.Adjusted))/lag(BBCA.JK.Adjusted)
    
  ) %>% 
  filter(complete.cases(.))

# Melihat pergerakan harga saham BBCA

BCA.filtered %>% 
  ggplot(aes(x=date, y=BBCA.JK.Adjusted))+
  geom_line(color="blue")+
  labs(
    title="Pergerakan Harga Saham BCA", 
    caption="source: Yahoo Finance"
    
  )+
  theme_ipsum_tw()


# Melihat pergerakan return saham BBCA
BCA.filtered %>% 
  ggplot(aes(x=date, y=return))+
  geom_line(color="blue")+
  labs(
    title="Pergerakan Return Saham BCA", 
    caption="Sumber : data diolah"
    
  )+
  theme_ipsum_tw()

# VaR - Historical Approach

BCA.filtered %>% 
  ggplot(aes(x=return, y=stat(density)))+
  geom_histogram(bins=50,
                 col=("red"),
                 fill=I("blue"),
                 alpha=I(.5))+
  geom_density(col="red")+
  geom_vline(
    xintercept=quantile(BCA.filtered$return,probs=c(0.05)),
    color="blue",
    linetype="dashed",
    size=1
    
  )+
    labs(
      title="Value at Risk - Historical Approach",
      caption=paste0("VaR=" ,round(quantile(BCA.filtered$return, probs=c(0.05)),4))
      
    )+
    theme_ipsum_tw()
                   
                   

summary (BBCA.JK)
glimpse(BBCA.JK)



