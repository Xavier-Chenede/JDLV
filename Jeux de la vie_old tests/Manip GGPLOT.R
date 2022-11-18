# install.packages('tidyverse')
# library(tidyverse)

#basic graph and data


lab=rep(c("A", "B","C"), times=4)  
lab

essai <- data.frame(x_val = 1:12, y_val = rep(5:10, each=2 ), status =rep(1, times=12) ,colo= 1:12, siz= rep(5:10, each=2 ), lab=lab)
essai
                    
ggplot(essai)+     
  aes(x=x_val, y = y_val)+ 
  geom_point(colour=essai$colo, size = 10)+
  geom_text(aes(x=x_val, y = y_val, label=status))+
  scale_y_continuous(breaks = 0.5:12.5, limits = c(0.5, 12.5))+
  scale_x_continuous(breaks = 0.5:12.5, limits = c(0.5, 12.5))+
  theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 1, linetype = 'solid',colour = "white"),
      panel.grid.minor = element_blank())
#
#       axis.title=element_blank(),
#       axis.text=element_blank(),
#       axis.ticks=element_blank())


# Data wo
#cell ()

nghb <- matrix(nrow = 3,ncol=3)
nghb[2,2]= NULL

nghb
