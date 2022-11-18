install.packages("ggplot2")
library(ggplot2)
install.packages('tidyverse')
library(tidyverse)
  
#graph size
siz<- 10


df_sql <- data.frame(x=rep(1:siz, each=siz), y=rep(1:siz, times=siz))#,color=rep(1:1, each=siz^2),mod=rep("ori", each=siz^2) )
(df_sql)

df_mod1 <- data.frame (x=c(5,5,5),y=c(6,7,8),color=rep(2:2, times= 3), mod=rep("mod", times= 3))
(df_mod1)

# df_entry <- rbind (df_sql,df_mod1)
# (df_entry)

merg <- left_join(df_sql,df_mod1, by=c("x", "y")) #%>% mutate(colo,1)

merg

df <- df_entry %>% arrange(x,y,mod) %>% distinct(x,y)
View(df)


ggplot(merg)+
  aes(x=x, y=y)+
  # geom_count(size=8,colour=merg$color)+
  geom_point(size=8,colour=merg$color)+
    scale_y_continuous(breaks = seq(0.5,siz-0.5, by=1))+
  scale_x_continuous(breaks = seq(0.5,siz-0.5, by=1))+
  theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"),
       panel.grid.major = element_line(size = 1, linetype = 'solid',colour = "white"),
       panel.grid.minor = element_blank(),
       axis.title=element_blank(),
       axis.text=element_blank(),
       axis.ticks=element_blank())

ggplot(merg)+
  aes(x=x, y=y)+
  geom_point(size=8,colour=merg$color)+
  scale_y_continuous(breaks = seq(0.5,siz-0.5, by=1))+
  scale_x_continuous(breaks = seq(0.5,siz-0.5, by=1))+
  theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 1, linetype = 'solid',colour = "white"),
        panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

ggplot(df_entry)+
  aes(x=x, y=y)+
  geom_count(size=8,colour=df_entry$color)+
  scale_y_continuous(breaks = seq(0.5,siz-0.5, by=1))+
  scale_x_continuous(breaks = seq(0.5,siz-0.5, by=1))+
  theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 1, linetype = 'solid',colour = "white"),
        panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
