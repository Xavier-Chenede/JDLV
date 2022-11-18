# install.packages('tidyverse')
 # library(tidyverse)
 # library(gganimate)

#### initial state ####

  # data
# base_mat <- read_csv2("C:/Users/fr43892k/OneDrive - Sanofi/CSO/R/Mes projets/Jeux de la vie/gliders cannon.csv",
#                  col_names = TRUE,n_max = 12)
# 
# base_mat <- read_csv2("C:/Users/fr43892k/OneDrive - Sanofi/CSO/R/Mes projets/Jeux de la vie/inline_10.csv",
#                       col_names = TRUE,n_max = 12)
# 
# base_mat <- read_csv2("C:/Users/fr43892k/OneDrive - Sanofi/CSO/R/Mes projets/Jeux de la vie/Alea20_20.csv",
#                       col_names = TRUE,n_max = 20)
 
# base_mat <- read_csv2("C:/Users/fr43892k/OneDrive - Sanofi/CSO/R/Mes projets/Jeux de la vie/Alea30_30.csv",
#                       col_names = TRUE,n_max = 30)

base_mat <- read_csv2("C:/Users/fr43892k/OneDrive - Sanofi/CSO/R/Mes projets/Jeux de la vie/glider.csv",
                 col_names = TRUE,n_max = 12)

nb_it <- 1


      # base_mat

  #basal dimension
  nb_x=ncol(base_mat)
  nb_y=nrow(base_mat)
  

  # matrix conversion 
mat0 <- matrix(data.matrix(base_mat, rownames.force = NA),nb_y,nb_x)#,dimnames=list(1:nb_x,1:nb_y))
      # mat0


  #initiate the iteration table with basal state (for graph purpose)

         #create a vector for df injection
pre_iteration <- as.vector(t(mat0))

        #dataframe creation (y are inverted because order y value in matrix are the opposite of the graphe !)
Iteration <- data.frame(x=rep(1:nb_x,times=nb_y), y=rep(nb_y:1, each=nb_x), z=pre_iteration,ite = rep(0,times=length(pre_iteration)))

seq(-0.5, 20.5, by=1)

# Check view********************************************************
# mat0
test <- ggplot(data = Iteration) +
geom_tile(mapping = aes(x = x, y = y,fill = z, group = ite))+
scale_x_discrete(labels=seq(-0.5, 20.5, by=1))+
  
ggtitle("STATE 0") +
# coord_cartesian(xlim = c(-0.5, 20.5), ylim = c(-0.5, 20.5)) +
  
  
theme(axis.ticks       = element_blank(),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "black"),
      panel.grid.minor = element_blank() 
      
      )
test
# end Check view*****************************************************

#### Generation of further states ####

  # Main loop start, i is the number of state to generate
for (i in 1:nb_it){
  
  # sum surrounding col/row 
  
  bef_lstx <- ncol(mat0)-1 
  bef_lsty <- nrow(mat0)-1
  
  print(bef_lstx)
  print(bef_lsty)
  

  
  
  lrs <- sum(mat0[bef_lsty,]) + sum(mat0[nrow(mat0),])
  frs <- sum(mat0[1,])+sum(mat0[2,])
  lcs <- sum(mat0[,bef_lstx]) + sum(mat0[,ncol(mat0)])
  fcs <- sum(mat0[,1])+sum(mat0[,2])

#   print (paste("2 last rows: " , lrs))
#   print (paste("2 first rows: ", frs))
#   print (paste("2 last cols: " , lcs))
#   print (paste("2 first cols: ", fcs))

  if (lrs > 0) {
    mat0 <- rbind(mat0, rep(0:0, times=ncol(mat0)))
    mat0 <- rbind(mat0, rep(0:0, times=ncol(mat0)))  
  }
  
  if (lcs >0) {
    mat0 <- cbind(mat0, rep(0:0, times=nrow(mat0))) 
    mat0 <- cbind(mat0, rep(0:0, times=nrow(mat0)))  
  }
  
  if (frs>0) {
    mat0 <- rbind(rep(0:0, times=ncol(mat0)),mat0)
    mat0 <- rbind(rep(0:0, times=ncol(mat0)),mat0)  
  }
  
  if (fcs >0) {
    mat0 <- cbind(rep(0:0, times=nrow(mat0)),mat0) 
    mat0 <- cbind(rep(0:0, times=nrow(mat0)),mat0)  
  }
  

  # max_j and max_k to exclude edgeline from calculation
  max_j <- nrow(mat0)-1 
  max_k <- ncol(mat0)-1

  # generate empty matrix for new state
  new_mat <- matrix(rep(0:0, times= (nrow(mat0)*ncol(mat0))), nrow=nrow(mat0), ncol=ncol(mat0))
  
  
  # mat0 scanning and calculation of new values in new_mat
  for (j in 2:max_j) {
    for (k in 2:max_k){

    # ====CALC STAT n+1====
      #manage alive cells
      if (mat0[j,k]==1) {
        
        if (sum(mat0[j-1,k-1],mat0[j,k-1],mat0[j+1,k-1],mat0[j-1,k],mat0[j+1,k],mat0[j-1,k+1],mat0[j,k+1],mat0[j+1,k+1] ) %in% c(2,3)) {
          new_mat[j,k]=1
        } else {
          new_mat[j,k]=0
        }
        
      #manage death cells
       } else {
        
        if (sum(mat0[j-1,k-1],mat0[j,k-1],mat0[j+1,k-1],mat0[j-1,k],mat0[j+1,k],mat0[j-1,k+1],mat0[j,k+1],mat0[j+1,k+1] ) ==3 ) {
          new_mat[j,k]=1
        } else {
          new_mat[j,k]=0
        }
      }
    # ====END CALC====
    }
  }
  

  # print (paste("round ",i,sep=""))
  

  # reinitialize matrix name to reprocess data at state n+1
  mat0 <- new_mat

  #create a vector for df injection and that will be reprocessed for next step
  new_it <- as.vector(t(new_mat))
  
  #create the new df to inject
    #redim
  nb_y=nrow(new_mat)
  nb_x=ncol(new_mat)
  
  current_it <- data.frame(x=rep(1:nb_x,times=nb_y), y=rep(nb_y:1, each=nb_x), z=new_it,ite = rep(i,times=length(new_it)))

  #combine all iteration in final df
  Iteration <- rbind(Iteration, current_it)
  
  
  # Correction of the coordinate if needed ==> Here is the galère :[
  
    
    print (paste("STATE#",i,",nb row: ",bef_lsty+1,", nb col: ",bef_lstx+1,sep=""))
    
          
          # print (paste("nb row mat_0 during round ",i,": ",bef_lsty+1,sep=""))
          # print (paste("nb row new_mat 0 during round ",i,": ",nrow(new_mat),sep=""))
          # print (paste("nb col mat_0 during round ",i,": ",bef_lstx+1,sep=""))
          # print (paste("nb col new_mat 0 during round ",i,": ",ncol(new_mat),sep=""))
  
  diff_row <-  nb_y - (bef_lsty+1)
  diff_col <-  nb_x - (bef_lstx+1)
  # if (diff_row>0){Iteration <- Iteration %>% mutate(y2=ifelse(ite<i,y+2,y)) %>% select(x,y2,z,ite) %>% rename("y"="y2")} 
  # if (diff_col>0){Iteration <- Iteration %>% mutate(x2=ifelse(ite<i,x+2,x)) %>% select(x2,y,z,ite) %>% rename("x"="x2")} 
  # 
  # 

  
  # Check value row 2, last before row,  col 2, last before col  
  # lbrs <- sum(mat0[nb_y-1,])
  # ro2s <- sum(mat0[2,])
  # lbcs <- sum(mat0[,nb_x-1])
  # co2s <- sum(mat0[,2])
  
  lbrs <- sum(mat0[nb_y,])
  ro2s <- sum(mat0[1,])
  lbcs <- sum(mat0[,nb_x])
  co2s <- sum(mat0[,1])
  
  
  
  # if (diff_row>0 & ro2s>0){Iteration <- Iteration %>% mutate(y2=ifelse(ite<i,y+1,y)) %>% select(x,y2,z,ite) %>% rename("y"="y2")}
  if (diff_row>0 & lbrs>0){Iteration <- Iteration %>% mutate(y2=ifelse(ite<i,y-1,y)) %>% select(x,y2,z,ite) %>% rename("y"="y2")}
  # if (diff_col>0 & co2s>0){Iteration <- Iteration %>% mutate(x2=ifelse(ite<i,x+1,x)) %>% select(x2,y,z,ite) %>% rename("x"="x2")}
  if (diff_col>0 & lbcs>0){Iteration <- Iteration %>% mutate(x2=ifelse(ite<i,x-1,x)) %>% select(x2,y,z,ite) %>% rename("x"="x2")}
  
  
  

  
  
  
  
        # print(paste ("diff_row=",diff_row, "at iteration# ",i))
        # print(paste ("diff_col=",diff_col, "at iteration# ",i))
        # print(paste ("nb_col=",nb_y,"bef_lstx+1=",bef_lstx+1, "at iteration# ",i))
      
        # og_view_data <- Iteration %>% filter(ite==i)
        # check <- ggplot(data =  og_view_data) +
        #   geom_tile(mapping = aes(x = x, y = y,fill = z, group = ite))
        # check
        
  
}  #end main loop

# Check view********************************************************
# ggplot(data = Iteration) +
#   geom_tile(mapping = aes(x = x, y = y,fill = z, group = ite)) +
#   facet_wrap(~ite)


# Animation Launching********************************************************


view_data <- Iteration %>% filter(z==1) 
View(view_data)


test <- ggplot(data = view_data) +
  geom_tile(mapping = aes(x = x, y = y,fill = ite, color = ite))


test +  transition_time(ite)+
transition_states(ite, transition_length = 1,state_length = 1)
# +
# labs(title = paste0("State: {frame_time} / ",i))



# 
# 
# animate(test, nframes=150)
# 
# 
# 
# ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop)) +
#   geom_point() +
#   geom_smooth(aes(group = year), 
#               method = "lm", 
#               show.legend = FALSE) +
#   facet_wrap(~continent, scales = "free") +
#   scale_x_log10() +
#   transition_manual(year)

