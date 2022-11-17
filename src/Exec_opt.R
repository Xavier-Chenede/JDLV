### Compute generations and visualization

##Input data and setting
        #G0 (input data)
d <-rand_mat
        # nb gen to compute
n <- 49
        #automatic setting

## run generation iterations

        expand_gen(d,n)

        # Create array combining all matrices
        
                #1 Adjust all the matrices to the same size
        adjust(l)
       
                #2 initiate array
        aggreg <- array( dim=c(nrow(l[[1]]),ncol(l[[1]]),length(l)))
        
                #3 populate array
                for (i in 1:length(l)){
                        aggreg[,,i] <- l[[i]]
                }
        
    
        
##Plots generations 
require (glue)
        #convert array to ggplot compatibility

        #1 initialization
        df <- data.frame(gen=c(),x=c(),y=c(),z=c())

        #2 array z-dimension
        z_dim <- c(dim(aggreg))[3]

        #3 loop each z of the array
        
        
       y <-  system.time( 
                
                              
for(m in 1:z_dim){
        print (glue("generation {m-1} ongoing"))
        mtrx <- aggreg[,,m]
        # mtrx
        g  <- c()
        # x  <- c()
        # y0 <- c()
        y  <- c()
        z  <- c()
        mat_g <- c()
        mat_x <- c() 
        mat_y <- c()
        mat_z <- c()
        
        x <- c(1:ncol(mtrx))
        y0 <- c(nrow(mtrx):1)
        
        colnames(mtrx) <- x
        rownames(mtrx) <- y0
        
        for(j in 1:length(y0)){
                
                for(i in 1:length(x)){
                        add_z <- c(mtrx[y0[y0=j],x[i]])
                        z <- c(z,add_z)
                        y <- c(y,rev(y0)[y0=j])
                        g <- c(g,m-1)
                }
                
                mat_x <- c(mat_x,x) 
                mat_y <- c(mat_y,y)
                y <- c()
                mat_z <- c(mat_z,z)
                z <- c()
                mat_g <- c(mat_g,g)
                g <- c()
                
        }
        
        df_mtx <- data.frame(gen=mat_g,x=mat_x, y=mat_y, z=mat_z)
        df_mtx$s <- rep(sum(df_mtx$z, na.rm= TRUE), times= length(df_mtx$z))
        df <- rbind(df,df_mtx)
        df_mtx <- data.frame()
       
}
)

y


        #4 Clean up dataframe (remove NA values)
keep_cleaned <- complete.cases(df)
df_c <- df[keep_cleaned,]


        #animated plots
require(gganimate)


ggp <- ggplot(df_c,(aes(x,y)))+geom_tile(aes(fill=z),show.legend = FALSE,na.rm = TRUE)+
        scale_x_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
        scale_y_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
        theme_bw()+
        theme(axis.ticks = element_blank())



anim <- ggp + transition_states(gen,transition_length = 0,state_length = 2)+transition_time(gen) +
        labs(title = "Generation: {frame_time}")

animate(anim, duration = 10 , fps = 5, width = 600, height = 600, renderer = gifski_renderer())




