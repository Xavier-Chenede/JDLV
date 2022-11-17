
#Plots generation from array
        # nb_gen <- 5
        # plot(aggreg [ , ,1], main = 'Generation 0', xlab = "", ylab = "", key=NULL)
        # 
        # for (i in 1:(nb_gen+1)){
        #         
        #         png(file=here::here("graphs",glue('gen___{i-1}.png')), width = 800, height = 700)
        #         plot(aggreg [,,i],main = glue('Generation {i-1}'), xlab = "", ylab = "", key=NULL)
        #         dev.off()
        # }

#convert array to ggplot compatibility

#array z-dimension 
z_dim <- c(dim(aggreg))[3]

i=5

        matrix1 <- aggreg[,,i]
        x <- c()
        y0 <- c()
        y <- c()
        z <- c()
        mat_x <- c() 
        mat_y <- c()
        mat_z <- c()
        
        x <- c(1:ncol(matrix1))
        y0 <- c(nrow(matrix1):1)
        
        colnames(matrix1) <- x
        rownames(matrix1) <- y0
        
        for(j in 1:length(y0)){
                
                for(i in 1:length(x)){
                       add_z <- c(matrix1[y0[y0=j],x[i]])
                       z <- c(z,add_z)
                       print(length(z))
                       y <- c(y,rev(y0)[y0=j])
        
                }
                mat_x <- c(mat_x,x) 
                mat_y <- c(mat_y,y)
                y <- c()
                mat_z <- c(mat_z,z)
                z <- c()
                
                
        }

df <- data.frame(x=mat_x, y=mat_y, z=mat_z)

ggplot(df,(aes(x,y)))+geom_tile(aes(fill=z))
 