###rules

# Une cellule possède huit voisins, qui sont les cellules adjacentes horizontalement,
# verticalement et diagonalement. À chaque itération, l'état d’une cellule est
# entièrement déterminé par l’état de ses huit cellules voisines, selon les règles
# suivantes :
# 
#
# rules#1=>une cellule vivante possédant deux ou trois cellules voisines vivantes le reste,
# sinon elle meurt
# rules#2=>une cellule morte possédant exactement trois cellules voisines vivantes devient
# vivante (elle naît) ;


# Input data
basal_struc0 <- m3

#"enrobage" => by default, add null row/column on the 4 sides of the matrice
col_na <- matrix(rep(NA, times=nrow(basal_struc0)))
basal_struc1 <- cbind(col_na,basal_struc0,col_na)

row_add <- matrix(ncol=ncol(basal_struc1))
basal_struc <- rbind(row_add, basal_struc1,row_add)

#create new empty result matrix (new generation = gen_next)
gen_next <- matrix(nrow=nrow(basal_struc), ncol=ncol(basal_struc))


#Function to generate the new generation of cells

newgen <- function (gen_0,cnt) {
        
        # rules#1
        for (r in 2:nrow(gen_0)-1){
                for (c in 2:ncol(gen_0)-1){
                        if (((sum(gen_0[r-1,c-1],
                                  gen_0[r-1,c],
                                  gen_0[r-1,c+1],
                                  gen_0[r,c+1],
                                  gen_0[r,c-1],
                                  gen_0[r+1,c-1],
                                  gen_0[r+1,c],
                                  gen_0[r+1,c+1],
                                  na.rm=TRUE)%in% c(2,3))) & (!is.na(gen_0[r,c]))) {
                                gen_next[r,c]=1
                        }
                }
                
        }
        
        # rules#2
        for (r in 2:nrow(gen_0)-1){
                for (c in 2:ncol(gen_0)-1){
                        if (((sum(gen_0[r-1,c-1],
                                  gen_0[r-1,c],
                                  gen_0[r-1,c+1],
                                  gen_0[r,c+1],
                                  gen_0[r,c-1],
                                  gen_0[r+1,c-1],
                                  gen_0[r+1,c],
                                  gen_0[r+1,c+1],
                                  na.rm=TRUE)==3)) & (is.na(gen_0[r,c])))  {
                                gen_next[r,c]=1
                        }
                }
                
        }
        gen_0 <<- gen_next
       

}

# regenerations reiteration

expand_gen <- function(nb_gen){
        print(glue("G0:"))
        print(basal_struc)
        aggreg <- array(gen_0, dim=c(5,5,1))
        
        for (i in 1:nb_gen){
                newgen(gen_0, nb_gen)
                # print(glue("G{i}:"))
                png(file=here::here("graphs",glue("gen___{i}.png")),width = 800, height = 700)
                plot(gen_0)
                dev.off()
        }
aggreg <<- array(c(aggreg,gen_0), dim=c(5,5,i+1))
print(aggreg)        
}


gen_0 <- basal_struc #initialize Gen_0
expand_gen(5)


plot(aggreg [,,1])


