#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#______________FUNCTIONS________________

##Input data and setting
#G0 (input data)
d <-rand_mat
# nb gen to compute
n <- 299
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


#4 Clean up dataframe (remove NA values)
keep_cleaned <- complete.cases(df)
df_c <- df[keep_cleaned,]

#______________END FUNCTIONS________________


# Define UI for application that draws a tile graphes
ui <- fluidPage(


    # Application title
    titlePanel("JEU DE LA VIE"),
    
    
    column(4,
           wellPanel(
               sliderInput("gen",
                           "Number of generations:",
                           min = 0,
                           max = n,
                           value = n%/%2), #center the default value
                "Basal config (Generation 0)",
                plotOutput("gen_0"),
                width=4,
                style = "overflow-y:scroll; max-height: 90vh; position:relative;"
            )
    ),
    column(8,
           wellPanel( 
               
                textOutput("lab_gen"),
                textOutput("count_alive"),
                textOutput("percent_var"),
                style = "overflow-y:scroll; max-height: 10vh; position:relative;", 
                plotOutput("gen_grid"),
                style = "overflow-y:scroll; max-height: 80vh; position:relative;" 
               
           )
    )
)
    

# Define server logic required to draw the tile graphs
server <- function(input, output) {

    output$gen_0 <- renderPlot({
        
        ggplot(df_c[df_c$gen==0,],(aes(x,y)))+
            geom_tile(aes(fill=z),show.legend = FALSE,na.rm = TRUE, colour="black")+
            coord_equal()+
            scale_x_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            scale_y_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            theme_bw()+
            theme(axis.ticks = element_blank())
    }) 
    
    output$gen_grid <- renderPlot({
    
         ggplot(df_c[df_c$gen==input$gen,],(aes(x,y)))+
            geom_tile(aes(fill=z),show.legend = FALSE,na.rm = TRUE,fill='green',colour="red")+
            coord_equal()+
            scale_x_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            scale_y_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            theme_bw()+
            theme(axis.ticks = element_blank())
    }) 
    
    output$lab_gen <- renderText({
            glue("Generation {input$gen}")
    }) 
    
    output$count_alive <- renderText({
              df <- df_c[df_c$gen==input$gen,]
              glue("Number of living cells: {sum(df$z)}")
    })
    
    output$percent_var <- renderText({
        df <- df_c[df_c$gen==input$gen,]
        d0 <- df_c[df_c$gen==0,]
        glue("% of variation: {round(sum(df$z)*100/sum(d0$z),1)-100}")
    })
            
}


# Run the application 
shinyApp(ui = ui, server = server)
