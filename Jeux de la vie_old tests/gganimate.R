
# install.packages('gganimate')
library(gganimate)

# install.packages('gifski')
library(gifski)


# install.packages('gapminder')
library(gapminder)

theme_set(theme_bw())
head(gapminder)


p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p + transition_time(year) +
  labs(title = "Year: {frame_time}")









View(Iteration)


view_data <- Iteration %>% filter(z==1) #%>% rename('year'='ite')



test <- ggplot(data = view_data) +
  geom_tile(mapping = aes(x = x, y = y,fill = ite, color = ite))

test

test +  transition_time(ite) 
# +
#   labs(title = "Year: {frame_time}")

help(transition_time)
