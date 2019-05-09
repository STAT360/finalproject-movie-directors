Director.Gross <- Movie_Data %>% 
  filter(Movie_Data$content_rating == "PG-13")

ggplot(PG13movies, aes(x = duration, y = gross, colour = imdb_score)) + 
  geom_point()



Director.Average.Gross <- function(director_name, gross) {
  
}