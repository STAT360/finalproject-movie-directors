Reduced.List <- Movie_Data %>% 
  count(director_name, na.rm = TRUE) %>% 
  filter(n >= 6) %>%
  slice(-1)

ReducedMovies <- Movie_Data %>%
  semi_join(Reduced.List, by = "director_name")


multiple.mod = lm(gross ~ director_name + budget + duration + actor_1_name + content_rating + imdb_score + title_year, data = ReducedMovies)
coef(multiple.mod) 
anova(multiple.mod)

coef(multiple.mod)["Movie_Data$title_year"]
