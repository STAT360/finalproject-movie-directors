Director.Gross <- Movie_Data %>% 
  group_by(director_name) %>% 
  summarise(Mean.Gross = sum(gross, na.rm = TRUE), 
            Mean.Budget = sum(budget, na.rm = TRUE)) %>%
  mutate(Net = Mean.Gross - Mean.Budget) %>% 
  arrange(desc(Net))

Director.Gross <- Movie_Data %>% 
  group_by(director_name) %>% 
  summarise(Mean.Gross = mean(gross, na.rm = TRUE), 
            Mean.Budget = mean(budget, na.rm = TRUE)) %>%
  mutate(Net = Mean.Gross - Mean.Budget) %>% 
  arrange(desc(Net))


ggplot(PG13movies, aes(x = duration, y = gross, colour = imdb_score)) + 
  geom_point()



gross.predictor <- function(director_name, actor_1_name, budget = 50000000, duration = 120, content_rating = "PG-13", imdb_score = 7.5, title_year = 2019, data = regression.table) {

  budget.coef <-  as.numeric(regression.table %>% filter(term == "budget") %>% select("estimate"))
  duration.coef <-  as.numeric(regression.table %>% filter(term == "duration") %>% select("estimate"))
  director.coef <-  as.numeric(regression.table %>% filter(term == director_name) %>% select("estimate"))
  actor.coef <-  as.numeric(regression.table %>% filter(term == actor_1_name) %>% select("estimate"))
  rating.coef <-  as.numeric(regression.table %>% filter(term == content_rating) %>% select("estimate"))
  imdb.coef <-  as.numeric(regression.table %>% filter(term == "imdb_score") %>% select("estimate"))
  year.coef <-  as.numeric(regression.table %>% filter(term == "title_year") %>% select("estimate"))


  dollars <- (1938713878.08 + (director.coef) + (actor.coef) + (budget*budget.coef) + (duration*duration.coef) + (rating.coef) + (imdb_score*imdb.coef) + (title_year*year.coef))
    
  return(dollars)
}

gross.predictor("Christopher Nolan", 150000000, 145, "Tom Hardy", "PG-13", 8.2, 2019)
gross.predictor("Steven Spielberg", 50000000, 130, "Tom Cruise", "PG-13", 7.8, 2019)
gross.predictor("Steven Spielberg", 50000000, 125, "Tom Cruise", "PG-13", 7.8, 2019)  