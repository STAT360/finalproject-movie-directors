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

