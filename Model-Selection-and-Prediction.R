#Install and Load Required Packages

#install.packages("tidyr")
#install.packages("dplyr")
library(tidyr)
library(dplyr)
library(boot)

#Import the Train and Test Datasets

imdb_data = read.csv("~/Desktop/Courses/MGSC 661/Midterm/Data/IMDB_data_Fall_2023.csv")
imdb_data_test = read.csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Midterm/Data/test_data_IMDB_Fall_2023.csv")


####PRE-PROCESSING FOR TRAIN DATASET

#1. Drop rows that are non-english, non-color, non usa /uk

data1 =  subset(imdb_data, language == "English" & 
                  (country == "USA" | country == "UK") &
                  colour_film == "Color")


#2. Drop columns which are not considered in the model

data2 = data1[, -c(1,2,3,6,10,11,13,23,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]


#3. Separate and dummify genres column

data3 = separate_rows(data2, genres, sep = "\\|")
data3 = mutate(data3, category = 1)
data3 = pivot_wider(data3, names_from = genres, values_from = category, values_fill = 0)

#4. Remove outlier rows

row_nums = c(1371, 349, 433, 1567, 281, 167, 166)
data3 = data3[-row_nums, ]


#5. Remove actor1_star_meter, actor2_star_meter, actor3_star_meter and 2 genre fields

col_names = c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter","Romance", "Musical")
data3 = data3[, !(names(data3) %in% col_names)]


#6. Adjusting col name for Sci-Fi
colnames(data3)[colnames(data3) == "Sci-Fi"] <- "Sci_Fi"


#7. Group Actors, Combine them to a single field and dummify

top_actors <- c(
  "Robert De Niro", "Bill Murray", "Will Ferrell", "Meryl Streep", "Kevin Spacey"
)

data3$actor1 <- ifelse(data3$actor1 %in% top_actors, data3$actor1, "Others")
data3$actor2 <- ifelse(data3$actor2 %in% top_actors, data3$actor2, "Others")
data3$actor3 <- ifelse(data3$actor3 %in% top_actors, data3$actor3, "Others")

data3$actor <- apply(data3[, c("actor1", "actor2", "actor3")], 1, function(x) paste(unique(na.omit(x)), collapse = "|"))
data3$actor <- gsub(" ", "", data3$actor)
data3$actor <- gsub("\\.", "", data3$actor)

data3 <- data3[, !(colnames(data3) %in% c("actor1", "actor2", "actor3"))]

data4 = separate_rows(data3, actor, sep = "\\|")
data4 = mutate(data4, category2 = 1)
data4 = pivot_wider(data4, names_from = actor, values_from = category2, values_fill = 0)


#8. Group Directors

top_directors <- c(
  "Woody Allen", "Steven Spielberg", "Spike Lee", "Clint Eastwood", "Steven Soderbergh"
)

data4$director <- ifelse(data4$director %in% top_directors, data4$director, "Others")


#9. Group Distributors

top_distributors <- c(
  "Warner Bros.", "Universal Pictures", "Paramount Pictures", "Twentieth Century Fox", 
  "Columbia Pictures Corporation")

data4$distributor <- ifelse(data4$distributor %in% top_distributors, data4$distributor, "Others")


#10. Group cinematographers

top_cinematographers <- c(
  "multiple", "Roger Deakins", "Mark Irwin", "John Bailey", "Matthew F. Leonetti")

data4$cinematographer <- ifelse(data4$cinematographer %in% top_cinematographers, data4$cinematographer, "Others")


#11. Group production companies

top_production_companies <- c(
  "Universal Pictures", "Paramount Pictures", "Columbia Pictures Corporation", "Warner Bros.", 
  "New Line Cinema"
)

data4$production_company <- ifelse(data4$production_company %in% top_production_companies, data4$production_company, "Others")


#12. Mapping for categories in maturity rating

data4 <- data4 %>%
  mutate(maturity_rating = case_when(
    maturity_rating %in% c("G", "Approved") ~ "PG",
    maturity_rating %in% c("X", "Passed") ~ "R",
    maturity_rating == "NC-17" ~ "R",
    maturity_rating %in% c("TV-G", "GP") ~ "PG",
    maturity_rating == "TV-14" ~ "PG-13",
    maturity_rating == "M" ~ "R",
    TRUE ~ maturity_rating
  ))


#13. Convert data type for all categorical predictors

categorical_predictors = c('release_month','maturity_rating','distributor','director',
                           'cinematographer','production_company')

data4[categorical_predictors] = lapply(data4[categorical_predictors], as.factor)


#14. Standardize all the numeric predictors

numeric_predictors = c("movie_budget","duration",
                       "nb_news_articles", "movie_meter_IMDBpro","nb_faces") 

for (col in numeric_predictors){
  data4[[col]] = scale(data4[[col]], center=TRUE) %>% as.vector()
}

#Define variables for combined actors and genres
actors_combined = colnames(data4)[33:38]
genres_combined = colnames(data4)[14:32]




####CREATE TRAIN-TEST SPLIT

set.seed(1)
train_ind <- sample(seq_len(nrow(data4)), size = floor(0.7 * nrow(data4)))

train_set <- data4[train_ind, ]
test_set <- data4[-train_ind, ]




####FIT THE FINAL SELECTED MODEL

formula = "imdb_score ~ poly(movie_budget, 1) + poly(duration, 2) + poly(nb_news_articles, 5) + 
poly(nb_faces, 1) + poly(movie_meter_IMDBpro,4) + director + maturity_rating + release_month + cinematographer + Drama + Biography + Sport + Horror + Thriller + 
  Crime + Comedy + Adventure + Sci_Fi + Action + Music + Fantasy + 
  History + Mystery + Family + War + Western + Animation + 
  Documentary + MerylStreep + RobertDeNiro + KevinSpacey + 
  BillMurray + Others" 

final_fit_lm = lm(formula,data=data4)
final_fit_glm = glm(formula, data=data4)
final_fit_train = lm(formula,data=train_set)

actual = test_set$imdb_score
prediction = predict(final_fit_train, test_set)
squared_error = (actual-prediction)^2
mse = mean(squared_error)
print(paste("MSE (Train-test Split): ",mse))

mse_cv = cv.glm(data = data4, glmfit = final_fit_glm, K = 20)$delta[1]
print(paste("MSE (k-fold CV): ",mse_cv))

print(paste("R-squared value: ",summary(final_fit_lm)$r.squared))



####PRE-PROCESSING FOR TEST DATASET

#1. Drop columns which are not considered in the model


data2t = imdb_data_test[, -c(1,2,3,6,10,11,13,23,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]

#2. Separate and dummify genres column

data3t = separate_rows(data2t, genres, sep = "\\|")
data3t = mutate(data3t, category = 1)
data3t = pivot_wider(data3t, names_from = genres, values_from = category, values_fill = 0)


#3. Remove actor1_star_meter, actor2_star_meter, actor3_star_meter and 2 genre fields

col_names = c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter","Romance", "Musical")
data3t = data3t[, !(names(data3t) %in% col_names)]

#4. Adjusting col name for Sci-Fi
colnames(data3t)[colnames(data3t) == "Sci-Fi"] <- "Sci_Fi"


#5. Group Actors, Combine them to a single field and dummify

data3t$actor1 <- ifelse(data3t$actor1 %in% top_actors, data3t$actor1, "Others")
data3t$actor2 <- ifelse(data3t$actor2 %in% top_actors, data3t$actor2, "Others")
data3t$actor3 <- ifelse(data3t$actor3 %in% top_actors, data3t$actor3, "Others")

data3t$actor <- apply(data3t[, c("actor1", "actor2", "actor3")], 1, function(x) paste(unique(na.omit(x)), collapse = "|"))
data3t$actor <- gsub(" ", "", data3t$actor)
data3t$actor <- gsub("\\.", "", data3t$actor)

data3t <- data3t[, !(colnames(data3t) %in% c("actor1", "actor2", "actor3"))]

data4t = separate_rows(data3t, actor, sep = "\\|")
data4t = mutate(data4t, category2 = 1)
data4t = pivot_wider(data4t, names_from = actor, values_from = category2, values_fill = 0)

#6. Group Directors

data4t$director <- ifelse(data4t$director %in% top_directors, data4t$director, "Others")

#7. Group Distributors

data4t$distributor <- ifelse(data4t$distributor %in% top_distributors, data4t$distributor, "Others")

#8. Group cinematographers

data4t$cinematographer <- ifelse(data4t$cinematographer %in% top_cinematographers, data4t$cinematographer, "Others")

#9. Group production companies

data4t$production_company <- ifelse(data4t$production_company %in% top_production_companies, data4t$production_company, "Others")

#10. Mapping for categories in maturity rating

data4t <- data4t %>%
  mutate(maturity_rating = case_when(
    maturity_rating %in% c("G", "Approved") ~ "PG",
    maturity_rating %in% c("X", "Passed") ~ "R",
    maturity_rating == "NC-17" ~ "R",
    maturity_rating %in% c("TV-G", "GP") ~ "PG",
    maturity_rating == "TV-14" ~ "PG-13",
    maturity_rating == "M" ~ "R",
    TRUE ~ maturity_rating
  ))

#11. Convert data type for all categorical predictors

data4t$release_month <- trimws(data4t$release_month)

categorical_predictors = c('release_month','maturity_rating','distributor','director',
                           'cinematographer','production_company')

data4t[categorical_predictors] = lapply(data4t[categorical_predictors], as.factor)

data4t$imdb_score = as.numeric(data4t$imdb_score)
data4t$movie_budget <- as.numeric(gsub("[, ]", "", data4t$movie_budget))

#12. Add all missing categories in the test data and set value=0

missing_columns <- setdiff(names(data4), names(data4t))
data4t[, missing_columns] <- 0

#13. Standardize all the numeric predictors

for (col in numeric_predictors){
  data4t[[col]] = scale(data4t[[col]], center=TRUE) %>% as.vector()
}


####PREDICTION ON NEW TEST DATASET

movie_name = c(imdb_data_test$movie_title)

pred <- predict(final_fit_lm, newdata = data4t)
pred_df <- data.frame(Movie=movie_name, Predicted_imdb_score = pred)



####PRINT FINAL RESULTS

print(paste("MSE (Train-test Split): ",mse))
print(paste("MSE (k-fold CV): ",mse_cv))
print(paste("R-squared value: ",summary(final_fit_lm)$r.squared))

print("Final Predictions on Test Dataset:")
print(pred_df)


#Outputs in stargazer 

library(stargazer)
mreg =  lm(imdb_score ~ poly(movie_budget, 1) + poly(duration, 2) + poly(nb_news_articles, 5) + 
             poly(nb_faces, 1) + poly(movie_meter_IMDBpro,4) + director + maturity_rating + release_month + cinematographer + Drama + Biography + Sport + Horror + Thriller + 
             Crime + Comedy + Adventure + Sci_Fi + Action + Music + Fantasy + 
             History + Mystery + Family + War + Western + Animation + 
             Documentary + MerylStreep + RobertDeNiro + KevinSpacey + 
             BillMurray + Others, data=data4)

stargazer(mreg, type="html")

