####Visualization####

#Distribution using original train set file

library(ggplot2)

data = read_csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Midterm/Data/IMDB_data_Fall_2023.csv")

attach(data)

#histogram
hist_data <- hist(imdb_score, breaks = 20, plot = FALSE)
# Create the bar chart
hist(imdb_score, col = "#FFCC33", breaks = 20, main = "Distribution of IMDb Score", ylim = c(0, 450), xlim = c(0, 10))
# Add labels to each bar
text(hist_data$mids, hist_data$counts, labels = hist_data$counts, pos = 3, col = "black", cex = 0.7)
summary(imdb_score)

#check if is normal dist
shapiro.test(data$imdb_score)

testdata = read_csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Midterm/Data/test_data_IMDB_Fall_2023.csv")
attach(testdata)
# Combine the two data vectors into one
combined_data <- list(data$movie_budget, testdata$movie_budget)

# Create a single box plot for both datasets
boxplot(combined_data, col = "#FFCC33", names = c("Train Set", "Test Set"))
title("Movie Budgets Comparison")

summary(data$movie_budget)
summary(testdata$movie_budget)

detach(data)
detach(testdata)


#Exploratory on preprocessed dataset 
#Load the dataset
imdb_data = read.csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Midterm/Data/IMDB_data_Fall_2023.csv")
attach(imdb_data)

#1. Drop rows that are non-english, non-color, non usa /uk

data1 =  subset(imdb_data, language == "English" & 
                  (country == "USA" | country == "UK") &
                  colour_film == "Color")


#2. Drop columns which are not considered in the model

data2 = data1[, -c(1,2,3,6,10,11,13,23,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]


#3. Separate and dummify genres column

#install.packages("tidyr")
#install.packages("dplyr")
library(tidyr)
library(dplyr)
library(ggplot2)

# Preprocessing: Splitting the concatenated genres into separate rows
data3 <- data2 %>%
  separate_rows(genres, sep = "\\|")  # Splitting concatenated genres into separate rows

#4. Remove outlier rows

row_nums = c(1371, 349, 433, 1567, 281, 167, 166)
data3 = data3[-row_nums, ]


#5. Remove actor1_star_meter, actor2_star_meter, actor3_star_meter and 4 genre fields

col_names = c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter","Romance", "Mystery", "Musical", "Animation")
data3 = data3[, !(names(data3) %in% col_names)]

#6. Adjusting col name for Sci-Fi
colnames(data3)[colnames(data3) == "Sci-Fi"] <- "Sci_Fi"


#7. Combining actor names

top_actors <- c(
  "Robert De Niro", "Bill Murray", "Will Ferrell", "Meryl Streep", "Kevin Spacey", 
  "J.K. Simmons", "Scarlett Johansson", "Morgan Freeman", "Matt Damon", 
  "Julia Roberts", "Jason Statham", "Harrison Ford", "Denzel Washington", 
  "Tom Wilkinson", "Liam Neeson", "Johnny Depp", "Jake Gyllenhaal", 
  "Steve Buscemi", "Sylvester Stallone", "Robert Duvall", "Philip Seymour Hoffman", 
  "Matthew McConaughey", "Kirsten Dunst", "Keanu Reeves", "James Franco", 
  "Bruce Willis", "Anne Hathaway", "Steve Carell", "Ryan Gosling", 
  "Dennis Quaid", "Channing Tatum", "Christian Bale", "Tom Hanks", 
  "Stephen Root", "Robin Williams", "Robert Downey Jr.", "Nicolas Cage", 
  "Jim Broadbent", "Brad Pitt", "Bradley Cooper", "Zooey Deschanel", 
  "Scott Glenn", "Milla Jovovich", "Mila Kunis", "Kate Winslet", 
  "Jennifer Garner", "Gerard Butler", "Clint Eastwood", "Colin Firth"
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


#8. Grouping Directors

top_directors <- c(
  "Woody Allen", "Steven Spielberg", "Spike Lee", "Clint Eastwood", "Steven Soderbergh", 
  "Martin Scorsese", "Wes Craven", "Kevin Smith", "Francis Ford Coppola", 
  "Joel Schumacher", "Barry Levinson", "Bobby Farrelly", "F. Gary Gray"
)


data4$director <- ifelse(data4$director %in% top_directors, data4$director, "Others")

#9. Grouping Distributors

top_distributors <- c(
  "Warner Bros.", "Universal Pictures", "Paramount Pictures", "Twentieth Century Fox", 
  "Columbia Pictures Corporation", "New Line Cinema", "Buena Vista Pictures",
  "Miramax", "United Artists", "Metro-Goldwyn-Mayer (MGM)", "Fox Searchlight Pictures",
  "Summit Entertainment", "Lionsgate", "Focus Features", "Screen Gems", "Lions Gate Films"
)

data4$distributor <- ifelse(data4$distributor %in% top_distributors, data4$distributor, "Others")

#10. Grouping cinematographers

top_cinematographers <- c(
  "multiple", "Roger Deakins", "Mark Irwin", "John Bailey", "Matthew F. Leonetti", 
  "Robert Elswit", "Jack N. Green", "Andrew Dunn", "Don Burgess", "Dean Cundey", 
  "Robert D. Yeoman", "Phedon Papamichael", "Peter Deming", "Julio Macat", "Matthew Libatique"
)

data4$cinematographer <- ifelse(data4$cinematographer %in% top_cinematographers, data4$cinematographer, "Others")

#11. Grouping production comapnies

top_production_companies <- c(
  "Universal Pictures", "Paramount Pictures", "Columbia Pictures Corporation", "Warner Bros.", 
  "New Line Cinema", "Twentieth Century Fox", "Metro-Goldwyn-Mayer (MGM)", "Touchstone Pictures", 
  "Miramax", "DreamWorks", "Lionsgate", "Walt Disney Pictures", "Fox Searchlight Pictures", 
  "Dimension Films", "Focus Features", "Fox 2000 Pictures"
)

data4$production_company <- ifelse(data4$production_company %in% top_production_companies, data4$production_company, "Others")

#12. Updating values in maturity_rating

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


#Final cleaned dataset is stored in data4
attach(data4)
View(data4)

# Load necessary libraries
library(ggplot2)

# Plotting Genre Frequency
ggplot(data4, aes(y = genres)) +
  geom_bar(fill = "#FFCC33", color = "black") +
  labs(title = "Frequency of Movies by Genre", x = "Genre", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


install.packages("knitr")
install.packages("kableExtra")

library(kableExtra)
library(forcats)


# Grouping data by release month and calculating the mean IMDb score for each month
average_scores <- data4 %>%
  group_by(release_month) %>%
  summarise(mean_imdb_score = mean(imdb_score, na.rm = TRUE))

# Arrange the data by mean IMDb score in descending order
sorted_average_scores <- average_scores %>%
  arrange(desc(mean_imdb_score))

# Print the sorted average IMDb scores
print(sorted_average_scores)

# Create a table for IMDb score by month in HTML format
styled_table <- sorted_average_scores %>%
  kable("html") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Month" = 1, "Mean IMDb Score" = 1))

styled_table
library(forcats)

directors_average_scores <- data4 %>%
  group_by(director) %>%
  summarise(mean_imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(desc(mean_imdb_score)) %>%
  top_n(10)  # Selecting top 10 directors based on average IMDb score

# Plotting the top directors by mean IMDb score
ggplot(directors_average_scores, aes(x = mean_imdb_score, y = fct_reorder(director, mean_imdb_score))) +
  geom_col(fill = "grey", color = "black") +
  labs(x = "Mean IMDb Score", y = "Director", title = "Top 10 Directors by IMDb Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


# Plotting IMDB Score Vs Duration
ggplot(imdb_data, aes(x = duration, y = imdb_score)) +
  geom_point() +
  labs(x = "Duration", y = "IMDB Score", title = "Scatter Plot of IMDB Score vs. Duration")

# Plotting nb_faces Vs IMDB Score
ggplot(imdb_data, aes(x = nb_faces, y = imdb_score)) +
  geom_point() +
  labs(x = "nb_faces", y = "IMDB Score", title = "Scatter Plot of IMDB Score vs. nb_faces")

# PLotting Maturity_Rating Vs IMDB Score
ggplot(imdb_data, aes(x = maturity_rating, y = imdb_score)) +
  geom_point() +
  labs(x = "Maturity_Rating", y = "IMDB Score", title = "Scatter Plot of IMDB Score vs. Maturity_Rating")

detach(data4)
detach(imdb_data)

####Exploratory####

####Simple linear reg####
#Numeric
library(readr)

install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)

require(car)
install.packages("openxlsx")

imdb_data =  read_csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Midterm/Data/IMDB_data_Fall_2023.csv")

#1. Drop rows that are non-english, non-color, non usa /uk

data1 =  subset(imdb_data, language == "English" & 
                  (country == "USA" | country == "UK") &
                  colour_film == "Color")


#2. Drop columns which are not considered in the model
data2 = data1[, -c(1,2,3,6,13,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]


#3. Convert month to numeric
full_month_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
data2$release_month = match(data2$release_month, full_month_names)


#4. Separate and dummify genres column

data3 = separate_rows(data2, genres, sep = "\\|")
data3 = mutate(data3, category = 1)
data3 = pivot_wider(data3, names_from = genres, values_from = category, values_fill = 0)
attach(data3)

predictors <- c(
  "movie_budget", "release_month", "release_year", "duration", "nb_news_articles", "actor1_star_meter",
  "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro", "Drama", "Biography",
  "Sport", "Horror", "Thriller", "Crime", "Comedy", "Romance", "Adventure", "`Sci-Fi`", "Action",
  "Music", "Fantasy", "History", "Mystery", "Family", "War", "Musical", "Western", "Animation", "Documentary"
)

# Create an empty dataframe to store results
results_df <- data.frame(Predictor = character(0), R_squared = numeric(0), P_value = numeric(0), MSE = numeric(0))

# Loop through each predictor
for (predictor in predictors) {
  # Fit the linear model
  lm_model <- lm(as.formula(paste("imdb_score ~", predictor)), data = data3)
  
  # Calculate residuals
  residuals <- residuals(lm_model)
  
  # Calculate Mean Squared Error (MSE)
  mse <- mean(residuals^2)
  
  # Store R-squared, p-value, and MSE in the results dataframe
  results_df <- rbind(results_df, data.frame(Predictor = predictor, R_squared = summary(lm_model)$r.squared, P_value = summary(lm_model)$coefficients[2, 4], MSE = mse))
  
  # Print the summary
  print(summary(lm_model))
  cat("Predictor:", predictor, "MSE:", mse, "\n")
}

# Display the results dataframe
print(results_df)

# Define the list of predictors
predictors <- c(
  "movie_budget", "release_month", "release_year", "duration", "nb_news_articles", "actor1_star_meter",
  "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro", "Drama", "Biography",
  "Sport", "Horror", "Thriller", "Crime", "Comedy", "Romance", "Adventure", "`Sci-Fi`", "Action",
  "Music", "Fantasy", "History", "Mystery", "Family", "War", "Musical", "Western", "Animation", "Documentary"
)

# Create an empty dataframe to store results
results_df <- data.frame(Predictor = character(0), MSE = numeric(0))

# Define the number of folds for cross-validation
num_folds <- 10

# Loop through each predictor
for (predictor in predictors) {
  # Create an empty vector to store MSE values for each fold
  mse_values <- numeric(num_folds)
  
  for (fold in 1:num_folds) {
    # Randomly split the data into training and testing sets for each fold
    set.seed(fold)
    indices <- sample(1:nrow(data3), nrow(data3) * 0.8)
    train_data <- data3[indices, ]
    test_data <- data3[-indices, ]
    
    # Fit the linear model on the training data
    lm_model <- lm(as.formula(paste("imdb_score ~", predictor)), data = train_data)
    
    # Calculate residuals on the test data
    residuals <- residuals(lm_model, newdata = test_data)
    
    # Calculate Mean Squared Error (MSE) for this fold
    mse_values[fold] <- mean(residuals^2)
  }
  
  # Calculate the average MSE over all folds
  mse <- mean(mse_values)
  
  # Store R-squared, p-value, and MSE in the results dataframe
  results_df <- rbind(results_df, data.frame(predictor = predictor, MSE = mse))
  
  # Print the MSE
  cat("Predictor:", predictor, "MSE:", mse, "\n")
}

# Display the results dataframe
print(results_df)

non_linearity_df <- data.frame(Predictor = character(0), p_vals_nl = numeric(0))

for (predictor in predictors) {
  model <- lm(as.formula(paste("imdb_score ~", predictor)), data = data3)
  
  # Get the p-value from the model
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
  
  # Add the predictor and its p-value to the data frame
  non_linearity_df <- rbind(non_linearity_df, data.frame(p_vals_nl = p_value))
}

non_linearity_df

results_df <- data.frame(Predictor = character(0), p_val_hs = numeric(0), outliers = character(0))

for (predictor in predictors) {
  # Fit the linear model
  lm_model <- lm(as.formula(paste("imdb_score ~", predictor)), data = data3)
  
  p_hs_test = ncvTest(lm_model)$p
  
  outlier_result = outlierTest(lm_model)
  if(length(outlier_result)>0){
    outlier_df = data.frame(rstudent = outlier_result$rstudent)
    outlier_indexes = as.numeric(rownames(outlier_df))
    ol_list = paste(outlier_indexes, collapse = ", ")
  }
  else{ol_list=""}
  # Store R-squared, p-value, and MSE in the results dataframe
  results_df <- rbind(results_df, data.frame(Predictor = predictor, p_val_hs = p_hs_test, outliers = ol_list))
  
}

#file_path <- "/Users/chiaralu/Desktop/Courses/MGSC 661/Midterm/linear_issues_numeric.xlsx"
#require(openxlsx)
#write.xlsx(results_df, file_path, sheetName = "Sheet3", colNames = TRUE)


#Categorical
# Create an empty dataframe to store results
results_df <- data.frame(Predictor = character(0), R_squared = numeric(0), MSE = numeric(0))

# Define the number of folds for cross-validation
num_folds <- 10

# Loop through each predictor
for (predictor in categorical_predictors) {
  # Create an empty vector to store MSE values for each fold
  mse_values <- numeric(num_folds)
  
  for (fold in 1:num_folds) {
    # Randomly split the data into training and testing sets for each fold
    set.seed(fold)
    indices <- sample(1:nrow(data3), nrow(data3) * 0.8)  # Adjust the split ratio as needed
    train_data <- data3[indices, ]
    test_data <- data3[-indices, ]
    
    # Fit the linear model on the training data
    lm_model <- lm(as.formula(paste("imdb_score ~", predictor)), data = train_data)
    
    # Calculate residuals on the test data
    residuals <- residuals(lm_model, newdata = test_data)
    
    # Calculate Mean Squared Error (MSE) for this fold
    mse_values[fold] <- mean(residuals^2)
  }
  
  # Calculate the average MSE over all folds
  mse <- mean(mse_values)
  
  # Store R-squared, p-value, and MSE in the results dataframe
  results_df <- rbind(results_df, data.frame(Predictor = predictor, 
                                             R_squared = summary(lm_model)$r.squared,
                                             MSE = mse))
  
}

file_path <- "D:/Fall 2023/MGSC661/IMDb Challenge/cat_pred_output.xlsx"
require(openxlsx)
write.xlsx(results_df, file_path, sheetName = "Sheet1", colNames = TRUE)


#For all genres combined in the same model

genres_combined = colnames(data3)[20:40]

formula <- as.formula(paste("imdb_score ~", paste(genres_combined[1:21], collapse = "+")))

lm_model <- lm(formula, data = data3)
residuals <- residuals(lm_model)
mse <- mean(residuals^2)



#For heteroskedasticity and outliers


results_df <- data.frame(Predictor = character(0), p_val_hs = numeric(0), outliers = character(0))

for (predictor in categorical_predictors) {
  # Fit the linear model
  lm_model <- lm(as.formula(paste("imdb_score ~", predictor)), data = data3)
  
  p_hs_test = ncvTest(lm_model)$p
  
  outlier_result = outlierTest(lm_model)
  if(length(outlier_result)>0){
    outlier_df = data.frame(rstudent = outlier_result$rstudent)
    outlier_indexes = as.numeric(rownames(outlier_df))
    ol_list = paste(outlier_indexes, collapse = ", ")
  }
  else{ol_list=""}
  # Store R-squared, p-value, and MSE in the results dataframe
  results_df <- rbind(results_df, data.frame(Predictor = predictor, p_val_hs = p_hs_test, outliers = ol_list))
  
}


#For all genres combined in the same model

genres_combined = colnames(data3)[20:40]

formula <- as.formula(paste("imdb_score ~", paste(genres_combined[1:21], collapse = "+")))

#Heteroskedasticity
p_hs_test = ncvTest(lm_model)$p

#Outliers
outlier_result = outlierTest(lm_model)
outlier_df = data.frame(rstudent = outlier_result$rstudent)
outlier_indexes = as.numeric(rownames(outlier_df))
ol_list = paste(outlier_indexes, collapse = ", ")

residualPlots(lm_model)

detach(data3)


####Multiple linear reg####
df = read.csv("/Users/emilywu/Desktop/MMA/MGSC 661/Mid term project/train_IMDB_data.CSV")
names(df)
attach(df)

#dependent variable
summary(imdb_score)
hist(imdb_score)
boxplot(imdb_score)

#dummify categorical variables
categorical <- list("release_day", "release_month", "release_year", "language", "country","maturity_rating",
                    "distributor", "director", "actor1", "actor2", "actor3", "colour_film",
                    "plot_keywords", "cinematographer", "production_company")

for (i in categorical) {
  df[[i]] <- as.factor(df[[i]])
}
attach(df)
regtest = lm(imdb_score~release_month)
#successfully dummified categorical vars

#EDA: summary, boxplot, hist,
library(psych)
describe(df, skew=TRUE)

# Use the summary function and capture the output as a data frame
describe_df <- as.data.frame(describe(df, skew=TRUE))

# Specify the file path where you want to save the CSV file
csv_file_path <- "/Users/emilywu/Desktop/MMA/MGSC 661/Mid term project/describe_output.csv"

# Save the data frame to a CSV file
write.csv(describe_df, file = csv_file_path, row.names = TRUE)

#quant var
quantVar=df[,c(4,5,9,13,15,18,20,22,25,40)]

#correlation (part of detecting colinearity)
library(correlation)

correlation_matrix <- cor(quantVar)

# Print or view the correlation matrix
print(round(correlation_matrix,2))

heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Correlation Matrix Heatmap",
        xlab = "Variables",
        ylab = "Variables",
        cexRow=0.5,
        cexCol=0.5)

#VIF detecting colinearity
library(car)
quantVar <- df[, c(5, 9, 13, 15, 18, 20, 22, 25, 40)]
colinearity_reg <- lm(imdb_score ~ ., data = quantVar)
vif(colinearity_reg)

colinearity_reg <- lm(imdb_score ~ ., data = df[,c(5:42)])
vif(colinearity_reg)

#Heteroskedasticity: NCVtest p<0.05 is detected
eda_reg <- list()  # Create an empty list to store regression models

for (col_name in names(df)[5:42]) {
  formula <- as.formula(paste("imdb_score ~", col_name))
  model <- lm(formula, data = df)
  eda_reg[[col_name]] <- model
  cat("Column Name:", col_name, "\n")
  print(ncvTest(model))
}
ncvTest(lm(imdb_score~movie_meter_IMDBpro))
ncvTest(lm(imdb_score~cinematographer))
ncvTest(lm(imdb_score~production_company))
ncvTest(lm(imdb_score~crime))
ncvTest(lm(imdb_score~action))
ncvTest(lm(imdb_score~adventure))
ncvTest(lm(imdb_score~scifi))
ncvTest(lm(imdb_score~thriller))
ncvTest(lm(imdb_score~musical))
ncvTest(lm(imdb_score~romance))
ncvTest(lm(imdb_score~western))
ncvTest(lm(imdb_score~sport))
ncvTest(lm(imdb_score~horror))
ncvTest(lm(imdb_score~drama))
ncvTest(lm(imdb_score~war))
ncvTest(lm(imdb_score~animation))

#outlierTest
# Load the 'car' package for the 'outlierTest()' function
library(car)
# Create an empty list to store the outlier test results
outlier_tests <- list()
# Loop through the columns from names(df)[5:42]
for (col_name in names(df)[5:42]) {
  # Extract the column from the data frame
  column_to_test <- df[[col_name]]
  
  # Perform the outlier test
  test_result <- outlierTest(lm(imdb_score~column_to_test))
  
  # Store the test result in the list
  outlier_tests[[col_name]] <- test_result
}

# Print or inspect the test results
print(outlier_tests)


#simple regression R square:
# Create an empty vector to store R-squared values
rsquared_values <- vector('numeric', length = length(names(df)[5:42]))

# Loop through the columns from names(df)[5:42]
for (i in 1:length(names(df)[5:42])) {
  col_name <- names(df)[5:42][i]
  formula <- as.formula(paste("imdb_score ~", col_name))
  model <- lm(formula, data = df)
  rsquared_values[i] <- summary(model)$r.squared
}

# Create a data frame with one column for R-squared values
rsquared_df <- data.frame(RSquared = rsquared_values)

# Print the data frame
print(rsquared_df)

detach(df)


#Residual plots
imdb_data = read.csv("/Users/chiaralu/Desktop/Courses/MGSC 661/Midterm/Data/IMDB_data_Fall_2023.csv")
attach(imdb_data)

#1. Drop rows that are non-english, non-color, non usa /uk

data1 =  subset(imdb_data, language == "English" & 
                  (country == "USA" | country == "UK") &
                  colour_film == "Color")


#2. Drop columns which are not considered in the model

data2 = data1[, -c(1,2,3,6,13,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]


#3. Convert month to numeric

full_month_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
data2$release_month = match(data2$release_month, full_month_names)


#4. Separate and dummify genres column

install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)

data3 = separate_rows(data2, genres, sep = "\\|")
data3 = mutate(data3, category = 1)

data3 = pivot_wider(data3, names_from = genres, values_from = category, values_fill = 0)

#use data3
attach(data3)
colnames(data3)[colnames(data3) == "Sci-Fi"] <- "Sci_Fi"
predictor_rank <- c(
  "duration",
  "Drama",
  "nb_news_articles",
  "Comedy",
  "release_year",
  "Biography",
  "Horror",
  "Action",
  "History",
  "movie_meter_IMDBpro",
  "War",
  "nb_faces",
  "Family",
  "release_month",
  "Thriller",
  "Sci_Fi",
  "Crime",
  "Documentary",
  "movie_budget",
  "Western",
  "Fantasy",
  "Sport",
  "Music",
  "Adventure",
  "actor3_star_meter",
  "Animation",
  "actor1_star_meter",
  "Romance",
  "Musical",
  "Mystery",
  "actor2_star_meter"
)

# Create an empty list to store the formulas
formula_list <- list()

# Loop through the predictors to construct the formulas
for (i in 1:length(predictor_rank)) {
  predictors <- predictor_rank[1:i]
  formula <- as.formula(paste("imdb_score ~", paste(predictors, collapse = " + ")))
  formula_list[[i]] <- formula
}

# Check the list of formulas
print(formula_list)


# Create an empty list to store the regression models
mreg <- list()

# Loop through predictor variables and add one predictor at a time
for (i in 1:length(formula_list)) {
  # Fit the model with the updated formula
  model <- lm(formula_list[[i]], data = data3)
  
  # Store the model in the list with an appropriate name
  mreg[[i]] <- model
}


# Access a specific model, e.g., for "movie_budget"
movie_budget_model <- mreg[["movie_budget"]]

# Create an empty list to store MSE values
mse_mreg <- list()

# Loop through the models in mreg
for (i in 1:length(mreg)) {
  # Get the model
  model <- mreg[[i]]
  
  # Get the predicted values
  predicted_values <- predict(model, newdata = data3)
  
  # Calculate the residuals
  residuals <- data3$imdb_score - predicted_values
  
  # Calculate MSE
  mse <- mean(residuals^2)
  
  # Store the MSE in the mse_mreg list
  mse_mreg[[i]] <- mse
}

# Print or access the list of MSE values
print(mse_mreg)

library(car)
residualPlots(mreg[[31]])

detach(data3)


####Spline####

# Load the required libraries
library(lmtest)
library(plm)
library(car)
library(tidyr)
library(dplyr)
library(splines)
library(ggplot2)
require(methods)

# Load the dataset
imdb_data <- read.csv("C:/Users/jayac/OneDrive/Desktop/McGill/Multivariate/Midterm/IMDB_data_Fall_2023.csv")

# 1. Drop rows that are non-English, non-color, non USA/UK
data1 <- subset(imdb_data, language == "English" & 
                  (country == "USA" | country == "UK") &
                  colour_film == "Color")

# 2. Drop columns which are not considered in the model
data2 <- data1[, -c(1,2,3,6,10,11,13,23,26)]

# 3. Convert month to numeric
full_month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
data2$release_month <- match(data2$release_month, full_month_names)

# 4. Separate and dummify genres column
data3 <- separate_rows(data2, genres, sep = "\\|")
data3 <- mutate(data3, category = 1)
data3 <- pivot_wider(data3, names_from = genres, values_from = category, values_fill = 0)

# Define your list of numerical non-linear predictors
numerical_predictors <- c("movie_budget", "release_month", "release_year", "duration", "nb_news_articles", "nb_faces", "movie_meter_IMDBpro")

attach(data3)
# Create a list to store the MSE for each predictor and degree combination
mse_list <- list()

# Set the degrees to consider
degrees <- c(1, 2, 3, 4, 5)

# Set the number of knots for each spline
num_knots <- 3

# Set the number of folds for cross-validation
K <- 20

# Perform the K-fold cross-validation for each predictor
for (predictor in numerical_predictors) {
  quantiles <- quantile(data3[[predictor]], c(1/4, 2/4, 3/4))
  a1 <- quantiles[1]
  a2 <- quantiles[2]
  a3 <- quantiles[3]
  
  mse_matrix <- numeric(length(degrees))  # Store the MSE for each degree
  
  for (a in 1:5) {
    mse_fold <- rep(0, K)
    
    # Perform K-fold cross-validation
    for (fold in 1:K) {
      set.seed(fold)  # Ensure reproducibility of the fold split
      train_indices <- sample(1:nrow(data3), size = floor(0.8 * nrow(data3)))
      train_data <- data3[train_indices, ]
      test_data <- data3[-train_indices, ]
      
      # Construct the model formula with splines
      formula <- as.formula(paste("imdb_score ~ bs(", predictor, ", knots=c(", a1, ",", a2, ",", a3, "), degree=", a, ")"))
      
      model <- glm(formula, data = train_data)
      
      # Make predictions on the test set
      predicted <- predict(model, newdata = test_data)
      
      # Calculate the mean squared error
      mse_fold[fold] <- mean((test_data$imdb_score - predicted)^2)
    }
    
    # Calculate the average MSE across folds for this degree
    avg_mse <- mean(mse_fold)
    
    # Store the average MSE for this degree
    mse_matrix[a] <- avg_mse
  }
  
  # Store the MSE matrix for this predictor
  mse_list[[predictor]] <- mse_matrix
}

detach(train_data)
detach(test_data)
detach(data3)

####Polynomial reg####
require(ggplot2)
test_data <- read.csv("IMDB_data_Fall_2023.csv")

#Load the dataset
imdb_data <- read.csv("IMDB_data_Fall_2023.csv")
attach(imdb_data)

#1. Drop rows that are non-english, non-color, non usa /uk

data1 =  subset(imdb_data, language == "English" & 
                  (country == "USA" | country == "UK") &
                  colour_film == "Color")


#2. Drop columns which are not considered in the model

data2 = data1[, -c(1,2,3,6,10,11,13,23,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]


#3. Convert month to numeric

full_month_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
data2$release_month = match(data2$release_month, full_month_names)


#4. Separate and dummify genres column

library(tidyr)
library(dplyr)

data3 = separate_rows(data2, genres, sep = "\\|")
data3 = mutate(data3, category = 1)
data3 = pivot_wider(data3, names_from = genres, values_from = category, values_fill = 0)
colnames(data3)[colnames(data3) == "Sci-Fi"] <- "Sci_Fi"


#5. Dummifying categorical predictors

categorical_predictors = c('release_month','maturity_rating','distributor','director','actor1','actor2','actor3','cinematographer','production_company')

data3[categorical_predictors] = lapply(data3[categorical_predictors], as.factor)

data3 = data3[data3$imdb_score<=10,]

#Final cleaned dataset is stored in data3
attach(data3)

non_linear <- c(
  "duration",
  "nb_news_articles",
  "release_year",
  "movie_meter_IMDBpro",
  "nb_faces",
  "release_month",
  "movie_budget"
)

linear <- c(
  "actor1_star_meter",
  "actor2_star_meter",
  "actor3_star_meter")
dummy <- c(
  "Animation",
  "Romance",
  "Musical",
  "Mystery",
  "Western",
  "Fantasy",
  "Sport",
  "Music",
  "Adventure",
  "Documentary",
  "Thriller",
  "Sci_Fi",
  "Family",
  "Crime",
  "War",
  "Biography",
  "Horror",
  "Action",
  "History",
  "Drama",
  "Comedy"
)

# Create an empty list to store the formulas
formula_list <- list()

# Loop through the predictors to construct the formulas
for (i in 1:length(non_linear)) {
  predictors <- non_linear[i]
  sub_list <- list()
  for (k in 1:5){
    formula <- as.formula(paste("imdb_score ~",
                                paste(
                                  paste(linear, collapse = " +"
                                  ),paste(dummy,collapse = " +"),sep = "+"
                                ),
                                paste("+poly(",predictors,",",k,")",sep="")))
    sub_list[[k]] <- formula
  }
  formula_list[[i]]<-sub_list
}

mse <- list()

for (i in 1:length(formula_list)){
  
  fit1<-lm(formula_list[[i]][[1]],data=data3)
  fit2<-lm(formula_list[[i]][[2]],data=data3)
  fit3<-lm(formula_list[[i]][[3]],data=data3)
  fit4<-lm(formula_list[[i]][[4]],data=data3)
  fit5<-lm(formula_list[[i]][[5]],data=data3)
  print(anova(fit1,fit2,fit3,fit4,fit5))
  
  predicted_values1 <- predict(fit1, newdata=data3)
  predicted_values2 <- predict(fit2, newdata=data3)
  predicted_values3 <- predict(fit3, newdata=data3)
  predicted_values4 <- predict(fit4, newdata=data3)
  predicted_values5 <- predict(fit5, newdata=data3)
  
  residuals1 <- data3$imdb_score - predicted_values1
  residuals2 <- data3$imdb_score - predicted_values2
  residuals3 <- data3$imdb_score - predicted_values3
  residuals4 <- data3$imdb_score - predicted_values4
  residuals5 <- data3$imdb_score - predicted_values5
  
  pred_mses <- c(mean(residuals1^2),mean(residuals2^2),mean(residuals3^2),mean(residuals4^2),mean(residuals5^2))
  
  mse[[i]] <- pred_mses
}

mse

chosen_degree <- lm(imdb_score ~ actor1_star_meter + actor2_star_meter + actor3_star_meter + 
                      poly(duration,5)+poly(nb_news_articles,5)+poly(release_year,3)+poly(movie_meter_IMDBpro,5)+poly(nb_faces,1)+poly(release_month,2)+poly(movie_budget,1))

chosen_degree
predicted_chosen_degree <- predict(chosen_degree, newdata=data3)
residuals_chosen_degree <- data3$imdb_score - predicted_chosen_degree
print(mean(residuals_chosen_degree^2))

chosen_degree_dummy_formula <- as.formula(                                
  paste("imdb_score ~ actor1_star_meter + actor2_star_meter + actor3_star_meter + 
  poly(duration,5)+poly(nb_news_articles,5)+poly(release_year,3)+poly(movie_meter_IMDBpro,5)+poly(nb_faces,1)+poly(release_month,2)+poly(movie_budget,1)+ ",
        paste(dummy,collapse = "+"),sep="")
)
chosen_degree_dummy_formula
chosen_degree_dummy <- lm(chosen_degree_dummy_formula)
predicted_chosen_degree_dummy <- predict(chosen_degree_dummy, newdata=data3)
residuals_chosen_degree_dummy <- data3$imdb_score - predicted_chosen_degree_dummy
print(mean(residuals_chosen_degree_dummy^2))

detach(imdb_data)

















