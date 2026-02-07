# 1. Data set Selection & Preparation


installed.packages("readxl")
installed.packages("tidyverse")
installed.packages("janitor")

library(readxl)
library(tidyverse)
library(janitor)


# Load data set
df <- read_excel("D:\\2nd Year\\R\\R Coursework\\Cancer_Data.xlsx")

# Clean column names
df <- df %>% clean_names()

# Remove irrelevant columns
df <- df %>% select(
  -id,
  -starts_with("unnamed")
)

# Convert diagnosis to factor
df$diagnosis <- factor(df$diagnosis, levels = c("B", "M"))

# Check missing values
print("Missing values per column:")
print(colSums(is.na(df)))

# Remove duplicates
df <- df %>% distinct()

# Summary of data set
print("Dataset Summary:")
print(summary(df))



# 2. Feature Engineering

# Function to cap outliers 

cap_outliers <- function(df) {
  df_num <- df[, sapply(df, is.numeric)]  # select numeric columns
  Q1 <- apply(df_num, 2, quantile, 0.25) 
  Q3 <- apply(df_num, 2, quantile, 0.75) 
  IQR_val <- Q3 - Q1                      
  
  # lower bound
  lower <- Q1 - 1.5 * IQR_val  
  # upper bound
  upper <- Q3 + 1.5 * IQR_val            
  
  # Track capped values per row
  capped_count <- rep(0, nrow(df))
  
  # Cap values outside bounds
  for (col in names(df_num)) {
    below <- df_num[[col]] < lower[col]
    above <- df_num[[col]] > upper[col]
    # count capped values
    capped_count <- capped_count + below + above 
    
    
    df_num[[col]][below] <- lower[col]
    df_num[[col]][above] <- upper[col]
  }
  
  # replace numeric columns
  df[, sapply(df, is.numeric)] <- df_num 
  return(list(df=df, capped_count=capped_count))
}

# Apply capping
result <- cap_outliers(df)
capped_df <- result$df
capped_count <- result$capped_count

cat("Total rows: ", nrow(df), "\n")
cat("Rows with capped values: ", sum(capped_count > 0), "\n")
cat("Total capped values: ", sum(capped_count), "\n")

# Scale numeric columns
scaled_df <- capped_df
scaled_df[, sapply(capped_df, is.numeric)] <- scale(capped_df[, sapply(capped_df, is.numeric)])

scaled_df$diagnosis <- ifelse(scaled_df$diagnosis == "M", 1, 0)

View(scaled_df)

