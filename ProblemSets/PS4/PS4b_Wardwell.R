# Load the tidyverse and sparklyr libraries

library(tidyverse)
library(sparklyr)

# Set up a connection to Spark
sc <- spark_connect(master = "local") 

# Create a dataframe of iris data
df1 <- as_tibble(iris)

# Copy the dataframe df1 to Spark
df <- copy_to(sc, df1)

#Verify the dataframes are different types. What class is each? Are any column names different across the two object? If so, why?
class(df1) 
class(df)

# Result class(df1) = "tbl_df", "tbl", "data.frame"
# Result class (df) = "tbl_spark" "tbl_sql"   "tbl_lazy"  "tbl"

# To see all column names side by side for comparison
data.frame(
  df1_columns = colnames(df1),
  df_columns = colnames(df)
)

# Result column names in the spark dataframe have "_" separators instead of "." separators. For example, "Sepal_Length" instead of "Sepal.Length". According to claude.ai, this is because Spark uses SQL naming conventions, and because "." has a special meaning in SWL, Spark is converting the "." to underscores.

# Use SQL command 'select'
df %>% select(Sepal_Length,Species) %>% head %>% print

# Use RDD operation 'filter'
df %>% filter(Sepal_Length>5.5) %>% head %>% print

# Combine the 'select' and 'filter' operations into one line using the dplyr pipeline
df %>% 
  select(Sepal_Length, Species) %>% 
  filter(Sepal_Length > 5.5) %>% 
  head() %>% 
  print()

# Use the 'groupby' operation to compute the average sepal length, number of observations, by each of the 3 iris species.
 df2 <- df %>% 
	group_by(Species) %>% 
	summarize(mean = mean(Sepal_Length), count = n()) %>% 
	head %>% 
	print

# Use the 'arrange' operation on the same grouped by information to sort by Sepal_Length.

df2 %>% arrange(Species) %>% head %>% print

# See screenshots for table results