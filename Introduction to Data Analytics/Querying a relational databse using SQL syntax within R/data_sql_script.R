#--------------------------------------------------------------------------------#
# Querying a relational database using SQL syntax within R
#--------------------------------------------------------------------------------#
# Author: Konstantinos I. Bougioukas
# Date: 2021-10-21



# libraries
library(here)
library(tidyverse)

library(DBI)
library(dbplyr)
library(RSQLite)




# Connect to the database ---------------------------------------
mammals <- DBI::dbConnect(RSQLite::SQLite(),
                          here("data", "portal_mammals.sqlite"))



# Explore that database -----------------------------------------

# mammals SQLite database can contain multiple tables (datasets)
dbListTables(mammals)



# List the fields (variables) in a particular table
dbListFields(mammals, "plots")
dbListFields(mammals, "surveys")
dbListFields(mammals, "species")






# SELECT statement -------------------------------------------------

## select the columns `record_id`, `year`, `species_id`, and `plot_id` 
## from the `surveys` table:

tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys")) %>% 
  head()





# WHERE clause (filtering rows) -------------------------------------

## Filtering with WHERE statement and using basic operators
tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys
                 WHERE species_id = 'DM'")) %>% 
  head()



## BETWEEN ... AND ....  operator
tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys
                 WHERE plot_id BETWEEN 3 AND 5")) %>% 
  head()


## IS NULL operator
tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys
                 WHERE species_id IS NULL")) %>% 
  head()


## IN operator
tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys
                 WHERE plot_id IN (1, 2, 7)")) %>% 
  head()




## OR operator
tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys
                 WHERE species_id = 'NL' OR species_id = 'PF' ")) %>% 
  head()




## OR with AND (NOTE: use parenthesis, SQL processes AND before OR)
tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys
                 WHERE (plot_id = 2 OR plot_id = 7) AND species_id = 'DM' ")) %>% 
  head()







# Additional useful expressions, `COUNT` and `LIMIT` ------------------

## COUNT species_id = 'DM'
tbl(mammals, sql("SELECT COUNT (species_id)
                 FROM surveys
                 WHERE species_id = 'DM'"))


## LIMIT (just a few rows)
tbl(mammals, sql("SELECT *
                 FROM surveys
                 LIMIT 10 "))

# with asterisk * we request all columns of the table



## WHERE and LIMIT (just a few rows for a particular year)
tbl(mammals, sql("SELECT *
                 FROM surveys
                 WHERE year = 1985 LIMIT 10 "))







# Sorting results sets with `ORDER BY` clause -----------------------

## ORDER BY species_id
tbl(mammals, sql("SELECT record_id, year, species_id, plot_id 
                 FROM surveys
                 ORDER BY species_id DESC")) %>% 
  head()






# Grouping Result Sets, `GROUP BY` ----------------------------------

## COUNT BY GROUP
tbl(mammals, sql("SELECT sex, COUNT(sex)
                 AS count FROM surveys GROUP BY sex
                 "))


## COUNT BY GROUP and use HAVING clause for selection 
tbl(mammals, sql("SELECT sex, COUNT(sex)
                 AS count FROM surveys GROUP BY sex
                 HAVING COUNT(sex) > 16000"))

