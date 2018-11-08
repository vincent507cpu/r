#This script is for a project titled "PROJECT: EXPLORING THE KAGGLE DATA SCIENCE SURVEY" on DataCamp.

# Loading necessary packages
library(tidyverse)

# Loading the data
responses <- read_csv("datasets/kagglesurvey.csv")

# Printing the first 10 rows
head(responses, n = 10)

# Printing the first respondents' tools and languages
responses$WorkToolsSelect

# Creating a new data frame called tools
tools <- data.frame(responses)

# Adding a new column to tools which splits the WorkToolsSelect column at the commas and unnests the new column
tools <- tools  %>% 
    mutate(work_tools = strsplit(as.character(WorkToolsSelect), ",")) %>%
    unnest(work_tools)

# Viewing the first 6 rows of tools
head(tools)

# Creating a new data frame
tool_count <- data.frame(tools)

# Grouping the data by work_tools, calculate the number of responses in each group
tool_count <- tool_count  %>% 
    group_by(work_tools)  %>% 
    summarise(sum = n())

# Sorting tool_count so that the most popular tools are at the top
tool_count <- tool_count %>%
    arrange(desc(sum))

# Printing the first 6 results
head(tool_count)

# Creating a bar chart of the work_tools column. 
# Arranging the bars so that the tallest are on the far right
ggplot(tool_count, aes(x = reorder(work_tools, sum), y = sum)) + 
    geom_bar(stat = "identity") +

# Rotating the bar labels 90 degrees
    theme(axis.text.x  = 
    element_text(angle=90, 
                 vjust=0.5,
                 hjust= 1))
                 
# Creating a new data frame called debate_tools
debate_tools <- data.frame(responses)

# Creating a new column called language preference, based on the conditions specified in the Instructions
debate_tools <- debate_tools  %>% 
   mutate(language_preference = case_when(
       grepl("R", WorkToolsSelect) & ! grepl("Python", WorkToolsSelect) ~ "R",
       ! grepl("R", WorkToolsSelect) & grepl("Python", WorkToolsSelect) ~ "Python",
       grepl("R", WorkToolsSelect) & grepl("Python", WorkToolsSelect) ~ "both",
       ! grepl("R", WorkToolsSelect) & ! grepl("Python", WorkToolsSelect) ~ "neither"
       ))

# Printing the first 6 rows
head(debate_tools)

# Creating a new data frame
debate_plot <- data.frame(debate_tools)

# Grouping by language preference and calculate number of responses
debate_plot <- debate_plot  %>% 
   group_by(language_preference)  %>% 
    summarise(num = n())  %>% 

# Removing the row for users of "neither"
    filter(!(language_preference %in% c("both", "neither")))

# Creating a bar chart
debate_plot %>%
    ggplot(aes(x = language_preference, y = num)) +
    geom_bar(stat = "identity")
    
# Creating a new data frame
recommendations <- data.frame(debate_tools)

# Grouping by language_preference and then LanguageRecommendationSelect
recommendations <- recommendations  %>% 
    group_by(language_preference, LanguageRecommendationSelect)  %>% 
    summarise(rec = n())

# Removing empty responses and include the top recommendations
recommendations <- recommendations %>%
    filter(!is.na(LanguageRecommendationSelect)) %>%
    arrange(language_preference, rec) %>%
    mutate(row_num = row_number(rec))  %>%
    filter(row_num > max(row_num)-4)
    
# Creating a faceted bar plot
ggplot(recommendations, aes(x = LanguageRecommendationSelect, y = rec)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ language_preference)

# Would R users find this statement TRUE or FALSE?
R_is_number_one = TRUE
