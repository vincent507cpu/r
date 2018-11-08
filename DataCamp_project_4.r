#This script is for a project titled "PROJECT: WRANGLING AND VISUALIZING MUSICAL DATA" on DataCamp.

# Loading the tidyverse meta-package
library(tidyverse)

# Reading in the McGill Billboard chord data
bb <- read_csv('datasets/bb_chords.csv')

# Taking a look at the first rows in bb
bb[1,]

# Counting the most common chords
bb_count <- bb %>%
    count(chord, sort = TRUE)
#another way to do this (my way, not the one DataCamp provided):
# Counting the most common chords
#bb_count <- bb %>%
#  group_by(chord) %>%
#  summarise(n = n()) %>%
#  arrange(desc(n))

# Displaying the top 20 chords
bb_count[1:20,]
#or
#head(bb_count, n =20)

# Creating a bar plot from `bb_count`
bb_count %>%
  slice(1:20) %>%
  mutate(share = n / sum(n),
         chord = reorder(chord, share)) %>%
  ggplot(aes(x = chord, y = share, fill = chord)) +
  geom_col() +
  coord_flip() +
  xlab("Chord") +
  ylab("Share of total chords") +
  theme(legend.position='none')

# Wrangling and counting bigrams
bb_bigram_count <- bb %>%
  mutate(next_chord = lead(chord), next_title = lead(title), 
         bigram = str_c(chord, next_chord, sep = " ")) %>%
  filter(title == next_title) %>% 
  count(bigram, sort = TRUE)

# Displaying the first 20 rows of bb_bigram_count
head(bb_bigram_count, n = 20)

# Creating a column plot from `bb_bigram_count`
bb_bigram_count %>%
  slice(1:20) %>%
  mutate(share = n / sum(n),
         bigram = reorder(bigram, share)) %>%
  ggplot(aes(x = bigram, y = share, fill = bigram)) +
  geom_col() +
  coord_flip() +
  xlab("Chord") +
  ylab("Share of total bigramds") +
    theme(legend.position='none')

# Finding and displaying the 30 artists with the most songs in the corpus
bb_30_artists <- bb %>%
  select(artist, title) %>%
  unique() %>%
  count(artist, sort = TRUE)

bb_30_artists %>%
  slice(1:30)
  
tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))

# Creating a new dataframe `bb_tagged` that includes a new column `instrument` from `tags`
bb_tagged <- bb %>%
    inner_join(tags)
    
# Displaying the new dataframe
bb_tagged

# The top 20 most common chords
top_20 <- bb_count$chord[1:20]

# Comparing the frequency of the 20 most common chords in piano- and guitar-driven songs
bb_tagged %>%
  filter(chord == top_20) %>%
  count(chord, instrument, sort = TRUE) %>%
  ggplot(aes(x = chord, y = n, fill = instrument)) +
    geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Chord") +
  ylab("Instrument") +
    facet_grid(. ~ instrument) +
    theme(legend.position='none')
    
# The top 20 most common bigrams
top_20_bigram <- bb_bigram_count$bigram[1:20]

# Creating a faceted plot comparing guitar- and piano-driven songs for bigram frequency
bb_tagged %>%
  mutate(next_chord = lead(chord), next_title = lead(title), 
         bigram = str_c(chord, next_chord, sep = " ")) %>%
  filter(bigram %in%top_20_bigram) %>%
  count(bigram, instrument, sort = TRUE) %>%
  ggplot(aes(x = bigram, y = n, fill = instrument)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Bigram") +
  ylab("Instrument") +
  facet_grid(. ~ instrument) +
  theme(legend.position='none')
  
# Set to TRUE or FALSE to reflect your answer.
hypothesis_valid <- TRUE

# Set to TRUE or FALSE to reflect your answer.
more_data_needed <- TRUE
