library(usmap)
library(ggplot2)
library(tidyverse)
library(quanteda)
library(tm)
library(topicmodels)
library(slam)
library(udpipe)
library(parallel)
library(kableExtra)
library(maptools)

# Directory with speeches
setwd("/Users/amundfylling/Documents/Github Prosjekter/trump-speeches/Transcripts all speeches 2016")

###############################################################################
# Pre-processing the data
###############################################################################

# Names of speech-files
filenames <- list.files()

us.states <- state.abb

filenames.df <- data.frame(name=filenames) %>%
  as_tibble() %>%
  mutate(year = as.double(substr(name, 1, 4)),                    # extract year from file name 
         state = substr(name, nchar(name)-6, nchar(name)-4)) %>% 
  filter(nchar(name) < 150,                                       # There were some problem reading files with longer names (most of these are however TV interviews)
         year == 2016,                                            # only use speeches in 2016
         grepl(paste(us.states, collapse = "|"), state),
         grepl("As spoken", name),                                
         !grepl("Interview", name))                               # Not interesting to have TV interviews etc. for this analysis

  
#View(filenames.df)
filenames <- c(filenames.df$name)

# Creating empty list
speeches <- list()

# For-loop to store all speeches in a list
for(i in 1:length(filenames)){
  speeches[[i]] <- read_file(filenames[[i]])
}

# Cleaning
speeches <- speeches %>%  
  lapply(function(x) gsub("\\s\\([^\\)]+\\)" ,"", x)) %>% # remove text inside paranthesis
  lapply(function(x) gsub("UNIDENTIFIED:\\s*(.*?)\\s*DONALD TRUMP:", "", x)) %>% # remove characters between "unidentified" and "donal trump" 
  lapply(function(x) gsub("DONALD TRUMP:" ,"", x)) %>%  # remove uppercase "donald trump"
  lapply(function(x) tolower(x)) # remove all lower case

# Create document-term-matrix
corpus <- Corpus(VectorSource(speeches))

dtm <- DocumentTermMatrix(corpus,
                          control = list( 
                            removePunctuation = T,
                            stopwords = T,
                            stemming = F,
                            removeNumbers = T,
                            wordLengths = c(4, 20)))

# exclude empty/sparse documents
dtm <- dtm[row_sums(dtm) > 10,]

# Estimate topic model
topic <- LDA(dtm,  # document term matrix
             k = 50, # specifify number of topics
             method = "Gibbs",
             control = list(
              seed = 1234, # enable replication
              burnin = 100,  # how often sampled before estimation recorded
              iter = 1000,  # number of iterations
               keep = 1,    # saves additional data per iteration (such as logLik)
               save = F,     # saves logLiklihood of all iterations
               verbose = 10  # report progress
             ))

# Term distribution for each topic
beta <- exp(topic@beta) # log of probability reported

# inspect the most common terms of topic 1
head(topic@terms[order(beta[15,], decreasing = T)], 20)

# inspect top 5 terms for all topics
apply(topic@beta, 1, function(x) head(topic@terms[order(x, decreasing = T)],5))

# Create table: 
top_table <- apply(topic@beta, 1, function(x) head(topic@terms[order(x, decreasing = T)],5))

# Make output nicer:
top_table %>% 
  t() %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  kable(col.names = c("Topic", "Term 1", "Term 2", "Term 3", "Term 4", "Term 5")) %>% 
  kable_classic_2(full_width = F, html_font = "Times New Roman")
  

# I choose to go forward with topic 42 -- regarding coal mining
# Store top 20 terms from coal mining topic in a character vector: 
topic_terms <- c(head(topic@terms[order(beta[42,], decreasing = T)], 20))

topic_terms %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  kable(col.names = c("#", "Terms")) %>% 
  kable_classic_2(full_width = F, html_font = "Times New Roman")

# Topic term count:
topic.term.count <- speeches %>%
  lapply(function(x) lengths(gregexpr(paste(topic_terms, collapse = "|"), x))) 

# Total word count
word.count <- c(lapply(gregexpr("[[:alpha:]]+", speeches), function(x) sum(x > 0)))

filenames.df$topic.term.count <- as.double(topic.term.count)
filenames.df$word.count <- as.double(word.count) 


# calculate relative word count:
us.map <- filenames.df %>%
  group_by(state) %>% 
  summarise(total.words = sum(word.count),
            total.terms = sum(topic.term.count),
            rel.term = total.terms/total.words*100) %>%
  mutate(state = gsub(" ", "", state)) %>%
  as_tibble()


# Map word count: 
us.map.plot <- plot_usmap(
  data = us.map,
  values = "rel.term",
  color = "black",
  labels = T) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "green",
                       midpoint = 0.95) +
  theme(legend.position = "NA")
  
# Using Coal Mine data:
coal.mines <- read.csv2("../Coal_Mines.csv", sep = ",", header = T) %>% 
  select(lat,lon) %>% 
  mutate(lat = as.double(lat),
         lon = as.double(lon))

# Transform lat and lon to fit in plot_usmap
transformed_data <- usmap_transform(coal.mines)

# Plot the finished plot
finished_plot <- 
  us.map.plot +
  geom_point(data = transformed_data, 
             aes(x = x, y = y), 
             size = 0.3,
             color = "red") +
  ggtitle("Relative Term Frequency of Coal Mining Related Words in Trump's 2016 Speeches",
          subtitle = "(Red Dots = Coal Mines)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5 ,size = 12))

# Save plot
ggsave("../coal_map.svg", 
       plot = finished_plot, 
       width = 10, 
       height = 7)

