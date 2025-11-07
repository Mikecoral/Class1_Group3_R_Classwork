library(tidyverse)
library(readxl)
library(tm)
library(topicmodels)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

file_path <- "merged_publications_Robotics.xlsx"

data <- read_excel(file_path, sheet = 1)
article_title_col <- "Article Title"
abstract_col <- "Abstract"
times_cited_col <- "Cited Reference Count"

preprocessed_data <- data %>%
  mutate(combined_text = paste0(
    ifelse(is.na(!!sym(article_title_col)), "", !!sym(article_title_col)),
    " ",
    ifelse(is.na(!!sym(abstract_col)), "", !!sym(abstract_col))
  )) %>%
  filter(str_trim(combined_text) != "") %>%
  rowid_to_column(var = "doc_id")

corpus_source <- VectorSource(preprocessed_data$combined_text)
corpus <- Corpus(corpus_source)

corpus_cleaned <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("en"))

dtm <- DocumentTermMatrix(corpus_cleaned)

sparsity_threshold <- 0.95
dtm_sparse <- removeSparseTerms(dtm, sparsity_threshold)
cat("Dimensions of final DTM:", dim(dtm_sparse), "\n")

K <- 10

set.seed(12345)
lda_model <- LDA(
  dtm_sparse,
  k = K,
  method = "Gibbs",
  control = list(seed = 12345, burnin = 1000, iter = 1000, thin = 100)
)

top_terms_per_topic <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms_per_topic)

top_terms_per_topic %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = paste("Top Terms per LDA Topic (K =", K, ")"),
    x = "Terms",
    y = "Beta (Word Probability)"
  ) +
  theme_minimal()
ggsave("Top_word.png")

vocab <- colnames(dtm_sparse)

par(mfrow = c(2, 5))
for (k in 1:K) {
  topic_terms <- posterior(lda_model)$terms[k, ]
  top_words <- sort(topic_terms, decreasing = TRUE)[1:50]
  wordcloud(
    words = names(top_words),
    freq = top_words,
    min.freq = 1,
    max.words = 50,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
  title(main = paste("Topic", k))
}

doc_topic_probs <- tidy(lda_model, matrix = "gamma")

dominant_topics <- doc_topic_probs %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  rename(dominant_topic = topic, dominant_gamma = gamma)

final_data_with_topics <- preprocessed_data %>%
  mutate(doc_id_char = as.character(doc_id)) %>%
  left_join(dominant_topics, by = c("doc_id_char" = "document")) %>%
  select(-doc_id_char)

print(head(final_data_with_topics %>% select(doc_id, dominant_topic, dominant_gamma, !!sym(times_cited_col), combined_text)))

topic_impact <- final_data_with_topics %>%
  filter(!is.na(dominant_topic)) %>%
  group_by(dominant_topic) %>%
  summarise(
    avg_citations = mean(!!sym(times_cited_col), na.rm = TRUE),
    median_citations = median(!!sym(times_cited_col), na.rm = TRUE),
    num_papers = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_citations))

print(topic_impact)

p2 <- ggplot(topic_impact, aes(x = factor(dominant_topic), y = avg_citations, fill = num_papers)) +
  geom_col() +
  labs(
    title = "Average Citations per Dominant LDA Topic",
    x = "Dominant Topic",
    y = "Average Times Cited"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "# Papers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Topic_w_citation.png", plot = p2)
