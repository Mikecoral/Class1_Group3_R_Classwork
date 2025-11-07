library(readxl)
library(tidyverse)
library(scales)
library(wordcloud)
library(RColorBrewer)

merged_data_rows <- read_excel("merged_publications__Robotics.xlsx")

author_split <- merged_data_rows %>%
  separate_rows(`Author Full Names`, sep = ";") %>%
  mutate(`Author Full Names` = str_trim(`Author Full Names`)) %>%
  filter(`Author Full Names` != "")

total_cites <- author_split %>%
  group_by(`Author Full Names`) %>%
  summarise(total_cites = sum(`Cited Reference Count`)) %>%
  arrange(desc(total_cites))

top_authors <- total_cites %>%
  slice_head(n = 10) %>%
  mutate(`Author Full Names` = factor(`Author Full Names`, levels = .$`Author Full Names`))

ggplot(top_authors, aes(x = `Author Full Names`, y = total_cites)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(
    title = "Top 10 Authors by Total Cited References",
    x = "Author",
    y = "Total Cited Reference Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

doc_type_stats <- merged_data_rows %>%
  separate_rows(`Document Type`, sep = ";") %>%
  mutate(`Document Type` = str_trim(`Document Type`)) %>%
  filter(!is.na(`Document Type`), `Document Type` != "") %>%
  count(`Document Type`, name = "frequency") %>%
  mutate(
    proportion = frequency / sum(frequency),
    percentage = scales::percent(proportion)
  ) %>%
  arrange(desc(proportion))

threshold <- 0.02

doc_type_simplified <- doc_type_stats %>%
  mutate(
    `Document Type` = ifelse(
      proportion < threshold,
      "Other",
      `Document Type`
    )
  ) %>%
  group_by(`Document Type`) %>%
  summarise(
    frequency = sum(frequency),
    proportion = sum(proportion),
    percentage = scales::percent(sum(proportion))
  ) %>%
  arrange(proportion) %>%
  mutate(`Document Type` = factor(`Document Type`, levels = rev(.$`Document Type`)))

ggplot(doc_type_simplified, aes(x = "", y = proportion, fill = `Document Type`)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = percentage),
    position = position_stack(vjust = 0.5),
    color = "white", 
    fontface = "bold",
    size = 4,
    stroke = 0.3
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Document Types",
    fill = "Document Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 15)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 20, 20)
  )

journal_freq <- merged_data_rows %>%
  filter(!is.na(`Journal Abbreviation`)) %>%
  mutate(`Journal Abbreviation` = str_trim(`Journal Abbreviation`)) %>%
  count(`Journal Abbreviation`, name = "frequency") %>%
  filter(frequency > 1)

set.seed(123)
wordcloud(
  words = journal_freq$`Journal Abbreviation`,
  freq = journal_freq$frequency,
  scale = c(5, 0.5),
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.3,
  colors = brewer.pal(8, "Dark2")
)
