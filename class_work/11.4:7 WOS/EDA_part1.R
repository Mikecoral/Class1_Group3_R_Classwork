library(tidyverse)
library(readxl)
library(dplyr)
library(treemapify)
library(ggplot2)
library(stringr)

file_path_1 <- "savedrecs-4.xls"
file_path_2 <- "savedrecs-5.xls"
file_path_3 <- "savedrecs-6.xls"
file_path_4 <- "savedrecs-7.xls"
file_path_5 <- "savedrecs-8.xls"

data1 <- read_excel(file_path_1, sheet = 1)
data2 <- read_excel(file_path_2, sheet = 1)
data3 <- read_excel(file_path_3, sheet = 1)
data4 <- read_excel(file_path_4, sheet = 1)
data5 <- read_excel(file_path_5, sheet = 1)

merged_data_rows <- rbind(data1, data2)
merged_data_rows <- rbind(merged_data_rows, data3)
merged_data_rows <- rbind(merged_data_rows, data4)
merged_data_rows <- rbind(merged_data_rows, data5)

print(dim(merged_data_rows))

category_counts <- merged_data_rows %>%
  
  filter(!is.na(`WoS Categories`)) %>%
  
  separate_rows(`WoS Categories`, sep = ";") %>%
  
  mutate(Category = str_trim(`WoS Categories`)) %>%
  
  count(Category, name = "Count", sort = TRUE) %>%
  
  filter(Category != "")

print(category_counts)

p1 <- category_counts %>%
  
  slice_head(n = 10) %>%
  
  ggplot(aes(area = Count,
             fill = Category,
             label = paste(Category, Count, sep = "\n"))) +
  
  geom_treemap() +
  
  geom_treemap_text(
    color = "white",
    place = "centre",
    size = 14,
    grow = TRUE
  ) +
  
  labs(
    title = "Top 10 WoS Categories Frequency Tree Diagram",
    caption = "The area of the rectangle represents the number of times that category appears."
  ) +
  
  theme(legend.position = "none")

ggsave("Treemap.png",plot=p1)

publication_counts <- merged_data_rows %>%
  
  count(`Publication Year`, name = "Count") %>%
  
  arrange(`Publication Year`)

print(publication_counts)

p2 <- publication_counts %>%
  
  filter(`Publication Year` < 2026) %>%
  
  ggplot(aes(x = `Publication Year`, y = Count)) +
  
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  
  labs(
    title = "Annual Publication Volume",
    x = "Publication Year",
    y = "Number of Publications"
  ) +
  theme_minimal()

print(p2)

ggsave("Pub_year_line_filtered.png", plot = p2)


country_counts <- merged_data_rows %>%
  
  filter(!is.na(`Publisher Address`)) %>%
  
  mutate(Country_Raw = str_extract(`Publisher Address`, "[^,]+$")) %>%
  
  mutate(Country_Raw = str_trim(Country_Raw)) %>%
  
  mutate(Country = str_extract(Country_Raw, "[A-Za-z ]+$")) %>%
  
  mutate(Country = str_trim(Country)) %>%
  
  filter(!is.na(Country) & Country != "") %>%
  
  count(Country, name = "Count", sort = TRUE)

print(head(country_counts, 20))

top_15_countries <- country_counts %>%
  slice_head(n = 15)

p_country <- ggplot(top_15_countries, aes(x = Count, y = reorder(Country, Count))) +
  geom_col(fill = "#0072B2") +
  
  labs(
    title = "Top 15 Publisher Countries",
    x = "Number of Publications",
    y = "Country"
  ) +
  theme_minimal()

print(p_country)

ggsave("Top_15_Countries_Barchart.png",
       plot = p_country,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)


library(writexl)
write_xlsx(merged_data_rows, "merged_publications_Robotics.xlsx")