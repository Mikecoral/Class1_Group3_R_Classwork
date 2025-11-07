library(tidyverse)
library(readxl)
library(dplyr)
library(treemapify)
library(ggplot2)
library(stringr)




# ---- 1. 加载必要的库 ----
# install.packages(c("tidyverse", "readxl", "tm", "topicmodels", "tidytext", "wordcloud", "RColorBrewer", "ldatuning")) # 首次运行需安装

library(tidyverse)
library(readxl)
library(tm)      # 文本挖掘
library(topicmodels) # LDA
library(tidytext) # Tidy 文本分析
library(wordcloud) # 词云
library(RColorBrewer) # 调色板
# library(ldatuning) # 主题数调优 (如果需要自动选择K)


file_path <- "merged_publications_Robotics.xlsx"

data <- read_excel(file_path, sheet=1)
article_title_col <- "Article Title" # 根据实际列名修改
abstract_col <- "Abstract"           # 根据实际列名修改
times_cited_col <- "Cited Reference Count" # 根据实际列名修改 (或者可能是 Times_Cited__Total )

# ---- 3. 数据清洗和预处理 ----

# 创建一个包含合并文本的新列，并处理缺失值
preprocessed_data <- data %>%
  # 合并标题和摘要，处理缺失值
  mutate(combined_text = paste0(
    ifelse(is.na(!!sym(article_title_col)), "", !!sym(article_title_col)),
    " ",
    ifelse(is.na(!!sym(abstract_col)), "", !!sym(abstract_col))
  )) %>%
  # 移除完全为空或只有空格的组合文本
  filter(str_trim(combined_text) != "") %>%
  # 添加行号作为文档ID
  rowid_to_column(var = "doc_id")


# 创建语料库 (Corpus)
# source 是 tm 包定义的函数，用于指定文本来源
corpus_source <- VectorSource(preprocessed_data$combined_text)
corpus <- Corpus(corpus_source)

# 文本预处理
corpus_cleaned <- corpus %>%
  # 转换为小写
  tm_map(content_transformer(tolower)) %>%
  # 移除标点符号
  tm_map(removePunctuation) %>%
  # 移除数字 (可选，取决于研究需求)
  tm_map(removeNumbers) %>%
  # 移除额外空白
  tm_map(stripWhitespace) %>%
  # 移除英文停用词 (可以根据领域需求自定义停用词列表)
  tm_map(removeWords, stopwords("en")) # 或者使用 tm::stopwords("english")

# 如果有特定领域停用词，可以在这里添加
# custom_stopwords <- c("research", "study", "paper", "article")
# corpus_cleaned <- tm_map(corpus_cleaned, removeWords, custom_stopwords)

# 创建文档-词项矩阵 (DTM)
dtm <- DocumentTermMatrix(corpus_cleaned)

# 移除稀疏性高的词项 (可选，提高计算效率)
# 例如，保留至少在 5% 的文档中出现过的词
sparsity_threshold <- 0.95 # 95% 的文档都没有这个词才移除
dtm_sparse <- removeSparseTerms(dtm, sparsity_threshold)
cat("Dimensions of final DTM:", dim(dtm_sparse), "\n")

K <- 10 # <--- 你需要根据实际情况或调优结果调整这个值


# ---- 5. 训练 LDA 模型 ----

# 使用 Gibbs Sampling 算法训练 LDA 模型
# 设置随机种子以确保结果可重复
set.seed(12345)
lda_model <- LDA(dtm_sparse, k = K, method = "Gibbs",
                 control = list(seed = 12345, burnin = 1000, iter = 1000, thin = 100))

# ---- 6. 探索和可视化模型 ----

# a) 查看每个主题的 top N 词
top_terms_per_topic <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>% # 获取每个主题权重最高的10个词
  ungroup() %>%
  arrange(topic, -beta) # 按主题和权重降序排列

print(top_terms_per_topic)

# b) 可视化每个主题的 top 词 (条形图)
top_terms_per_topic %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() + # 需要配合 reorder_within 使用
  labs(title = paste("Top Terms per LDA Topic (K =", K, ")"),
       x = "Terms", y = "Beta (Word Probability)") +
  theme_minimal()
ggsave("Top_word.png")

# c) (可选) 词云可视化主题
# 获取词汇表
vocab <- colnames(dtm_sparse)

# 为每个主题绘制词云
# 为每个主题绘制词云
par(mfrow = c(2, 5))  # 调整布局以适应 K 个主题
for (k in 1:K) {
  # 提取第 k 个主题的词概率
  topic_terms <- posterior(lda_model)$terms[k, ]
  # 排序并选取前 50 个词
  top_words <- sort(topic_terms, decreasing = TRUE)[1:50]
  # 绘制词云
  wordcloud(words = names(top_words), freq = top_words,
            min.freq = 1, max.words = 50,
            random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
  title(main = paste("Topic", k))
}


# d) 查看每篇文档最可能的主题
# 获取文档-主题概率矩阵
doc_topic_probs <- tidy(lda_model, matrix = "gamma")

# 找到每篇文档概率最高的主题
dominant_topics <- doc_topic_probs %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  rename(dominant_topic = topic, dominant_gamma = gamma)

# 将主导主题信息合并回原始数据
final_data_with_topics <- preprocessed_data %>%
  # 注意：doc_id 是行号，而 document 是字符串形式的 "1", "2", ...
  # 需要将 doc_id 转换为字符并与 document 匹配
  mutate(doc_id_char = as.character(doc_id)) %>%
  left_join(dominant_topics, by = c("doc_id_char" = "document")) %>%
  select(-doc_id_char) # 移除临时列

# 查看结果
print(head(final_data_with_topics %>% select(doc_id, dominant_topic, dominant_gamma, !!sym(times_cited_col), combined_text)))

# ---- 7. 关联高影响力因素 ----

# 分析每个主题的平均被引次数
topic_impact <- final_data_with_topics %>%
  filter(!is.na(dominant_topic)) %>% # 移除未分配主导主题的文档
  group_by(dominant_topic) %>%
  summarise(
    avg_citations = mean(!!sym(times_cited_col), na.rm = TRUE),
    median_citations = median(!!sym(times_cited_col), na.rm = TRUE),
    num_papers = n(),
    .groups = 'drop' # 明确指定分组行为
  ) %>%
  arrange(desc(avg_citations)) # 按平均被引次数降序排列

print(topic_impact)

# 可视化主题与平均被引次数的关系
p2 <- ggplot(topic_impact, aes(x = factor(dominant_topic), y = avg_citations, fill = num_papers)) +
  geom_col() +
  labs(title = "Average Citations per Dominant LDA Topic",
       x = "Dominant Topic", y = "Average Times Cited") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "# Papers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Topic_w_citation.png",plot=p2)