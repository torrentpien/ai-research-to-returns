library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)

# 0. 匯入資料
results <- list()
i <- 0

con <- file("arxiv-metadata-oai-snapshot.json", "r")

sp500 <- read_csv("sp500_top10_stocks_clean.csv", show_col_types = FALSE)

stream_in(con, handler = function(df) {
  i <<- i + 1
  
  df_filtered <- df[df$update_date >= "2017-01-01", ]
  
  cat("第", i, "批：讀入", nrow(df), "筆，保留", nrow(df_filtered), "筆\n")
  
  if (nrow(df_filtered) > 0) {
    results[[i]] <<- df_filtered
  }
}, pagesize = 10000)

close(con)

cat("results 長度:", length(results), "\n")

# 1. 合併 arXiv 資料
final <- bind_rows(results)

cat("final rows:", nrow(final), "\n")
cat("final cols:", ncol(final), "\n")

colnames(final)
str(final, max.level = 1)
head(final)

# 2. 統計所有 arXiv category
category_counts <- final %>%
  separate_rows(categories, sep = " ") %>%
  count(categories, sort = TRUE)

cat("category 數量:", nrow(category_counts), "\n")

# 3. 篩出 AI 相關論文
ai_for_finance <- c(
  "cs.LG",
  "cs.AI",
  "cs.CL",
  "cs.CV",
  "stat.ML",
  "cs.NE"
)

ai_papers <- final %>%
  filter(
    grepl(
      paste0("\\b(", paste(ai_for_finance, collapse = "|"), ")\\b"),
      categories
    )
  )

cat("AI paper 數量:", nrow(ai_papers), "\n")

# 4. AI papers 時間欄位整理
ai_papers_clean <- ai_papers %>%
  mutate(
    update_date = as.Date(update_date),
    year = year(update_date),
    month = floor_date(update_date, "month"),
    quarter = paste0(year(update_date), "Q", quarter(update_date))
  )

# 5. AI 子領域分類
ai_papers_clean <- ai_papers_clean %>%
  mutate(
    ai_subfield = case_when(
      str_detect(categories, "\\bcs\\.CL\\b") ~ "LLM_NLP",
      str_detect(categories, "\\bcs\\.CV\\b") ~ "Computer_Vision",
      str_detect(categories, "\\bcs\\.RO\\b") ~ "Robotics",
      str_detect(categories, "\\bcs\\.LG\\b|\\bstat\\.ML\\b") ~ "Machine_Learning",
      str_detect(categories, "\\bcs\\.AI\\b") ~ "General_AI",
      str_detect(categories, "\\bcs\\.NE\\b") ~ "Neural_Networks",
      TRUE ~ "Other_AI"
    )
  )

# 存檔
saveRDS(ai_papers_clean, "ai_papers_clean.rds")
write_csv(ai_papers_clean, "ai_papers_clean.csv")

cat("已存檔：ai_papers_clean.rds (", 
    round(file.info("ai_papers_clean.rds")$size / 1024^2, 1), "MB)\n")


# 6. 產出子領域月聚合 csv（給 ai_subfield_analysis.R 用）
ai_monthly_subfield <- ai_papers_clean %>%
  count(month, ai_subfield, name = "ai_paper_count")

write_csv(ai_monthly_subfield, "ai_monthly_subfield.csv")

saveRDS(ai_monthly_subfield, "ai_monthly_subfield.rds")

cat("子領域月資料：", nrow(ai_monthly_subfield), "筆\n")
print(head(ai_monthly_subfield))
print(table(ai_monthly_subfield$ai_subfield))
