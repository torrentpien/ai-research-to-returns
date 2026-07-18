# ============================================================
#  rd_disclosure_analysis.R
#  檢驗「2022 後 AI 公司論文數驟降 = 公司藏私 (disclosure shift)」假說
#
#  研究設計:論文數 = 研究活動 x 揭露意願 x 資料涵蓋率
#  單看論文數下降無法區分三者,因此用四條獨立管道三角驗證:
#    (1) 投入面   : R&D 費用            (rd_expense_annual.csv, SEC EDGAR)
#    (2) 公開論文 : arXiv 公司論文數     (company_annual.rds, OpenAlex affiliation)
#    (3) 智財揭露 : 專利申請數           (8_company_patent_raw_data.csv, Google Patents)
#    (4) 產品產出 : notable AI models   (all_ai_models.csv, Epoch AI)
#
#  Part 0 先做「資料涵蓋率診斷」:如果驟降是 OpenAlex 對近年 arXiv
#  論文的機構標註涵蓋率下降 (artifact),那麼「全部公司」的論文數
#  會同步下降,而不是只有八大公司。反之若是八大公司藏私,
#  八大公司在「全公司論文池」中的占比應該下降。兩個訊號可以分離
#  artifact 與真實的揭露行為改變。
#
#  需要檔案 (與本檔同目錄):
#    rd_expense_annual.csv, company_annual.rds,
#    8_company_patent_raw_data.csv, all_ai_models.csv,
#    paper_company_panel.rds, ai_monthly.csv
#
#  輸出目錄: rd_disclosure_outputs/
# ============================================================

# setwd("C:/Users/User/BIG_DATA/LEVEL4")   # <- 改成你的資料夾

pkgs <- c("dplyr","tidyr","ggplot2","readr","lubridate",
          "stringr","purrr","lmtest","sandwich","scales")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

filter <- dplyr::filter
select <- dplyr::select

OUT_DIR <- "rd_disclosure_outputs"
dir.create(OUT_DIR, showWarnings = FALSE)

TARGET_TICKERS <- c("GOOGL","MSFT","META","NVDA","AMZN","AAPL","TSLA","AVGO")
YEAR_MIN <- 2017; YEAR_MAX <- 2025

cat("============================================================\n")
cat(" R&D / 專利 / notable models 三角驗證:藏私假說檢驗\n")
cat("============================================================\n\n")

# ============================================================
# Part 0 | 資料涵蓋率診斷
#   問題:2022 起八大公司論文數掉了約 8-9 成。
#   檢驗:OpenAlex 標註為 company 的「所有公司」論文,
#         佔全部 AI 論文的比率是否也同步崩跌?
# ============================================================
cat("===== Part 0. 資料涵蓋率診斷 =====\n")

pcp <- readRDS("paper_company_panel.rds") %>%
  mutate(year = as.integer(substr(as.character(publication_date), 1, 4)))

ai_total <- read_csv("ai_monthly.csv", show_col_types = FALSE) %>%
  mutate(year = year(as.Date(month))) %>%
  group_by(year) %>%
  summarise(total_ai_papers = sum(ai_papers), .groups = "drop")

# 八大公司的機構名稱關鍵字 (與 ai_8_company_analysis.R 的對照表一致)
BIG8_PATTERNS <- c("Google","DeepMind","Microsoft","LinkedIn","Meta ",
                   "Nvidia","Amazon","Apple","Tesla","Broadcom")
is_big8 <- function(x) {
  out <- rep(FALSE, length(x))
  for (p in BIG8_PATTERNS) out <- out | grepl(p, x, fixed = TRUE)
  out
}

coverage <- pcp %>%
  filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
  mutate(big8 = is_big8(institution_name)) %>%
  group_by(year) %>%
  summarise(
    company_works = n_distinct(work_id),
    big8_works    = n_distinct(work_id[big8]),
    .groups = "drop"
  ) %>%
  left_join(ai_total, by = "year") %>%
  mutate(
    company_share_pct = 100 * company_works / total_ai_papers,
    big8_share_of_company_pct = 100 * big8_works / company_works
  )

cat("\n[涵蓋率診斷表]\n")
cat("  company_share_pct         = 全部公司論文 / 全部AI論文\n")
cat("  big8_share_of_company_pct = 八大公司論文 / 全部公司論文\n\n")
print(as.data.frame(coverage), row.names = FALSE)
write_csv(coverage, file.path(OUT_DIR, "out_rd0_coverage_diagnostic.csv"))

p0 <- coverage %>%
  select(year, company_share_pct, big8_share_of_company_pct) %>%
  pivot_longer(-year, names_to = "metric", values_to = "pct") %>%
  mutate(metric = recode(metric,
    company_share_pct = "All-company papers / all AI papers",
    big8_share_of_company_pct = "Big-8 papers / all company papers")) %>%
  ggplot(aes(year, pct, color = metric)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = YEAR_MIN:YEAR_MAX) +
  labs(title = "Coverage diagnostic: artifact vs. genuine disclosure shift",
       subtitle = paste0("If only Big-8 went secret, red line stays flat;\n",
                         "both fall => coverage artifact AND disclosure shift coexist"),
       x = NULL, y = "%", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(OUT_DIR, "fig_rd0_coverage_diagnostic.png"), p0,
       width = 9, height = 5.5, dpi = 160)

# ============================================================
# Part 1 | R&D 投入 vs 論文產出 (publication intensity)
# ============================================================
cat("\n===== Part 1. R&D vs 論文:publication intensity =====\n")

# R&D:用「財報期間中點」對齊到日曆年,處理 MSFT(6月)/NVDA(1月)/
# AVGO(11月)等不同財年結算日 (fiscal_year 標籤 != 日曆年)
rd <- read_csv("rd_expense_annual.csv", show_col_types = FALSE) %>%
  mutate(
    period_end = as.Date(fiscal_period_end, tryFormats = c("%Y/%m/%d","%Y-%m-%d")),
    cal_year   = year(period_end %m-% months(6)),   # 財報期間中點所在年
    rd_billion = rd_expense_usd / 1e9
  ) %>%
  filter(ticker %in% TARGET_TICKERS, cal_year >= YEAR_MIN, cal_year <= YEAR_MAX) %>%
  select(ticker, cal_year, rd_billion, tag_used)

# 注意:AMZN 沒有單獨的 R&D 科目,揭露的是 "Technology and Content"
# (含 AWS 基礎設施成本),數字系統性偏高、與其他公司不可比。
# 迴歸分析分「含 AMZN」與「不含 AMZN」兩版。
amzn_note <- rd %>% filter(ticker == "AMZN") %>% distinct(tag_used)
cat("\n[資料警告] AMZN 使用科目:", amzn_note$tag_used,
    "(含基礎設施成本,非純研發費用)\n")

papers <- readRDS("company_annual.rds") %>%
  filter(ticker != "GOOG",                       # GOOG/GOOGL 重複,留 GOOGL
         ticker %in% TARGET_TICKERS,
         year >= YEAR_MIN, year <= YEAR_MAX) %>%
  select(ticker, year, n_papers)

panel <- papers %>%
  inner_join(rd, by = c("ticker", "year" = "cal_year")) %>%
  mutate(
    post2022 = as.integer(year >= 2022),
    papers_per_billion = n_papers / rd_billion
  ) %>%
  arrange(ticker, year)

cat("\n[各公司 papers per $1B R&D]\n")
intensity_wide <- panel %>%
  select(ticker, year, papers_per_billion) %>%
  mutate(papers_per_billion = round(papers_per_billion, 2)) %>%
  pivot_wider(names_from = ticker, values_from = papers_per_billion)
print(as.data.frame(intensity_wide), row.names = FALSE)
write_csv(panel, file.path(OUT_DIR, "out_rd1_panel_firm_year.csv"))

pre_post <- panel %>%
  group_by(ticker, post2022) %>%
  summarise(papers = sum(n_papers), rd = sum(rd_billion), .groups = "drop") %>%
  mutate(intensity = papers / rd) %>%
  select(ticker, post2022, intensity) %>%
  pivot_wider(names_from = post2022, values_from = intensity,
              names_prefix = "per_B_") %>%
  mutate(ratio_post_pre = per_B_1 / per_B_0)
cat("\n[發表密度 pre(2017-21) vs post(2022-25), papers per $1B]\n")
print(as.data.frame(pre_post), row.names = FALSE)
write_csv(pre_post, file.path(OUT_DIR, "out_rd1_intensity_pre_post.csv"))

p1 <- panel %>%
  filter(ticker %in% c("GOOGL","MSFT","META","AMZN","NVDA","AAPL")) %>%  # AVGO/TSLA 論文太稀疏
  ggplot(aes(year, papers_per_billion, color = ticker)) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey40") +
  scale_y_log10() +
  scale_x_continuous(breaks = YEAR_MIN:YEAR_MAX) +
  labs(title = "Publication intensity: AI papers per $1B R&D (log scale)",
       subtitle = "AMZN denominator = Technology & Content (inflated); AVGO/TSLA too sparse to plot",
       x = NULL, y = "papers per $1B R&D (log)", color = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path(OUT_DIR, "fig_rd1_publication_intensity.png"), p1,
       width = 9, height = 5.5, dpi = 160)

# ============================================================
# Part 2 | 迴歸:2022 結構斷點
#   Spec A: log(papers+1) = a_i + b*log(RD) + c*Post2022
#                           + d*log(RD)xPost2022 + e   (firm FE, cluster SE)
#   Spec B: quasi-Poisson, papers ~ Post2022 + firm FE + offset(log RD)
#           => exp(coef of Post2022) = post/pre 的「每元R&D論文產出」倍率
#   注意:N = 8 家 x 9 年,cluster 數只有 8,推論屬探索性;
#         嚴謹版應改用 wild cluster bootstrap (fwildclusterboot)。
# ============================================================
cat("\n===== Part 2. 結構斷點迴歸 =====\n")

run_break_models <- function(df, label) {
  cat("\n----------", label, "----------\n")
  df <- df %>% mutate(log_papers = log(n_papers + 1), log_rd = log(rd_billion))

  mA <- lm(log_papers ~ log_rd * post2022 + factor(ticker), data = df)
  vA <- vcovCL(mA, cluster = df$ticker, type = "HC1")
  cat("[Spec A: OLS firm-FE, cluster-by-firm SE]\n")
  print(coeftest(mA, vcov = vA)[c("log_rd","post2022","log_rd:post2022"), ])

  # Chow 型檢定:post2022 與交互項聯合為 0
  mA0 <- lm(log_papers ~ log_rd + factor(ticker), data = df)
  wt  <- waldtest(mA0, mA, vcov = vA)
  cat(sprintf("\n[Chow-type Wald] post2022 + interaction jointly = 0: F=%.2f, p=%.4g\n",
              wt$F[2], wt$`Pr(>F)`[2]))

  mB <- glm(n_papers ~ post2022 + factor(ticker) + offset(log(rd_billion)),
            family = quasipoisson(), data = df)
  vB <- vcovCL(mB, cluster = df$ticker, type = "HC1")
  ctB <- coeftest(mB, vcov = vB)
  cat("\n[Spec B: quasi-Poisson, offset = log(R&D)]\n")
  print(ctB["post2022", , drop = FALSE])
  cat(sprintf("  => post/pre 每$1B R&D 論文產出倍率 = exp(%.3f) = %.3f (下降 %.1f%%)\n",
              ctB["post2022","Estimate"], exp(ctB["post2022","Estimate"]),
              100 * (1 - exp(ctB["post2022","Estimate"]))))
  invisible(list(A = mA, B = mB))
}

# 只用論文數有實質規模的公司 (AVGO 總數 3 篇、TSLA 1 篇,計數模型無意義)
panel_reg <- panel %>% filter(!ticker %in% c("AVGO","TSLA"))
run_break_models(panel_reg, "六家 (排除 AVGO/TSLA)")
run_break_models(panel_reg %>% filter(ticker != "AMZN"),
                 "五家 (再排除 AMZN,因 R&D 科目不可比)")

# ============================================================
# Part 3 | 專利:替代揭露管道
#   若「藏私」= 從發論文轉向專利保護,專利申請不應同步崩跌。
#   重要偏誤:專利申請後 ~18 個月才公開,資料截至 2026 年初,
#   因此 2024 年之後的 filing_year 嚴重低估 (truncation),
#   分析只用 filing_year <= 2023。
#   另注意:此檔為八家公司「全部」專利,非 AI 專利;
#   建議後續在 BigQuery 用 CPC 分類 (G06N 等) 篩 AI 專利。
# ============================================================
cat("\n===== Part 3. 專利申請數 (filing year, <=2023) =====\n")

PATENT_PARENT_MAP <- c(
  "Google/Alphabet" = "GOOGL", "Microsoft" = "MSFT", "Meta/Facebook" = "META",
  "NVIDIA" = "NVDA", "Amazon" = "AMZN", "Apple" = "AAPL",
  "Tesla" = "TSLA", "Broadcom/Avago" = "AVGO"
)

patents <- read_csv("8_company_patent_raw_data.csv", show_col_types = FALSE) %>%
  mutate(ticker = PATENT_PARENT_MAP[parent_company]) %>%
  filter(!is.na(ticker), filing_year >= 2015, filing_year <= 2023) %>%
  group_by(ticker, filing_year) %>%
  summarise(patents = sum(patent_count), .groups = "drop")

patents_wide <- patents %>%
  pivot_wider(names_from = ticker, values_from = patents, values_fill = 0)
cat("\n[專利申請數 by filing year]\n")
print(as.data.frame(patents_wide), row.names = FALSE)
write_csv(patents, file.path(OUT_DIR, "out_rd3_patents_filing_year.csv"))

p3 <- patents %>%
  ggplot(aes(filing_year, patents, color = ticker)) +
  geom_line(linewidth = 1) + geom_point(size = 1.6) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_log10(labels = comma_format()) +
  labs(title = "Patent filings by company (log scale)",
       subtitle = "All patents (not AI-only); 2024+ dropped due to 18-month publication lag truncation",
       x = "Filing year", y = "patent filings (log)", color = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path(OUT_DIR, "fig_rd3_patents.png"), p3,
       width = 9, height = 5.5, dpi = 160)

# ============================================================
# Part 4 | Notable AI models (Epoch AI)
#   若研究活動真的停了,重要模型發布也應下降;
#   若只是不發論文,模型會照常(甚至更多)發布。
# ============================================================
cat("\n===== Part 4. Notable AI models by company =====\n")

models_raw <- read_csv("all_ai_models.csv", show_col_types = FALSE)

MODEL_ORG_PATTERNS <- list(
  GOOGL = "(?i)google|deepmind|alphabet|waymo",
  MSFT  = "(?i)microsoft",
  META  = "(?i)\\bmeta\\b|facebook",
  NVDA  = "(?i)nvidia",
  AMZN  = "(?i)amazon|\\baws\\b",
  AAPL  = "(?i)apple",
  TSLA  = "(?i)tesla",
  AVGO  = "(?i)broadcom"
)

models <- map_dfr(names(MODEL_ORG_PATTERNS), function(tk) {
  models_raw %>%
    filter(str_detect(coalesce(Organization, ""), MODEL_ORG_PATTERNS[[tk]])) %>%
    transmute(ticker = tk,
              year = year(as.Date(`Publication date`)))
}) %>%
  filter(!is.na(year), year >= 2015, year <= YEAR_MAX) %>%
  count(ticker, year, name = "notable_models")

models_wide <- models %>%
  pivot_wider(names_from = ticker, values_from = notable_models, values_fill = 0) %>%
  arrange(year)
cat("\n[Notable models by company-year]\n")
print(as.data.frame(models_wide), row.names = FALSE)
write_csv(models, file.path(OUT_DIR, "out_rd4_notable_models.csv"))

p4 <- models %>%
  filter(ticker %in% c("GOOGL","MSFT","META","NVDA","AMZN","AAPL")) %>%
  ggplot(aes(year, notable_models, fill = ticker)) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = 2015:YEAR_MAX) +
  labs(title = "Notable AI models released per year (Epoch AI)",
       subtitle = "Measured papers collapse after 2022, yet notable model releases accelerate",
       x = NULL, y = "notable models", fill = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path(OUT_DIR, "fig_rd4_notable_models.png"), p4,
       width = 10, height = 5.5, dpi = 160)

# ============================================================
# Part 5 | 三角驗證總圖:四條管道指數化 (2020 = 100)
# ============================================================
cat("\n===== Part 5. 三角驗證 (indexed, 2020 = 100) =====\n")

agg_papers  <- papers  %>% group_by(year) %>% summarise(v = sum(n_papers)) %>%
  mutate(series = "arXiv papers (OpenAlex, biased post-2021)")
agg_rd      <- rd %>% filter(!is.na(cal_year)) %>% group_by(year = cal_year) %>%
  summarise(v = sum(rd_billion)) %>% mutate(series = "R&D expense (8 firms, $B)")
agg_patents <- patents %>% group_by(year = filing_year) %>%
  summarise(v = sum(patents)) %>% mutate(series = "Patent filings (<=2023)")
agg_models  <- models %>% group_by(year) %>% summarise(v = sum(notable_models)) %>%
  mutate(series = "Notable AI models (Epoch)")

tri <- bind_rows(agg_papers, agg_rd, agg_patents, agg_models) %>%
  filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
  group_by(series) %>%
  mutate(index = 100 * v / v[year == 2020]) %>%
  ungroup()

write_csv(tri, file.path(OUT_DIR, "out_rd5_triangulation_indexed.csv"))

p5 <- ggplot(tri, aes(year, index, color = series)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey60") +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = YEAR_MIN:YEAR_MAX) +
  labs(title = "Four disclosure channels, indexed to 2020 = 100 (8 firms aggregate)",
       subtitle = paste0("R&D keeps rising, notable models surge, patents stable, only measured papers collapse\n",
                         "=> inconsistent with research stopping; consistent with coverage artifact + disclosure shift"),
       x = NULL, y = "index (2020 = 100)", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))
ggsave(file.path(OUT_DIR, "fig_rd5_triangulation.png"), p5,
       width = 10, height = 6, dpi = 160)

cat("\n============================================================\n")
cat(" 完成。輸出於", OUT_DIR, ":\n")
cat("  out_rd0_coverage_diagnostic.csv / fig_rd0_coverage_diagnostic.png\n")
cat("  out_rd1_panel_firm_year.csv / out_rd1_intensity_pre_post.csv /\n")
cat("    fig_rd1_publication_intensity.png\n")
cat("  (Part 2 迴歸結果印在 console)\n")
cat("  out_rd3_patents_filing_year.csv / fig_rd3_patents.png\n")
cat("  out_rd4_notable_models.csv / fig_rd4_notable_models.png\n")
cat("  out_rd5_triangulation_indexed.csv / fig_rd5_triangulation.png\n")
cat("============================================================\n")
