#  Level 1 | Macro   : 整體股市 vs 總 AI 論文數 / 引用數 (描述統計 + 時間序列)
#  Level 2 | Sector  : 科技業 (市值加權) 報酬 vs AI 論文數 / 引用數
#  Level 3 | Subfield: LLM / CV / Robotics ... vs 對應產業 (IRF + Panel)

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
library(purrr)

library(tseries)
library(forecast)
library(lmtest)
library(sandwich)
library(plm)
library(dynlm)
library(vars)
library(zoo)

filter <- dplyr::filter
set.seed(42)

cat("=============================================\n")
cat(" AI x 股價 三層分析\n")
cat(" Monthly time-series lag + Quarterly outputs\n")
cat("=============================================\n\n")

#  0. 讀檔 + 共用設定
ai_monthly_raw <- read_csv("ai_monthly.csv", show_col_types = FALSE) %>%
  mutate(
    month = as.Date(month),
    quarter = floor_date(month, "quarter")
  )

sf_raw <- read_csv("ai_monthly_subfield.csv", show_col_types = FALSE) %>%
  mutate(
    month = as.Date(month),
    quarter = floor_date(month, "quarter")
  )

sp500 <- read_csv("sp500_top10_stocks_clean.csv", show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date), Ticker = as.character(Ticker))

tech_tickers <- c("AAPL","MSFT","GOOGL","GOOG","META","NVDA","AMZN","TSLA","AVGO")

# 靜態市值權重 (近似 2026 市值占比;沒有逐月股數,故用靜態權重) 
# todo: 之後若拿到逐月市值,可改成動態權重。GOOG/GOOGL 同公司拆兩股別,各給一半。
mcap_weight <- c(
  AAPL = 0.20, MSFT = 0.19, NVDA = 0.18,
  AMZN = 0.13, META = 0.09,
  GOOGL = 0.075, GOOG = 0.075,
  AVGO = 0.05, TSLA = 0.05
)

# citation 截斷處理參數
CITATION_CUTOFF_MONTHS <- 24
cutoff_date <- max(ai_monthly_raw$month) %m-% months(CITATION_CUTOFF_MONTHS)

cat("資料期間:\n")
cat("  AI論文:", as.character(min(ai_monthly_raw$month)), "~",
    as.character(max(ai_monthly_raw$month)), "\n")
cat("  股價:", as.character(min(sp500$Date)), "~",
    as.character(max(sp500$Date)), "\n")
cat("  Citation cutoff (僅用此日前的引用):", as.character(cutoff_date), "\n\n")

# 共用工具函數
# 個股月底報酬 (月底->月底 log return)
make_stock_monthly <- function(sp_df, tickers = NULL) {
  d <- sp_df
  if (!is.null(tickers)) d <- d %>% filter(Ticker %in% tickers)
  d %>%
    mutate(month = floor_date(Date, "month")) %>%
    group_by(Ticker, month) %>%
    arrange(Date, .by_group = TRUE) %>%
    summarise(month_end_price = last(Adj_Close), .groups = "drop") %>%
    arrange(Ticker, month) %>%
    group_by(Ticker) %>%
    mutate(log_return = log(month_end_price / dplyr::lag(month_end_price))) %>%
    ungroup() %>%
    filter(!is.na(log_return))
}

# ADF 單根檢定 (測是否定態)
adf_report <- function(x, name) {
  x <- na.omit(x)
  if (length(x) < 10) { cat("  ", name, ": 樣本太少\n"); return(invisible()) }
  r <- tryCatch(suppressWarnings(adf.test(x)), error = function(e) NULL)
  if (is.null(r)) { cat("  ", name, ": 檢定失敗\n"); return(invisible()) }
  cat(sprintf("  %-28s ADF=%.3f p=%.4f -> %s\n", name, r$statistic, r$p.value,
              if (r$p.value < 0.05) "定態 ✓" else "不定態(有單根)"))
}

ts_analysis <- function(df, x_col, y_col, label, max_lag = 3) {
  cat("\n----------", label, "----------\n")
  d <- df %>% dplyr::select(month, x = all_of(x_col), y = all_of(y_col)) %>%
    drop_na() %>% arrange(month)
  if (nrow(d) < 30) { cat("樣本不足,跳過\n"); return(NULL) }
  
  for (L in 1:max_lag) d[[paste0("x_l", L)]] <- dplyr::lag(d$x, L)
  d$y_l1 <- dplyr::lag(d$y, 1)
  d2 <- d %>% drop_na()
  
  # ADF
  cat("[ADF 定態檢定]\n"); adf_report(d$x, x_col); adf_report(d$y, y_col)
  
  # ARDL with HAC
  fml <- as.formula(paste0("y ~ x + ",
                           paste0("x_l", 1:max_lag, collapse = " + "),
                           " + y_l1"))
  m <- lm(fml, data = d2)
  hac <- coeftest(m, vcov = NeweyWest(m, lag = max_lag, prewhite = FALSE))
  cat("[ARDL + Newey-West HAC]\n"); print(hac)
  
  # 累積效應
  cum <- sum(coef(m)[c("x", paste0("x_l", 1:max_lag))], na.rm = TRUE)
  cat(sprintf("  >> 累積 %d 個月效應 = %+.4f, R²=%.3f\n",
              max_lag + 1, cum, summary(m)$r.squared))
  
  # Granger 雙向
  gd <- d %>% dplyr::select(x, y) %>% drop_na()
  g1 <- tryCatch(grangertest(y ~ x, order = 2, data = gd), error=function(e) NULL)
  g2 <- tryCatch(grangertest(x ~ y, order = 2, data = gd), error=function(e) NULL)
  cat(sprintf("[Granger] AI->報酬 p=%.3f | 報酬->AI p=%.3f\n",
              if(!is.null(g1)) g1$`Pr(>F)`[2] else NA,
              if(!is.null(g2)) g2$`Pr(>F)`[2] else NA))
  
  invisible(list(model = m, hac = hac, cum_effect = cum,
                 granger_ai_to_ret = if(!is.null(g1)) g1$`Pr(>F)`[2] else NA))
}

# VAR + IRF
var_irf <- function(df, x_col, y_col, label, n_ahead = 12) {
  d <- df %>% dplyr::select(x = all_of(x_col), y = all_of(y_col)) %>%
    drop_na() %>% as.matrix()
  if (nrow(d) < 30) return(NULL)
  d <- d[, c("y", "x")]
  sel <- tryCatch(VARselect(d, lag.max = 6, type = "const"),
                  error = function(e) NULL)
  if (is.null(sel)) return(NULL)
  p <- max(1, sel$selection["AIC(n)"])
  vm <- VAR(d, p = p, type = "const")
  ir <- irf(vm, impulse = "x", response = "y",
            n.ahead = n_ahead, boot = TRUE, runs = 200)
  data.frame(
    label = label, h = 0:n_ahead,
    irf = ir$irf$x[,1], lower = ir$Lower$x[,1], upper = ir$Upper$x[,1]
  ) %>% mutate(sig = ifelse(sign(lower) == sign(upper), "*", ""))
}


#  Level 1 | MACRO — 整體股市 vs 總 AI 論文 / 引用
cat("\n\n##############################################\n")
cat("#  LEVEL 1 | MACRO\n")
cat("##############################################\n")

# 1-A. AI 論文描述統計 (by year / quarter / month / category)
cat("\n===== 1-A. AI 論文描述統計 =====\n")

# by year
desc_year <- ai_monthly_raw %>%
  mutate(year = year(month)) %>%
  group_by(year) %>%
  summarise(papers = sum(ai_papers),
            citations = sum(total_citations),
            .groups = "drop")
cat("\n[論文/引用 by 年]\n"); print(desc_year)

# by quarter
desc_quarter <- ai_monthly_raw %>%
  group_by(quarter) %>%
  summarise(
    papers = sum(ai_papers),
    citations = sum(total_citations),
    avg_citations = mean(avg_citations, na.rm = TRUE),
    .groups = "drop"
  )
cat("\n[論文/引用 by 季]\n"); print(desc_quarter)

# by category (子領域)
desc_cat <- sf_raw %>%
  group_by(ai_subfield) %>%
  summarise(total_papers = sum(ai_paper_count),
            avg_monthly = round(mean(ai_paper_count), 1),
            .groups = "drop") %>%
  arrange(desc(total_papers))
cat("\n[論文 by 子領域]\n"); print(desc_cat)

# by category and quarter
desc_cat_quarter <- sf_raw %>%
  group_by(quarter, ai_subfield) %>%
  summarise(
    papers = sum(ai_paper_count),
    .groups = "drop"
  ) %>%
  arrange(quarter, ai_subfield)
cat("\n[論文 by 季度 x 子領域]\n"); print(desc_cat_quarter)

# by month 的基本統計
cat("\n[每月論文數摘要]\n"); print(summary(ai_monthly_raw$ai_papers))

# 1-B. 整體市場 (市值加權 + 等權重) 月報酬
cat("\n===== 1-B. 整體市場月報酬 =====\n")

all_monthly <- make_stock_monthly(sp500, tech_tickers) %>%
  mutate(w = mcap_weight[Ticker])

market <- all_monthly %>%
  group_by(month) %>%
  summarise(
    mkt_vw = sum(log_return * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
    mkt_ew = mean(log_return, na.rm = TRUE),
    mkt_vol = sd(log_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(quarter = floor_date(month, "quarter"))

market_quarter_desc <- market %>%
  group_by(quarter) %>%
  summarise(
    mkt_vw_q = sum(mkt_vw, na.rm = TRUE),
    mkt_ew_q = sum(mkt_ew, na.rm = TRUE),
    avg_monthly_mkt_vw = mean(mkt_vw, na.rm = TRUE),
    avg_monthly_mkt_ew = mean(mkt_ew, na.rm = TRUE),
    mkt_vol_q = sd(mkt_vw, na.rm = TRUE),
    .groups = "drop"
  )

cat("市值加權月報酬 平均:", round(mean(market$mkt_vw, na.rm=T), 4),
    " sd:", round(sd(market$mkt_vw, na.rm=T), 4), "\n")

cat("\n[季度市場報酬描述統計]\n"); print(market_quarter_desc)

# 1-C. 合併 AI 變數 + 變換
ai_macro <- ai_monthly_raw %>%
  mutate(
    total_citations_clean = if_else(month <= cutoff_date, total_citations, NA_real_),
    log_papers      = log(ai_papers),
    d_log_papers    = c(NA, diff(log_papers)),
    log_citations   = log(total_citations_clean + 1),
    d_log_citations = c(NA, diff(log_citations))
  )

L1 <- market %>% inner_join(ai_macro, by = "month") %>% arrange(month)

# inner_join 後若有 quarter.x / quarter.y，統一處理成 quarter。
if ("quarter.x" %in% names(L1) && "quarter.y" %in% names(L1)) {
  L1 <- L1 %>%
    mutate(quarter = quarter.x) %>%
    dplyr::select(-quarter.x, -quarter.y)
}

cat("Level 1 合併後筆數:", nrow(L1), "\n")

# 1-D. 時間序列分析: 市場報酬 vs AI 論文 / 引
cat("\n===== 1-D. 時間序列分析 =====\n")

L1_papers_vw <- ts_analysis(L1, "d_log_papers", "mkt_vw",
                            "市值加權市場報酬 vs AI論文成長")
L1_papers_ew <- ts_analysis(L1, "d_log_papers", "mkt_ew",
                            "等權重市場報酬 vs AI論文成長")
L1_cit_vw    <- ts_analysis(L1, "d_log_citations", "mkt_vw",
                            "市值加權市場報酬 vs AI引用成長(未截斷期)")

# IRF
irf_L1 <- var_irf(L1, "d_log_papers", "mkt_vw", "Market(VW) <- AI papers")

#  Level 2 | SECTOR — 科技業(市值加權)報酬 vs AI
cat("\n\n##############################################\n")
cat("#  LEVEL 2 | SECTOR (科技業)\n")
cat("##############################################\n")

# 本資料 9 檔全是科技股,故科技業報酬 = Level 1 的市值加權報酬
sector <- all_monthly %>%
  group_by(month) %>%
  summarise(tech_vw = sum(log_return * w) / sum(w),
            tech_ew = mean(log_return), .groups = "drop") %>%
  mutate(quarter = floor_date(month, "quarter"))

# sector 的季度描述統計
sector_quarter_desc <- sector %>%
  group_by(quarter) %>%
  summarise(
    tech_vw_q = sum(tech_vw, na.rm = TRUE),
    tech_ew_q = sum(tech_ew, na.rm = TRUE),
    avg_monthly_tech_vw = mean(tech_vw, na.rm = TRUE),
    avg_monthly_tech_ew = mean(tech_ew, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n[科技業季度報酬描述統計]\n"); print(sector_quarter_desc)

L2 <- sector %>% inner_join(ai_macro, by = "month") %>% arrange(month)

if ("quarter.x" %in% names(L2) && "quarter.y" %in% names(L2)) {
  L2 <- L2 %>%
    mutate(quarter = quarter.x) %>%
    dplyr::select(-quarter.x, -quarter.y)
}

cat("\n===== 2. 科技業時間序列分析 =====\n")

L2_papers <- ts_analysis(L2, "d_log_papers", "tech_vw",
                         "科技業(市值加權)報酬 vs AI論文成長")
L2_cit    <- ts_analysis(L2, "d_log_citations", "tech_vw",
                         "科技業(市值加權)報酬 vs AI引用成長")

irf_L2 <- var_irf(L2, "d_log_papers", "tech_vw", "Tech(VW) <- AI papers")


#  Level 3 | SUBFIELD — 子領域 vs 對應產業
cat("\n\n##############################################\n")
cat("#  LEVEL 3 | SUBFIELD\n")
cat("##############################################\n")

# 子領域 wide + log 差分
sf_wide <- sf_raw %>%
  dplyr::select(month, ai_subfield, ai_paper_count) %>%
  pivot_wider(names_from = ai_subfield, values_from = ai_paper_count,
              values_fill = 0) %>%
  arrange(month)

sub_cols <- setdiff(colnames(sf_wide), "month")

sf_tr <- sf_wide
for (col in sub_cols) {
  lv <- log(sf_tr[[col]] + 1)
  sf_tr[[paste0("d_log_", col)]] <- c(NA, diff(lv))
}

cat("\n[子領域差分變異檢查]\n")
for (col in sub_cols) {
  s <- sd(sf_tr[[paste0("d_log_", col)]], na.rm = TRUE)
  cat(sprintf("  d_log_%-18s sd=%.4f %s\n", col, s,
              if (is.na(s) || s == 0) "★異常" else "OK"))
}

# 配對表 (子領域 -> 對應產業公司)
pairings <- list(
  LLM_NLP          = c("MSFT","GOOGL","GOOG","META","AMZN"),
  Computer_Vision  = c("NVDA","TSLA"),
  Robotics         = c("NVDA","AVGO","TSLA"),
  Machine_Learning = tech_tickers,
  General_AI       = c("MSFT","GOOGL","META","NVDA","AMZN"),
  Neural_Networks  = c("NVDA","AVGO","GOOGL")
)

# 只保留資料中存在的子領域
pairings <- pairings[names(pairings) %in% sub_cols]

cat("\n將分析的配對:\n")
for (nm in names(pairings))
  cat(sprintf("  %-18s -> %s\n", nm, paste(pairings[[nm]], collapse=", ")))

# 3-A. 個別配對 ARDL + Granger
cat("\n===== 3-A. 子領域 x 個股 ARDL =====\n")

stock_all <- make_stock_monthly(sp500, tech_tickers)

analyze_pair <- function(sf_name, ticker) {
  d_col <- paste0("d_log_", sf_name)
  df <- stock_all %>% filter(Ticker == ticker) %>%
    inner_join(sf_tr %>% dplyr::select(month, all_of(d_col)), by="month") %>%
    rename(x = !!d_col) %>%
    mutate(x_l1 = dplyr::lag(x,1), x_l2 = dplyr::lag(x,2),
           x_l3 = dplyr::lag(x,3), y_l1 = dplyr::lag(log_return,1)) %>%
    drop_na()
  if (nrow(df) < 30 || sd(df$x) == 0 || sd(df$log_return) == 0) return(NULL)
  m <- lm(log_return ~ x + x_l1 + x_l2 + x_l3 + y_l1, data = df)
  hac <- coeftest(m, vcov = NeweyWest(m, lag=3, prewhite=FALSE))
  g <- tryCatch(grangertest(log_return ~ x, order=2,
                            data=df[,c("x","log_return")]), error=function(e) NULL)
  data.frame(
    subfield = sf_name, ticker = ticker, n = nrow(df),
    coef_contemp = round(hac["x","Estimate"], 4),
    p_contemp    = round(hac["x","Pr(>|t|)"], 3),
    cum_4M       = round(sum(coef(m)[c("x","x_l1","x_l2","x_l3")], na.rm=T), 4),
    r2           = round(summary(m)$r.squared, 3),
    granger_p    = round(if(!is.null(g)) g$`Pr(>F)`[2] else NA, 3)
  )
}

L3_table <- map_dfr(names(pairings), function(sf_name) {
  map_dfr(pairings[[sf_name]], function(tk) analyze_pair(sf_name, tk))
})

cat("\n[子領域配對結果總表]\n"); print(L3_table)
write_csv(L3_table, "out_L3_pair_results.csv")

# 3-B. Panel FE (含整體 AI 熱度控制)
cat("\n===== 3-B. Panel FE (控制整體 AI 熱度) =====\n")

ai_total_ctrl <- ai_macro %>% dplyr::select(month, d_log_papers) %>%
  rename(d_log_total = d_log_papers)

L3_panel_summary <- list()
for (sf_name in names(pairings)) {
  d_col <- paste0("d_log_", sf_name)
  pdf <- stock_all %>% filter(Ticker %in% pairings[[sf_name]]) %>%
    inner_join(sf_tr %>% dplyr::select(month, all_of(d_col)), by="month") %>%
    rename(x = !!d_col) %>%
    left_join(ai_total_ctrl, by="month") %>%
    group_by(Ticker) %>% arrange(month) %>%
    mutate(x_l1=dplyr::lag(x,1), x_l2=dplyr::lag(x,2),
           y_l1=dplyr::lag(log_return,1)) %>%
    ungroup() %>% drop_na()
  if (nrow(pdf) < 50) next
  
  fe_ctrl <- plm(log_return ~ x + x_l1 + x_l2 + d_log_total + y_l1,
                 data = pdf, index = c("Ticker","month"), model = "within")
  cat(sprintf("\n--- %s (firms=%d, obs=%d) [控制整體AI熱度] ---\n",
              sf_name, n_distinct(pdf$Ticker), nrow(pdf)))
  print(coeftest(fe_ctrl, vcov = vcovHC(fe_ctrl, type="HC1",
                                        cluster="group", method="arellano")))
  L3_panel_summary[[sf_name]] <- fe_ctrl
}

# 3-C. 各子領域 IRF
cat("\n===== 3-C. 子領域 VAR/IRF =====\n")

irf_L3 <- map_dfr(names(pairings), function(sf_name) {
  d_col <- paste0("d_log_", sf_name)
  sec <- stock_all %>% filter(Ticker %in% pairings[[sf_name]]) %>%
    group_by(month) %>% summarise(sec_ret = mean(log_return), .groups="drop")
  dd <- sec %>% inner_join(sf_tr %>% dplyr::select(month, all_of(d_col)),
                           by="month") %>% rename(x = !!d_col)
  res <- var_irf(dd, "x", "sec_ret", sf_name)
  if (is.null(res)) return(NULL)
  res
})


#  視覺化
cat("\n\n===== 產生圖表 =====\n")

# 圖1: L1 趨勢 — AI 論文 vs 累積市場報酬
p1 <- ggplot(L1, aes(x = month)) +
  geom_col(aes(y = ai_papers / max(ai_papers) * max(cumsum(replace_na(mkt_vw,0)))),
           fill="steelblue", alpha=0.4) +
  geom_line(aes(y = cumsum(replace_na(mkt_vw,0))), color="darkred", linewidth=1) +
  labs(title="Level 1: AI papers (bars) vs cumulative market return (line)",
       x=NULL, y="Cumulative VW market return") + theme_minimal()

# 圖2: L1 散點 (差分)
p2 <- ggplot(L1 %>% drop_na(d_log_papers, mkt_vw),
             aes(d_log_papers, mkt_vw)) +
  geom_point(alpha=.6) + geom_smooth(method="lm", color="darkred") +
  labs(title="Level 1: Market return vs Δlog(AI papers)",
       subtitle="差分後,去除共同趨勢的偽相關",
       x="AI 論文月成長率", y="市值加權市場報酬") + theme_minimal()

# 圖3: L1+L2 IRF
irf_macro <- bind_rows(irf_L1, irf_L2)

p3 <- ggplot(irf_macro, aes(h, irf)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=.3) +
  geom_line(color="darkblue", linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~label, scales="free_y") +
  labs(title="IRF: AI 論文成長衝擊 → 報酬 (12個月)",
       x="月", y="報酬反應") + theme_minimal()

# 圖4: L3 子領域 IRF
p4 <- ggplot(irf_L3, aes(h, irf)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=.3) +
  geom_line(color="darkblue", linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~label, scales="free_y") +
  labs(title="Level 3: 各子領域論文成長 → 對應產業報酬 IRF",
       x="月", y="報酬反應") + theme_minimal()

# 圖5: L3 熱力圖
p5 <- ggplot(L3_table, aes(ticker, subfield, fill=cum_4M)) +
  geom_tile(color="white") +
  geom_text(aes(label=sprintf("%.3f%s", cum_4M,
                              ifelse(p_contemp<0.1,"*",""))), size=3) +
  scale_fill_gradient2(low="#d62728", mid="white", high="#2ca02c", midpoint=0) +
  labs(title="Level 3: 子領域論文成長對個股的累積4個月效應",
       subtitle="* = 當期係數 p<0.10 (HAC)", x="股票", y="子領域") +
  theme_minimal()

# 圖6: 新增季度 AI 論文趨勢圖
p6 <- ggplot(desc_quarter, aes(x = quarter, y = papers)) +
  geom_col(fill = "steelblue", alpha = 0.6) +
  labs(title = "Quarterly AI papers",
       x = NULL, y = "AI papers per quarter") +
  theme_minimal()

# 圖7: 新增季度子領域趨勢圖
p7 <- ggplot(desc_cat_quarter, aes(x = quarter, y = papers, fill = ai_subfield)) +
  geom_col(position = "stack", alpha = 0.8) +
  labs(title = "Quarterly AI papers by subfield",
       x = NULL, y = "AI papers per quarter", fill = "Subfield") +
  theme_minimal()

for (p in list(p1,p2,p3,p4,p5,p6,p7)) print(p)

ggsave("fig_L1_trend.png", p1, width=10, height=5, dpi=150)
ggsave("fig_L1_scatter.png", p2, width=8, height=6, dpi=150)
ggsave("fig_L1L2_irf.png", p3, width=10, height=5, dpi=150)
ggsave("fig_L3_irf.png", p4, width=11, height=7, dpi=150)
ggsave("fig_L3_heatmap.png", p5, width=10, height=5, dpi=150)
ggsave("fig_quarterly_ai_papers.png", p6, width=10, height=5, dpi=150)
ggsave("fig_quarterly_ai_subfield.png", p7, width=11, height=6, dpi=150)


#  輸出彙整
write_csv(L1, "out_L1_macro.csv")
write_csv(L2, "out_L2_sector.csv")
write_csv(desc_year, "out_desc_by_year.csv")
write_csv(desc_cat, "out_desc_by_category.csv")

# 季度輸出
write_csv(desc_quarter, "out_desc_by_quarter.csv")
write_csv(desc_cat_quarter, "out_desc_by_quarter_category.csv")
write_csv(market_quarter_desc, "out_market_by_quarter.csv")
write_csv(sector_quarter_desc, "out_sector_by_quarter.csv")

cat("\n=============================================\n")
cat(" 完成! 輸出檔案:\n")
cat("  月頻主分析資料:\n")
cat("    out_L1_macro.csv, out_L2_sector.csv,\n")
cat("    out_L3_pair_results.csv\n")
cat("  年度/類別描述統計:\n")
cat("    out_desc_by_year.csv, out_desc_by_category.csv\n")
cat("  新增季度描述統計:\n")
cat("    out_desc_by_quarter.csv,\n")
cat("    out_desc_by_quarter_category.csv,\n")
cat("    out_market_by_quarter.csv,\n")
cat("    out_sector_by_quarter.csv\n")
cat("  圖表:\n")
cat("    fig_L1_trend.png, fig_L1_scatter.png,\n")
cat("    fig_L1L2_irf.png, fig_L3_irf.png, fig_L3_heatmap.png,\n")
cat("    fig_quarterly_ai_papers.png, fig_quarterly_ai_subfield.png\n")
cat("=============================================\n")
