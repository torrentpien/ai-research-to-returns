# ============================================================
#  citation_effect_analysis.R
#  問題:citation 數字是否對股票價格有影響?
#  方法:與 pre2022_subsample_analysis.R 相同的三重驗證
#    (1) 個別公司 ARDL:落後 1-6 月 citation 成長(季調) -> 超額報酬
#    (2) Pooled panel:firm FE + month FE, lag 1-12
#    (3) 事件研究:「事後高被引論文」發表日的異常報酬
#
#  ★ 認定上的關鍵限制:cited_by_count 是資料檢索日 (2026) 的
#    累積值,論文發表當下市場看不到這個數字。因此本檔檢驗的是
#    「事後證明重要(高被引)的研究是否伴隨股價效果」,
#    不是「市場對 citation 資訊的即時反應」。
#
#  citation 年齡截斷 (truncation) 的處理:
#    - 月頻分析限 2017-2021:檢索時所有論文已累積 >=4 年引用
#    - 事件研究以「同發表年 cohort 內的百分位 (top 5%)」定義
#      高被引,同齡互比,消除年齡造成的機械性差異
#
#  需要檔案: panel_monthly.csv, stock_raw.rds, paper_company_panel.rds
# ============================================================

# setwd("C:/Users/User/BIG_DATA/LEVEL4")   # <- 改成你的資料夾

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate)
  library(stringr); library(purrr); library(lmtest); library(sandwich)
})
filter <- dplyr::filter; select <- dplyr::select

FIRMS <- c("GOOGL","MSFT","META","AMZN","NVDA","AAPL")
BREAK_DATE <- as.Date("2022-01-01")

pm <- read_csv("panel_monthly.csv", show_col_types = FALSE) %>%
  filter(ticker != "GOOG") %>%
  mutate(month_date = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
  arrange(ticker, month_date)

cat("===== 公司月 citation 摘要 (2017-2021) =====\n")
pm %>% filter(year <= 2021, ticker %in% FIRMS) %>% group_by(ticker) %>%
  summarise(mean_cit = round(mean(total_citations)),
            zero_months = sum(total_citations == 0), n = n(),
            .groups = "drop") %>% print()

# ============================================================
# (1) 個別公司 ARDL:x = d_log(月 total_citations + 1)
# ============================================================
cat("\n===== (1) 個別公司 2017-2021: 累積 lag 1..6 =====\n")

scan_firm_cit <- function(tk, H = 6, yr_max = 2021) {
  d <- pm %>% filter(ticker == tk, year <= yr_max) %>%
    mutate(x = c(NA, diff(log(total_citations + 1))), mo = factor(month))
  ok <- !is.na(d$x); d$x_sa <- NA_real_
  d$x_sa[ok] <- resid(lm(x ~ mo, data = d[ok, ]))       # 季節調整
  for (L in 1:H) d[[paste0("x", L)]] <- lag(d$x_sa, L)
  d <- d %>% drop_na(excess_return, all_of(paste0("x", 1:H)))
  if (nrow(d) < 40) return(NULL)
  f <- as.formula(paste("excess_return ~", paste0("x", 1:H, collapse = "+")))
  m <- lm(f, data = d); V <- NeweyWest(m, lag = 6, prewhite = FALSE)
  map_dfr(1:H, function(h) {
    cols <- paste0("x", 1:h)
    L <- setNames(rep(0, length(coef(m))), names(coef(m))); L[cols] <- 1
    cum <- sum(coef(m)[cols]); se <- as.numeric(sqrt(t(L) %*% V %*% L))
    data.frame(ticker = tk, h = h, cum = round(cum, 3),
               p = round(2*pnorm(abs(cum/se), lower.tail = FALSE), 3))
  })
}
r1 <- bind_rows(lapply(FIRMS, scan_firm_cit))
print(r1 %>% mutate(cell = sprintf("%+.2f(p=%.2f)", cum, p)) %>%
        select(ticker, h, cell) %>%
        pivot_wider(names_from = h, values_from = cell, names_prefix = "h") %>%
        as.data.frame(), row.names = FALSE)

# ============================================================
# (2) Pooled panel:firm FE + month FE, lag 1..12
# ============================================================
cat("\n===== (2) Pooled 2017-2021, firm FE + month FE, lag 1..12 =====\n")
H <- 12
pd <- pm %>% filter(ticker %in% FIRMS, year <= 2021) %>%
  group_by(ticker) %>% arrange(month_date) %>%
  mutate(x = c(NA, diff(log(total_citations + 1))), mo = factor(month)) %>%
  ungroup()
ok <- !is.na(pd$x); pd$x_sa <- NA_real_
pd$x_sa[ok] <- resid(lm(x ~ mo:ticker, data = pd[ok, ]))
for (L in 1:H) pd[[paste0("x", L)]] <-
  ave(pd$x_sa, pd$ticker, FUN = function(v) dplyr::lag(v, L))
pd <- pd %>% drop_na(monthly_return, all_of(paste0("x", 1:H)))
f <- as.formula(paste("monthly_return ~", paste0("x", 1:H, collapse = "+"),
                      "+ factor(ticker) + factor(month_date)"))
mp <- lm(f, data = pd)
Vp <- vcovCL(mp, cluster = pd$month_date, type = "HC1")
r2 <- map_dfr(1:H, function(h) {
  cols <- paste0("x", 1:h)
  L <- setNames(rep(0, length(coef(mp))), names(coef(mp))); L[cols] <- 1
  cum <- sum(coef(mp)[cols]); se <- as.numeric(sqrt(t(L) %*% Vp %*% L))
  data.frame(h = h, cum = round(cum, 4), se = round(se, 4),
             p = round(2*pnorm(abs(cum/se), lower.tail = FALSE), 3))
})
cat(sprintf("N=%d firm-months\n", nrow(pd)))
print(r2, row.names = FALSE)
cat("  注意:12 個 horizon 同時檢定,僅最長 horizon 邊際顯著 (p~0.09)\n")
cat("  不足以構成證據;且最長 horizon 樣本重疊最嚴重。\n")

# ============================================================
# (3) 事件研究:事後高被引論文 (同年 cohort top 5%) 發表日
# ============================================================
cat("\n===== (3) 事件研究:高被引論文發表日 =====\n")

pcp <- readRDS("paper_company_panel.rds")
TK <- function(nm) case_when(
  grepl("Google|DeepMind", nm)    ~ "GOOGL",
  grepl("Microsoft|LinkedIn", nm) ~ "MSFT",
  grepl("Meta ", nm)              ~ "META",
  grepl("Nvidia", nm)             ~ "NVDA",
  grepl("Amazon", nm)             ~ "AMZN",
  grepl("Apple", nm)              ~ "AAPL",
  TRUE ~ NA_character_)

w8 <- pcp %>%
  mutate(pub_date = as.Date(publication_date), yr = year(pub_date),
         tk = TK(institution_name)) %>%
  filter(!is.na(tk), !is.na(pub_date), yr >= 2017, yr <= 2025) %>%
  distinct(work_id, tk, .keep_all = TRUE) %>%
  group_by(yr) %>%                                   # 同齡互比,消除截斷
  mutate(cit_pctl = percent_rank(cited_by_count)) %>% ungroup()

ev <- w8 %>% filter(cit_pctl >= 0.95) %>%
  distinct(tk, pub_date) %>%
  mutate(period = ifelse(pub_date < BREAK_DATE, "pre2022", "post2022"))
cat("\n[高被引論文事件數 by 公司 x 期間]\n"); print(table(ev$tk, ev$period))

st <- readRDS("stock_raw.rds") %>%
  transmute(date = as.Date(date), ticker = as.character(symbol),
            adj = as.numeric(adjusted)) %>%
  filter(ticker != "GOOG") %>% arrange(ticker, date) %>%
  group_by(ticker) %>% mutate(r = log(adj / lag(adj))) %>% ungroup() %>%
  filter(!is.na(r))
mkt <- st %>% group_by(date) %>%
  mutate(r_mkt_ex = (sum(r) - r) / (n() - 1), ar = r - r_mkt_ex) %>% ungroup()

car_one <- function(tk, ed, a, b) {
  d <- mkt %>% filter(ticker == tk)
  i0 <- which(d$date >= ed)[1]; if (is.na(i0)) return(NA_real_)
  idx <- (i0 + a):(i0 + b)
  if (min(idx) < 1 || max(idx) > nrow(d)) return(NA_real_)
  sum(d$ar[idx])
}
wins <- list(c(0,1), c(-1,1), c(0,5))
r3 <- map_dfr(wins, function(w) {
  ev %>% rowwise() %>%
    mutate(car = car_one(tk, pub_date, w[1], w[2])) %>% ungroup() %>%
    filter(!is.na(car)) %>% group_by(period) %>%
    summarise(window = sprintf("[%d,%+d]", w[1], w[2]), n = n(),
              mean_car_pct = round(100*mean(car), 3),
              t = round(mean(car)/(sd(car)/sqrt(n())), 2), .groups = "drop")
})
print(as.data.frame(r3 %>% arrange(window, desc(period))), row.names = FALSE)
cat("\n  caveat: GOOGL 佔 pre-2022 事件約一半,事件群聚與 benchmark drift\n")
cat("  同 pre2022_subsample_analysis.R 的事件研究;負向 CAR 只能解讀為\n")
cat("  「沒有正向反應」,不能解讀為高被引論文傷害股價。\n")

cat("\n完成。\n")
