# ============================================================
#  pre2022_subsample_analysis.R
#  問題:切開 2022 前後,2022 之前公司論文對自身股價
#        是否有正相關 / 推動效果?
#
#  2017-2021 的 OpenAlex 涵蓋率穩定 (見 rd_disclosure_analysis.R
#  Part 0),因此子樣本分析不受 post-2021 artifact 污染。
#
#  兩個檢定 (皆已修正 METHOD_REVIEW.md 指出的問題:
#  用超額報酬、只用落後項、論文成長率先做季節調整):
#
#  (1) 公司層月頻:落後 1-3 月論文成長(季調) -> 當月超額報酬
#      - 個別公司 ARDL + Newey-West
#      - Pooled panel + firm FE + month-of-sample FE
#        (時間固定效果吸收整體市場與 AI 熱度,認定只來自
#         「同一個月內,誰的論文成長比其他公司快」)
#  (2) 日頻事件研究:notable AI model (Epoch AI,多半伴隨論文)
#      發布日的異常報酬,pre-2022 vs post-2022
#      AR = 自身日報酬 - 其他 7 檔等權平均 (leave-one-out,
#           扣掉科技股共同波動,留下公司特有反應)
#
#  需要檔案: panel_monthly.csv, stock_raw.rds, all_ai_models.csv
# ============================================================

# setwd("C:/Users/User/BIG_DATA/LEVEL4")   # <- 改成你的資料夾

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate)
  library(stringr); library(purrr); library(lmtest); library(sandwich)
})
filter <- dplyr::filter; select <- dplyr::select

BREAK_DATE <- as.Date("2022-01-01")
FIRMS <- c("GOOGL","MSFT","META","AMZN","NVDA","AAPL")

# ============================================================
# (1) 公司層月頻子樣本迴歸
# ============================================================
cat("===== (1) 月頻:落後論文成長(季調) -> 超額報酬 =====\n")

pm <- read_csv("panel_monthly.csv", show_col_types = FALSE) %>%
  filter(ticker != "GOOG") %>%
  mutate(month_date = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
  arrange(ticker, month_date)

run_firm <- function(tk, yr_max) {
  d <- pm %>% filter(ticker == tk, year <= yr_max) %>%
    mutate(x = c(NA, diff(log(n_papers + 1))), mo = factor(month))
  ok <- !is.na(d$x)
  d$x_sa <- NA_real_
  d$x_sa[ok] <- resid(lm(x ~ mo, data = d[ok, ]))      # 季節調整
  d <- d %>%
    mutate(x1 = lag(x_sa,1), x2 = lag(x_sa,2), x3 = lag(x_sa,3)) %>%
    drop_na(excess_return, x1, x2, x3)
  if (nrow(d) < 40) return(NULL)                       # 樣本太短跳過
  m  <- lm(excess_return ~ x1 + x2 + x3, data = d)
  V  <- NeweyWest(m, lag = 3, prewhite = FALSE)
  L  <- c(0,1,1,1)
  cum <- sum(coef(m)[2:4]); se <- as.numeric(sqrt(t(L) %*% V %*% L))
  data.frame(ticker = tk, n = nrow(d),
             cum_lag123 = round(cum, 4), se = round(se, 4),
             p_cum = round(2*pnorm(abs(cum/se), lower.tail = FALSE), 3))
}

cat("\n[A] 2017-2021 子樣本 (每家 ~56 個月):\n")
print(bind_rows(lapply(FIRMS, run_firm, yr_max = 2021)), row.names = FALSE)
cat("  注意:5-6 家同時檢定,單一 p<0.05 需經多重檢定校正解讀\n")

cat("\n[B] 對照:全樣本 2017-2025:\n")
print(bind_rows(lapply(FIRMS, run_firm, yr_max = 2025)), row.names = FALSE)

cat("\n[C] Pooled panel 2017-2021, firm FE + month FE, cluster by month:\n")
pd <- pm %>% filter(ticker %in% FIRMS, year <= 2021) %>%
  group_by(ticker) %>% arrange(month_date) %>%
  mutate(x = c(NA, diff(log(n_papers + 1))), mo = factor(month)) %>% ungroup()
ok <- !is.na(pd$x)
pd$x_sa <- NA_real_
pd$x_sa[ok] <- resid(lm(x ~ mo:ticker, data = pd[ok, ]))
pd <- pd %>% group_by(ticker) %>% arrange(month_date) %>%
  mutate(x1 = lag(x_sa,1), x2 = lag(x_sa,2), x3 = lag(x_sa,3)) %>%
  ungroup() %>% drop_na(monthly_return, x1, x2, x3)
mp <- lm(monthly_return ~ x1 + x2 + x3 + factor(ticker) + factor(month_date),
         data = pd)
Vp <- vcovCL(mp, cluster = pd$month_date, type = "HC1")
print(coeftest(mp, vcov = Vp)[c("x1","x2","x3"), ])
L <- setNames(rep(0, length(coef(mp))), names(coef(mp))); L[c("x1","x2","x3")] <- 1
cum <- sum(coef(mp)[c("x1","x2","x3")]); se <- as.numeric(sqrt(t(L) %*% Vp %*% L))
cat(sprintf("累積(1-3月): %+.4f (se=%.4f, p=%.3f), N=%d firm-months\n",
            cum, se, 2*pnorm(abs(cum/se), lower.tail = FALSE), nrow(pd)))

# ============================================================
# (2) 日頻事件研究:notable model 發布日
# ============================================================
cat("\n===== (2) 事件研究:notable model 發布日異常報酬 =====\n")

st <- readRDS("stock_raw.rds") %>%
  transmute(date = as.Date(date), ticker = as.character(symbol),
            adj = as.numeric(adjusted)) %>%
  filter(ticker != "GOOG") %>% arrange(ticker, date) %>%
  group_by(ticker) %>% mutate(r = log(adj / lag(adj))) %>% ungroup() %>%
  filter(!is.na(r))

mkt <- st %>% group_by(date) %>%
  mutate(r_mkt_ex = (sum(r) - r) / (n() - 1),   # 其他 7 檔等權
         ar = r - r_mkt_ex) %>% ungroup()

mr <- read_csv("all_ai_models.csv", show_col_types = FALSE)
PAT <- list(GOOGL = "(?i)google|deepmind|alphabet|waymo",
            MSFT = "(?i)microsoft", META = "(?i)\\bmeta\\b|facebook",
            NVDA = "(?i)nvidia", AMZN = "(?i)amazon|\\baws\\b",
            AAPL = "(?i)apple")
ev <- map_dfr(names(PAT), function(tk)
  mr %>% filter(str_detect(coalesce(Organization, ""), PAT[[tk]])) %>%
    transmute(ticker = tk, edate = as.Date(`Publication date`))) %>%
  filter(!is.na(edate), edate >= as.Date("2017-01-01")) %>%
  distinct(ticker, edate) %>%
  mutate(period = ifelse(edate < BREAK_DATE, "pre2022", "post2022"))

cat("\n[事件數 by 公司 x 期間]\n"); print(table(ev$ticker, ev$period))

car_one <- function(tk, ed, w_from, w_to) {
  d <- mkt %>% filter(ticker == tk) %>% arrange(date)
  i0 <- which(d$date >= ed)[1]                 # 事件日或次一交易日
  if (is.na(i0)) return(NA_real_)
  idx <- (i0 + w_from):(i0 + w_to)
  if (min(idx) < 1 || max(idx) > nrow(d)) return(NA_real_)
  sum(d$ar[idx])
}

windows <- list(c(0,0), c(0,1), c(-1,1), c(0,5))
res <- map_dfr(windows, function(w) {
  ev %>% rowwise() %>%
    mutate(car = car_one(ticker, edate, w[1], w[2])) %>% ungroup() %>%
    filter(!is.na(car)) %>%
    group_by(period) %>%
    summarise(window = sprintf("[%d,%+d]", w[1], w[2]), n = n(),
              mean_car_pct = round(100 * mean(car), 3),
              t = round(mean(car) / (sd(car)/sqrt(n())), 2),
              p = round(2*pt(abs(mean(car)/(sd(car)/sqrt(n()))), n()-1,
                             lower.tail = FALSE), 3),
              .groups = "drop")
})
cat("\n[平均 CAR by 期間 x 窗口]\n")
cat("  caveat: 事件在時間上有群聚 (尤其 GOOGL),t 檢定假設事件獨立,\n")
cat("  嚴謹版應以 calendar-time portfolio 或 clustered bootstrap 重估\n\n")
print(as.data.frame(res %>% arrange(window, desc(period))), row.names = FALSE)

cat("\n完成。\n")
