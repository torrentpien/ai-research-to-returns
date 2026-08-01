# ============================================================
#  time_series_model.R
#  時間序列版本
#    依變項  :股票價值 (log 等權價格指數,「水準」而非報酬)
#    自變項  :AI 論文發表數 (2022 斷點:不區分 / 區分兩種版本)
#    控制變項:研發經費、專利公開數、notable AI models
#
#  ------------------------------------------------------------
#  【為什麼時間序列版本可以用「股價水準」當依變項?】
#
#  panel 版 (firm_value_model.R) 被迫用報酬,是因為缺資產負債表
#  資料無法建 Tobin's Q,而水準對水準的迴歸會是偽迴歸。
#  但在單一時間序列裡,偽迴歸有正規解法:共整合 (cointegration)。
#  若 log(股價) 與 log(論文數) 都是 I(1),且兩者的線性組合是 I(0),
#  代表存在「長期均衡關係」,此時水準迴歸不但不偽,還正是
#  Engle & Granger (1987) 定義下的長期關係估計。
#  反之若檢定不出共整合,就證明「兩條線一起漲」確實只是
#  共同趨勢 (Granger & Newbold 1974),必須退回差分。
#  => 所以本檔的核心不是「跑一條迴歸」,而是「先檢定能不能跑」。
#
#  【方法順序】
#   1. 單根檢定 (ADF + KPSS 雙向確認) → 判定各序列的整合階數
#   2. 共整合檢定 (三種方法交叉驗證):
#        a. Engle-Granger 兩階段殘差檢定
#        b. Johansen trace test (urca)
#        c. ARDL Bounds test (Pesaran, Shin & Smith 2001)
#           ★ 主要方法:允許 I(0)/I(1) 混合,小樣本表現較佳,
#             且已在專案 README 的參考文獻中
#   3. 視結果估長期係數 + 誤差修正模型 (ECM) 短期動態
#   4. 結構斷點:
#        a. 外生指定 2022 (Chow test + 虛擬變數/交互項)
#        b. ★ 內生未知斷點 (Quandt-Andrews supF + Bai-Perron)
#          讓資料自己指出斷點在哪,而非預設 2022 —— 若資料找到的
#          斷點不在 2022,則「2022 斷點」的敘事本身就需要修正
#   5. 各公司分別重跑 (6 條個別時間序列)
#
#  【已知限制】
#   - R&D 為年度資料,月頻化為階梯函數 (每年才跳一次),
#     其短期動態資訊有限,主要作為長期趨勢控制。
#   - 論文序列有強季節性 (投稿截止日),短期方程加入月份虛擬變數。
#   - 2022 後論文計數受 OpenAlex 涵蓋率污染 (見 rd_disclosure_analysis.R),
#     故斷點分析同時提供 share-based 版本作為對照。
#
#  需要檔案: stock_raw.rds, panel_monthly.csv, rd_expense_annual.csv,
#            8_company_patent_raw_data.csv, all_ai_models.csv, ai_monthly.csv
#  輸出目錄: ts_model_outputs/
# ============================================================

# setwd("C:/Users/User/BIG_DATA/LEVEL4")   # <- 改成你的資料夾

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate)
  library(stringr); library(purrr); library(lmtest); library(sandwich)
  library(tseries); library(urca); library(strucchange); library(zoo)
})
filter <- dplyr::filter; select <- dplyr::select
set.seed(42)

OUT_DIR <- "ts_model_outputs"; dir.create(OUT_DIR, showWarnings = FALSE)
FIRMS <- c("GOOGL","MSFT","META","AMZN","NVDA","AAPL")

# ============================================================
# 0. 建立月頻序列 (2017-01 ~ 2025-12, T = 108)
# ============================================================
cat("============================================================\n")
cat(" 0. 建立月頻時間序列\n")
cat("============================================================\n")

# --- 依變項:6 家等權價格指數 (log) ---
st <- readRDS("stock_raw.rds") %>%
  transmute(date = as.Date(date), ticker = as.character(symbol),
            adj = as.numeric(adjusted)) %>%
  filter(ticker %in% FIRMS, !is.na(adj)) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(ticker, month) %>% arrange(date, .by_group = TRUE) %>%
  summarise(px = last(adj), .groups = "drop") %>%
  arrange(ticker, month) %>% group_by(ticker) %>%
  mutate(lr = log(px / lag(px))) %>% ungroup()

idx <- st %>% filter(!is.na(lr)) %>% group_by(month) %>%
  summarise(ew_lr = mean(lr), .groups = "drop") %>% arrange(month) %>%
  mutate(log_index = cumsum(ew_lr))          # log 等權價格指數

# --- 自變項:6 家公司自身 AI 論文 (月) ---
pmn <- read_csv("panel_monthly.csv", show_col_types = FALSE) %>%
  filter(ticker %in% FIRMS) %>%
  transmute(ticker, month = as.Date(sprintf("%04d-%02d-01", year, month)),
            n_papers)
grid <- expand_grid(ticker = FIRMS,
                    month = seq(as.Date("2017-01-01"), as.Date("2025-12-01"), by = "month"))
papers_m <- grid %>% left_join(pmn, by = c("ticker","month")) %>%
  mutate(n_papers = replace_na(n_papers, 0)) %>%
  group_by(month) %>% summarise(papers = sum(n_papers), .groups = "drop")

# 對照:全體 arXiv AI 論文 (不受單一公司 affiliation 標註影響)
ai_all <- read_csv("ai_monthly.csv", show_col_types = FALSE) %>%
  transmute(month = as.Date(month), papers_all = ai_papers)

# --- 控制:R&D (年度 → 月階梯) ---
rd_a <- read_csv("rd_expense_annual.csv", show_col_types = FALSE) %>%
  mutate(period_end = as.Date(fiscal_period_end,
                              tryFormats = c("%Y/%m/%d","%Y-%m-%d")),
         yr = year(period_end %m-% months(6)), rd_b = rd_expense_usd/1e9) %>%
  filter(ticker %in% FIRMS, yr >= 2016, yr <= 2025) %>%
  group_by(yr) %>% summarise(rd_b = sum(rd_b), .groups = "drop")
rd_m <- tibble(month = seq(as.Date("2017-01-01"), as.Date("2025-12-01"), by="month")) %>%
  mutate(yr = year(month)) %>% left_join(rd_a, by = "yr") %>%
  mutate(rd_b = na.locf(rd_b, na.rm = FALSE)) %>% select(month, rd_b)

# --- 控制:專利公開數 (月,真實月頻變動) ---
PMAP <- c("Google/Alphabet"="GOOGL","Microsoft"="MSFT","Meta/Facebook"="META",
          "NVIDIA"="NVDA","Amazon"="AMZN","Apple"="AAPL")
pat_m <- read_csv("8_company_patent_raw_data.csv", show_col_types = FALSE) %>%
  mutate(ticker = PMAP[parent_company]) %>%
  filter(!is.na(ticker), pub_year >= 2017, pub_year <= 2025,
         pub_month >= 1, pub_month <= 12) %>%
  group_by(month = as.Date(sprintf("%04d-%02d-01", pub_year, pub_month))) %>%
  summarise(patents = sum(patent_count), .groups = "drop")

# --- 控制:notable AI models (月) ---
MPAT <- list(GOOGL="(?i)google|deepmind|alphabet|waymo", MSFT="(?i)microsoft",
             META="(?i)\\bmeta\\b|facebook", NVDA="(?i)nvidia",
             AMZN="(?i)amazon|\\baws\\b", AAPL="(?i)apple")
mr <- read_csv("all_ai_models.csv", show_col_types = FALSE)
mod_m <- map_dfr(names(MPAT), function(tk)
  mr %>% filter(str_detect(coalesce(Organization, ""), MPAT[[tk]])) %>%
    transmute(month = floor_date(as.Date(`Publication date`), "month"))) %>%
  filter(!is.na(month)) %>% count(month, name = "models")

TS <- idx %>%
  left_join(papers_m, by = "month") %>% left_join(ai_all, by = "month") %>%
  left_join(rd_m, by = "month") %>% left_join(pat_m, by = "month") %>%
  left_join(mod_m, by = "month") %>%
  filter(month >= as.Date("2017-02-01"), month <= as.Date("2025-12-01")) %>%
  mutate(models = replace_na(models, 0),
         y        = log_index,
         lpapers  = log(papers + 1),
         lpapers_all = log(papers_all),
         lrd      = log(rd_b),
         lpat     = log(patents),
         lmod     = log(models + 1),
         post2022 = as.integer(month >= as.Date("2022-01-01")),
         mo       = factor(month(month))) %>%
  arrange(month)

cat(sprintf("T = %d 個月 (%s ~ %s)\n", nrow(TS),
            format(min(TS$month)), format(max(TS$month))))
write_csv(TS, file.path(OUT_DIR, "out_ts_monthly_series.csv"))

# ============================================================
# 1. 單根檢定:ADF (H0=有單根) + KPSS (H0=定態) 雙向確認
# ============================================================
cat("\n============================================================\n")
cat(" 1. 單根檢定 (整合階數判定)\n")
cat("============================================================\n")

ur_report <- function(x, nm) {
  x <- na.omit(x)
  a  <- suppressWarnings(adf.test(x))
  k  <- suppressWarnings(kpss.test(x))
  dx <- diff(x)
  a2 <- suppressWarnings(adf.test(dx))
  k2 <- suppressWarnings(kpss.test(dx))
  verdict <- if (a$p.value > 0.05 && k$p.value < 0.05 &&
                 a2$p.value < 0.05 && k2$p.value > 0.05) "I(1)"
             else if (a$p.value < 0.05 && k$p.value > 0.05) "I(0)"
             else "不明確"
  data.frame(series = nm,
             ADF_lv_p = round(a$p.value,3),  KPSS_lv_p = round(k$p.value,3),
             ADF_d1_p = round(a2$p.value,3), KPSS_d1_p = round(k2$p.value,3),
             verdict = verdict)
}
ur <- bind_rows(
  ur_report(TS$y,       "log_index (股價)"),
  ur_report(TS$lpapers, "log_papers (6家)"),
  ur_report(TS$lpapers_all, "log_papers (全arXiv)"),
  ur_report(TS$lrd,     "log_RD"),
  ur_report(TS$lpat,    "log_patents"),
  ur_report(TS$lmod,    "log_models"))
print(ur, row.names = FALSE)
cat("  註:KPSS p 值上下限被截斷於 0.01/0.10,故 0.1 代表 '>=0.1'\n")
write_csv(ur, file.path(OUT_DIR, "out_ts1_unitroot.csv"))

# ============================================================
# 2. 共整合檢定 (三法交叉驗證)
# ============================================================
cat("\n============================================================\n")
cat(" 2. 共整合檢定:股價 ~ 論文 + 研發 + 專利\n")
cat("============================================================\n")

# --- 2a. Engle-Granger ---
cat("\n[2a. Engle-Granger 兩階段]\n")
eg <- lm(y ~ lpapers + lrd + lpat, data = TS)
eg_res <- resid(eg)
eg_adf <- suppressWarnings(adf.test(eg_res))
cat(sprintf("  長期迴歸殘差 ADF: stat=%.3f, p=%.3f\n",
            eg_adf$statistic, eg_adf$p.value))
cat("  ★ 注意:殘差 ADF 不能用標準臨界值 (需 Engle-Granger 臨界值,更嚴格)\n")
cat("     k=3 時 EG 5% 臨界值約 -4.35;標準 ADF 5% 約 -3.43\n")
cat(sprintf("  => 以 EG 臨界值判斷:%s\n",
            ifelse(eg_adf$statistic < -4.35, "拒絕無共整合 (存在共整合)",
                   "無法拒絕『無共整合』")))

# --- 2b. Johansen trace test ---
cat("\n[2b. Johansen trace test]\n")
jo_data <- TS %>% select(y, lpapers, lrd, lpat) %>% as.matrix()
jo <- ca.jo(jo_data, type = "trace", ecdet = "const", K = 2)
jo_s <- summary(jo)
print(jo_s@teststat)
cat("臨界值 (10/5/1 pct):\n"); print(jo_s@cval)
r0 <- jo_s@teststat[length(jo_s@teststat)]      # r=0 的統計量
c5 <- jo_s@cval[nrow(jo_s@cval), "5pct"]
cat(sprintf("\n  r=0 檢定: stat=%.2f vs 5%% 臨界值=%.2f => %s\n", r0, c5,
            ifelse(r0 > c5, "拒絕 r=0 (至少一組共整合)", "無法拒絕 r=0 (無共整合)")))

# --- 2b-2. 弱外生性檢定:化解 Johansen 與 ARDL 的分歧 ★關鍵 ---
#   Johansen 檢定的是「整個系統裡有沒有共整合向量」,
#   但共整合向量存在,不代表『股價』就是會被拉回均衡的那個變數。
#   若股價的調整係數 alpha = 0 (弱外生),則該長期關係是存在於
#   其他變數之間 (例如 R&D 與專利長期同動),與股價無關。
cat("\n[2b-2. 弱外生性檢定:誰才是對長期均衡做調整的變數?]\n")
cat("  H0: 該變數的調整係數 alpha = 0 (不對長期均衡做修正)\n")
vn <- c("y (股價)","lpapers","lrd","lpat")
we <- map_dfr(1:4, function(i) {
  A <- diag(4)[, -i, drop = FALSE]
  tst <- tryCatch(alrtest(jo, A = A, r = 1), error = function(e) NULL)
  if (is.null(tst)) return(tibble())
  st_ <- summary(tst)@teststat
  tibble(series = vn[i], LR = round(st_, 3),
         p = round(pchisq(st_, 1, lower.tail = FALSE), 4),
         adjusts = ifelse(pchisq(st_, 1, lower.tail = FALSE) < 0.05,
                          "會調整", "弱外生(不調整)"))
})
print(as.data.frame(we), row.names = FALSE)
cat("  ★ 若 y(股價) 為弱外生,則 Johansen 找到的共整合關係與股價無關,\n")
cat("    這正是它與 ARDL Bounds / Engle-Granger 結論分歧的原因。\n")
write_csv(we, file.path(OUT_DIR, "out_ts2_weak_exogeneity.csv"))

# --- 2c. ARDL Bounds test (Pesaran, Shin & Smith 2001) ★主要方法 ---
cat("\n[2c. ARDL Bounds test (PSS 2001) — 主要方法]\n")
D <- TS %>%
  mutate(dy = c(NA, diff(y)), dlp = c(NA, diff(lpapers)),
         dlrd = c(NA, diff(lrd)), dlpat = c(NA, diff(lpat)),
         y_1 = lag(y), lp_1 = lag(lpapers), lrd_1 = lag(lrd), lpat_1 = lag(lpat),
         dy_1 = lag(dy), dlp_1 = lag(dlp), dlpat_1 = lag(dlpat)) %>%
  drop_na(dy, dlp, dlrd, dlpat, y_1, lp_1, lrd_1, lpat_1, dy_1, dlp_1, dlpat_1)

ecm_u <- lm(dy ~ y_1 + lp_1 + lrd_1 + lpat_1 +
              dlp + dlrd + dlpat + dy_1 + dlp_1 + dlpat_1 + mo, data = D)
ecm_r <- lm(dy ~ dlp + dlrd + dlpat + dy_1 + dlp_1 + dlpat_1 + mo, data = D)
bt <- anova(ecm_r, ecm_u)
Fstat <- bt$F[2]
cat(sprintf("  Bounds F = %.3f  (k=3 個自變項)\n", Fstat))
cat("  PSS(2001) Table CI(iii) 臨界值 [I(0) 下界, I(1) 上界]:\n")
cat("    10%: 2.72 - 3.77 | 5%: 3.23 - 4.35 | 1%: 4.29 - 5.61\n")
verdict <- if (Fstat > 4.35) "F > I(1) 上界 => 存在長期關係 (共整合)" else
           if (Fstat < 3.23) "F < I(0) 下界 => 無長期關係" else "落在上下界之間 => 不確定"
cat("  =>", verdict, "\n")
# ECT 係數 (y_1):顯著為負才是有效的誤差修正
cat(sprintf("  誤差修正項 y_{t-1} 係數 = %.4f (t = %.2f)\n",
            coef(ecm_u)["y_1"], coeftest(ecm_u)["y_1","t value"]))

# ============================================================
# 3. 短期動態 (差分 ARDL) + 2022 斷點
# ============================================================
cat("\n============================================================\n")
cat(" 3. 短期動態 ARDL (差分) 與 2022 斷點\n")
cat("============================================================\n")

S <- TS %>%
  mutate(dy = c(NA, diff(y)), dlp = c(NA, diff(lpapers)),
         dlrd = c(NA, diff(lrd)), dlpat = c(NA, diff(lpat)),
         dlmod = c(NA, diff(lmod))) %>%
  mutate(across(c(dlp, dlpat, dlmod), list(l1 = ~lag(.x,1), l2 = ~lag(.x,2),
                                           l3 = ~lag(.x,3)))) %>%
  mutate(dy_1 = lag(dy)) %>%
  drop_na(dy, dlp_l1, dlp_l2, dlp_l3, dlrd, dlpat_l1, dlmod_l1, dy_1)

hac <- function(m) coeftest(m, vcov = NeweyWest(m, lag = 6, prewhite = FALSE))
cum_test <- function(m, cols) {
  V <- NeweyWest(m, lag = 6, prewhite = FALSE)
  L <- setNames(rep(0, length(coef(m))), names(coef(m))); L[cols] <- 1
  b <- sum(coef(m)[cols]); se <- as.numeric(sqrt(t(L) %*% V %*% L))
  sprintf("%+.4f (se=%.4f, p=%.3f)", b, se, 2*pnorm(abs(b/se), lower.tail=FALSE))
}

cat("\n[3a. 不區分斷點]\n")
m_a <- lm(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + dlrd + dlpat_l1 + dlmod_l1 + dy_1 + mo,
          data = S)
print(hac(m_a)[c("dlp_l1","dlp_l2","dlp_l3","dlrd","dlpat_l1","dlmod_l1"), ])
cat("  論文累積 1-3 月效果:", cum_test(m_a, c("dlp_l1","dlp_l2","dlp_l3")), "\n")

cat("\n[3b. 區分斷點:2022 虛擬變數 + 交互項]\n")
S <- S %>% mutate(p1_post = dlp_l1*post2022, p2_post = dlp_l2*post2022,
                  p3_post = dlp_l3*post2022)
m_b <- lm(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + p1_post + p2_post + p3_post +
            post2022 + dlrd + dlpat_l1 + dlmod_l1 + dy_1 + mo, data = S)
print(hac(m_b)[c("dlp_l1","p1_post","post2022"), ])
cat("  pre-2022 累積:", cum_test(m_b, c("dlp_l1","dlp_l2","dlp_l3")), "\n")
cat("  斷點差異 (交互項合計):", cum_test(m_b, c("p1_post","p2_post","p3_post")), "\n")
wt <- waldtest(m_a, m_b, vcov = NeweyWest(m_b, lag=6, prewhite=FALSE))
cat(sprintf("  Chow 型 Wald (所有斷點項聯合=0): F=%.3f, p=%.3f\n",
            wt$F[2], wt$`Pr(>F)`[2]))

# ============================================================
# 4. 內生未知斷點:讓資料自己找斷點在哪
# ============================================================
cat("\n============================================================\n")
cat(" 4. 內生未知斷點檢定\n")
cat("============================================================\n")

bp_data <- S %>% select(dy, dlp_l1, dlp_l2, dlp_l3, dlrd, dlpat_l1) %>% na.omit()
fs <- Fstats(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + dlrd + dlpat_l1,
             data = bp_data, from = 0.15)
qa <- sctest(fs, type = "supF")
cat(sprintf("[Quandt-Andrews supF] stat=%.3f, p=%.4f => %s\n",
            qa$statistic, qa$p.value,
            ifelse(qa$p.value < 0.05, "存在結構斷點", "找不到顯著的結構斷點")))
peak_i <- which.max(fs$Fstats) + floor(0.15*nrow(bp_data))
peak_month <- S$month[peak_i]
cat(sprintf("  supF 最大值出現在第 %d 個觀測 ≈ %s\n", peak_i,
            ifelse(is.na(peak_month), "NA", format(peak_month))))

bp <- breakpoints(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + dlrd + dlpat_l1,
                  data = bp_data, h = 0.15)
cat("\n[Bai-Perron 多重斷點] BIC 選出的斷點數:",
    length(na.omit(bp$breakpoints)), "\n")
if (!all(is.na(bp$breakpoints)))
  cat("  斷點位置 ≈", paste(format(S$month[bp$breakpoints]), collapse=", "), "\n")
cat("  ★ 若資料找到的斷點不在 2022 年初,則『2022 斷點』的敘事需要修正\n")

# ============================================================
# 5. 各公司個別時間序列 (6 條)
# ============================================================
cat("\n============================================================\n")
cat(" 5. 各公司個別時間序列 ARDL (差分, 落後 1-3 月)\n")
cat("============================================================\n")

firm_ts <- map_dfr(FIRMS, function(tk) {
  d <- st %>% filter(ticker == tk) %>% select(month, lr) %>%
    left_join(grid %>% filter(ticker == tk) %>% select(month) %>%
                left_join(pmn %>% filter(ticker == tk), by = "month") %>%
                mutate(n_papers = replace_na(n_papers, 0)) %>% select(month, n_papers),
              by = "month") %>%
    left_join(read_csv("8_company_patent_raw_data.csv", show_col_types = FALSE) %>%
                mutate(ticker = PMAP[parent_company]) %>%
                filter(ticker == tk, pub_year >= 2017, pub_month >= 1) %>%
                group_by(month = as.Date(sprintf("%04d-%02d-01", pub_year, pub_month))) %>%
                summarise(patents = sum(patent_count), .groups = "drop"), by = "month") %>%
    arrange(month) %>%
    mutate(x = c(NA, diff(log(n_papers + 1))), dpat = c(NA, diff(log(patents + 1))),
           mo = factor(month(month))) %>%
    filter(!is.na(lr))
  ok <- !is.na(d$x); d$x_sa <- NA_real_
  if (sum(ok) < 40) return(tibble())
  d$x_sa[ok] <- resid(lm(x ~ mo, data = d[ok, ]))
  d <- d %>% mutate(x1 = lag(x_sa,1), x2 = lag(x_sa,2), x3 = lag(x_sa,3),
                    dpat1 = lag(dpat,1), y_1 = lag(lr)) %>%
    drop_na(lr, x1, x2, x3, dpat1, y_1)
  if (nrow(d) < 40) return(tibble())
  m <- lm(lr ~ x1 + x2 + x3 + dpat1 + y_1, data = d)
  V <- NeweyWest(m, lag = 6, prewhite = FALSE)
  L <- setNames(rep(0, length(coef(m))), names(coef(m))); L[c("x1","x2","x3")] <- 1
  b <- sum(coef(m)[c("x1","x2","x3")]); se <- as.numeric(sqrt(t(L) %*% V %*% L))
  tibble(ticker = tk, n = nrow(d), cum_1_3M = round(b,4), se = round(se,4),
         p = round(2*pnorm(abs(b/se), lower.tail = FALSE), 3))
})
print(as.data.frame(firm_ts), row.names = FALSE)
write_csv(firm_ts, file.path(OUT_DIR, "out_ts5_by_firm.csv"))

cat("\n完成。輸出於", OUT_DIR, "\n")
