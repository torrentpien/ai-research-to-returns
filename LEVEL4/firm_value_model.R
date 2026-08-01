# ============================================================
#  firm_value_model.R
#  依變項:股票價值   自變項:論文發表數(含 2022 斷點)
#  控制變項:研發經費、專利數、notable AI models
#
#  ------------------------------------------------------------
#  【模型選擇的三個關鍵決定,理由寫在這裡】
#
#  (1) 依變項為什麼是「報酬」而不是「股價/市值」?
#      創新→企業價值的標準模型是 Griliches (1981) / Hall, Jaffe &
#      Trajtenberg (2005) 的 market value equation,用 Tobin's Q:
#        log(V/A) = f(R&D/A, Patents/R&D, ...)
#      但它需要 total assets、book value、shares outstanding,
#      本專案資料沒有這些欄位(只有股價),因此無法計算市值或 Q。
#      在沒有 deflator 的情況下把股價「水準」對論文數迴歸,正是
#      本專案 Level 0 已經正確否定的偽迴歸。故依變項用超額報酬
#      (annual_return - sp500_return),它是定態的價值「變化率」。
#      => 若日後補上資產負債表資料,應改跑 Tobin's Q 版本,
#         那才是文獻標準的「價值」迴歸。
#
#  (2) 為什麼自變項用成長率而非水準?
#      報酬本身是價值的變化。若論文影響價值「水準」,則
#      論文的「變化」才對應報酬。growth-on-growth 才是量綱一致的。
#
#  (3) ★ 2022 斷點測試必須用 share-based 論文數 ★
#      原始論文計數在 2022 有一個「量測誤差的結構性斷點」
#      (OpenAlex 涵蓋率 9.9% -> 1.5%,見 rd_disclosure_analysis.R)。
#      用原始計數測 2022 斷點,post 期係數會因 attenuation bias
#      機械性地被壓向 0 —— 也就是說「效果在 2022 後消失」這個
#      結論會被量測誤差自動製造出來,不是真實的結構改變。
#      因此斷點模型以 share-based 論文數(公司論文/全公司論文池,
#      涵蓋率在分子分母抵銷)為主,原始計數僅作對照。
#
#  【樣本限制】
#      6 家公司 x 7 年 = 42 firm-years (AVGO 3篇/TSLA 1篇 已排除)。
#      cluster 數只有 6,一般 cluster-robust SE 嚴重低估標準誤,
#      故所有推論以 wild cluster bootstrap (Webb 6-point weights,
#      restricted/WCR) 為準。月頻版本提供較高檢定力作為交叉驗證。
#
#  【專利控制變項用 pub_year 而非 filing_year】
#      filing_year 受 18 個月公開延遲截斷 (2024 後嚴重低估),
#      pub_year 至 2025 皆完整;且專利「公開日」才是資訊進入
#      市場的時點,經濟意義上也更正確。
#
#  需要檔案: panel_annual.rds, panel_monthly.csv, rd_expense_annual.csv,
#            8_company_patent_raw_data.csv, all_ai_models.csv,
#            paper_company_panel.rds
#  輸出目錄: firm_value_outputs/
# ============================================================

# setwd("C:/Users/User/BIG_DATA/LEVEL4")   # <- 改成你的資料夾

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate)
  library(stringr); library(purrr); library(lmtest); library(sandwich)
})
filter <- dplyr::filter; select <- dplyr::select
set.seed(42)

OUT_DIR <- "firm_value_outputs"; dir.create(OUT_DIR, showWarnings = FALSE)
FIRMS <- c("GOOGL","MSFT","META","AMZN","NVDA","AAPL")
B_BOOT <- 999

# ============================================================
# 0. Wild cluster bootstrap (WCR, Webb 6-point weights)
#    6 個 cluster 時 Rademacher 只有 2^6=64 種抽法,p 值粒度太粗,
#    故用 Webb weights (6 點分布)。
# ============================================================
webb_draw <- function(n) {
  v <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  sample(v, n, replace = TRUE)
}

wcr_pvalue <- function(full_formula, restricted_var, data, cluster_var, B = B_BOOT) {
  # H0: restricted_var 的係數 = 0
  m_full <- lm(full_formula, data = data)
  V <- sandwich::vcovCL(m_full, cluster = data[[cluster_var]], type = "HC1")
  if (!restricted_var %in% names(coef(m_full))) return(list(t = NA, p = NA, beta = NA))
  t_obs <- coef(m_full)[restricted_var] / sqrt(V[restricted_var, restricted_var])

  # 受限模型 (把該變數拿掉) 取殘差與配適值
  rhs <- attr(terms(full_formula), "term.labels")
  rhs_r <- setdiff(rhs, restricted_var)
  f_r <- reformulate(rhs_r, response = all.vars(full_formula)[1])
  m_r <- lm(f_r, data = data)
  fit_r <- fitted(m_r); res_r <- resid(m_r)

  cl <- as.character(data[[cluster_var]]); ucl <- unique(cl)
  yname <- all.vars(full_formula)[1]
  t_star <- numeric(B)
  for (b in seq_len(B)) {
    w <- setNames(webb_draw(length(ucl)), ucl)
    d_b <- data; d_b[[yname]] <- fit_r + res_r * w[cl]
    m_b <- lm(full_formula, data = d_b)
    Vb <- sandwich::vcovCL(m_b, cluster = d_b[[cluster_var]], type = "HC1")
    t_star[b] <- coef(m_b)[restricted_var] / sqrt(Vb[restricted_var, restricted_var])
  }
  list(beta = unname(coef(m_full)[restricted_var]), t = unname(t_obs),
       p = mean(abs(t_star) >= abs(t_obs), na.rm = TRUE))
}

report <- function(fml, data, vars, label) {
  cat("\n---------- ", label, " ----------\n")
  m <- lm(fml, data = data)
  cat(sprintf("N=%d, 參數=%d, 殘差自由度=%d, R2=%.3f\n",
              nrow(data), length(coef(m)), df.residual(m), summary(m)$r.squared))
  out <- map_dfr(vars, function(v) {
    r <- wcr_pvalue(fml, v, data, "ticker")
    data.frame(term = v, beta = round(r$beta, 4), t = round(r$t, 2),
               p_wildboot = round(r$p, 3))
  })
  print(out, row.names = FALSE)
  invisible(out)
}

# ============================================================
# 1. 建立年度 panel
# ============================================================
cat("============================================================\n")
cat(" 建立年度 panel\n")
cat("============================================================\n")

pa <- readRDS("panel_annual.rds") %>%
  filter(ticker %in% FIRMS, year >= 2017, year <= 2025) %>%
  select(ticker, year, n_papers, total_citations, annual_return,
         sp500_return, excess_return)

# --- R&D (財報期間中點對齊日曆年) ---
rd <- read_csv("rd_expense_annual.csv", show_col_types = FALSE) %>%
  mutate(period_end = as.Date(fiscal_period_end,
                              tryFormats = c("%Y/%m/%d","%Y-%m-%d")),
         year = year(period_end %m-% months(6)),
         rd_b = rd_expense_usd / 1e9) %>%
  filter(ticker %in% FIRMS, year >= 2017, year <= 2025) %>%
  select(ticker, year, rd_b)

# --- 專利 (用 pub_year:未截斷,且為資訊公開時點) ---
PMAP <- c("Google/Alphabet"="GOOGL","Microsoft"="MSFT","Meta/Facebook"="META",
          "NVIDIA"="NVDA","Amazon"="AMZN","Apple"="AAPL")
pat <- read_csv("8_company_patent_raw_data.csv", show_col_types = FALSE) %>%
  mutate(ticker = PMAP[parent_company]) %>%
  filter(!is.na(ticker), pub_year >= 2016, pub_year <= 2025) %>%
  group_by(ticker, year = pub_year) %>%
  summarise(patents = sum(patent_count), .groups = "drop")

# --- notable AI models ---
MPAT <- list(GOOGL="(?i)google|deepmind|alphabet|waymo", MSFT="(?i)microsoft",
             META="(?i)\\bmeta\\b|facebook", NVDA="(?i)nvidia",
             AMZN="(?i)amazon|\\baws\\b", AAPL="(?i)apple")
mr <- read_csv("all_ai_models.csv", show_col_types = FALSE)
mods <- map_dfr(names(MPAT), function(tk)
  mr %>% filter(str_detect(coalesce(Organization, ""), MPAT[[tk]])) %>%
    transmute(ticker = tk, year = year(as.Date(`Publication date`)))) %>%
  filter(!is.na(year), year >= 2016, year <= 2025) %>%
  count(ticker, year, name = "models")

# --- share-based 論文數 (artifact-robust) ---
pcp <- readRDS("paper_company_panel.rds")
TK <- function(nm) case_when(
  grepl("Google|DeepMind", nm) ~ "GOOGL", grepl("Microsoft|LinkedIn", nm) ~ "MSFT",
  grepl("Meta ", nm) ~ "META", grepl("Nvidia", nm) ~ "NVDA",
  grepl("Amazon", nm) ~ "AMZN", grepl("Apple", nm) ~ "AAPL", TRUE ~ NA_character_)
pool <- pcp %>% mutate(yr = year(as.Date(publication_date))) %>%
  filter(yr >= 2017, yr <= 2025) %>% distinct(work_id, yr) %>%
  count(yr, name = "pool")
share <- pcp %>%
  mutate(yr = year(as.Date(publication_date)), tk = TK(institution_name)) %>%
  filter(!is.na(tk), yr >= 2017, yr <= 2025) %>%
  distinct(work_id, tk, yr) %>% count(tk, yr, name = "w") %>%
  left_join(pool, by = "yr") %>%
  transmute(ticker = tk, year = yr, paper_share = w / pool)

ann <- pa %>%
  left_join(rd, by = c("ticker","year")) %>%
  left_join(pat, by = c("ticker","year")) %>%
  left_join(mods, by = c("ticker","year")) %>%
  left_join(share, by = c("ticker","year")) %>%
  mutate(models = replace_na(models, 0)) %>%
  arrange(ticker, year) %>%
  group_by(ticker) %>%
  mutate(
    d_papers   = log(n_papers + 1)   - lag(log(n_papers + 1)),
    d_share    = log(paper_share)    - lag(log(paper_share)),
    d_rd       = log(rd_b)           - lag(log(rd_b)),
    d_patents  = log(patents)        - lag(log(patents)),
    d_models   = log(models + 1)     - lag(log(models + 1)),
    L_d_papers  = lag(d_papers),  L_d_share   = lag(d_share),
    L_d_patents = lag(d_patents), L_d_models  = lag(d_models),
    post = as.integer(year >= 2022)
  ) %>% ungroup()

cat("\n[年度 panel 可用觀測]\n")
print(ann %>% group_by(ticker) %>%
        summarise(n_all = n(),
                  n_usable = sum(!is.na(excess_return) & !is.na(L_d_papers) &
                                 !is.na(d_rd) & !is.na(L_d_patents)),
                  .groups = "drop") %>% as.data.frame(), row.names = FALSE)

D <- ann %>% drop_na(excess_return, L_d_papers, L_d_share, d_rd, L_d_patents, L_d_models)
cat(sprintf("\n最終分析樣本: N=%d firm-years, 公司數=%d, 年份 %d-%d\n",
            nrow(D), n_distinct(D$ticker), min(D$year), max(D$year)))
write_csv(D, file.path(OUT_DIR, "out_annual_model_panel.csv"))

# ============================================================
# 2. 主模型階梯 (依變項 = 超額報酬)
#    逐步加入控制變項,觀察係數穩定性
# ============================================================
cat("\n============================================================\n")
cat(" 模型階梯:依變項 = 年度超額報酬 (excess_return)\n")
cat(" 自變項 = 落後一年的論文成長;推論 = wild cluster bootstrap\n")
cat("============================================================\n")

report(excess_return ~ L_d_papers + factor(ticker),
       D, "L_d_papers", "M1: 論文 + 公司固定效果")

report(excess_return ~ L_d_papers + d_rd + factor(ticker),
       D, c("L_d_papers","d_rd"), "M2: + 研發經費成長")

report(excess_return ~ L_d_papers + d_rd + L_d_patents + factor(ticker),
       D, c("L_d_papers","d_rd","L_d_patents"), "M3: + 專利成長")

report(excess_return ~ L_d_papers + d_rd + L_d_patents + L_d_models + factor(ticker),
       D, c("L_d_papers","d_rd","L_d_patents","L_d_models"),
       "M4: + notable models 成長 (完整控制)")

report(excess_return ~ L_d_papers + d_rd + L_d_patents + L_d_models +
         factor(ticker) + factor(year),
       D, c("L_d_papers","d_rd","L_d_patents","L_d_models"),
       "M5: 雙向固定效果 (公司 + 年度)")

# ============================================================
# 3. 2022 斷點模型
#    ★ 主模型用 share-based 論文數 (量測誤差無斷點)
#      原始計數版本僅作對照,其交互項會被 attenuation bias 污染
# ============================================================
cat("\n============================================================\n")
cat(" 2022 斷點模型\n")
cat("============================================================\n")

Db <- D %>% mutate(papers_x_post = L_d_papers * post,
                   share_x_post  = L_d_share  * post)

report(excess_return ~ L_d_share + share_x_post + post + d_rd + L_d_patents +
         factor(ticker),
       Db, c("L_d_share","share_x_post"),
       "M6 [主]: share-based 論文 x Post2022 (artifact-robust)")

report(excess_return ~ L_d_papers + papers_x_post + post + d_rd + L_d_patents +
         factor(ticker),
       Db, c("L_d_papers","papers_x_post"),
       "M7 [對照]: 原始計數 x Post2022 (交互項受量測誤差污染,勿直接解讀)")

cat("\n[分期估計:不合併,各自估 pre/post]\n")
for (pp in c(0, 1)) {
  sub <- Db %>% filter(post == pp)
  lbl <- ifelse(pp == 0, "pre-2022 (2019-2021)", "post-2022 (2022-2025)")
  if (nrow(sub) < 15) { cat(" ", lbl, ": 樣本不足 (N=", nrow(sub), ")\n"); next }
  report(excess_return ~ L_d_share + d_rd + factor(ticker), sub,
         c("L_d_share","d_rd"), paste("M8:", lbl))
}

# ============================================================
# 4. 月頻交叉驗證 (檢定力較高)
#    年度控制變項以階梯函數帶入
# ============================================================
cat("\n============================================================\n")
cat(" 月頻交叉驗證:N 大得多,作為年度結果的穩健性檢查\n")
cat("============================================================\n")

pmn <- read_csv("panel_monthly.csv", show_col_types = FALSE) %>%
  filter(ticker %in% FIRMS) %>%
  mutate(month_date = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
  arrange(ticker, month_date) %>%
  left_join(ann %>% select(ticker, year, d_rd, d_patents = L_d_patents),
            by = c("ticker","year")) %>%
  group_by(ticker) %>%
  mutate(x = c(NA, diff(log(n_papers + 1))), mo = factor(month)) %>% ungroup()

ok <- !is.na(pmn$x); pmn$x_sa <- NA_real_
pmn$x_sa[ok] <- resid(lm(x ~ mo:ticker, data = pmn[ok, ]))   # 季節調整
pmn <- pmn %>% group_by(ticker) %>% arrange(month_date) %>%
  mutate(x1 = lag(x_sa,1), x2 = lag(x_sa,2), x3 = lag(x_sa,3),
         post = as.integer(year >= 2022)) %>% ungroup() %>%
  drop_na(excess_return, x1, x2, x3, d_rd, d_patents)

cat(sprintf("月頻樣本: N=%d firm-months\n", nrow(pmn)))
mM <- lm(excess_return ~ x1 + x2 + x3 + d_rd + d_patents +
           factor(ticker) + factor(month_date), data = pmn)
VM <- vcovCL(mM, cluster = pmn$month_date, type = "HC1")
cat("\n[月頻:落後 1-3 月論文成長,公司 FE + 月份 FE,按月 cluster]\n")
print(coeftest(mM, vcov = VM)[c("x1","x2","x3","d_rd","d_patents"), ])
L <- setNames(rep(0, length(coef(mM))), names(coef(mM))); L[c("x1","x2","x3")] <- 1
cum <- sum(coef(mM)[c("x1","x2","x3")]); se <- as.numeric(sqrt(t(L) %*% VM %*% L))
cat(sprintf("累積 1-3 月效果: %+.4f (se=%.4f, p=%.3f)\n",
            cum, se, 2*pnorm(abs(cum/se), lower.tail = FALSE)))

cat("\n完成。分析樣本已存於", file.path(OUT_DIR, "out_annual_model_panel.csv"), "\n")
