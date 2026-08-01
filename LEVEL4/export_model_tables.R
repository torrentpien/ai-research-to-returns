# ============================================================
#  export_model_tables.R
#  把 firm_value_model.R 與 time_series_model.R 的所有結果
#  整理成可直接放進論文的表格 (CSV)。
#
#  讀取兩支模型腳本已存好的分析樣本,重跑迴歸並擷取係數,
#  確保表格與 console 輸出完全一致。
#
#  前置:先跑過 firm_value_model.R 與 time_series_model.R
#  輸出:model_tables/  (8 張表)
# ============================================================

# setwd("C:/Users/User/BIG_DATA/LEVEL4")

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(lmtest); library(sandwich); library(lubridate)
  library(tseries); library(urca); library(strucchange)
})
filter <- dplyr::filter; select <- dplyr::select
set.seed(42)

OUT <- "model_tables"; dir.create(OUT, showWarnings = FALSE)
B_BOOT <- 999

stars <- function(p) ifelse(is.na(p), "",
                     ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                     ifelse(p < 0.10, "*", ""))))
fmt <- function(b, p) ifelse(is.na(b), "", sprintf("%.3f%s", b, stars(p)))

# ============================================================
# A. Panel 模型 (firm_value_model.R)
# ============================================================
cat("A. Panel 模型表格...\n")
D <- read_csv("firm_value_outputs/out_annual_model_panel.csv", show_col_types = FALSE)

webb_draw <- function(n) sample(c(-sqrt(1.5),-1,-sqrt(.5),sqrt(.5),1,sqrt(1.5)),
                                n, replace = TRUE)
wcr <- function(fml, v, data, B = B_BOOT) {
  m <- lm(fml, data = data)
  if (!v %in% names(coef(m))) return(c(beta = NA, se = NA, t = NA, p = NA))
  V <- vcovCL(m, cluster = data$ticker, type = "HC1")
  se <- sqrt(V[v, v]); t_obs <- coef(m)[v] / se
  rhs <- setdiff(attr(terms(fml), "term.labels"), v)
  m_r <- lm(reformulate(rhs, all.vars(fml)[1]), data = data)
  fit_r <- fitted(m_r); res_r <- resid(m_r)
  cl <- as.character(data$ticker); ucl <- unique(cl); yn <- all.vars(fml)[1]
  ts_ <- vapply(seq_len(B), function(b) {
    w <- setNames(webb_draw(length(ucl)), ucl)
    d <- data; d[[yn]] <- fit_r + res_r * w[cl]
    mb <- lm(fml, data = d)
    coef(mb)[v] / sqrt(vcovCL(mb, cluster = d$ticker, type = "HC1")[v, v])
  }, numeric(1))
  c(beta = unname(coef(m)[v]), se = unname(se), t = unname(t_obs),
    p = mean(abs(ts_) >= abs(t_obs), na.rm = TRUE))
}

panel_specs <- list(
  M1 = list(f = excess_return ~ L_d_papers + factor(ticker),
            v = c("L_d_papers"), lab = "M1 論文+公司FE", d = D),
  M2 = list(f = excess_return ~ L_d_papers + d_rd + factor(ticker),
            v = c("L_d_papers","d_rd"), lab = "M2 +研發", d = D),
  M3 = list(f = excess_return ~ L_d_papers + d_rd + L_d_patents + factor(ticker),
            v = c("L_d_papers","d_rd","L_d_patents"), lab = "M3 +專利", d = D),
  M4 = list(f = excess_return ~ L_d_papers + d_rd + L_d_patents + L_d_models + factor(ticker),
            v = c("L_d_papers","d_rd","L_d_patents","L_d_models"),
            lab = "M4 完整控制", d = D),
  M5 = list(f = excess_return ~ L_d_papers + d_rd + L_d_patents + L_d_models +
              factor(ticker) + factor(year),
            v = c("L_d_papers","d_rd","L_d_patents","L_d_models"),
            lab = "M5 雙向FE", d = D)
)

panel_res <- imap_dfr(panel_specs, function(s, nm) {
  m <- lm(s$f, data = s$d)
  map_dfr(s$v, function(v) {
    r <- wcr(s$f, v, s$d)
    tibble(model = nm, label = s$lab, term = v,
           coef = round(r["beta"],4), se = round(r["se"],4),
           t = round(r["t"],2), p_wildboot = round(r["p"],3),
           N = nrow(s$d), R2 = round(summary(m)$r.squared,3),
           df_resid = df.residual(m))
  })
})
panel_res$sig <- stars(panel_res$p_wildboot)
write_csv(panel_res, file.path(OUT, "T1_panel_spec_ladder.csv"))

# 寬表 (論文用版面:列=變數,欄=模型)
T1_wide <- panel_res %>% mutate(cell = fmt(coef, p_wildboot)) %>%
  select(term, model, cell) %>% pivot_wider(names_from = model, values_from = cell) %>%
  mutate(term = recode(term, L_d_papers = "論文成長(t-1)", d_rd = "研發成長(t)",
                       L_d_patents = "專利成長(t-1)", L_d_models = "Models成長(t-1)"))
meta1 <- panel_res %>% distinct(model, N, R2, df_resid) %>%
  pivot_longer(c(N, R2, df_resid), names_to = "term", values_to = "cell") %>%
  mutate(cell = as.character(cell)) %>%
  pivot_wider(names_from = model, values_from = cell)
write_csv(bind_rows(T1_wide, meta1), file.path(OUT, "T1_panel_spec_ladder_wide.csv"))

# 斷點模型
Db <- D %>% mutate(papers_x_post = L_d_papers*post, share_x_post = L_d_share*post)
bp_specs <- list(
  M6 = list(f = excess_return ~ L_d_share + share_x_post + post + d_rd +
              L_d_patents + factor(ticker),
            v = c("L_d_share","share_x_post"),
            lab = "M6 share-based x Post2022 [主]", d = Db),
  M7 = list(f = excess_return ~ L_d_papers + papers_x_post + post + d_rd +
              L_d_patents + factor(ticker),
            v = c("L_d_papers","papers_x_post"),
            lab = "M7 原始計數 x Post2022 [對照]", d = Db),
  M8pre  = list(f = excess_return ~ L_d_share + d_rd + factor(ticker),
                v = c("L_d_share","d_rd"), lab = "M8 pre-2022 分期",
                d = Db %>% filter(post == 0)),
  M8post = list(f = excess_return ~ L_d_share + d_rd + factor(ticker),
                v = c("L_d_share","d_rd"), lab = "M8 post-2022 分期",
                d = Db %>% filter(post == 1))
)
bp_res <- imap_dfr(bp_specs, function(s, nm) {
  m <- lm(s$f, data = s$d)
  map_dfr(s$v, function(v) {
    r <- wcr(s$f, v, s$d)
    tibble(model = nm, label = s$lab, term = v,
           coef = round(r["beta"],4), se = round(r["se"],4),
           t = round(r["t"],2), p_wildboot = round(r["p"],3),
           N = nrow(s$d), R2 = round(summary(m)$r.squared,3))
  })
})
bp_res$sig <- stars(bp_res$p_wildboot)
write_csv(bp_res, file.path(OUT, "T2_panel_breakpoint.csv"))

# 多重檢定
all_p <- c(panel_res$p_wildboot, bp_res$p_wildboot)
all_n <- c(paste0(panel_res$model,"_",panel_res$term),
           paste0(bp_res$model,"_",bp_res$term))
T3 <- tibble(test = all_n, p_raw = all_p, p_BH = round(p.adjust(all_p, "BH"),3)) %>%
  arrange(p_raw) %>% mutate(survives_FDR10 = p_BH < 0.10)
write_csv(T3, file.path(OUT, "T3_multiple_testing_BH.csv"))
cat("   通過 BH-FDR 0.10 的檢定數:", sum(T3$survives_FDR10), "/", nrow(T3), "\n")

# ============================================================
# B. 時間序列模型 (time_series_model.R)
# ============================================================
cat("B. 時間序列模型表格...\n")
TS <- read_csv("ts_model_outputs/out_ts_monthly_series.csv", show_col_types = FALSE) %>%
  mutate(mo = factor(month(month)))

# T4 單根檢定 (直接取用已存檔)
file.copy("ts_model_outputs/out_ts1_unitroot.csv",
          file.path(OUT, "T4_unit_root_tests.csv"), overwrite = TRUE)

# T5 共整合檢定總表
eg <- lm(y ~ lpapers + lrd + lpat, data = TS)
eg_adf <- suppressWarnings(adf.test(resid(eg)))
jo <- ca.jo(TS %>% select(y, lpapers, lrd, lpat) %>% as.matrix(),
            type = "trace", ecdet = "const", K = 2)
js <- summary(jo)
Dd <- TS %>% mutate(dy = c(NA, diff(y)), dlp = c(NA, diff(lpapers)),
                    dlrd = c(NA, diff(lrd)), dlpat = c(NA, diff(lpat)),
                    y_1 = lag(y), lp_1 = lag(lpapers), lrd_1 = lag(lrd),
                    lpat_1 = lag(lpat), dy_1 = lag(dy), dlp_1 = lag(dlp),
                    dlpat_1 = lag(dlpat)) %>%
  drop_na(dy, dlp, dlrd, dlpat, y_1, lp_1, lrd_1, lpat_1, dy_1, dlp_1, dlpat_1)
ecm_u <- lm(dy ~ y_1 + lp_1 + lrd_1 + lpat_1 + dlp + dlrd + dlpat +
              dy_1 + dlp_1 + dlpat_1 + mo, data = Dd)
ecm_r <- lm(dy ~ dlp + dlrd + dlpat + dy_1 + dlp_1 + dlpat_1 + mo, data = Dd)
Fb <- anova(ecm_r, ecm_u)$F[2]

T5 <- tibble(
  test = c("Engle-Granger 殘差 ADF", "Johansen trace (r=0)",
           "ARDL Bounds F (PSS 2001, k=3)", "誤差修正項 ECT (y_{t-1})"),
  statistic = round(c(eg_adf$statistic, js@teststat[4], Fb, coef(ecm_u)["y_1"]), 3),
  critical_5pct = c("-4.35 (EG)", round(js@cval[4,"5pct"],2),
                    "3.23 [I(0)] - 4.35 [I(1)]", "t 檢定"),
  conclusion = c(
    ifelse(eg_adf$statistic < -4.35, "有共整合", "無共整合"),
    ifelse(js@teststat[4] > js@cval[4,"5pct"], "拒絕 r=0 (系統內有共整合)", "無共整合"),
    ifelse(Fb > 4.35, "有長期關係", ifelse(Fb < 3.23, "無長期關係", "不確定")),
    sprintf("t=%.2f, %s", coeftest(ecm_u)["y_1","t value"],
            ifelse(abs(coeftest(ecm_u)["y_1","t value"]) > 1.96, "顯著", "不顯著"))))
write_csv(T5, file.path(OUT, "T5_cointegration_tests.csv"))

# T6 弱外生性 (已存檔)
file.copy("ts_model_outputs/out_ts2_weak_exogeneity.csv",
          file.path(OUT, "T6_weak_exogeneity.csv"), overwrite = TRUE)

# T7 短期 ARDL (含/不含斷點)
S <- TS %>%
  mutate(dy = c(NA, diff(y)), dlp = c(NA, diff(lpapers)), dlrd = c(NA, diff(lrd)),
         dlpat = c(NA, diff(lpat)), dlmod = c(NA, diff(lmod))) %>%
  mutate(across(c(dlp, dlpat, dlmod),
                list(l1 = ~lag(.x,1), l2 = ~lag(.x,2), l3 = ~lag(.x,3)))) %>%
  mutate(dy_1 = lag(dy)) %>%
  drop_na(dy, dlp_l1, dlp_l2, dlp_l3, dlrd, dlpat_l1, dlmod_l1, dy_1) %>%
  mutate(p1_post = dlp_l1*post2022, p2_post = dlp_l2*post2022, p3_post = dlp_l3*post2022)

hac_tab <- function(m, vars, lab) {
  ct <- coeftest(m, vcov = NeweyWest(m, lag = 6, prewhite = FALSE))
  tibble(model = lab, term = vars,
         coef = round(ct[vars,"Estimate"],4), se = round(ct[vars,"Std. Error"],4),
         t = round(ct[vars,"t value"],2), p = round(ct[vars,"Pr(>|t|)"],3))
}
cum_row <- function(m, cols, lab, nm) {
  V <- NeweyWest(m, lag = 6, prewhite = FALSE)
  L <- setNames(rep(0, length(coef(m))), names(coef(m))); L[cols] <- 1
  b <- sum(coef(m)[cols]); se <- as.numeric(sqrt(t(L) %*% V %*% L))
  tibble(model = lab, term = nm, coef = round(b,4), se = round(se,4),
         t = round(b/se,2), p = round(2*pnorm(abs(b/se), lower.tail=FALSE),3))
}
m_a <- lm(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + dlrd + dlpat_l1 + dlmod_l1 + dy_1 + mo, data = S)
m_b <- lm(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + p1_post + p2_post + p3_post + post2022 +
            dlrd + dlpat_l1 + dlmod_l1 + dy_1 + mo, data = S)
T7 <- bind_rows(
  hac_tab(m_a, c("dlp_l1","dlp_l2","dlp_l3","dlrd","dlpat_l1","dlmod_l1"), "A 不區分斷點"),
  cum_row(m_a, c("dlp_l1","dlp_l2","dlp_l3"), "A 不區分斷點", "論文累積1-3月"),
  hac_tab(m_b, c("dlp_l1","p1_post","post2022"), "B 區分斷點"),
  cum_row(m_b, c("dlp_l1","dlp_l2","dlp_l3"), "B 區分斷點", "pre-2022 累積"),
  cum_row(m_b, c("p1_post","p2_post","p3_post"), "B 區分斷點", "斷點差異(交互項合計)")
) %>% mutate(sig = stars(p))
write_csv(T7, file.path(OUT, "T7_short_run_ARDL.csv"))

# T8 結構斷點檢定
wt <- waldtest(m_a, m_b, vcov = NeweyWest(m_b, lag = 6, prewhite = FALSE))
bpd <- S %>% select(dy, dlp_l1, dlp_l2, dlp_l3, dlrd, dlpat_l1) %>% na.omit()
fs <- Fstats(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + dlrd + dlpat_l1, data = bpd, from = 0.15)
qa <- sctest(fs, type = "supF")
bpm <- breakpoints(dy ~ dlp_l1 + dlp_l2 + dlp_l3 + dlrd + dlpat_l1, data = bpd, h = 0.15)
peak_i <- which.max(fs$Fstats) + floor(0.15*nrow(bpd))
T8 <- tibble(
  test = c("Chow 型 Wald (指定 2022)", "Quandt-Andrews supF (未知斷點)",
           "supF 峰值位置", "Bai-Perron (BIC) 斷點數"),
  statistic = c(round(wt$F[2],3), round(qa$statistic,3), NA, length(na.omit(bpm$breakpoints))),
  p_value = c(round(wt$`Pr(>F)`[2],3), round(qa$p.value,4), NA, NA),
  conclusion = c(
    ifelse(wt$`Pr(>F)`[2] < 0.05, "有斷點", "無顯著斷點"),
    ifelse(qa$p.value < 0.05, "有斷點", "無顯著斷點"),
    format(S$month[peak_i]),
    ifelse(length(na.omit(bpm$breakpoints)) == 0, "未選出任何斷點", "有斷點")))
write_csv(T8, file.path(OUT, "T8_structural_break_tests.csv"))

# T9 各公司時間序列 (已存檔)
file.copy("ts_model_outputs/out_ts5_by_firm.csv",
          file.path(OUT, "T9_by_firm_timeseries.csv"), overwrite = TRUE)

cat("\n完成。表格輸出於", OUT, ":\n")
cat("  T1 panel 模型階梯 (長表+寬表)\n  T2 panel 斷點模型\n")
cat("  T3 多重檢定 BH-FDR\n  T4 單根檢定\n  T5 共整合檢定\n")
cat("  T6 弱外生性\n  T7 短期 ARDL\n  T8 結構斷點檢定\n  T9 各公司時間序列\n")
