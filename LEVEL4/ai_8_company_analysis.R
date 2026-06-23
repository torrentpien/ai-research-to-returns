# ============================================================
#  ai_8_company_analysis.R
#  分析 8 家公司：GOOGL, MSFT, META, NVDA, AMZN, AAPL, TSLA, AVGO
#
#  需要的現有檔案：
#   ai_monthly.csv    
#   ai_monthly_subfield.csv
#   paper_company_panel.rds       
#   stock_raw.rds       
#   panel_annual.rds
#   panel_monthly.csv
#
#  若以上任一不存在，腳本會嘗試從備用來源重建。
#  不需要 clean_data.R，不需要重新抓資料。
# ============================================================

setwd("C:/Users/User/BIG_DATA/LEVEL4")  

# 套件
pkgs <- c("dplyr","tidyr","lubridate","stringr","ggplot2",
          "readr","purrr","tseries","lmtest","sandwich",
          "plm","vars","zoo", "scales", "patchwork")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

panel_annual <- readRDS("panel_annual.rds")
pa <- panel_annual %>% filter(ticker != "GOOG")

# Figure 1: AI Papers by Company - Annual (2017-2025)

p_trend_papers <- ggplot(pa, aes(x = year, y = n_papers, color = parent_company)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2017:2025) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "AI Papers by Company - Annual (2017-2025)",
    x = "Year",
    y = "Number of Papers",
    color = "Company"
  ) +
  theme_minimal()

ggsave("01_trend_papers_annual.png", p_trend_papers,
       width = 12, height = 5, dpi = 400)

# =========================================================
# Figure 2: Annual Spearman Correlation Heatmap
# =========================================================

compute_cor_annual <- function(data) {
  data %>%
    filter(ticker != "GOOG") %>%
    group_by(parent_company) %>%
    filter(n() >= 3) %>%
    summarise(
      period            = "Annual",
      papers_vs_return  = cor(n_papers, annual_return,
                              use = "complete.obs", method = "spearman"),
      papers_vs_excess  = cor(n_papers, excess_return,
                              use = "complete.obs", method = "spearman"),
      papers_vs_price   = cor(n_papers, price_end,
                              use = "complete.obs", method = "spearman"),
      cite_vs_return    = cor(total_citations, annual_return,
                              use = "complete.obs", method = "spearman"),
      cite_vs_excess    = cor(total_citations, excess_return,
                              use = "complete.obs", method = "spearman"),
      cite_vs_price     = cor(total_citations, price_end,
                              use = "complete.obs", method = "spearman"),
      n_obs = n(),
      .groups = "drop"
    )
}

plot_heatmap <- function(cor_df, title) {
  cor_df %>%
    select(parent_company,
           "Papers\nvs Return"     = papers_vs_return,
           "Papers\nvs Excess"     = papers_vs_excess,
           "Papers\nvs Price"      = papers_vs_price,
           "Citations\nvs Return"  = cite_vs_return,
           "Citations\nvs Excess"  = cite_vs_excess,
           "Citations\nvs Price"   = cite_vs_price) %>%
    pivot_longer(-parent_company, names_to = "metric", values_to = "cor") %>%
    ggplot(aes(x = metric, y = parent_company, fill = cor)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(cor, 2)), size = 3.5, fontface = "bold") +
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#4575b4",
      midpoint = 0, limits = c(-1, 1), name = "Spearman r"
    ) +
    labs(
      title = title,
      subtitle = "Spearman Correlation | Blue = Positive | Red = Negative",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      plot.title  = element_text(face = "bold")
    )
}

cor_a <- compute_cor_annual(pa)
write.csv(cor_a, "correlation_spearman_annual.csv", row.names = FALSE)

p_heat_a <- plot_heatmap(cor_a, "Annual Spearman Correlation Heatmap")
ggsave("17_heatmap_annual.png", p_heat_a,
       width = 11, height = 6, dpi = 400)

# Figure 3: Microsoft full annual analysis
#   Papers vs Stock Price
#   Papers vs Return & Excess Return
#   Citations vs Stock Price

make_company_full_analysis <- function(company_name) {
  df <- pa %>% filter(parent_company == company_name)
  if (nrow(df) == 0) stop("No data found for company: ", company_name)
  
  scale_price  <- max(df$n_papers, na.rm = TRUE) / max(df$price_end, na.rm = TRUE)
  scale_return <- max(df$n_papers, na.rm = TRUE)
  scale_cite_price <- max(df$total_citations, na.rm = TRUE) / max(df$price_end, na.rm = TRUE)
  
  pA <- ggplot(df, aes(x = year)) +
    geom_col(aes(y = n_papers), fill = "steelblue", alpha = 0.5, width = 0.6) +
    geom_line(aes(y = price_end * scale_price), color = "firebrick", size = 1.2) +
    geom_point(aes(y = price_end * scale_price), color = "firebrick", size = 3) +
    scale_y_continuous(
      name = "AI Papers",
      sec.axis = sec_axis(~ . / scale_price,
                          name = "Stock Price (USD)",
                          labels = dollar_format())
    ) +
    scale_x_continuous(breaks = 2017:2025) +
    labs(title = paste(company_name, "| Papers vs Stock Price"),
         x = NULL,
         caption = "Bar = AI Papers | Red line = Stock Price") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  pB <- ggplot(df, aes(x = year)) +
    geom_col(aes(y = n_papers), fill = "steelblue", alpha = 0.5, width = 0.6) +
    geom_line(aes(y = annual_return * scale_return), color = "darkgreen", size = 1.2) +
    geom_point(aes(y = annual_return * scale_return), color = "darkgreen", size = 3) +
    geom_line(aes(y = excess_return * scale_return), color = "purple", size = 1, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    scale_y_continuous(
      name = "AI Papers",
      sec.axis = sec_axis(~ . / scale_return,
                          name = "Return (%)",
                          labels = percent_format())
    ) +
    scale_x_continuous(breaks = 2017:2025) +
    labs(title = paste(company_name, "| Papers vs Return & Excess Return"),
         x = NULL,
         caption = "Bar = Papers | Green = Annual Return | Purple dashed = Excess Return") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  pC <- ggplot(df, aes(x = year)) +
    geom_col(aes(y = total_citations), fill = "darkorange", alpha = 0.5, width = 0.6) +
    geom_line(aes(y = price_end * scale_cite_price), color = "firebrick", size = 1.2) +
    geom_point(aes(y = price_end * scale_cite_price), color = "firebrick", size = 3) +
    scale_y_continuous(
      name = "Total Citations",
      sec.axis = sec_axis(~ . / scale_cite_price,
                          name = "Stock Price (USD)",
                          labels = dollar_format())
    ) +
    scale_x_continuous(breaks = 2017:2025) +
    labs(title = paste(company_name, "| Citations vs Stock Price"),
         x = "Year",
         caption = "Bar = Citations | Red line = Stock Price") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  combined <- pA / pB / pC
  comp_clean <- gsub("[/ ]", "_", company_name)
  ggsave(paste0("21_", comp_clean, "_full_analysis.png"),
         combined, width = 12, height = 14, dpi = 400)
  
  combined
}

companies <- unique(pa$parent_company)

for (comp in companies) {
  make_company_full_analysis(comp)
}

cat("Done. Saved company full analysis figures for:\n")
print(companies)


filter <- dplyr::filter
select <- dplyr::select
set.seed(42)

OUT_DIR <- "ai_8_outputs"
dir.create(OUT_DIR, showWarnings = FALSE)

TARGET_TICKERS <- c("GOOGL","MSFT","META","NVDA","AMZN","AAPL","TSLA","AVGO")

# GOOGL+GOOG 原本各 0.075，合併給 GOOGL 0.15
MCAP_WEIGHT <- c(
  AAPL=0.20, MSFT=0.19, NVDA=0.18,
  AMZN=0.13, META=0.09, GOOGL=0.15,
  AVGO=0.05, TSLA=0.05
)
MCAP_WEIGHT <- MCAP_WEIGHT[TARGET_TICKERS]
MCAP_WEIGHT <- MCAP_WEIGHT / sum(MCAP_WEIGHT)

CITATION_CUTOFF_MONTHS <- 24

cat(" AI x 股價分析( 8 家公司版)\n")


# 1. 讀入 ai_monthly.csv
if (file.exists("ai_monthly.csv")) {
  cat("讀入 ai_monthly.csv\n")
  ai_monthly_raw <- read_csv("ai_monthly.csv", show_col_types = FALSE) %>%
    mutate(month   = as.Date(month),
           quarter = floor_date(month, "quarter"))
} else if (file.exists("ai_panel.rds") || file.exists("ai_panel.csv")) {
  cat("找不到 ai_monthly.csv，從 ai_panel 重建\n")
  ai_panel_tmp <- if (file.exists("ai_panel.rds")) readRDS("ai_panel.rds") else
    read_csv("ai_panel.csv", show_col_types = FALSE)
  ai_monthly_raw <- ai_panel_tmp %>%
    mutate(pub_date = as.Date(pub_date),
           month    = floor_date(pub_date, "month")) %>%
    filter(!is.na(month)) %>%
    group_by(month) %>%
    summarise(ai_papers       = n(),
              total_citations = sum(cited_by_count, na.rm=TRUE),
              avg_citations   = mean(cited_by_count, na.rm=TRUE),
              .groups = "drop") %>%
    arrange(month) %>%
    mutate(quarter = floor_date(month, "quarter"))
  write_csv(ai_monthly_raw %>% select(-quarter), "ai_monthly.csv")
  cat("  重建完成並儲存 ai_monthly.csv\n")
} else {
  stop("找不到 ai_monthly.csv 也沒有 ai_panel.rds/csv，請先跑 FINAL_PART3 STEP 12")
}

# 2. 建立子領域月度統計
#一篇論文可同時歸屬多個子領域

SUBFIELD_MAP <- c(
  "cs.CL"  = "LLM_NLP",
  "cs.NE"  = "Neural_Networks",
  "cs.CV"  = "Computer_Vision",
  "cs.RO"  = "Robotics",
  "cs.LG"  = "Machine_Learning",
  "stat.ML"= "Machine_Learning",
  "cs.AI"  = "General_AI"
)

build_subfield <- function(src) {
  src %>%
    mutate(pub_date2 = as.Date(pub_date),
           month     = floor_date(pub_date2, "month")) %>%
    filter(!is.na(month), month >= as.Date("2017-01-01"),
           !is.na(categories), categories != "") %>%
    mutate(cats = str_split(str_trim(categories), "\\s+")) %>%
    unnest(cats) %>%
    filter(cats %in% names(SUBFIELD_MAP)) %>%
    mutate(ai_subfield = SUBFIELD_MAP[cats]) %>%
    distinct(id, month, ai_subfield) %>%
    group_by(month, ai_subfield) %>%
    summarise(ai_paper_count = n(), .groups = "drop") %>%
    arrange(month, ai_subfield) %>%
    mutate(quarter = floor_date(month, "quarter"))
}

FORCE_REBUILD_SUBFIELD <- TRUE

if (file.exists("ai_monthly_subfield.csv")) {
  cat("讀入 ai_monthly_subfield.csv\n")
  sf_raw <- read_csv("ai_monthly_subfield.csv", show_col_types = FALSE) %>%
    mutate(month   = as.Date(month),
           quarter = floor_date(month, "quarter"))
} else if (file.exists("papers_ai.rds")) {
  cat("從 papers_ai.rds 建立子領域統計\n")
  paper_ai <- readRDS("papers_ai.rds")
  sf_raw   <- build_subfield(paper_ai)
  write_csv(sf_raw %>% select(-quarter), "ai_monthly_subfield.csv")
  cat("  已儲存 ai_monthly_subfield.csv\n")
} else if (file.exists("ai_panel.rds")) {
  cat("從 ai_panel.rds 建立子領域統計\n")
  ai_panel_tmp <- readRDS("ai_panel.rds")
  if (!"categories" %in% names(ai_panel_tmp))
    stop("ai_panel.rds 沒有 categories 欄，無法建立子領域統計")
  sf_raw <- build_subfield(ai_panel_tmp)
  write_csv(sf_raw %>% select(-quarter), "ai_monthly_subfield.csv")
  cat("  已儲存 ai_monthly_subfield.csv\n")
} else {
  stop("找不到 papers_ai.rds / ai_panel.rds，無法建立子領域統計")
}

# 3. 讀入股價資料

if (file.exists("stock_raw.rds")) {
  cat("讀入 stock_raw.rds\n")
  stock_raw <- readRDS("stock_raw.rds")
  sp500 <- stock_raw %>%
    filter(symbol %in% TARGET_TICKERS) %>%
    transmute(Date      = as.Date(date),
              Ticker    = as.character(symbol),
              Adj_Close = as.numeric(adjusted)) %>%
    filter(!is.na(Date), !is.na(Adj_Close))
} else if (file.exists("sp500_top10_stocks_clean.csv")) {
  cat("讀入 sp500_top10_stocks_clean.csv\n")
  sp500 <- read_csv("sp500_top10_stocks_clean.csv", show_col_types = FALSE) %>%
    mutate(Date = as.Date(Date), Ticker = as.character(Ticker)) %>%
    filter(Ticker %in% TARGET_TICKERS)
} else {
  stop("找不到 stock_raw.rds，請先跑 FINAL_PART3 STEP 15")
}

cat("\n資料期間確認:\n")
cat("  AI論文:", format(min(ai_monthly_raw$month)), "~",
    format(max(ai_monthly_raw$month)), "\n")
cat("  股價  :", format(min(sp500$Date)), "~",
    format(max(sp500$Date)), "\n")
cat("  Tickers:", paste(sort(unique(sp500$Ticker)), collapse=", "), "\n")
cat("  子領域 :", paste(sort(unique(sf_raw$ai_subfield)), collapse=", "), "\n\n")


# 4. 共用函式
make_stock_monthly <- function(sp_df, tickers = TARGET_TICKERS) {
  sp_df %>%
    filter(Ticker %in% tickers) %>%
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


# 公司層級 AI 論文月資料
# 定義：以 OpenAlex institution_type == "company" 的 affiliation，
#       透過 institution_name 對應到母公司與 ticker。

load_or_build_company_monthly <- function() {
  # 目標：建立 Ticker-month 層級的公司論文數。
  # 重點：只要有 n_papers 就能跑；total_citations 若不存在，補 0，不讓程式中斷。
  
  if (file.exists("company_monthly.rds")) {
    cat("讀入 company_monthly.rds\n")
    cm <- readRDS("company_monthly.rds")
  } else if (file.exists("company_monthly.csv")) {
    cat("讀入 company_monthly.csv\n")
    cm <- read_csv("company_monthly.csv", show_col_types = FALSE)
  } else if (file.exists("panel_monthly.rds")) {
    cat("找不到 company_monthly，改用 panel_monthly.rds\n")
    cm <- readRDS("panel_monthly.rds")
  } else if (file.exists("panel_monthly.csv")) {
    cat("找不到 company_monthly，改用 panel_monthly.csv\n")
    cm <- read_csv("panel_monthly.csv", show_col_types = FALSE)
  } else {
    cat("找不到 company_monthly/panel_monthly，嘗試從 paper_company_panel 重建\n")
    
    if (file.exists("paper_company_panel.rds")) {
      paper_company_panel <- readRDS("paper_company_panel.rds")
    } else if (file.exists("paper_company_panel.csv")) {
      paper_company_panel <- read_csv("paper_company_panel.csv", show_col_types = FALSE)
    } else {
      stop("找不到 company_monthly/panel_monthly，也找不到 paper_company_panel，無法建立各公司論文數")
    }
    
    company_inst_exact <- tribble(
      ~institution_name,                                  ~parent_company, ~ticker,
      "Google (United States)",                           "Alphabet",      "GOOGL",
      "Google (United Kingdom)",                          "Alphabet",      "GOOGL",
      "Google (Canada)",                                  "Alphabet",      "GOOGL",
      "Google (Israel)",                                  "Alphabet",      "GOOGL",
      "Google (Switzerland)",                             "Alphabet",      "GOOGL",
      "Google DeepMind (United Kingdom)",                 "Alphabet",      "GOOGL",
      "DeepMind (United Kingdom)",                        "Alphabet",      "GOOGL",
      "Microsoft (United States)",                        "Microsoft",     "MSFT",
      "Microsoft (United Kingdom)",                       "Microsoft",     "MSFT",
      "Microsoft (Canada)",                               "Microsoft",     "MSFT",
      "Microsoft (France)",                               "Microsoft",     "MSFT",
      "Microsoft (Germany)",                              "Microsoft",     "MSFT",
      "Microsoft (India)",                                "Microsoft",     "MSFT",
      "Microsoft (Israel)",                               "Microsoft",     "MSFT",
      "Microsoft (Netherlands)",                          "Microsoft",     "MSFT",
      "Microsoft (Norway)",                               "Microsoft",     "MSFT",
      "Microsoft (Denmark)",                              "Microsoft",     "MSFT",
      "Microsoft (Finland)",                              "Microsoft",     "MSFT",
      "Microsoft (Switzerland)",                          "Microsoft",     "MSFT",
      "Microsoft Research (United Kingdom)",              "Microsoft",     "MSFT",
      "Microsoft Research (India)",                       "Microsoft",     "MSFT",
      "Microsoft Research Asia (China)",                  "Microsoft",     "MSFT",
      "Microsoft Research Montréal (Canada)",             "Microsoft",     "MSFT",
      "Microsoft Research New England (United States)",   "Microsoft",     "MSFT",
      "Microsoft Research New York City (United States)", "Microsoft",     "MSFT",
      "LinkedIn (United States)",                         "Microsoft",     "MSFT",
      "Meta (United States)",                             "Meta",          "META",
      "Meta (Israel)",                                    "Meta",          "META",
      "Meta (United Kingdom)",                            "Meta",          "META",
      "Nvidia (United States)",                           "NVIDIA",        "NVDA",
      "Nvidia (United Kingdom)",                          "NVIDIA",        "NVDA",
      "Amazon (United States)",                           "Amazon",        "AMZN",
      "Amazon (United Kingdom)",                          "Amazon",        "AMZN",
      "Amazon (Germany)",                                 "Amazon",        "AMZN",
      "Apple (United States)",                            "Apple",         "AAPL",
      "Apple (United Kingdom)",                           "Apple",         "AAPL",
      "Apple (Germany)",                                  "Apple",         "AAPL",
      "Apple (Israel)",                                   "Apple",         "AAPL",
      "Tesla (United States)",                            "Tesla",         "TSLA",
      "Broadcom (United States)",                         "Broadcom",      "AVGO",
      "Broadcom (Israel)",                                "Broadcom",      "AVGO"
    )
    
    cm <- paper_company_panel %>%
      inner_join(company_inst_exact, by = "institution_name", relationship = "many-to-many") %>%
      mutate(
        pub_date = as.Date(publication_date),
        year     = lubridate::year(pub_date),
        month_no = lubridate::month(pub_date),
        ym       = format(pub_date, "%Y-%m")
      ) %>%
      filter(!is.na(pub_date), year >= 2017, ticker %in% TARGET_TICKERS) %>%
      group_by(parent_company, ticker, year, month_no, ym) %>%
      summarise(
        n_papers        = n_distinct(work_id),
        total_citations = if ("cited_by_count" %in% names(cur_data())) sum(cited_by_count, na.rm = TRUE) else 0,
        avg_citations   = if ("cited_by_count" %in% names(cur_data())) mean(cited_by_count, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) %>%
      rename(month = month_no)
    
    saveRDS(cm, "company_monthly.rds")
    write_csv(cm, "company_monthly.csv")
    cat("  已重建並儲存 company_monthly.rds / company_monthly.csv\n")
  }
  
  # 欄位名稱標準化
  if (!"ticker" %in% names(cm) && "Ticker" %in% names(cm)) {
    cm <- cm %>% rename(ticker = Ticker)
  }
  if (!"n_papers" %in% names(cm)) {
    if ("company_papers" %in% names(cm)) {
      cm <- cm %>% rename(n_papers = company_papers)
    } else {
      stop("company_monthly/panel_monthly 需要 n_papers 或 company_papers 欄位")
    }
  }
  if (!"total_citations" %in% names(cm)) {
    cm$total_citations <- 0
  }
  
  # 標準化月份欄位為 Date
  if ("month_date" %in% names(cm)) {
    cm <- cm %>% mutate(month_date = as.Date(month_date))
  } else if ("ym" %in% names(cm)) {
    cm <- cm %>% mutate(month_date = as.Date(paste0(as.character(ym), "-01")))
  } else if (all(c("year", "month") %in% names(cm))) {
    cm <- cm %>% mutate(month_date = as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month))))
  } else if ("month" %in% names(cm) && inherits(cm$month, "Date")) {
    cm <- cm %>% mutate(month_date = as.Date(month))
  } else {
    stop("company_monthly 需要 month_date、ym、year+month，或 Date 型態的 month 欄位")
  }
  
  cm %>%
    filter(ticker %in% TARGET_TICKERS, !is.na(month_date)) %>%
    transmute(
      Ticker = as.character(ticker),
      month  = month_date,
      company_papers = as.numeric(n_papers),
      company_citations = as.numeric(total_citations)
    ) %>%
    group_by(Ticker, month) %>%
    summarise(
      company_papers = sum(company_papers, na.rm = TRUE),
      company_citations = sum(company_citations, na.rm = TRUE),
      .groups = "drop"
    )
}

adf_report <- function(x, name) {
  x <- na.omit(x)
  if (length(x) < 10) { cat("  ", name, ": 樣本太少\n"); return(invisible()) }
  r <- tryCatch(suppressWarnings(adf.test(x)), error = function(e) NULL)
  if (is.null(r)) { cat("  ", name, ": 檢定失敗\n"); return(invisible()) }
  cat(sprintf("  %-28s ADF=%.3f p=%.4f -> %s\n", name, r$statistic, r$p.value,
              if (r$p.value < 0.05) "定態 OK" else "不定態(有單根)"))
}

ts_analysis <- function(df, x_col, y_col, label, max_lag = 3) {
  cat("\n----------", label, "----------\n")
  d <- df %>% select(month, x = all_of(x_col), y = all_of(y_col)) %>%
    drop_na() %>% arrange(month)
  if (nrow(d) < 30) { cat("樣本不足,跳過\n"); return(invisible(NULL)) }
  for (L in 1:max_lag) d[[paste0("x_l",L)]] <- dplyr::lag(d$x, L)
  d$y_l1 <- dplyr::lag(d$y, 1)
  d2 <- d %>% drop_na()
  cat("[ADF]\n"); adf_report(d$x, x_col); adf_report(d$y, y_col)
  fml <- as.formula(paste0("y ~ x + ",
                           paste0("x_l",1:max_lag,collapse=" + "), " + y_l1"))
  m   <- lm(fml, data=d2)
  hac <- coeftest(m, vcov=NeweyWest(m, lag=max_lag, prewhite=FALSE))
  cat("[ARDL + Newey-West HAC]\n"); print(hac)
  cum <- sum(coef(m)[c("x", paste0("x_l",1:max_lag))], na.rm=TRUE)
  cat(sprintf("  >> 累積 %d 個月效應 = %+.4f, R2=%.3f\n",
              max_lag+1, cum, summary(m)$r.squared))
  gd <- d %>% select(x, y) %>% drop_na()
  g1 <- tryCatch(grangertest(y ~ x, order=2, data=gd), error=function(e) NULL)
  g2 <- tryCatch(grangertest(x ~ y, order=2, data=gd), error=function(e) NULL)
  cat(sprintf("[Granger] AI->報酬 p=%.3f | 報酬->AI p=%.3f\n",
              if(!is.null(g1)) g1$"Pr(>F)"[2] else NA,
              if(!is.null(g2)) g2$"Pr(>F)"[2] else NA))
  invisible(list(model=m, hac=hac, cum_effect=cum,
                 granger_p=if(!is.null(g1)) g1$"Pr(>F)"[2] else NA))
}

var_irf <- function(df, x_col, y_col, label, n_ahead = 12) {
  d <- df %>% select(x = all_of(x_col), y = all_of(y_col)) %>%
    drop_na() %>% as.matrix()
  if (nrow(d) < 30) { cat("  IRF 樣本不足:", label, "\n"); return(NULL) }
  d <- d[, c("y","x")]
  tryCatch({
    sel <- VARselect(d, lag.max=min(6, floor(nrow(d)/5)), type="const")
    p   <- max(1, sel$selection["AIC(n)"])
    vm  <- VAR(d, p=p, type="const")
    ir  <- irf(vm, impulse="x", response="y",
               n.ahead=n_ahead, boot=TRUE, runs=200)
    data.frame(
      label=label, h=0:n_ahead,
      irf=ir$irf$x[,1], lower=ir$Lower$x[,1], upper=ir$Upper$x[,1]
    ) %>% mutate(sig=ifelse(sign(lower)==sign(upper),"*",""))
  }, error=function(e) {
    cat("  IRF 失敗:", label, "-", conditionMessage(e), "\n"); NULL
  })
}

analyze_pair <- function(sf_name, ticker, stock_all, sf_tr) {
  d_col <- paste0("d_log_", sf_name)
  if (!d_col %in% names(sf_tr)) return(NULL)
  df <- stock_all %>% filter(Ticker == ticker) %>%
    inner_join(sf_tr %>% select(month, all_of(d_col)), by="month") %>%
    rename(x = !!d_col) %>%
    mutate(x_l1=dplyr::lag(x,1), x_l2=dplyr::lag(x,2),
           x_l3=dplyr::lag(x,3), y_l1=dplyr::lag(log_return,1)) %>%
    drop_na()
  if (nrow(df) < 30 || sd(df$x)==0 || sd(df$log_return)==0) return(NULL)
  m   <- lm(log_return ~ x + x_l1 + x_l2 + x_l3 + y_l1, data=df)
  hac <- coeftest(m, vcov=NeweyWest(m, lag=3, prewhite=FALSE))
  g   <- tryCatch(grangertest(log_return ~ x, order=2,
                              data=df[,c("x","log_return")]),
                  error=function(e) NULL)
  data.frame(
    subfield=sf_name, ticker=ticker, n=nrow(df),
    coef_contemp=round(hac["x","Estimate"],4),
    p_contemp   =round(hac["x","Pr(>|t|)"],3),
    cum_4M      =round(sum(coef(m)[c("x","x_l1","x_l2","x_l3")],na.rm=TRUE),4),
    r2          =round(summary(m)$r.squared,3),
    granger_p   =round(if(!is.null(g)) g$"Pr(>F)"[2] else NA, 3)
  )
}

# 5. Level 1/2

cat("# LEVEL 1/2 | BASKET vs TOTAL AI PAPERS\n")

stock_all <- make_stock_monthly(sp500, TARGET_TICKERS)

basket <- stock_all %>%
  mutate(w = MCAP_WEIGHT[Ticker]) %>%
  group_by(month) %>%
  summarise(
    basket_vw = sum(log_return * w, na.rm=TRUE) / sum(w, na.rm=TRUE),
    basket_ew = mean(log_return, na.rm=TRUE),
    .groups = "drop"
  )

cutoff_date <- max(ai_monthly_raw$month) %m-% months(CITATION_CUTOFF_MONTHS)

ai_macro <- ai_monthly_raw %>%
  mutate(
    total_citations_clean = if_else(month <= cutoff_date,
                                    as.numeric(total_citations), NA_real_),
    log_papers      = log(as.numeric(ai_papers)),
    d_log_papers    = c(NA_real_, diff(log_papers)),
    log_citations   = log(total_citations_clean + 1),
    d_log_citations = c(NA_real_, diff(log_citations))
  )

L12 <- basket %>% inner_join(ai_macro, by="month") %>% arrange(month)
if ("quarter.x" %in% names(L12))
  L12 <- L12 %>% mutate(quarter=quarter.x) %>% select(-quarter.x,-quarter.y)

ts_analysis(L12, "d_log_papers", "basket_vw", "Market(VW) <- AI papers")
ts_analysis(L12, "d_log_papers", "basket_ew", "Tech(EW) <- AI papers")

irf_L1 <- var_irf(L12, "d_log_papers", "basket_vw", "Market(VW) <- AI papers")
irf_L2 <- var_irf(L12, "d_log_papers", "basket_ew", "Tech(EW) <- AI papers")
irf_macro <- bind_rows(irf_L1, irf_L2)

write_csv(irf_macro, file.path(OUT_DIR, "out_L12_irf_8companies.csv"))
write_csv(L12,       file.path(OUT_DIR, "out_L12_8company_basket.csv"))

# 5-B. Total AI papers shock -> 各公司個別 IRF
#     這裡不使用 basket，也不需要任何權重。
cat("\n 5-B. Total AI papers shock -> 各公司個別 IRF \n")
irf_L12_company <- map_dfr(TARGET_TICKERS, function(tk) {
  dd <- stock_all %>%
    filter(Ticker == tk) %>%
    inner_join(ai_macro %>% select(month, d_log_papers), by = "month") %>%
    arrange(month)
  
  out <- var_irf(dd, "d_log_papers", "log_return",
                 paste0(tk, " <- AI papers"), n_ahead = 12)
  
  if (is.null(out) || nrow(out) == 0) return(tibble())
  out %>%
    mutate(ticker = tk,
           shock = "d_log_papers",
           response = "log_return",
           .before = 1)
})
write_csv(irf_L12_company,
          file.path(OUT_DIR, "out_L12_irf_by_company_total_ai.csv"))

# 5-C. 各公司自己的 AI 論文數 shock -> 各公司自己的股票報酬 IRF
#     這才是「各家論文數對各家股價」：
#     GOOGL papers -> GOOGL return, MSFT papers -> MSFT return, ...
cat("\n 5-C. Own-company AI papers shock -> own-company stock IRF \n")
company_monthly_raw <- load_or_build_company_monthly()

company_papers_tr <- stock_all %>%
  distinct(Ticker, month) %>%
  left_join(company_monthly_raw, by = c("Ticker", "month")) %>%
  mutate(
    company_papers = replace_na(company_papers, 0),
    company_citations = replace_na(company_citations, 0)
  ) %>%
  arrange(Ticker, month) %>%
  group_by(Ticker) %>%
  mutate(
    log_company_papers = log(company_papers + 1),
    d_log_company_papers = log_company_papers - dplyr::lag(log_company_papers),
    log_company_citations = log(company_citations + 1),
    d_log_company_citations = log_company_citations - dplyr::lag(log_company_citations)
  ) %>%
  ungroup()

cat("\n[各公司月論文數摘要]\n")
company_papers_tr %>%
  group_by(Ticker) %>%
  summarise(
    months = n(),
    total_papers = sum(company_papers, na.rm = TRUE),
    active_months = sum(company_papers > 0, na.rm = TRUE),
    sd_d_log_papers = sd(d_log_company_papers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

irf_L12_company_own_papers <- map_dfr(TARGET_TICKERS, function(tk) {
  dd <- stock_all %>%
    filter(Ticker == tk) %>%
    inner_join(company_papers_tr %>%
                 filter(Ticker == tk) %>%
                 select(month, d_log_company_papers),
               by = "month") %>%
    arrange(month)
  
  if (nrow(drop_na(dd, d_log_company_papers, log_return)) < 30 ||
      is.na(sd(dd$d_log_company_papers, na.rm = TRUE)) ||
      sd(dd$d_log_company_papers, na.rm = TRUE) == 0) {
    cat("  跳過", tk, ": 公司論文數變化不足或樣本不足\n")
    return(tibble())
  }
  
  out <- var_irf(dd, "d_log_company_papers", "log_return",
                 paste0(tk, " stock <- ", tk, " AI papers"), n_ahead = 12)
  
  if (is.null(out) || nrow(out) == 0) return(tibble())
  out %>%
    mutate(ticker = tk,
           shock = "d_log_company_papers",
           response = "log_return",
           .before = 1)
})

write_csv(company_papers_tr,
          file.path(OUT_DIR, "out_company_monthly_papers_8companies.csv"))
write_csv(irf_L12_company_own_papers,
          file.path(OUT_DIR, "out_L12_irf_by_company_own_papers.csv"))

# 6. Level 3
cat("# LEVEL 3 | SUBFIELD x STOCK\n")

sf_wide <- sf_raw %>%
  select(month, ai_subfield, ai_paper_count) %>%
  pivot_wider(names_from=ai_subfield, values_from=ai_paper_count,
              values_fill=0) %>%
  arrange(month)

sub_cols <- setdiff(names(sf_wide), c("month","quarter"))
sf_tr <- sf_wide
for (col in sub_cols) {
  lv <- log(sf_tr[[col]] + 1)
  sf_tr[[paste0("d_log_",col)]] <- c(NA_real_, diff(lv))
}

cat("\n[子領域差分變異確認]\n")
for (col in sub_cols) {
  s <- sd(sf_tr[[paste0("d_log_",col)]], na.rm=TRUE)
  cat(sprintf("  d_log_%-18s sd=%.4f %s\n", col, s,
              if(is.na(s)||s==0) "異常" else "OK"))
}

pairings <- list(
  LLM_NLP         = c("MSFT","GOOGL","META","AMZN"),
  Computer_Vision  = c("NVDA","TSLA","AAPL"),
  Robotics         = c("NVDA","AVGO","TSLA"),
  Machine_Learning = TARGET_TICKERS,
  General_AI       = c("MSFT","GOOGL","META","NVDA","AMZN"),
  Neural_Networks  = c("NVDA","AVGO","GOOGL")
)
pairings <- pairings[names(pairings) %in% sub_cols]
pairings <- lapply(pairings, function(x) intersect(x, TARGET_TICKERS))

cat("\n配對確認:\n")
for (nm in names(pairings))
  cat(sprintf("  %-18s -> %s\n", nm, paste(pairings[[nm]], collapse=", ")))

# 6-A. 個股 ARDL
cat("\n 6-A. 子領域 x 個股 ARDL \n")
L3_table <- map_dfr(names(pairings), function(sf_name) {
  map_dfr(pairings[[sf_name]], function(tk)
    analyze_pair(sf_name, tk, stock_all, sf_tr))
})
cat("\n[結果總表]\n"); print(L3_table)
write_csv(L3_table, file.path(OUT_DIR, "out_L3_pair_results_8companies.csv"))

# 6-B. Panel FE
cat("\n 6-B. Panel FE \n")
ai_total_ctrl <- ai_macro %>% select(month, d_log_papers) %>%
  rename(d_log_total = d_log_papers)

panel_rows <- list()
for (sf_name in names(pairings)) {
  d_col <- paste0("d_log_", sf_name)
  pdf <- stock_all %>% filter(Ticker %in% pairings[[sf_name]]) %>%
    inner_join(sf_tr %>% select(month, all_of(d_col)), by="month") %>%
    rename(x = !!d_col) %>%
    left_join(ai_total_ctrl, by="month") %>%
    group_by(Ticker) %>% arrange(month, .by_group=TRUE) %>%
    mutate(x_l1=dplyr::lag(x,1), x_l2=dplyr::lag(x,2),
           y_l1=dplyr::lag(log_return,1)) %>%
    ungroup() %>% drop_na()
  if (nrow(pdf) < 50) next
  fe <- tryCatch(
    plm(log_return ~ x + x_l1 + x_l2 + d_log_total + y_l1,
        data=pdf, index=c("Ticker","month"), model="within"),
    error=function(e) { cat("Panel FE 失敗:", sf_name, "\n"); NULL })
  if (is.null(fe)) next
  ct <- coeftest(fe, vcov=vcovHC(fe, type="HC1", cluster="group", method="arellano"))
  cat(sprintf("\n--- %s (firms=%d, obs=%d) ---\n",
              sf_name, n_distinct(pdf$Ticker), nrow(pdf)))
  print(ct)
  panel_rows[[sf_name]] <- data.frame(
    subfield=sf_name, term=rownames(ct),
    estimate=ct[,"Estimate"], p_value=ct[,"Pr(>|t|)"],
    row.names=NULL)
}
if (length(panel_rows) > 0)
  write_csv(bind_rows(panel_rows),
            file.path(OUT_DIR, "out_L3_panel_FE_8companies.csv"))

# 6-C. 子領域 IRF
cat("\n 6-C. 子領域 IRF \n")



# 6-C-2. 新版需求：每個 AI 子領域 shock -> 每一家公司的個別 IRF
#        這裡不做平均、不做 basket、不使用權重。
cat("\n 6-C-2. Subfield shock -> 各公司個別 IRF \n")
irf_L3_company <- map_dfr(sub_cols, function(sf_name) {
  d_col <- paste0("d_log_", sf_name)
  
  map_dfr(TARGET_TICKERS, function(tk) {
    dd <- stock_all %>%
      filter(Ticker == tk) %>%
      inner_join(sf_tr %>% select(month, all_of(d_col)), by = "month") %>%
      arrange(month) %>%
      rename(x = !!d_col)
    
    out <- var_irf(dd, "x", "log_return",
                   paste0(sf_name, " -> ", tk), n_ahead = 12)
    
    if (is.null(out) || nrow(out) == 0) return(tibble())
    out %>%
      mutate(subfield = sf_name,
             ticker = tk,
             shock = d_col,
             response = "log_return",
             .before = 1)
  })
})
write_csv(irf_L3_company,
          file.path(OUT_DIR, "out_L3_irf_by_company_subfield.csv"))

# 7. 產圖
cat("\n 產圖 \n")

if (!is.null(irf_macro) && nrow(irf_macro) > 0) {
  p1 <- ggplot(irf_macro, aes(h, irf)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=.3) +
    geom_line(color="darkblue", linewidth=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    facet_wrap(~label, scales="free_y") +
    labs(title="IRF: AI papers shock -> 8-company returns (12 months)",
         x="月", y="market pricing") +
    theme_minimal(base_size=13)
  ggsave(file.path(OUT_DIR,"fig_L1L2_irf_8companies.png"),
         p1, width=10, height=5, dpi=150)
  cat("fig_L1L2_irf_8companies.png OK\n")
}

if (nrow(L3_table) > 0) {
  p2 <- ggplot(L3_table, aes(x=ticker, y=subfield, fill=cum_4M)) +
    geom_tile(color="white") +
    geom_text(aes(label=sprintf("%.3f%s", cum_4M,
                                ifelse(p_contemp<0.1,"*",""))), size=3) +
    scale_fill_gradient2(low="#d62728", mid="white", high="#2ca02c",
                         midpoint=0, name="cum_4M") +
    scale_x_discrete(limits=TARGET_TICKERS) +
    labs(title="Subfield paper growth: cumulative 4-month effect on stock returns",
         subtitle="* = contemporaneous p<0.10 (HAC)",
         x="stock", y="Subfield") +
    theme_minimal(base_size=12) +
    theme(axis.text.x=element_text(angle=30, hjust=1))
  ggsave(file.path(OUT_DIR,"fig_L3_heatmap_8companies.png"),
         p2, width=10, height=5, dpi=150)
  cat("fig_L3_heatmap_8companies.png OK\n")
}

# Total AI papers shock -> 各公司個別 IRF 圖
if (exists("irf_L12_company") && nrow(irf_L12_company) > 0) {
  p_company_total <- ggplot(irf_L12_company, aes(h, irf)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=.3) +
    geom_line(color="darkblue", linewidth=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    facet_wrap(~ticker, scales="free_y", ncol=4) +
    labs(title="IRF: Total AI papers shock -> individual stock returns",
         subtitle="Each panel is one of the 8 firms; response variable = monthly log return",
         x="月", y="脈衝反應") +
    theme_minimal(base_size=12)
  ggsave(file.path(OUT_DIR,"fig_L12_irf_by_company_total_ai.png"),
         p_company_total, width=12, height=7, dpi=150)
  cat("fig_L12_irf_by_company_total_ai.png OK\n")
}

# Own-company AI papers shock -> 各公司自己股票報酬 IRF 圖
if (exists("irf_L12_company_own_papers") && nrow(irf_L12_company_own_papers) > 0) {
  p_company_own <- ggplot(irf_L12_company_own_papers, aes(h, irf)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=.3) +
    geom_line(color="darkblue", linewidth=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    facet_wrap(~ticker, scales="free_y", ncol=4) +
    labs(title="IRF: Own-company AI papers shock -> own-company stock returns",
         subtitle="Each panel uses the firm's own AI paper count as the impulse; response = monthly log return",
         x="月", y="脈衝反應") +
    theme_minimal(base_size=12)
  ggsave(file.path(OUT_DIR,"fig_L12_irf_by_company_own_papers.png"),
         p_company_own, width=12, height=7, dpi=150)
  cat("fig_L12_irf_by_company_own_papers.png OK\n")
}

# Subfield shock -> 各公司個別 IRF 圖：全部合併版本
if (exists("irf_L3_company") && nrow(irf_L3_company) > 0) {
  p_l3_company_all <- ggplot(irf_L3_company, aes(h, irf)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=.25) +
    geom_line(color="darkblue", linewidth=.7) +
    geom_hline(yintercept=0, linetype="dashed") +
    facet_grid(subfield ~ ticker, scales="free_y") +
    labs(title="IRF: AI subfield shock -> individual stock returns",
         subtitle="Rows = AI subfields; columns = firms; response variable = monthly log return",
         x="月", y="脈衝反應") +
    theme_minimal(base_size=9)
  ggsave(file.path(OUT_DIR,"fig_L3_irf_by_company_subfield_all.png"),
         p_l3_company_all, width=16, height=12, dpi=150)
  cat("fig_L3_irf_by_company_subfield_all.png OK\n")
  
  # 每個子領域各輸出一張，比全部合併版本更適合放簡報
  for (sf_name in unique(irf_L3_company$subfield)) {
    pp <- irf_L3_company %>%
      filter(subfield == sf_name) %>%
      ggplot(aes(h, irf)) +
      geom_ribbon(aes(ymin=lower, ymax=upper), fill="steelblue", alpha=.3) +
      geom_line(color="darkblue", linewidth=1) +
      geom_hline(yintercept=0, linetype="dashed") +
      facet_wrap(~ticker, scales="free_y", ncol=4) +
      labs(title=paste0("IRF: ", sf_name, " shock -> individual stock returns"),
           x="月", y="脈衝反應") +
      theme_minimal(base_size=12)
    
    ggsave(file.path(OUT_DIR,
                     paste0("fig_L3_irf_by_company_", sf_name, ".png")),
           pp, width=12, height=7, dpi=150)
  }
  cat("fig_L3_irf_by_company_<subfield>.png OK\n")
}

p4 <- ggplot(L12, aes(x=month)) +
  geom_col(aes(y=ai_papers / max(ai_papers,na.rm=TRUE) *
                 max(abs(cumsum(replace_na(basket_vw,0))),na.rm=TRUE)),
           fill="steelblue", alpha=0.4) +
  geom_line(aes(y=cumsum(replace_na(basket_vw,0))),
            color="darkred", linewidth=1) +
  labs(title="AI papers (bars) vs cumulative 8-company VW return (line)",
       x=NULL, y="Cumulative VW return / scaled AI papers") +
  theme_minimal()
ggsave(file.path(OUT_DIR,"fig_trend_8companies.png"),
       p4, width=10, height=5, dpi=150)
cat("fig_trend_8companies.png OK\n")

# 8. Own-company × Subfield AI papers shock
#    研究問題：
#    MSFT 自己的 LLM_NLP 論文數成長 shock -> MSFT 股價報酬
#    GOOGL 自己的 Machine_Learning 論文數成長 shock -> GOOGL 股價報酬


cat("# LEVEL 3-B | OWN-COMPANY x SUBFIELD IRF\n")

# 8-1. 讀入 paper_company_panel
if (file.exists("paper_company_panel.rds")) {
  cat("讀入 paper_company_panel.rds\n")
  paper_company_panel2 <- readRDS("paper_company_panel.rds")
} else if (file.exists("paper_company_panel.csv")) {
  cat("讀入 paper_company_panel.csv\n")
  paper_company_panel2 <- read_csv("paper_company_panel.csv", show_col_types = FALSE)
} else {
  stop("找不到 paper_company_panel.rds / paper_company_panel.csv，無法建立公司 × 子領域論文數")
}

if (!"categories" %in% names(paper_company_panel2)) {
  stop("paper_company_panel 缺少 categories 欄位，無法分類 AI 子領域")
}

if (!"institution_name" %in% names(paper_company_panel2)) {
  stop("paper_company_panel 缺少 institution_name 欄位，無法對應公司")
}

if (!"publication_date" %in% names(paper_company_panel2)) {
  stop("paper_company_panel 缺少 publication_date 欄位，無法建立月資料")
}

if (!"work_id" %in% names(paper_company_panel2)) {
  stop("paper_company_panel 缺少 work_id 欄位，無法避免重複計算同一篇論文")
}

# 8-2. 建立 institution_name -> ticker 對照表
company_inst_exact2 <- tribble(
  ~institution_name,                                  ~parent_company, ~ticker,
  "Google (United States)",                           "Alphabet",      "GOOGL",
  "Google (United Kingdom)",                          "Alphabet",      "GOOGL",
  "Google (Canada)",                                  "Alphabet",      "GOOGL",
  "Google (Israel)",                                  "Alphabet",      "GOOGL",
  "Google (Switzerland)",                             "Alphabet",      "GOOGL",
  "Google DeepMind (United Kingdom)",                 "Alphabet",      "GOOGL",
  "DeepMind (United Kingdom)",                        "Alphabet",      "GOOGL",
  
  "Microsoft (United States)",                        "Microsoft",     "MSFT",
  "Microsoft (United Kingdom)",                       "Microsoft",     "MSFT",
  "Microsoft (Canada)",                               "Microsoft",     "MSFT",
  "Microsoft (France)",                               "Microsoft",     "MSFT",
  "Microsoft (Germany)",                              "Microsoft",     "MSFT",
  "Microsoft (India)",                                "Microsoft",     "MSFT",
  "Microsoft (Israel)",                               "Microsoft",     "MSFT",
  "Microsoft (Netherlands)",                          "Microsoft",     "MSFT",
  "Microsoft (Norway)",                               "Microsoft",     "MSFT",
  "Microsoft (Denmark)",                              "Microsoft",     "MSFT",
  "Microsoft (Finland)",                              "Microsoft",     "MSFT",
  "Microsoft (Switzerland)",                          "Microsoft",     "MSFT",
  "Microsoft Research (United Kingdom)",              "Microsoft",     "MSFT",
  "Microsoft Research (India)",                       "Microsoft",     "MSFT",
  "Microsoft Research Asia (China)",                  "Microsoft",     "MSFT",
  "Microsoft Research Montréal (Canada)",             "Microsoft",     "MSFT",
  "Microsoft Research New England (United States)",   "Microsoft",     "MSFT",
  "Microsoft Research New York City (United States)", "Microsoft",     "MSFT",
  "LinkedIn (United States)",                         "Microsoft",     "MSFT",
  
  "Meta (United States)",                             "Meta",          "META",
  "Meta (Israel)",                                    "Meta",          "META",
  "Meta (United Kingdom)",                            "Meta",          "META",
  
  "Nvidia (United States)",                           "NVIDIA",        "NVDA",
  "Nvidia (United Kingdom)",                          "NVIDIA",        "NVDA",
  
  "Amazon (United States)",                           "Amazon",        "AMZN",
  "Amazon (United Kingdom)",                          "Amazon",        "AMZN",
  "Amazon (Germany)",                                 "Amazon",        "AMZN",
  
  "Apple (United States)",                            "Apple",         "AAPL",
  "Apple (United Kingdom)",                           "Apple",         "AAPL",
  "Apple (Germany)",                                  "Apple",         "AAPL",
  "Apple (Israel)",                                   "Apple",         "AAPL",
  
  "Tesla (United States)",                            "Tesla",         "TSLA",
  
  "Broadcom (United States)",                         "Broadcom",      "AVGO",
  "Broadcom (Israel)",                                "Broadcom",      "AVGO"
)

# 8-3. 建立 公司 × 子領域 × 月份 的 AI 論文數
#      重要：
#      一篇論文可以同時屬於多個子領域。
#      同一篇論文在同一家公司、同一子領域、同一月份只算一次。-

company_subfield_monthly_raw <- paper_company_panel2 %>%
  inner_join(company_inst_exact2, by = "institution_name", relationship = "many-to-many") %>%
  mutate(
    pub_date = as.Date(publication_date),
    month = floor_date(pub_date, "month"),
    categories = as.character(categories)
  ) %>%
  filter(
    !is.na(month),
    month >= as.Date("2017-01-01"),
    ticker %in% TARGET_TICKERS,
    !is.na(categories),
    categories != ""
  ) %>%
  mutate(cats = str_split(str_trim(categories), "\\s+")) %>%
  unnest(cats) %>%
  filter(cats %in% names(SUBFIELD_MAP)) %>%
  mutate(
    ai_subfield = unname(SUBFIELD_MAP[cats]),
    Ticker = as.character(ticker),
    cited_by_count2 = if ("cited_by_count" %in% names(.)) as.numeric(cited_by_count) else 0
  ) %>%
  distinct(work_id, Ticker, month, ai_subfield, .keep_all = TRUE) %>%
  group_by(Ticker, month, ai_subfield) %>%
  summarise(
    company_subfield_papers = n_distinct(work_id),
    company_subfield_citations = sum(cited_by_count2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Ticker, ai_subfield, month)

# 補齊沒有發表論文的月份為 0
subfields_to_use <- sort(unique(unname(SUBFIELD_MAP)))

company_subfield_panel <- stock_all %>%
  distinct(Ticker, month) %>%
  filter(Ticker %in% TARGET_TICKERS) %>%
  crossing(ai_subfield = subfields_to_use) %>%
  left_join(
    company_subfield_monthly_raw,
    by = c("Ticker", "month", "ai_subfield")
  ) %>%
  mutate(
    company_subfield_papers = replace_na(company_subfield_papers, 0),
    company_subfield_citations = replace_na(company_subfield_citations, 0)
  ) %>%
  arrange(Ticker, ai_subfield, month) %>%
  group_by(Ticker, ai_subfield) %>%
  mutate(
    log_company_subfield_papers = log(company_subfield_papers + 1),
    d_log_company_subfield_papers =
      log_company_subfield_papers - dplyr::lag(log_company_subfield_papers)
  ) %>%
  ungroup()

write_csv(
  company_subfield_panel,
  file.path(OUT_DIR, "out_company_subfield_monthly_8companies.csv")
)

cat("\n[公司 × 子領域月論文數摘要]\n")
company_subfield_panel %>%
  group_by(Ticker, ai_subfield) %>%
  summarise(
    total_papers = sum(company_subfield_papers, na.rm = TRUE),
    active_months = sum(company_subfield_papers > 0, na.rm = TRUE),
    sd_d_log = sd(d_log_company_subfield_papers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ai_subfield, Ticker) %>%
  print(n = 100)

# 8-4. 公司自己的子領域論文數 shock -> 公司自己的股票報酬 IRF
#
#      例：
#      MSFT 的 LLM_NLP 論文數成長 shock -> MSFT monthly log return
#      GOOGL 的 Machine_Learning 論文數成長 shock -> GOOGL monthly log return

cat("\n Own-company subfield papers shock -> own-company stock return IRF \n")

irf_company_own_subfield <- map_dfr(subfields_to_use, function(sf_name) {
  map_dfr(TARGET_TICKERS, function(tk) {
    
    dd <- stock_all %>%
      filter(Ticker == tk) %>%
      select(month, log_return) %>%
      inner_join(
        company_subfield_panel %>%
          filter(Ticker == tk, ai_subfield == sf_name) %>%
          select(month, d_log_company_subfield_papers),
        by = "month"
      ) %>%
      arrange(month) %>%
      filter(
        is.finite(log_return),
        is.finite(d_log_company_subfield_papers)
      )
    
    if (
      nrow(dd) < 30 ||
      is.na(sd(dd$d_log_company_subfield_papers, na.rm = TRUE)) ||
      sd(dd$d_log_company_subfield_papers, na.rm = TRUE) == 0 ||
      is.na(sd(dd$log_return, na.rm = TRUE)) ||
      sd(dd$log_return, na.rm = TRUE) == 0
    ) {
      cat("  跳過", tk, "-", sf_name, ": 樣本不足或該公司該子領域論文數沒有變化\n")
      return(tibble())
    }
    
    out <- var_irf(
      dd,
      x_col = "d_log_company_subfield_papers",
      y_col = "log_return",
      label = paste0(tk, " stock <- ", tk, " ", sf_name, " papers"),
      n_ahead = 12
    )
    
    if (is.null(out) || nrow(out) == 0) return(tibble())
    
    out %>%
      mutate(
        ticker = tk,
        subfield = sf_name,
        shock = "own_company_subfield_papers",
        response = "own_company_log_return",
        .before = 1
      )
  })
})

write_csv(
  irf_company_own_subfield,
  file.path(OUT_DIR, "out_L3_irf_by_company_own_subfield.csv")
)

# 8-5. 畫圖：公司自己的子領域論文數 -> 公司自己的股價

if (nrow(irf_company_own_subfield) > 0) {
  
  p_own_subfield_all <- ggplot(irf_company_own_subfield, aes(h, irf)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = .25) +
    geom_line(color = "darkblue", linewidth = .7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(subfield ~ ticker, scales = "free_y") +
    labs(
      title = "IRF: Own-company AI subfield papers shock -> own-company stock returns",
      subtitle = "Rows = AI subfields; columns = firms; impulse = firm's own subfield paper growth; response = monthly log return",
      x = "月",
      y = "脈衝反應"
    ) +
    theme_minimal(base_size = 9)
  
  ggsave(
    file.path(OUT_DIR, "fig_L3_irf_by_company_own_subfield_all.png"),
    p_own_subfield_all,
    width = 16,
    height = 12,
    dpi = 150
  )
  
  cat("fig_L3_irf_by_company_own_subfield_all.png OK\n")
  
  # 每個子領域各輸出一張圖，較適合放簡報
  for (sf_name in unique(irf_company_own_subfield$subfield)) {
    
    pp <- irf_company_own_subfield %>%
      filter(subfield == sf_name) %>%
      ggplot(aes(h, irf)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = .3) +
      geom_line(color = "darkblue", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~ticker, scales = "free_y", ncol = 4) +
      labs(
        title = paste0("IRF: Own-company ", sf_name, " papers shock -> own-company stock returns"),
        subtitle = "Impulse = firm's own subfield paper growth; response = firm's monthly log return",
        x = "月",
        y = "脈衝反應"
      ) +
      theme_minimal(base_size = 12)
    
    ggsave(
      file.path(
        OUT_DIR,
        paste0("fig_L3_irf_by_company_own_subfield_", sf_name, ".png")
      ),
      pp,
      width = 12,
      height = 7,
      dpi = 150
    )
  }
  
  cat("fig_L3_irf_by_company_own_subfield_<subfield>.png OK\n")
  
} else {
  cat("沒有可繪製的 own-company subfield IRF 結果；可能是公司 × 子領域論文數太稀疏。\n")
}


# 9. Own-company x Subfield cumulative 1-12 month effects
#    研究問題：
#    公司自己的 AI 子領域論文數成長
#    -> 公司自己的股票月報酬
#
#    例：
#    MSFT 自己的 LLM_NLP 論文數成長 -> MSFT stock return
#    NVDA 自己的 Machine_Learning 論文數成長 -> NVDA stock return

cat("# LEVEL 3-C | OWN-COMPANY x SUBFIELD CUMULATIVE EFFECTS 1-12M\n")


# 9-0. 檢查前面需要的物件是否存在

if (!exists("company_subfield_panel")) {
  stop("找不到 company_subfield_panel。請先執行前面建立『公司 × 子領域 × 月份』論文數的程式。")
}

if (!exists("stock_all")) {
  stop("找不到 stock_all。請先執行前面處理股票月報酬的程式。")
}

if (!exists("TARGET_TICKERS")) {
  stop("找不到 TARGET_TICKERS。請先定義 8 家公司 ticker。")
}

if (!exists("OUT_DIR")) {
  OUT_DIR <- "ai_8_outputs"
}

if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}

# 9-1. 建立公司 × 子領域資料摘要

company_subfield_summary <- company_subfield_panel %>%
  group_by(Ticker, ai_subfield) %>%
  summarise(
    total_papers = sum(company_subfield_papers, na.rm = TRUE),
    active_months = sum(company_subfield_papers > 0, na.rm = TRUE),
    sd_d_log = sd(d_log_company_subfield_papers, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  company_subfield_summary,
  file.path(OUT_DIR, "out_company_subfield_summary_8companies.csv")
)

cat("\n[公司 × 子領域論文數摘要]\n")
print(company_subfield_summary, n = 100)

# 9-2. 定義函數：估計累積 1 到 12 個月效果
#
# 模型：
# log_return_t =
#   alpha
#   + beta0 * x_t
#   + beta1 * x_{t-1}
#   + ...
#   + beta11 * x_{t-11}
#   + gamma * log_return_{t-1}
#   + error_t
#
# 其中：
# x = 公司自己的某 AI 子領域論文數成長
#
# cum_1M  = beta0
# cum_2M  = beta0 + beta1
# cum_3M  = beta0 + beta1 + beta2
# ...
# cum_12M = beta0 + beta1 + ... + beta11

fit_own_company_subfield_cum <- function(tk, sf_name, max_h = 12) {
  
  dd <- stock_all %>%
    filter(Ticker == tk) %>%
    select(month, log_return) %>%
    inner_join(
      company_subfield_panel %>%
        filter(Ticker == tk, ai_subfield == sf_name) %>%
        select(month, d_log_company_subfield_papers),
      by = "month"
    ) %>%
    arrange(month) %>%
    mutate(
      y_l1 = dplyr::lag(log_return),
      x_l0 = d_log_company_subfield_papers
    )
  
  # 建立 x_l1 到 x_l11
  if (max_h >= 2) {
    for (L in 1:(max_h - 1)) {
      dd[[paste0("x_l", L)]] <- dplyr::lag(dd$x_l0, L)
    }
  }
  
  x_cols <- paste0("x_l", 0:(max_h - 1))
  
  df <- dd %>%
    select(month, log_return, y_l1, all_of(x_cols)) %>%
    filter(if_all(everything(), ~ is.finite(.x)))
  
  # 樣本太少或沒有變異就跳過
  if (
    nrow(df) < max(36, max_h + 10) ||
    is.na(sd(df$x_l0, na.rm = TRUE)) ||
    sd(df$x_l0, na.rm = TRUE) == 0 ||
    is.na(sd(df$log_return, na.rm = TRUE)) ||
    sd(df$log_return, na.rm = TRUE) == 0
  ) {
    cat("  跳過", tk, "-", sf_name, ": 樣本不足或論文數變化不足\n")
    return(tibble())
  }
  
  form <- as.formula(
    paste(
      "log_return ~ y_l1 +",
      paste(x_cols, collapse = " + ")
    )
  )
  
  m <- tryCatch(
    lm(form, data = df),
    error = function(e) NULL
  )
  
  if (is.null(m)) {
    cat("  跳過", tk, "-", sf_name, ": lm 無法估計\n")
    return(tibble())
  }
  
  b <- coef(m)
  
  # HAC covariance matrix
  V <- tryCatch(
    sandwich::NeweyWest(
      m,
      lag = min(6, floor(nrow(df) / 4)),
      prewhite = FALSE,
      adjust = TRUE
    ),
    error = function(e) vcov(m)
  )
  
  # 逐一計算 cum_1M 到 cum_12M
  out <- purrr::map_dfr(1:max_h, function(hh) {
    
    use_cols <- paste0("x_l", 0:(hh - 1))
    
    # 如果某些係數因共線性被丟掉，該 horizon 設為 NA
    if (!all(use_cols %in% names(b))) {
      return(tibble(
        Ticker = tk,
        ai_subfield = sf_name,
        horizon = hh,
        cum_effect = NA_real_,
        se = NA_real_,
        z = NA_real_,
        p_value = NA_real_,
        n_obs = nrow(df)
      ))
    }
    
    if (any(is.na(b[use_cols]))) {
      return(tibble(
        Ticker = tk,
        ai_subfield = sf_name,
        horizon = hh,
        cum_effect = NA_real_,
        se = NA_real_,
        z = NA_real_,
        p_value = NA_real_,
        n_obs = nrow(df)
      ))
    }
    
    L <- rep(0, length(b))
    names(L) <- names(b)
    L[use_cols] <- 1
    
    cum_beta <- sum(b[use_cols], na.rm = TRUE)
    
    se_cum <- tryCatch(
      sqrt(as.numeric(t(L) %*% V %*% L)),
      error = function(e) NA_real_
    )
    
    z_val <- ifelse(!is.na(se_cum) && se_cum > 0, cum_beta / se_cum, NA_real_)
    p_val <- ifelse(!is.na(z_val), 2 * pnorm(abs(z_val), lower.tail = FALSE), NA_real_)
    
    tibble(
      Ticker = tk,
      ai_subfield = sf_name,
      horizon = hh,
      cum_effect = cum_beta,
      se = se_cum,
      z = z_val,
      p_value = p_val,
      n_obs = nrow(df)
    )
  })
  
  out
}

# 9-3. 對 8 家公司 × 所有子領域計算累積 1-12 月效果

subfields_to_use <- sort(unique(company_subfield_panel$ai_subfield))

cat("\n 計算 own-company x subfield cumulative 1-12M effects \n")

own_subfield_cum_1to12 <- purrr::map_dfr(subfields_to_use, function(sf_name) {
  purrr::map_dfr(TARGET_TICKERS, function(tk) {
    fit_own_company_subfield_cum(tk, sf_name, max_h = 12)
  })
}) %>%
  left_join(
    company_subfield_summary,
    by = c("Ticker", "ai_subfield")
  ) %>%
  mutate(
    sig = case_when(
      !is.na(p_value) & p_value < 0.01 ~ "***",
      !is.na(p_value) & p_value < 0.05 ~ "**",
      !is.na(p_value) & p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    horizon_label = paste0(horizon, "M"),
    label = ifelse(
      is.na(cum_effect),
      "",
      paste0(round(cum_effect, 3), sig)
    )
  )

write_csv(
  own_subfield_cum_1to12,
  file.path(OUT_DIR, "out_L3_own_company_subfield_cumulative_1to12.csv")
)

cat("\n已輸出：out_L3_own_company_subfield_cumulative_1to12.csv\n")


# 9-4. 找出每個 公司 × 子領域 的最佳累積月份
#      這裡用「最大正向 cum_effect」定義效果最好。

best_positive_by_pair <- own_subfield_cum_1to12 %>%
  filter(is.finite(cum_effect)) %>%
  group_by(Ticker, ai_subfield) %>%
  slice_max(order_by = cum_effect, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(cum_effect)) %>%
  mutate(
    best_label = paste0(round(cum_effect, 3), sig, "\n", horizon, "M")
  )

write_csv(
  best_positive_by_pair,
  file.path(OUT_DIR, "out_L3_own_company_subfield_best_positive.csv")
)

cat("\n[最大正向累積效果 Top 30]\n")
best_positive_by_pair %>%
  select(Ticker, ai_subfield, horizon, cum_effect, p_value, total_papers, active_months, n_obs) %>%
  arrange(desc(cum_effect)) %>%
  print(n = 30)


# 9-5. 另外找出絕對值最大的效果

best_abs_by_pair <- own_subfield_cum_1to12 %>%
  filter(is.finite(cum_effect)) %>%
  mutate(abs_effect = abs(cum_effect)) %>%
  group_by(Ticker, ai_subfield) %>%
  slice_max(order_by = abs_effect, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(abs_effect)) %>%
  mutate(
    best_abs_label = paste0(round(cum_effect, 3), sig, "\n", horizon, "M")
  )

write_csv(
  best_abs_by_pair,
  file.path(OUT_DIR, "out_L3_own_company_subfield_best_abs.csv")
)

cat("\n[絕對值最大累積效果 Top 30]\n")
best_abs_by_pair %>%
  select(Ticker, ai_subfield, horizon, cum_effect, abs_effect, p_value, total_papers, active_months, n_obs) %>%
  arrange(desc(abs_effect)) %>%
  print(n = 30)

# 9-6. 圖 1：每個 公司 × 子領域 的最佳正向效果 heatmap
#      格子數字：
#      第一行 = 最大正向累積效果
#      第二行 = 發生在第幾個月累積 horizon

if (nrow(best_positive_by_pair) > 0) {
  
  p_best_positive <- ggplot(
    best_positive_by_pair,
    aes(x = Ticker, y = ai_subfield, fill = cum_effect)
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = best_label), size = 3.2) +
    scale_fill_gradient2(
      low = "#f4b6ad",
      mid = "white",
      high = "#2ca02c",
      midpoint = 0,
      na.value = "grey90"
    ) +
    labs(
      title = "Best cumulative effect: own-company AI subfield papers -> own-company stock returns",
      subtitle = "Cell = largest positive cumulative effect across horizons 1-12M; label shows effect and best horizon. * p<0.10, ** p<0.05, *** p<0.01 HAC",
      x = "Company",
      y = "AI subfield",
      fill = "Best cumulative effect"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      panel.grid = element_blank()
    )
  
  ggsave(
    file.path(OUT_DIR, "fig_L3_own_company_subfield_best_positive_heatmap.png"),
    p_best_positive,
    width = 12,
    height = 7,
    dpi = 160
  )
  
  cat("fig_L3_own_company_subfield_best_positive_heatmap.png OK\n")
}

# 9-7. 圖 2：累積 1 到 12 月全部 heatmap
#      每個 facet 是一個累積月份。

plot_all_horizon <- own_subfield_cum_1to12 %>%
  filter(is.finite(cum_effect)) %>%
  mutate(
    horizon_label = factor(
      paste0(horizon, "M"),
      levels = paste0(1:12, "M")
    )
  )

if (nrow(plot_all_horizon) > 0) {
  
  p_all_horizon <- ggplot(
    plot_all_horizon,
    aes(x = Ticker, y = ai_subfield, fill = cum_effect)
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 2.2) +
    scale_fill_gradient2(
      low = "#f4b6ad",
      mid = "white",
      high = "#2ca02c",
      midpoint = 0,
      na.value = "grey90"
    ) +
    facet_wrap(~ horizon_label, ncol = 4) +
    labs(
      title = "Cumulative 1-12M effects: own-company AI subfield papers -> own-company stock returns",
      subtitle = "Each facet is cumulative horizon. Cell = cumulative effect. * p<0.10, ** p<0.05, *** p<0.01 HAC",
      x = "Company",
      y = "AI subfield",
      fill = "Cumulative effect"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      panel.grid = element_blank()
    )
  
  ggsave(
    file.path(OUT_DIR, "fig_L3_own_company_subfield_cumulative_1to12_heatmaps.png"),
    p_all_horizon,
    width = 16,
    height = 14,
    dpi = 160
  )
  
  cat("fig_L3_own_company_subfield_cumulative_1to12_heatmaps.png OK\n")
}

# 9-8. 圖 3：Top 20 最大正向效果排名

top20_positive <- best_positive_by_pair %>%
  filter(is.finite(cum_effect)) %>%
  arrange(desc(cum_effect)) %>%
  slice_head(n = 20) %>%
  mutate(
    combo = paste0(Ticker, " - ", ai_subfield, " (", horizon, "M)")
  )

if (nrow(top20_positive) > 0) {
  
  p_top20 <- ggplot(
    top20_positive,
    aes(x = reorder(combo, cum_effect), y = cum_effect)
  ) +
    geom_col(fill = "#2ca02c", alpha = 0.85) +
    coord_flip() +
    geom_text(
      aes(label = paste0(round(cum_effect, 3), sig)),
      hjust = -0.1,
      size = 3
    ) +
    labs(
      title = "Top 20 best positive cumulative effects",
      subtitle = "Own-company AI subfield papers -> own-company stock returns; best horizon selected from 1-12M",
      x = NULL,
      y = "Best cumulative effect"
    ) +
    theme_minimal(base_size = 11)
  
  ggsave(
    file.path(OUT_DIR, "fig_L3_own_company_subfield_top20_positive.png"),
    p_top20,
    width = 11,
    height = 8,
    dpi = 160
  )
  
  cat("fig_L3_own_company_subfield_top20_positive.png OK\n")
}

cat("\n完成：Own-company x subfield cumulative 1-12M effects\n")
cat("主要輸出：\n")
cat("1. out_L3_own_company_subfield_cumulative_1to12.csv\n")
cat("2. out_L3_own_company_subfield_best_positive.csv\n")
cat("3. out_L3_own_company_subfield_best_abs.csv\n")
cat("4. fig_L3_own_company_subfield_best_positive_heatmap.png\n")
cat("5. fig_L3_own_company_subfield_cumulative_1to12_heatmaps.png\n")
cat("6. fig_L3_own_company_subfield_top20_positive.png\n")
