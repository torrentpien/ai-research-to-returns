#  8家公司：R&D Expense (財報對應到真實月份,階梯狀) vs 自身AI論文數(自然年)
#  時間範圍：2017-01 ~ 2025-12
#  頻率：年度
#  每家公司各一張圖，雙軸疊圖
#  + 新增：8家合計圖(R&D加總用階梯邏輯, 論文數用年度加總折線)
#
#  本檔案會自動：
#    1. 從 SEC EDGAR 抓 R&D Expense 原始資料 (若本機已有CSV則直接讀,但部分資料如AMZN需要手動查詢,所以就直接讀csv最快)
#    2. 讀入各公司自身AI論文數 (company_annual.rds)
#    3. 畫階梯圖(8張個別圖 + 1張合計圖)
#
#  核心邏輯：
#    R&D 不做任何平移/估算/pro-rate，直接用財報申報的
#    period_start ~ period_end 當X軸時間區間，
#    用階梯函數畫水平線段，高度=該期R&D金額，換期時垂直跳升/下降。
#    例如 MSFT "Year ended June 30, 2025" -> 線段畫在
#    2024-07-01 ~ 2025-06-30 這段時間, 高度=該期R&D金額。
#    論文數則用自然年聚合，疊在同一張圖右軸。
#
#    【合計圖的特殊邏輯】
#    8家公司財年結算日不同(MSFT=6月,NVDA=1月,AVGO=11月,其餘=12月)，
#    不能直接把"fiscal_year標籤相同"的金額加總(那樣會混用不同時間段)。
#    正確做法：在時間軸上找出所有公司"財報換期"的時間點(即所有
#    period_start的集合)，在每一個換期時間點上，計算當下8家公司
#    "各自最新生效"的R&D金額，加總起來，形成一條新的(更密集的)
#    階梯線。這樣任何時刻的加總值，都精確反映「此刻已知的8家公司
#    最新申報R&D加總」，不會把不同財年的數字錯誤地按相同標籤合併。

setwd("C:/Users/User/BIG_DATA")   # <- 改成你的資料夾

pkgs <- c("httr","jsonlite","dplyr","tidyr","ggplot2","readr",
          "stringr","lubridate","purrr","tibble")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

select <- dplyr::select
filter <- dplyr::filter

OUT_DIR <- "rd_papers_outputs"
dir.create(OUT_DIR, showWarnings = FALSE)

TARGET_TICKERS <- c("GOOGL","MSFT","META","NVDA","AMZN","AAPL","TSLA","AVGO")
DATE_MIN <- as.Date("2017-01-01")
DATE_MAX <- as.Date("2025-12-31")

# SEC 強制要求所有 API 請求帶 User-Agent
SEC_USER_AGENT <- "y@gmail.com"  # <- 改成你的email
SEC_HEADERS <- add_headers(`User-Agent` = SEC_USER_AGENT,
                           `Accept-Encoding` = "gzip, deflate")
sec_sleep <- function() Sys.sleep(0.2)

cat(" R&D Expense (階梯,財報原始期間) vs AI論文數(自然年/季) 趨勢圖\n")


# 1. 取得/讀取 R&D Expense 原始資料

if (file.exists("rd_expense_annual.csv")) {
  
  cat("[1/3] 偵測到本機已有 rd_expense_annual.csv，直接讀取\n")
  rd_annual_src    <- read_csv("rd_expense_annual.csv", show_col_types = FALSE)
  
} else {
  
  cat("[1/3] 找不到現成的R&D CSV，開始向 SEC EDGAR 抓取...\n")
  
  resp_tickers <- GET("https://www.sec.gov/files/company_tickers.json", SEC_HEADERS)
  if (status_code(resp_tickers) != 200)
    stop("無法取得 company_tickers.json，狀態碼: ", status_code(resp_tickers),
         "\n請確認 SEC_USER_AGENT 是否已改成你的email，以及網路連線")
  
  ticker_json <- fromJSON(content(resp_tickers, "text", encoding = "UTF-8"))
  ticker_df <- map_dfr(ticker_json, function(x)
    tibble(cik_str = x$cik_str, ticker = x$ticker, title = x$title))
  
  cik_lookup <- ticker_df %>%
    filter(ticker %in% TARGET_TICKERS) %>%
    mutate(cik_padded = str_pad(cik_str, 10, pad = "0")) %>%
    distinct(ticker, .keep_all = TRUE)
  
  missing_tk <- setdiff(TARGET_TICKERS, cik_lookup$ticker)
  if (length(missing_tk) > 0)
    warning("找不到以下公司的CIK: ", paste(missing_tk, collapse=", "))
  
  RD_TAG_CANDIDATES <- c(
    "ResearchAndDevelopmentExpense",
    "ResearchAndDevelopmentExpenseExcludingAcquiredInProcessCost",
    "ResearchAndDevelopmentExpenseSoftwareExcludingAcquiredInProcessCost"
  )
  
  fetch_concept <- function(cik_padded, tag) {
    url <- sprintf("https://data.sec.gov/api/xbrl/companyconcept/CIK%s/us-gaap/%s.json",
                   cik_padded, tag)
    resp <- tryCatch(GET(url, SEC_HEADERS), error = function(e) NULL)
    sec_sleep()
    if (is.null(resp) || status_code(resp) != 200) return(NULL)
    tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE),
             error = function(e) NULL)
  }
  
  fetch_rd_for_ticker <- function(ticker, cik_padded) {
    cat(sprintf("  %-6s (CIK %s) ... ", ticker, cik_padded))
    for (tag in RD_TAG_CANDIDATES) {
      parsed <- fetch_concept(cik_padded, tag)
      if (!is.null(parsed) && !is.null(parsed$units) && !is.null(parsed$units$USD)) {
        units <- parsed$units$USD
        if (nrow(units) > 0) {
          cat("成功 (tag:", tag, ", rows:", nrow(units), ")\n")
          return(units %>% mutate(ticker = ticker, cik = cik_padded, tag_used = tag) %>%
                   as_tibble())
        }
      }
    }
    cat("找不到任何候選tag的資料\n")
    NULL
  }
  
  rd_raw_all <- map2(cik_lookup$ticker, cik_lookup$cik_padded, fetch_rd_for_ticker) %>%
    bind_rows()
  
  if (nrow(rd_raw_all) == 0)
    stop("所有公司都抓不到R&D資料，請檢查 SEC_USER_AGENT 設定或網路連線")
  
  write_csv(rd_raw_all, "rd_expense_raw_all.csv")
  
  rd_annual_src <- rd_raw_all %>%
    filter(form == "10-K", fp == "FY") %>%
    mutate(start_date = as.Date(start), end_date = as.Date(end),
           filed_date = as.Date(filed),
           period_days = as.numeric(end_date - start_date)) %>%
    filter(period_days >= 340, period_days <= 380) %>%
    group_by(ticker, fy) %>%
    slice_max(filed_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(ticker, cik, fiscal_year = fy, fiscal_period_end = end_date,
              rd_expense_usd = val, form, filed_date, tag_used) %>%
    arrange(ticker, fiscal_year)
  
  write_csv(rd_annual_src, "rd_expense_annual.csv")
  cat("  抓取完成，已儲存 rd_expense_annual.csv \n")
}

cat("R&D 年度原始筆數:", nrow(rd_annual_src), "\n")

# 2. 計算每一期R&D的「真實申報起訖日期」
cat("[2/3] 計算各期R&D的實際申報起訖日期...\n")

rd_annual_raw <- rd_annual_src %>%
  filter(ticker %in% TARGET_TICKERS) %>%
  mutate(
    period_end   = as.Date(fiscal_period_end),
    period_start = period_end %m-% months(12) + days(1)
  ) %>%
  filter(period_end >= DATE_MIN, period_start <= DATE_MAX) %>%
  select(ticker, fiscal_year, period_start, period_end, rd_expense_usd, tag_used)

cat("R&D 年度(2017-2025範圍內)筆數:", nrow(rd_annual_raw), "\n")

if (nrow(rd_annual_raw) == 0)
  stop("篩選後R&D年度資料為0筆，請檢查 rd_expense_annual.csv 的內容與欄位")


# 3. 讀入各公司自身論文數（自然年 + 自然季）
if (!file.exists("company_annual.rds"))
  stop("找不到 company_annual.rds，請確認有該檔")

papers_annual <- readRDS("company_annual.rds") %>%
  filter(ticker %in% TARGET_TICKERS, year >= 2017, year <= 2025) %>%
  select(ticker, year, n_papers) %>%
  mutate(
    period_start = as.Date(paste0(year, "-01-01")),
    period_end   = as.Date(paste0(year, "-12-31"))
  )


cat("論文 年度資料筆數:", nrow(papers_annual), "\n")


# 4. 把每一期資料轉成「階梯線段」格式 (單家公司用)

cat("[3/3] 轉換為階梯線段格式並繪圖...\n\n")

to_step_segments <- function(df, value_col) {
  df %>%
    mutate(.value = .data[[value_col]]) %>%
    select(ticker, period_start, period_end, .value) %>%
    arrange(ticker, period_start) %>%
    group_by(ticker) %>%
    mutate(seg_id = row_number()) %>%
    ungroup() %>%
    pivot_longer(cols = c(period_start, period_end),
                 names_to = "edge", values_to = "date") %>%
    arrange(ticker, seg_id, edge) %>%
    select(ticker, seg_id, date, value = .value)
}

rd_annual_seg    <- to_step_segments(rd_annual_raw, "rd_expense_usd") %>%
  mutate(value_billion = value / 1e9)

papers_annual_seg    <- to_step_segments(papers_annual, "n_papers")


# 5. 畫圖：年度階梯圖 (8張個別圖)
cat("===== 產生年度階梯圖(個別8家) =====\n")

plot_step_annual <- function(tk) {
  rd_d   <- rd_annual_seg %>% filter(ticker == tk)
  paper_y <- papers_annual %>% filter(ticker == tk) %>% arrange(year)
  
  if (nrow(rd_d) == 0) { cat("  跳過(無R&D資料):", tk, "\n"); return(NULL) }
  if (nrow(paper_y) == 0) { cat("  警告(無年度論文資料):", tk, "\n") }
  
  rd_max    <- max(rd_d$value_billion, na.rm = TRUE)
  paper_max <- max(paper_y$n_papers, na.rm = TRUE)
  scale_factor <- if (is.finite(paper_max) && paper_max > 0) rd_max / paper_max else 1
  
  # 論文數：直接用 company_annual.rds 的 year + n_papers 畫點，
  # 每個點對應該年實際論文數，標在當年1月1日，點與點之間用直線連接
  paper_y <- paper_y %>%
    mutate(
      plot_date    = as.Date(paste0(year, "-01-01")),
      value_scaled = n_papers * scale_factor
    )
  
  # 階梯拆成兩種線分開畫：
  #   水平段(該期生效期間, period_start~period_end) -> 實線
  #   垂直跳躍段(上一期結束 -> 下一期開始, 數值的轉換) -> 虛線
  # 這樣視覺上是「扎實的水平階梯 + 虛線銜接的跳躍」，
  # 而不是整條線都是虛線。
  rd_step_pts <- rd_annual_raw %>%
    filter(ticker == tk) %>%
    arrange(period_start) %>%
    mutate(value_billion = rd_expense_usd / 1e9)
  
  rd_horizontal <- rd_step_pts %>%
    transmute(x = period_start, xend = period_end,
              y = value_billion, yend = value_billion)
  
  rd_vertical <- rd_step_pts %>%
    arrange(period_start) %>%
    transmute(
      x    = period_end,
      xend = lead(period_start),
      y    = value_billion,
      yend = lead(value_billion)
    ) %>%
    filter(!is.na(xend))
  
  p <- ggplot() +
    geom_segment(data = rd_horizontal, aes(x = x, xend = xend, y = y, yend = yend),
                 color = "steelblue", linewidth = 1.3, linetype = "solid") +
    geom_segment(data = rd_vertical, aes(x = x, xend = xend, y = y, yend = yend),
                 color = "steelblue", linewidth = 1.3, linetype = "dashed") +
    geom_line(data = paper_y, aes(x = plot_date, y = value_scaled),
              color = "darkred", linewidth = 1.1) +
    geom_point(data = paper_y, aes(x = plot_date, y = value_scaled),
               color = "darkred", size = 2) +
    scale_y_continuous(
      name = "R&D Expense (Billion USD) [財報原始申報期間,階梯狀]",
      sec.axis = sec_axis(~ . / scale_factor, name = "AI 論文數 (篇) [自然年,點連直線]")
    ) +
    scale_x_date(breaks = seq(as.Date("2017-01-01"), as.Date("2025-01-01"), by = "1 year"),
                 date_labels = "%Y",
                 limits = c(DATE_MIN, DATE_MAX),
                 expand = c(0, 0)) +
    labs(
      title = paste0(tk, "：R&D Expense vs 自身AI論文數（年度，階梯狀）"),
      subtitle = "藍實線=R&D申報期間(水平段) ; 藍虛線=財年換期銜接(跳躍段) ; 紅點線=AI論文數(自然年,右軸)",
      x = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.title.y.left  = element_text(color = "steelblue"),
      axis.title.y.right = element_text(color = "darkred"),
      panel.grid.minor = element_blank()
    )
  
  fname <- file.path(OUT_DIR, paste0("fig_step_annual_", tk, ".png"))
  ggsave(fname, p, width = 10, height = 5.5, dpi = 150)
  cat("  完成:", fname, "\n")
  p
}

invisible(map(TARGET_TICKERS, plot_step_annual))

# 6.8家合計圖
#
#    R&D加總邏輯(階梯，考慮財年重疊)：
#    Step A: 收集所有8家公司"財報換期"的時間點(每筆R&D的period_start)，
#            這些時間點構成一條時間軸上的"事件點"集合。
#    Step B: 在每個事件點上，對每家公司分別判斷："此刻"(該時間點)
#            該公司「最新生效」的R&D是哪一筆——也就是
#            period_start <= 該時間點 且 period_end >= 該時間點
#            的那一筆(若有多筆重疊取period_start最近的)。
#    Step C: 把8家"此刻生效值"加總，得到該時間點的合計R&D。
#    Step D: 把這些事件點依時間排序，相鄰兩點之間用水平線連接
#            (階梯狀)，換到下一個事件點時垂直跳到新的加總值。
#
#    論文數加總邏輯(不用階梯)：
#    直接把8家公司「同一個自然年」的n_papers加總，畫成一般折線
#    (用直線連接每年的加總值，不是階梯)。

cat("\n===== 產生8家合計圖 =====\n")

# --- 6-A. R&D 合計：階梯邏輯(逐事件點重新計算8家當下生效值並加總) ---

compute_aggregate_rd_step <- function(rd_df) {
  # rd_df: 含 ticker, period_start, period_end, rd_expense_usd 的長表
  # 收集所有公司的period_start當作"事件點"，並加上DATE_MAX作為終點
  event_points <- sort(unique(c(rd_df$period_start, DATE_MAX + days(1))))
  
  # 對每個事件點，計算8家公司"此刻"各自最新生效的R&D並加總
  agg_values <- map_dbl(event_points, function(t) {
    # 對每家公司，找出 period_start <= t 且 period_end >= t 的那一筆
    # 若同一時間點有多筆候選(理論上不該發生，但保險起見取period_start最近的)
    active <- rd_df %>%
      filter(period_start <= t, period_end >= t) %>%
      group_by(ticker) %>%
      slice_max(period_start, n = 1, with_ties = FALSE) %>%
      ungroup()
    sum(active$rd_expense_usd, na.rm = TRUE)
  })
  
  tibble(date = event_points, total_rd_usd = agg_values) %>%
    filter(total_rd_usd > 0)   # 排除8家都還沒生效前的0值區間
}

rd_agg <- compute_aggregate_rd_step(rd_annual_raw) %>%
  mutate(total_rd_billion = total_rd_usd / 1e9) %>%
  arrange(date)

cat("R&D合計 事件點(換期)數量:", nrow(rd_agg), "\n")
print(rd_agg %>% mutate(date = as.character(date)))

#  6-B. 論文數合計：自然年加總，一般直線(不用階梯) 

papers_agg <- papers_annual %>%
  group_by(year) %>%
  summarise(total_papers = sum(n_papers, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(paste0(year, "-01-01")))  # 標在年初,與個別圖規則一致(已修正)

cat("\n論文數合計(8家加總,按自然年):\n")
print(papers_agg)

# 6-C. 畫圖 

rd_max_agg    <- max(rd_agg$total_rd_billion, na.rm = TRUE)
paper_max_agg <- max(papers_agg$total_papers, na.rm = TRUE)
scale_factor_agg <- if (is.finite(paper_max_agg) && paper_max_agg > 0)
  rd_max_agg / paper_max_agg else 1

papers_agg <- papers_agg %>% mutate(value_scaled = total_papers * scale_factor_agg)

# 同個別圖邏輯：水平段(維持到下個事件點前)用實線，
# 垂直跳躍(換期瞬間的數值轉換)用虛線。
rd_agg_sorted <- rd_agg %>% arrange(date)

rd_agg_horizontal <- rd_agg_sorted %>%
  transmute(
    x    = date,
    xend = lead(date),
    y    = total_rd_billion,
    yend = total_rd_billion
  ) %>%
  filter(!is.na(xend))

rd_agg_vertical <- rd_agg_sorted %>%
  transmute(
    x    = lead(date),
    xend = lead(date),
    y    = total_rd_billion,
    yend = lead(total_rd_billion)
  ) %>%
  filter(!is.na(xend), !is.na(yend))

p_agg <- ggplot() +
  geom_segment(data = rd_agg_horizontal, aes(x = x, xend = xend, y = y, yend = yend),
               color = "steelblue", linewidth = 1.3, linetype = "solid") +
  geom_segment(data = rd_agg_vertical, aes(x = x, xend = xend, y = y, yend = yend),
               color = "steelblue", linewidth = 1.3, linetype = "dashed") +
  geom_line(data = papers_agg, aes(x = date, y = value_scaled),
            color = "darkred", linewidth = 1.1) +
  geom_point(data = papers_agg, aes(x = date, y = value_scaled),
             color = "darkred", size = 2) +
  scale_y_continuous(
    name = "8家R&D Expense總和 (Billion USD) [階梯狀,考慮財年重疊]",
    sec.axis = sec_axis(~ . / scale_factor_agg, name = "8家AI論文數總和 (篇) [自然年,點連直線]")
  ) +
  scale_x_date(breaks = seq(as.Date("2017-01-01"), as.Date("2025-01-01"), by = "1 year"),
               date_labels = "%Y",
               limits = c(DATE_MIN, DATE_MAX),
               expand = c(0, 0)) +
  labs(
    title = "8家公司合計：R&D Expense總和 vs AI論文數總和",
    subtitle = "藍實線=R&D生效期間(水平段) ; 藍虛線=財年換期銜接(跳躍段) ; 紅點線=8家論文數加總(自然年,右軸)",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y.left  = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "darkred"),
    panel.grid.minor = element_blank()
  )

fname_agg <- file.path(OUT_DIR, "fig_step_annual_AGGREGATE_8companies.png")
ggsave(fname_agg, p_agg, width = 11, height = 6, dpi = 150)
cat("\n完成:", fname_agg, "\n")

cat("\n============================================================\n")
cat(" 全部完成\n")
cat("  個別8張: fig_step_annual_<ticker>.png\n")
cat("  合計1張: fig_step_annual_AGGREGATE_8companies.png\n")
cat("============================================================\n")