# ai-research-to-returns

Explores whether AI research output, measured through arXiv paper counts and citations, helps predict returns in AI-exposed U.S. technology stocks. The project uses monthly time-series econometric methods across three levels: a value-weighted mega-cap technology portfolio, a technology-sector benchmark, and subfield-to-firm pairwise analyses.


## Motivation
Since 2017, AI research output has grown dramatically, alongside a remarkable boom in technology stocks. This raises an important question: does academic research output actually influence tech stock prices, or are the two merely coincident trends? This project investigates that question rigorously through time-series econometric analysis, while carefully addressing common methodological pitfalls, including spurious correlation, citation truncation bias, and serial autocorrelation.

## Data
 
| Source | Description | Range |
|---|---|---|
| **arXiv metadata** | Monthly count of papers in AI categories (`cs.LG`, `cs.AI`, `cs.CL`, `cs.CV`, `cs.NE`, `stat.ML`) and their citation counts | 2017-01 – 2026-05 |
| **Yahoo Finance** | Daily adjusted close prices for 9 mega-cap US tech stocks (AAPL, MSFT, GOOGL, GOOG, META, NVDA, AMZN, TSLA, AVGO) | 2010 – 2026-02 |
| **SEC EDGAR / annual reports** | Annual R&D expense for the 8 target firms, aligned to each firm’s actual fiscal reporting period | 2017 – 2025 |

#### AI papers by year
 
| Year | Papers | Citations |
|---|---|---|
| 2017 | 15,862 | 792,401 |
| 2019 | 35,040 | 869,116 |
| 2021 | 51,678 | 549,667 |
| 2023 | 67,488 | 347,380 |
| 2025 | 107,653 | 30,900 |

#### AI papers by subfield (2017–2026 total)
 
| Subfield | Total | Avg / month |
|---|---|---|
| Machine_Learning | 189,977 | 1,681 |
| Computer_Vision | 176,460 | 1,562 |
| LLM_NLP | 104,120 | 921 |
| General_AI | 45,361 | 401 |
| Robotics | 11,537 | 102 |
| Neural_Networks | 4,624 | 41 |

## Method

The project uses monthly time-series econometric analysis to test whether AI research output predicts returns in AI-exposed U.S. technology stocks. To reduce spurious correlation, the analysis uses log-differenced AI paper counts and month-end log stock returns, with HAC-robust standard errors and lagged specifications.

| Level | Question | Methods |
|---|---|---|
| **L1 Portfolio** | Does aggregate AI research output predict returns of a value-weighted mega-cap technology portfolio? | ADF, ARDL + Newey-West HAC, Granger causality, VAR/IRF |
| **L2 Tech Sector Benchmark** | Do the same AI signals appear in the technology-sector portfolio specification? | ARDL + HAC, Granger causality, VAR/IRF |
| **L3 Subfield / Firm Pairs** | Do specific AI subfields predict returns of matched technology firms? | Pairwise ARDL, Granger causality, Panel FE with total AI growth control, VAR/IRF |
| **L4 Own-company Research Effect** | Does a firm's own AI research output predict its own stock returns, and does the post-2021 paper decline reflect lower R&D investment? | Spearman correlation, company-level descriptive analysis, R&D-expense comparison, VAR / IRF |

#### Subfield-to-firm mapping (Level 3):
 
| Subfield | Mapped tickers | Rationale |
|---|---|---|
| LLM_NLP | MSFT, GOOGL, GOOG, META, AMZN | LLM / cloud / search |
| Computer_Vision | NVDA, TSLA | Autonomous driving, visual acceleration |
| Robotics | NVDA, AVGO, TSLA | Hardware / automation |
| Machine_Learning | All 9 tickers | General ML |
| General_AI | MSFT, GOOGL, META, NVDA, AMZN | Broad AI leaders |
| Neural_Networks | NVDA, AVGO, GOOGL | Deep learning / chips |

#### Key methodological choices
 
- **Log returns from month-end to month-end.** Avoids the bias of within-month returns that drops the cross-month component.
- **Newey-West HAC standard errors.** Monthly financial regression residuals routinely show serial autocorrelation and heteroskedasticity.
- **Citation cutoff at 24 months.** Avoids the truncation bias where recent papers haven't accumulated citations yet.
- **Static market-cap weights.** Without monthly share-outstanding data, value weights use approximate 2026 market-cap shares (adjustable in code).
- **Control for overall AI growth in Level 3.** Panel FE includes total AI paper growth as a control, so subfield-specific effects are isolated from the general AI hype. 
- **Trend analysis in Level 4**, which compares the evolution of AI publication output and stock prices over time.
- **Spearman rank correlation in Level 4**, which measures the relationship between paper counts, citations, stock returns, and stock prices.
- **Company-level impulse-response analysis (VAR/IRF) in Level 4**, which traces the estimated dynamic response of a firm's monthly log returns to shocks in its own AI subfield publication growth.
- **Fiscal-period R&D alignment.** R&D expense is plotted over each firm’s actual fiscal reporting period rather than being forced into calendar years. This avoids incorrectly summing firms with different fiscal year-end dates.

## Results

#### Level 0: a trend that looks suspicious
<img width="1500" height="750" alt="fig_L1_trend" src="https://github.com/user-attachments/assets/32fd7f76-a495-4423-bdd4-9546ebf5cedf" />

At first glance the relationship looks compelling — AI papers (blue bars) and cumulative value-weighted market return (red line) both climb steadily through the decade. A naive correlation in levels would yield a strong positive coefficient. But two time-series that both trend upward will always look correlated, regardless of any actual causal link. This is the classic spurious regression problem (Granger & Newbold, 1974). The right test is whether changes in one predict changes in the other.
 
#### Level 1 & 2 — Aggregate effects are insignificant
<img width="1200" height="900" alt="fig_L1_scatter" src="https://github.com/user-attachments/assets/df54bae6-b8e1-4bd4-aae3-c54207d147c3" />

Once we move from levels to first-differences (i.e., monthly growth rates), the relationship collapses. The fitted line is essentially flat, the slope is statistically indistinguishable from zero, and the confidence band easily covers a horizontal line. The visual co-movement in the previous figure was almost entirely a shared time trend — months with faster AI paper growth do not see higher market returns.
 
ARDL regressions with Newey-West HAC errors confirm the picture: across both the value-weighted market and the tech sector, no contemporaneous or lagged coefficient on AI paper growth reaches conventional significance, and R² stays below 0.08.

<img width="1200" height="600" alt="fig_L1L2_irf" src="https://github.com/user-attachments/assets/053f1246-829a-4d67-8a38-e613312a1285" />

The VAR-based impulse response function shows the only mild signal in the macro analysis: a positive bump at **month 2** (~+1.8%) in both market and tech-sector returns following a one-SD shock to AI paper growth. The 95% bootstrap CI barely excludes zero at the peak and the response decays back to noise within 3–4 months. So *if* there is an effect, it is delayed by about two months and short-lived — broadly consistent with an efficient market that has already priced in research activity through earlier channels (products, earnings, news).

#### Level 3 — Semiconductor-related firms show the strongest exploratory signals
<img width="1200" height="600" alt="fig_L3_heatmap" src="https://github.com/user-attachments/assets/6ddbe5bc-5991-4385-b405-6df437d1d181" />

Of 27 subfield-by-firm pairs, only two reach the conventional `p < 0.10` threshold (marked with `*`), and both involve semiconductor stocks:
 
| Pair | Cumulative 4-month effect | p-value | Interpretation |
|---|---|---|---|
| **Machine_Learning × AVGO** | +0.404 | 0.091 | Broadcom benefits from general ML research heat |
| **General_AI × NVDA** | +0.193 | 0.068 | Nvidia benefits from broad AI research |
| Computer_Vision × NVDA | — | Granger p = 0.012 | CV research leads Nvidia returns |
| Robotics × AVGO | — | Granger p = 0.028 | Robotics research leads Broadcom returns |

The economic story is intuitive: **research activity translates into compute demand first**, so semiconductor suppliers (NVDA, AVGO) capture the effect before software / platform firms. The heatmap row for Machine_Learning is uniformly green-tinted across most tickers, suggesting a broad but mostly noisy positive tilt, while LLM_NLP — the subfield with the most public hype — shows uniformly small, insignificant effects on MSFT, GOOGL, META, etc. Computer_Vision is essentially flat for NVDA in the contemporaneous regression but leads it in the Granger sense, suggesting the effect operates through a lag rather than instantaneously.

#### Level 4 — Firm-level research output and stock performance
<img width="1200" alt="annual_papers_trend" src="https://github.com/user-attachments/assets/9f1c60da-e82c-4b54-95c4-cfbc68298d0d" />
The annual publication trend reveals a major shift after 2021.

Across most large technology firms, AI publication output increased rapidly between 2017 and 2020 before reaching a peak around 2020–2021. Alphabet, Microsoft, and Meta account for the majority of visible AI research output during this period.
However, publication counts decline sharply beginning in 2022 despite the continued expansion of the AI industry and the emergence of generative AI. This does not necessarily imply that these firms reduced their AI R&D activity. A more plausible interpretation is that leading AI companies became less willing to disclose frontier AI work through public arXiv papers.

This interpretation is supported by Movva et al. (NAACL 2024), who analyzed more than 16,000 LLM-related arXiv papers from 2018 to 2023 and found that industry accounted for a smaller share of LLM publications in 2023, largely because Google and other Big Tech companies published less. In their institution-level analysis, Google, Microsoft, Amazon, and Meta were the four institutions with the largest decreases in LLM publication share in 2023.

| Company | Pre-2023 LLM arXiv paper share | 2023 share | Change |
|---|---:|---:|---:|
| Google | 6.7% | 3.8% | -2.9 pp |
| Microsoft | 6.8% | 5.4% | -1.4 pp |
| Amazon | 3.0% | 1.9% | -1.1 pp |
| Meta | 2.9% | 1.9% | -1.0 pp |
| Four-company total | 19.3% | 13.0% | -6.3 pp |

Movva et al. suggest that this decline may reflect a deprioritization of basic research or heightened secrecy due to competition. Therefore, the post-2021 decline in visible AI papers should be interpreted not as a collapse in AI innovation, but as a shift in how major AI firms disclose their research.

<img width="1200" alt="rd_expense_vs_ai_papers" src="https://github.com/user-attachments/assets/96ed6007-42c7-4ac7-a704-897926de559c" />

The R&D expense comparison supports the interpretation mentioned above. While AI paper counts of the eight firm peak around 2020 and decline sharply after 2021, the combined R&D expense of the eight firms continues to rise throughout the same period. This suggests that the fall in arXiv AI publications should not be interpreted as a decline in actual research investment. Instead, the divergence is more consistent with a disclosure shift: major technology firms continued to spend heavily on R&D, but a smaller share of that research appeared as public arXiv papers after 2021. In other words, public AI paper counts became a weaker observable measure of these firms’ underlying AI research activity.

<img width="1200" alt="annual_heatmap" src="https://github.com/user-attachments/assets/adefbf30-6a7c-4fdf-9cab-fe07858c61eb" />

The annual Spearman correlation heatmap shows substantial heterogeneity across firms. Microsoft and Apple display moderately positive correlations between AI publication activity and stock returns, whereas Alphabet exhibits consistently negative correlations. Nvidia, Meta, Amazon, Tesla, and Broadcom show mixed relationships. The absence of a consistent sign across firms suggests that AI publication output is not universally associated with market performance. Any relationship appears to be highly firm-specific.

<img width="1200" alt="Microsoft_full_analysis" src="https://github.com/user-attachments/assets/2001a911-8ed2-468d-a6e0-2e0f5944c0f4" />

Microsoft provides a clear case of how the relationship between visible AI research output and market valuation changed after 2021. From 2017 to 2021, Microsoft's AI publication counts and stock price moved in broadly similar directions. As Microsoft's visible AI research output increased, its stock price also rose. In this earlier period, public AI research activity and market valuation appeared to move together. After 2021, however, the relationship breaks down. Microsoft's arXiv AI paper count declines sharply beginning in 2022, while its stock price reaches new highs by 2024–2025.

This divergence should not be interpreted as evidence that Microsoft reduced its actual AI research activity. Rather, it is more consistent with a shift in disclosure behavior: leading AI firms may have become less willing to publish frontier AI work openly on arXiv as competition intensified, which is consistent with previous assumption.

<img width="1200" alt="irf_by_company_own_papers" src="https://github.com/user-attachments/assets/c398c645-2ece-4608-9afc-8d95fab80552" />

The VAR-based impulse response function shows that even when the analysis is moved to the firm level, the signal remains short-lived.  Across most firms, the largest estimated response of monthly log returns to a shock in the firm’s own AI paper growth reaches its largest magnitude within the first one to two monthly horizons, roughly before the 2.5-month mark on the plot. This is visible for Microsoft, Meta, Nvidia, Broadcom, Google, and Amazon, where the impulse response either peaks or reaches its largest movement in the horizon.

This finding is consistent with the earlier Levels 1–3 results. Research output may contain some short-run information, but equity markets appear to incorporate it quickly.

## Files

**`clean_data.R`** — Data preparation. Streams the arXiv metadata snapshot, filters AI-related papers (cs.LG, cs.AI, cs.CL, cs.CV, etc.), and aggregates monthly paper counts and citation metrics for downstream analysis.

**`ai_full_analysis.R`** — Main analysis. Runs the three-level study using time-series methods (VAR, IRF, Granger causality) and panel regressions to estimate how AI research activity leads or correlates with market and sector returns.

**`LEVEL4/ai_8_company_analysis.R`** — Extended 8-company analysis. Runs firm-level and subfield-level tests for GOOGL, MSFT, META, NVDA, AMZN, AAPL, TSLA, and AVGO, including Spearman correlations, company case studies, VAR / IRF, and cumulative 1–12 month effects.

**`LEVEL4/ai_monthly.csv`** — Monthly aggregate AI research data. Contains total AI paper counts and citation metrics used for portfolio-level and market-level analysis.

**`LEVEL4/ai_monthly_subfield.csv`** — Monthly AI subfield data. Aggregates AI papers by subfield, including Machine Learning, Computer Vision, LLM/NLP, General AI, Robotics, and Neural Networks.

**`LEVEL4/paper_company_panel.rds`** — Company-affiliated paper panel. Links AI papers to corporate institutions and is used to build firm-level and firm-subfield paper counts.

**`LEVEL4/stock_raw.rds`** — Raw stock price data. Contains daily adjusted stock prices used to compute month-end prices and monthly log returns.

**`LEVEL4/panel_annual.rds`** — Annual firm-level panel. Combines company AI paper counts, citations, stock prices, returns, and excess returns for annual trend and correlation analysis.

**`LEVEL4/panel_monthly.csv`** — Monthly firm-level panel. Provides company-month paper and market variables for firm-level time-series analysis.

**`LEVEL4/plot_rd_expense_and_papers.R`** — R&D expense and paper counts of the eight firms comparison plot. Aligns annual R&D expense to actual fiscal reporting periods and compares it with firm-level AI paper counts.

**`LEVEL4/rd_expense_annual.csv`** — Annual R&D expense data. Contains SEC-derived R&D expense records for the 8 target firms.

**`LEVEL4/company_annual.rds`** — Annual company paper counts. Provides firm-year AI paper totals used in the R&D comparison plot.
