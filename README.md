# ai-research-to-returns

Explores whether AI research output, measured through arXiv paper counts and citations, helps predict returns in AI-exposed U.S. technology stocks. The project uses monthly time-series econometric methods across three levels: a value-weighted mega-cap technology portfolio, a technology-sector benchmark, and subfield-to-firm pairwise analyses.


## Motivation
Since 2017, AI research output has grown dramatically, alongside a remarkable boom in technology stocks. This raises an important question: does academic research output actually influence tech stock prices, or are the two merely coincident trends? This project investigates that question rigorously through time-series econometric analysis, while carefully addressing common methodological pitfalls, including spurious correlation, citation truncation bias, and serial autocorrelation.

## Data
 
| Source | Description | Range |
|---|---|---|
| **arXiv metadata** | Monthly count of papers in AI categories (`cs.LG`, `cs.AI`, `cs.CL`, `cs.CV`, `cs.NE`, `stat.ML`) and their citation counts | 2017-01 – 2026-05 |
| **Yahoo Finance** | Daily adjusted close prices for 9 mega-cap US tech stocks (AAPL, MSFT, GOOGL, GOOG, META, NVDA, AMZN, TSLA, AVGO) | 2010 – 2026-02 |

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
| **L4 Subfield / Firm Pairs** | | |

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

## Files

**`clean_data.R`** — Data preparation. Streams the arXiv metadata snapshot, filters AI-related papers (cs.LG, cs.AI, cs.CL, cs.CV, etc.), and aggregates monthly paper counts and citation metrics for downstream analysis.

**`ai_full_analysis.R`** — Main analysis. Runs the three-level study using time-series methods (VAR, IRF, Granger causality) and panel regressions to estimate how AI research activity leads or correlates with market and sector returns.
