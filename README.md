# ai-research-to-returns

Explores the relationship between AI research output (arXiv papers & citations) and stock market returns across three levels: macro (overall market), sector (tech), and subfield (LLM, CV, Robotics, etc.).

## Files

**`clean_data.R`** — Data preparation. Streams the arXiv metadata snapshot, filters AI-related papers (cs.LG, cs.AI, cs.CL, cs.CV, etc.), and aggregates monthly paper counts and citation metrics for downstream analysis.

**`ai_full_analysis.R`** — Main analysis. Runs the three-level study using time-series methods (VAR, IRF, Granger causality) and panel regressions to estimate how AI research activity leads or correlates with market and sector returns.
