# 驗證方法檢驗報告

> 檢驗對象:`ai_full_analysis.R`(Level 1–3)與 `LEVEL4/ai_8_company_analysis.R`(Level 4)。
> 附帶產出:`LEVEL4/rd_disclosure_analysis.R` — 用 R&D、專利、notable AI models 三角驗證「2022 後藏私」假說,所有數字均以 repo 內的資料實際執行得出(輸出於 `LEVEL4/rd_disclosure_outputs/`)。

---

## 一、總評

整體研究設計的骨架是正確的:用一階差分處理共同趨勢(避免 Granger & Newbold 1974 的偽迴歸)、Newey–West HAC 標準誤、citation 24 個月截斷、雙向 Granger 檢定、Panel FE 控制整體 AI 熱度,這些選擇都站得住腳,README 也誠實地把 Level 3 的結果定位為 exploratory。

但有兩個問題會直接影響結論的可信度,必須優先處理:

1. **Level 4 的「2022 年論文驟降」大部分是資料涵蓋率的 artifact,不能直接當成藏私的證據**(詳見第三節 P1)。
2. **Level 4 的「best horizon」選擇性報告是教科書等級的 data snooping**,該部分的 p 值不可解讀(詳見第三節 P2)。

其餘問題(季節性、靜態權重、多重檢定等)屬於可修正的技術缺陷,修正後 Level 1–3 的主結論(整體無顯著預測力、半導體股有微弱訊號)大概率不變,但穩健性會大幅提升。

---

## 二、做得對的地方

| 設計 | 評價 |
|---|---|
| levels → first differences(`d_log_papers`) | 正確,這是對付共同趨勢偽相關的標準第一步 |
| Newey–West HAC 標準誤 | 正確,月頻金融資料的標配 |
| Citation 24 個月截斷(`CITATION_CUTOFF_MONTHS`) | 正確意識到 truncation bias,少見的細心處理 |
| Granger 雙向檢定(AI→報酬、報酬→AI) | 正確,有考慮反向因果 |
| Panel FE + `d_log_total` 控制整體 AI 熱度 | 正確,子領域效果與整體熱度分離的想法很好 |
| R&D 以財報實際期間畫階梯圖(`plot_rd_expense_and_papers.R`) | 正確且罕見地嚴謹,MSFT(6月)/NVDA(1月)財年錯位問題處理得很好 |
| 月底對月底 log return | 正確 |

---

## 三、主要問題(依嚴重程度排序)

### P1|「2022 論文驟降」主要是 OpenAlex 涵蓋率 artifact,不是(只有)藏私

這是對你們第二個問題(藏私假說)最重要的發現。用 `paper_company_panel.rds` 對照 `ai_monthly.csv` 實際計算:

| 年 | 全部 AI 論文 | **全部公司**(2,562 家)affiliated 論文 | 佔比 | 八大公司論文 | 八大 / 全部公司 |
|---|---:|---:|---:|---:|---:|
| 2019 | 35,040 | 3,347 | 9.6% | 1,540 | 46.0% |
| 2020 | 46,656 | 4,609 | **9.9%** | 2,075 | **45.0%** |
| 2021 | 51,678 | 3,835 | 7.4% | 1,594 | 41.6% |
| 2022 | 54,155 | 1,505 | **2.8%** | 322 | **21.4%** |
| 2023 | 67,488 | 1,514 | 2.2% | 276 | 18.2% |
| 2025 | 107,653 | 1,568 | 1.5% | 205 | 13.1% |

兩個關鍵事實:

- **全部 2,562 家公司**(含新創、中國廠商等,不只八大)的 affiliated 論文在 2022 同一年從 ~4,600 篇崩到 ~1,500 篇,而同期 arXiv AI 論文總量成長一倍。全世界所有公司同時開始藏私是不可能的——這是 OpenAlex 對近年 arXiv preprint 的機構標註涵蓋率下降(近年論文多數尚未出版、缺 publisher metadata,機構匹配不完整,愈近的年份愈嚴重,性質上和 citation truncation 是同一類偏誤)。
- 外部驗證也支持 undercount:panel 中 Alphabet 2022 只有 101 篇、Meta 只有 23 篇,但 Meta 在 2022–2024 是出名的 open research 路線(LLaMA 系列),Google Research 官網 2022–2023 每年列出數千篇論文。Movva et al. (2024) 量到的 Google 佔比下降是 6.7%→3.8%(約 -43%),不是 -85%。

**但藏私訊號並非不存在**:八大公司在「全公司論文池」中的占比從 45% 掉到 13%。這個 within-pool share 是 artifact-robust 的指標(涵蓋率下降同時打擊分子與分母,比率大致抵銷),它顯示八大確實比其他公司退得更多——方向與 Movva et al. 一致,只是幅度遠小於絕對數字看起來的 85–90%。

**結論:README Level 4 目前把絕對論文數的崩跌直接解讀為 disclosure shift,是把「量測問題」與「行為改變」混在一起。正確的說法是:驟降 = 大量 artifact + 中等程度的真實揭露轉變。** 建議 README 的 Level 4 敘事與 Microsoft 個案圖都加上這個 caveat,推論一律改用 share-based 指標。

### P2|Level 4「best cumulative horizon」= data snooping,p 值不可用

`ai_8_company_analysis.R` 第 9-4 節對每個公司 × 子領域配對,從 1–12 個月的累積效果中**挑出最大正值**畫熱力圖(`best_positive_by_pair`、`slice_max(order_by = cum_effect)`),並標註原始 p 值。這在統計上不成立:

- 對 12 個高度相關的雜訊估計取最大值,期望值必然為正——**純白噪音也會畫出一片綠色的熱力圖**。
- 挑最大值之後,單一 horizon 的 HAC p 值完全沒有考慮 selection,標 `*` 會嚴重誤導。

修正方式(擇一或並用):
1. **事前指定 horizon**(例如用 L1–L3 已發現的 2 個月峰值),只報告該 horizon;
2. 報告**全部 12 個 horizon**(9-7 節那張圖是對的,保留它,刪掉 best-positive 圖)並做 FDR 校正;
3. 若堅持報告 max,需用 **placebo / block-bootstrap** 建立「max over 12 horizons」的 empirical null,重新計算 p 值。

同理,9-5 節「絕對值最大」的表也一樣有選擇偏誤。

### P3|論文序列有強季節性,VAR 的「shock」有近半是可預期的行事曆效果

實際計算:`d_log(ai_papers)` 的變異中 **45% 可由 month-of-year 固定效果解釋**(2 月/3 月/10 月為正尖峰、1 月/7 月為負——即 ICML、CVPR、NeurIPS 等投稿截止日)。IRF 把這些**事前完全可預期**的行事曆波動當成「創新衝擊」,違反 VAR shock 應為 news 的假設,會稀釋甚至扭曲估計。

修正:對 `d_log_papers` 先做季節調整(迴歸殘差法:對 11 個月份 dummy 取殘差,或 STL),或改用 YoY(12 期差分),再進 ARDL/VAR。公司層級序列同理。

### P4|靜態 2026 市值權重 → look-ahead bias

`mcap_weight` 用近似 2026 年的市值占比套用到 2017–2025 全樣本。NVDA 權重 0.18 是 AI 行情**發生之後**的結果,等於在檢驗「AI 研究能否預測報酬」時,先用未來資訊加重了事後贏家。這會系統性誇大 AI 相關的相關性。

修正:用**逐月落後市值**(月初市值 = 上月底股數×價格;股數可從 Yahoo/Compustat 取季頻線性內插),或至少改用等權重為主結果、或直接用現成指數(XLK、QQQ)當 sector benchmark。README 已註明此限制,但它應該升級為主要 caveat,因為偏誤方向恰好偏向「找到效果」。

### P5|「累積效應」含當期項,不能解讀為預測力

`ts_analysis()` 與 `analyze_pair()` 的 `cum = x + x_l1 + x_l2 + x_l3` 把**當月** x 也加進去。當月論文數與當月報酬同時決定(且月底才知道整月論文數),這一項既不可交易、也含反向因果(行情好→公司多發論文?)。要主張 predictability,累積效應應只加 `x_l1...x_l3`;要主張 contemporaneous association,則另外單獨報告當期係數。兩種口徑分開,結論才乾淨。

### P6|多重檢定沒有校正

Level 3 有 27+ 個配對 × 多個統計量。27 個檢定在 α=0.10 下,**純雜訊也預期 2.7 個顯著**——而實際只找到 2 個(AVGO×ML、NVDA×General_AI)。這其實是「與全域虛無假設一致」的結果,README 的敘事(半導體先受惠)超過了證據強度。建議:對整張 `L3_table` 做 Benjamini–Hochberg FDR,並在 README 明確報告「顯著個數 vs 偶然期望個數」。Granger p 值同理。

### P7|年度 Spearman:n≤9、levels-on-levels、且對稀疏公司無意義

`compute_cor_annual()` 的問題:
- 每家公司只有最多 9 個年度觀測值(`filter(n() >= 3)` 甚至允許 3 個),n=9 時 Spearman ρ 的 95% 信賴區間寬達 ±0.7,熱力圖上的顏色差異基本是雜訊,至少要標 p 值或直接刪掉。
- `papers_vs_price`、`cite_vs_price` 是**水準對水準**:price_end 有趨勢、papers 先升後降,這正是專案在 Level 0 自己批評過的偽相關,卻在 Level 4 重新犯了。建議只保留 vs return / vs excess_return 欄。
- AVGO(總論文 3 篇)、TSLA(1 篇)的相關係數無意義,應直接排除而非畫進熱力圖。

### P8|稀疏公司月序列上跑 log(x+1) 差分與 VAR

公司×月(尤其公司×子領域×月)論文數大量為 0 或個位數,`d_log(x+1)` 在 0↔1 之間跳動,產生人工的大幅「成長率」。程式只在 `sd==0` 時跳過,稀疏但非零的序列照跑 VAR,估計不可靠。建議:設 active_months 門檻(如 ≥60% 月份非零才進月頻分析)、其餘公司改年頻;計數資料的正規做法是 **Poisson/negative binomial(PPML)**,不是 log(x+1) OLS。

### P9|其他技術性問題(影響較小,但值得修)

| 問題 | 位置 | 建議 |
|---|---|---|
| ADF 檢定印出後不影響後續流程;僅用 ADF 也易誤判 | `adf_report()` | 補 KPSS 做交叉驗證;若不定態應改 spec,而非只印警告 |
| 含 `y_l1` 的迴歸再套 HAC:若殘差仍自相關,OLS 對 lagged DV 是不一致的 | `ts_analysis()` | 先做 Breusch–Godfrey 檢定殘差;乾淨就不需要 NW,不乾淨則 NW 也救不了,需加 lag 階數 |
| NW lag 固定 = 3 | 同上 | 用 `floor(0.75*T^(1/3))` 或 `bwNeweyWest()` 自動選 |
| VAR Cholesky ordering `c("y","x")` 未說明;此排序令 x 對 y 的當期效果為 0(h=0 必為 0) | `var_irf()` | 這其實是保守且合理的識別,但要在文中說明,並報告反向排序的穩健性 |
| IRF bootstrap `runs = 200` 偏少 | 同上 | ≥1000 |
| `VARselect` 用 AIC,短樣本易選過長 lag | 同上 | 短樣本建議 BIC/HQ,或報告對 p 的敏感度 |
| 月頻公司迴歸用 raw log return,無市場因子控制 | L3、L4 | `panel_monthly.csv` 裡已經有 `excess_return`/`sp500_return`,直接改用超額報酬,或把市場報酬加入右邊;現在的估計混合了 beta 暴露與 alpha |
| GOOG/GOOGL 在 L1 各給 0.075 權重,等於 Alphabet 被放入兩次高度相關的序列 | `ai_full_analysis.R` | 對市值加權組合影響不大(權重拆半),但等權重 `mkt_ew` 中 Alphabet 實質權重加倍,建議 L1 也只留一個股別 |
| AMZN 的「R&D」是 *Technology and Content*(含 AWS 基礎設施成本),與其他 7 家不可比 | `rd_expense_annual.csv` | 圖表加註;迴歸提供排除 AMZN 的版本(新腳本已做) |

---

## 四、建議的更好驗證方法

按投入產出比排序:

1. **Share-based 指標取代絕對論文數**(成本最低、收益最大)。八大公司論文數 ÷ 全公司 affiliated 論文數(或 ÷ arXiv 總量)——涵蓋率 artifact 在分子分母大致抵銷。Level 4 所有結論改用 share 後重新檢視。
2. **事件研究(event study)**。與其用月頻 VAR 找平均效果,不如對「重大論文/模型發布」(`all_ai_models.csv` 有精確發布日)做日頻 CAR(±5 日、市場模型調整)。識別乾淨、樣本天然對齊,而且直接回答「市場是否對研究揭露定價」。這是本研究最值得加的一塊。
3. **Placebo / block-bootstrap 推論**。把論文序列做 moving-block bootstrap 或隨機平移,重跑整條 pipeline(含「挑最大」的步驟),得到 empirical null;現有顯著結果若在 placebo 分布的 95 分位之外才算數。這一次性解決 P2 與 P6。
4. **季節調整**(P3):month dummies 殘差或 YoY 成長率。
5. **PPML(Poisson pseudo-ML)處理計數**(P8):`glm(n_papers ~ ..., family = quasipoisson)`,天然處理 0,不需要 log(x+1)。
6. **超額報酬 + 落後市值權重**(P4、P9):資料都已在手上。
7. 推論層面:8 家公司做 cluster SE 只有 8 個 cluster,建議 wild cluster bootstrap(`fwildclusterboot`)。

---

## 五、藏私假說的資料檢驗(新分析:`LEVEL4/rd_disclosure_analysis.R`)

邏輯:**論文數 = 研究活動 × 揭露意願 × 資料涵蓋率**。單看論文數下降無法區分三者,因此用四條獨立管道三角驗證,並先做涵蓋率診斷(Part 0,即上文 P1 的表)。

實際執行結果(全部可由腳本重現):

**(1) R&D 投入(SEC EDGAR)— 持續上升。** 八家合計 R&D 由 2020 年約 1,090 億美元升至 2025 年約 2,200 億(指數 100→202)。

**(2) 發表密度(papers per $1B R&D)— 崩跌 90%。** Quasi-Poisson(offset = log R&D、firm FE、cluster SE):post-2022 係數 = −2.31(p < 10⁻¹²),即每一元 R&D 的可觀測論文產出下降 90.1%(排除 AMZN 後 90.3%,穩健)。Chow 型 Wald 檢定 2022 斷點:F = 26.8,p < 10⁻⁷。個別公司 post/pre 比率:META 0.02、GOOGL 0.09、NVDA 0.16、MSFT 0.19。**但注意:這 90% 是「量測到的」下降,依 P1,其中大部分是 OpenAlex 涵蓋率,不能全數解讀為藏私。**

**(3) 專利申請(Google Patents,filing year ≤2023 避開 18 個月公開延遲截斷)— 大致平穩。** 2022 年八家合計申請數與 2020 年相當(指數 ~92),沒有任何與論文同步的崩跌;NVDA、TSLA 的申請數甚至在升。若研究活動真的萎縮,專利應同步下滑;若從「發論文」轉向「trade secret」,專利也會降——兩者都沒發生。注意此檔為八家**全部**專利而非 AI 專利,建議後續在 BigQuery 用 CPC 分類(G06N、G06V 等)重抓 AI 子集。

**(4) Notable AI models(Epoch AI)— 不減反增。** 八家合計 2020=100 → 2024=325。Alphabet 38→62(2022→2024)、MSFT 14→36、NVDA 6→45(2025)。**這是藏私假說最有力的一條證據:前沿產出照常(甚至加速)發布,只是不再以 arXiv 論文形式完整揭露。**

**綜合判讀:**

| 管道 | 2022 後走勢 | 與「研究停止」相容? | 與「藏私」相容? |
|---|---|---|---|
| R&D 費用 | ↑(+30–100%) | 否 | 是 |
| 專利申請 | ≈ 平穩 | 否 | 是(未轉向純 trade secret) |
| Notable models | ↑↑(3 倍) | 否 | 是 |
| 可觀測 arXiv 論文 | ↓↓(−85~90%) | — | 部分(需扣除 artifact) |

四條線的組合排除了「AI 公司研究活動萎縮」的解釋;支持「公司持續高強度研發、但公開論文揭露減少」——**方向上支持你們的藏私假說**。但量化幅度上,−85~90% 的觀測降幅中有一大塊是 OpenAlex 涵蓋率 artifact,artifact-robust 的 share 指標(45%→13%)才是可引用的藏私證據強度。寫進論文時建議引 Movva et al. (2024) 的 share 數字互相印證,並明確區分這兩層。

---

## 六、可執行的修正清單

- [ ] README Level 4:把絕對論文數驟降的敘事改為「artifact + disclosure shift」雙因,引用 Part 0 涵蓋率表
- [ ] 刪除(或以 placebo p 值重做)`best_positive` / `best_abs` 熱力圖;保留 1–12M 全 horizon 圖
- [ ] `d_log_papers` 系列季節調整後重跑 ARDL / VAR / IRF
- [ ] L1/L2 權重改為落後市值或等權重主結果;或改用 XLK/QQQ
- [ ] 累積效應改為僅含 lag 項的版本(另報當期係數)
- [ ] `L3_table` 加 BH-FDR 欄;README 報告顯著個數 vs 偶然期望
- [ ] 年度 Spearman:刪 vs price 欄、排除 AVGO/TSLA、加 p 值
- [ ] 月頻公司迴歸改用 `excess_return`
- [ ] IRF bootstrap runs 提高到 1000;報告 Cholesky ordering 穩健性
- [ ] (加分)對 `all_ai_models.csv` 的發布日做日頻事件研究
- [ ] (加分)在 BigQuery 用 CPC 重抓 AI 專利子集,重跑 Part 3
