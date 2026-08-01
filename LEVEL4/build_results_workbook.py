"""
把 model_tables/ 的 9 張 CSV 組成一份多分頁 Excel 活頁簿。

來源:LEVEL4/export_model_tables.R 的輸出(該腳本重跑 firm_value_model.R
與 time_series_model.R 的迴歸並擷取係數)。
表格內容為統計estimation結果,非可重算的試算表模型,故以數值形式存放。

用法: python3 build_results_workbook.py
輸出: model_results.xlsx
"""

import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from openpyxl.comments import Comment

SRC = "model_tables"
OUT = "model_results.xlsx"

FONT = "Arial"
HDR_FILL = PatternFill("solid", fgColor="1F3864")
HDR_FONT = Font(name=FONT, bold=True, color="FFFFFF", size=10)
TITLE_FONT = Font(name=FONT, bold=True, size=13, color="1F3864")
NOTE_FONT = Font(name=FONT, italic=True, size=9, color="595959")
BODY_FONT = Font(name=FONT, size=10)
SIG_FILL = PatternFill("solid", fgColor="FFF2CC")      # 名目顯著
THIN = Side(style="thin", color="BFBFBF")
BORDER = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)

# (檔名, 分頁名, 標題, 附註)
SHEETS = [
    ("T1_panel_spec_ladder_wide.csv", "T1 Panel模型階梯",
     "表1  Panel 模型階梯(依變項:年度超額報酬)",
     "係數後 * p<0.10, ** p<0.05, *** p<0.01(wild cluster bootstrap, Webb weights, 999次)。"
     "樣本 6家公司 x 2019-2025 = 40 firm-years。M5 加入年度固定效果。"),
    ("T1_panel_spec_ladder.csv", "T1b Panel完整",
     "表1b  Panel 模型階梯(完整統計量)",
     "se 為 cluster-robust 標準誤;p_wildboot 為 wild cluster bootstrap p 值(僅6個cluster,以此為準)。"),
    ("T2_panel_breakpoint.csv", "T2 Panel斷點",
     "表2  Panel 2022 斷點模型",
     "M6 為主模型,用 share-based 論文數(對 OpenAlex 涵蓋率偏誤穩健)。"
     "M7 用原始計數,其交互項受量測誤差污染,不可直接解讀。M8 為分期估計。"),
    ("T3_multiple_testing_BH.csv", "T3 多重檢定",
     "表3  多重檢定校正(Benjamini-Hochberg FDR)",
     "共 22 個係數檢定。校正後最小 p 值為 0.176,無任何檢定通過 FDR 0.10。"),
    ("T4_unit_root_tests.csv", "T4 單根檢定",
     "表4  單根檢定(ADF + KPSS 雙向確認)",
     "ADF 之 H0 為「有單根」;KPSS 之 H0 為「定態」。KPSS p 值被截斷於 0.01/0.10,"
     "故 0.1 代表「>=0.1」。判定 I(1) 需 ADF 於水準不拒絕、於差分拒絕,且 KPSS 相反。"),
    ("T5_cointegration_tests.csv", "T5 共整合",
     "表5  共整合檢定(三法交叉驗證)",
     "Engle-Granger 殘差 ADF 不可用標準臨界值,k=3 時 EG 5% 臨界值約 -4.35。"
     "ARDL Bounds 為主要方法(PSS 2001 Table CI(iii))。三法結論之分歧由表6解決。"),
    ("T6_weak_exogeneity.csv", "T6 弱外生性",
     "表6  弱外生性檢定(化解表5的方法分歧)",
     "H0:該變數的調整係數 alpha = 0(不對長期均衡做修正)。"
     "股價與論文數皆為弱外生,代表 Johansen 找到的共整合關係存在於研發與專利之間,與股價無關。"),
    ("T7_short_run_ARDL.csv", "T7 短期ARDL",
     "表7  時間序列短期動態 ARDL(差分,月頻 T=104)",
     "Newey-West HAC 標準誤(lag=6)。模型含月份虛擬變數以控制投稿截止日季節性。"
     "dlp = 論文成長,dlpat = 專利成長,dlrd = 研發成長,dlmod = notable models 成長。"),
    ("T8_structural_break_tests.csv", "T8 結構斷點",
     "表8  結構斷點檢定(外生指定 vs 內生未知)",
     "Quandt-Andrews 與 Bai-Perron 讓資料自行尋找斷點,不預設 2022。"
     "supF 峰值落在 2022-06,方向與假說一致,但未達統計顯著。"),
    ("T9_by_firm_timeseries.csv", "T9 各公司時序",
     "表9  各公司個別時間序列 ARDL(累積 1-3 月效果)",
     "依變項為各公司月報酬;自變項為該公司自身論文成長(已季節調整),控制專利成長與落後報酬。"
     "6 家中僅 META 顯著;多重檢定下屬探索性結果。"),
]

INDEX_ROWS = [
    ("表1 / 1b", "Panel 模型階梯", "論文成長係數在 M1-M5 全為負且皆不顯著"),
    ("表2", "Panel 2022 斷點", "主模型交互項 p=0.250,無顯著結構斷點"),
    ("表3", "多重檢定 BH-FDR", "22 個檢定中 0 個通過 FDR 0.10"),
    ("表4", "單根檢定", "股價、論文、研發、models 皆為 I(1);專利判定不明確"),
    ("表5", "共整合檢定", "ARDL Bounds F=1.355 < 下界,無長期關係;Johansen 為唯一例外"),
    ("表6", "弱外生性", "股價 alpha=0 不可拒絕(p=0.058),共整合關係與股價無關"),
    ("表7", "短期 ARDL", "論文累積 1-3 月 +0.056 (p=0.096),僅邊際顯著"),
    ("表8", "結構斷點", "Chow p=0.312、supF p=0.388、Bai-Perron 選出 0 個斷點"),
    ("表9", "各公司時間序列", "僅 META 顯著 (+0.156, p=0.012),其餘 5 家為零"),
]


def style_sheet(ws, title, note, df):
    ws.sheet_view.showGridLines = False

    ws["A1"] = title
    ws["A1"].font = TITLE_FONT
    ws.row_dimensions[1].height = 20

    hdr_row = 3
    for j, col in enumerate(df.columns, start=1):
        c = ws.cell(row=hdr_row, column=j, value=str(col))
        c.font = HDR_FONT
        c.fill = HDR_FILL
        c.alignment = Alignment(horizontal="center", vertical="center", wrap_text=True)
        c.border = BORDER
    ws.row_dimensions[hdr_row].height = 28

    # 找出用來判斷顯著的欄位
    p_cols = [c for c in df.columns
              if str(c).lower() in ("p", "p_wildboot", "p_value", "p_raw")]

    for i, (_, row) in enumerate(df.iterrows(), start=hdr_row + 1):
        is_sig = False
        if p_cols:
            v = row[p_cols[0]]
            try:
                is_sig = pd.notna(v) and float(v) < 0.10
            except (TypeError, ValueError):
                is_sig = False
        for j, col in enumerate(df.columns, start=1):
            val = row[col]
            if pd.isna(val):
                val = ""
            c = ws.cell(row=i, column=j, value=val)
            c.font = BODY_FONT
            c.border = BORDER
            if isinstance(val, float):
                c.number_format = "0.0000" if abs(val) < 1 else "0.000"
                c.alignment = Alignment(horizontal="right")
            else:
                c.alignment = Alignment(horizontal="left", vertical="center")
            if is_sig:
                c.fill = SIG_FILL

    note_row = hdr_row + len(df) + 2
    ws.cell(row=note_row, column=1, value="註:" + note).font = NOTE_FONT
    ws.merge_cells(start_row=note_row, start_column=1,
                   end_row=note_row, end_column=max(4, len(df.columns)))
    ws.cell(row=note_row, column=1).alignment = Alignment(wrap_text=True, vertical="top")
    ws.row_dimensions[note_row].height = 46

    src = ws.cell(row=note_row + 1, column=1,
                  value="資料來源:LEVEL4/export_model_tables.R(重跑 firm_value_model.R "
                        "與 time_series_model.R 之迴歸)")
    src.font = NOTE_FONT

    for j, col in enumerate(df.columns, start=1):
        longest = max([len(str(col))] + [len(str(v)) for v in df[col].astype(str)])
        ws.column_dimensions[get_column_letter(j)].width = min(max(longest + 3, 11), 46)

    ws.freeze_panes = ws.cell(row=hdr_row + 1, column=1)


def main():
    wb = Workbook()

    # ---- 索引頁 ----
    ws = wb.active
    ws.title = "說明"
    ws.sheet_view.showGridLines = False
    ws["A1"] = "AI 論文發表與股票價值:模型結果總表"
    ws["A1"].font = Font(name=FONT, bold=True, size=15, color="1F3864")
    ws["A2"] = ("依變項:股票價值 | 自變項:AI 論文發表數(含 2022 斷點)| "
                "控制變項:研發經費、專利數、notable AI models")
    ws["A2"].font = Font(name=FONT, size=10, color="595959")

    ws["A4"] = "總結論"
    ws["A4"].font = Font(name=FONT, bold=True, size=11)
    ws["A5"] = ("論文發表數對股票價值,在 panel 與時間序列兩種架構下皆無穩健關係:"
                "panel 模型的論文係數全為負且經多重檢定校正後無一存活;"
                "時間序列的共整合檢定顯示股價不存在長期均衡關係(弱外生);"
                "短期僅有邊際顯著的正向效果 (+0.056, p=0.096);"
                "2022 結構斷點在外生指定與內生搜尋下皆未獲統計支持。")
    ws["A5"].font = Font(name=FONT, size=10)
    ws.merge_cells("A5:D5")
    ws["A5"].alignment = Alignment(wrap_text=True, vertical="top")
    ws.row_dimensions[5].height = 62

    hdr = 7
    for j, h in enumerate(["表次", "內容", "主要結果"], start=1):
        c = ws.cell(row=hdr, column=j, value=h)
        c.font = HDR_FONT
        c.fill = HDR_FILL
        c.alignment = Alignment(horizontal="center")
        c.border = BORDER
    for i, row in enumerate(INDEX_ROWS, start=hdr + 1):
        for j, v in enumerate(row, start=1):
            c = ws.cell(row=i, column=j, value=v)
            c.font = BODY_FONT
            c.border = BORDER
            c.alignment = Alignment(vertical="center")
    ws.column_dimensions["A"].width = 12
    ws.column_dimensions["B"].width = 24
    ws.column_dimensions["C"].width = 62
    ws.column_dimensions["D"].width = 20

    warn = ws.cell(row=hdr + len(INDEX_ROWS) + 2, column=1,
                   value="重要限制:panel 僅 6 家公司(cluster=6),推論以 wild cluster "
                         "bootstrap 為準;2022 後論文計數受 OpenAlex 涵蓋率偏誤影響,"
                         "斷點分析以 share-based 指標為主。")
    warn.font = Font(name=FONT, italic=True, size=9, color="C00000")
    ws.merge_cells(start_row=hdr + len(INDEX_ROWS) + 2, start_column=1,
                   end_row=hdr + len(INDEX_ROWS) + 2, end_column=3)
    warn.alignment = Alignment(wrap_text=True, vertical="top")
    ws.row_dimensions[hdr + len(INDEX_ROWS) + 2].height = 34
    ws["A1"].comment = Comment(
        "所有數值由 R 腳本估計產生,非本活頁簿計算,故不含公式。", "analysis")

    # ---- 各結果頁 ----
    for fname, sheet, title, note in SHEETS:
        if fname.endswith("_wide.csv"):
            # 版面用寬表:儲存格已含顯著性星號,全欄以文字讀入,
            # 避免部分欄被讀成數值、部分被讀成字串而對齊不一致。
            df = pd.read_csv(f"{SRC}/{fname}", dtype=str,
                             keep_default_na=False, na_values=[""])
            df = df.replace({"NA": "—"}).fillna("—")
        else:
            df = pd.read_csv(f"{SRC}/{fname}")
        ws = wb.create_sheet(sheet[:31])
        style_sheet(ws, title, note, df)

    wb.save(OUT)
    print(f"已輸出 {OUT},共 {len(wb.sheetnames)} 個分頁")
    print("分頁:", ", ".join(wb.sheetnames))


if __name__ == "__main__":
    main()
