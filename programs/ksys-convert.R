# 5カラム目：シート名　aliasnameから変換
# registration -> JPLSG登録
# nonparticipation -> 臨床研究不参加報告

# prtpath <- ""
rawdatapath <- paste0(prtpath, "/KSYSCONVTEST")
outputpath <- paste0(prtpath, "/KSYSCONVOUTPUT")
# rawdataフォルダ内のファイル名を取得
file_list <- list.files(rawdatapath)

for (i in 1:length(file_list)){
  # Shift-JISで列もデータとして取り込む
  res <- try(input_csv <- read.csv(paste(rawdatapath, file_list[i], sep="/"), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F), silent=T)
  # 空ファイルは処理スキップ
  if(class(res) != "try-error") {
    # JPLSG登録、不参加共通
    # 入力ファイルに1行目と同じ内容で列名をセット
    colnames(input_csv) <- input_csv[1, ]
    input_csv <- input_csv[-1, ]
    # "バージョン" 1	カラム目
    # "1"固定の行を挿入
    output_csv <- data.frame("バージョン" = rep("1", nrow(input_csv)))
    # pJ作成日  	2	カラム目
    output_csv$作成日 <- input_csv$作成日
    # pJ最終更新日  	3	カラム目
    output_csv$最終更新日 <- input_csv$最終更新日
    # pJ試験名  	4	カラム目
    # NAをセット
    output_csv$試験名 <- NA
    # pJシート名  	5	カラム目
    # rawdataのシート名英数字別名から変換して格納
    if (input_csv[1,"シート名英数字別名"] == "registration") {
      output_csv$シート名 <- "JPLSG登録"
      # JPLSG登録のみ
      # pJシート作成時施設名  	6	カラム目
      output_csv$シート作成時施設名 <- NA
      # pJ現施設名  	7	カラム目
      output_csv$現施設名 <- NA
      # pJシート作成時診療科名  	8	カラム目
      output_csv$シート作成時診療科名 <- NA
      # pJシート作成時団体別施設CD  	9	カラム目
      output_csv$シート作成時団体別施設コード <- input_csv$シート作成時団体別施設コード
      # pJ現担当医  	10	カラム目
      # DR_NAMEとして格納するがデータがないのでシート作成時担当医をセット
      output_csv$現担当医 <- input_csv$シート作成時担当医
      # pJ登録CD  	11	カラム目
      output_csv$登録コード <- input_csv$登録コード
      # pJ症例登録番号  	12	カラム目
      output_csv$症例登録番号 <- NA
      # pJ登録診断名  	13	カラム目　NUM
      output_csv$登録診断名 <- input_csv$field3
      # 14-15カラム目は空値
      output_csv$col14 <- NA
      output_csv$col15 <- NA
      # pJJPLSG同意取得日  	16	カラム目
      output_csv$JPLSG同意取得日 <- input_csv$field12
      # pJ中央診断提出同意  	17	カラム目　NUM
      output_csv$中央診断提出同意 <- input_csv$field7
      # 18-19カラム目は空値
      output_csv$col18 <- NA
      output_csv$col19 <- NA
      # pJ中央診断提出同意取得日  	20	カラム目
      output_csv$中央診断提出同意取得日 <- input_csv$field13
      # pJ余剰検体提供同意  	21	カラム目
      output_csv$余剰検体提供同意 <- NA
      # 22カラム目は空値
      output_csv$col22 <- NA
      # pJ参加予定のJPLSG臨床研究有無  	23	カラム目　NUM
      output_csv$JPLSG臨床研究有無 <- input_csv$field6
      # 24カラム目は空値
      output_csv$col24 <- NA
      # pJ参加予定のJPLSG臨床研究  	25	カラム目　NUM
      output_csv$参加予定のJPLSG臨床研究 <- input_csv$field4
      # 26カラム目は空値
      output_csv$col26 <- NA
      # pJJPLSG臨床研究参加予定なしの理由  	27	カラム目
      output_csv$JPLSG臨床研究参加予定なしの理由 <- input_csv$field9
      # 28-29カラム目は空値
      output_csv$col28 <- NA
      output_csv$col29 <- NA
      # pJ不参加理由その他  	30	カラム目
      output_csv$不参加理由その他 <- NA
      # 31カラム目は空値
      output_csv$col31 <- NA
      # pJ担当医名  	32	カラム目
      output_csv$担当医名 <- NA
      # 33カラム目は空値
      output_csv$col33 <- NA
      # pJ治療開始予定日  	34	カラム目
      output_csv$治療開始予定日 <- input_csv$field11
      # 35カラム目は空値
      output_csv$col35 <- NA
      # pJFAX日  	36	カラム目
      output_csv$FAX日 <- input_csv$field18
      # 37カラム目は空値
      output_csv$col37 <- NA
      # pJ生年月日  	38	カラム目
      output_csv$生年月日 <- input_csv$field20
      # pJ性別  	39	カラム目　NUM
      output_csv$性別 <- input_csv$field21
      # 40-41カラム目は空値
      output_csv$col40 <- NA
      output_csv$col41 <- NA
      # pJ英文INI  	42	カラム目
      output_csv$英文イニシャル <- input_csv$field22
      # 43カラム目は空値
      output_csv$col43 <- NA
      # pJ和文名前の一文字目  	44	カラム目
      output_csv$和文名前の一文字目 <- NA
      # 45カラム目は空値
      output_csv$col45 <- NA
      # pJ住所  	46	カラム目
      output_csv$住所 <- NA
    } else if (input_csv[1,"シート名英数字別名"] == "nonparticipation") {
      # pJシート名  	5	カラム目
      # rawdataのシート名英数字別名から変換して格納
      output_csv$シート名 <- "臨床研究不参加報告"
      # pX施設名
      # pX診療科名 :
      # pX施設CD
      # pX生年月日
      # pA診断名
      # pX担当医
      # pX登録CD
      # pX患者INI
      # pX性別
      # pX記入者
      # pA診断名_その他詳細
      # pA不参加理由
      # pA該当JPLSG臨床研究
      # pA該当臨床研究
      # 6カラム目は空値
      output_csv$col6 <- NA
      # 施設名  	7	カラム目
      output_csv$施設名 <- input_csv$シート作成時施設名
      # 診療科名  	8	カラム目
      output_csv$診療科名	<- input_csv$シート作成時診療科名
      # 団体別施設コード  	9	カラム目
      output_csv$施設CD <- input_csv$シート作成時団体別施設コード
      # 10-14カラム目は空値　生年月日、診断日は取り込まない、担当医は情報なし
      output_csv$col10 <- NA
      output_csv$col11 <- NA
      output_csv$col12 <- NA
      output_csv$col13 <- NA
      output_csv$col14 <- NA
      output_csv$登録コード <- input_csv$登録コード
      # 16-26カラム目は空値 イニシャル、性別、ログインユーザ情報なし
      output_csv$col16 <- NA
      output_csv$col17 <- NA
      output_csv$col18 <- NA
      output_csv$col19 <- NA
      output_csv$col20 <- NA
      output_csv$col21 <- NA
      output_csv$col22 <- NA
      output_csv$col23 <- NA
      output_csv$col24 <- NA
      output_csv$col25 <- NA
      output_csv$col26 <- NA
      # 診断CD 　27カラム目NUM
      output_csv$診断名	<-  input_csv$field1
      # 28カラム目は空値
      output_csv$col28 <- NA
      # 診断名_その他詳細 　29カラム目
      output_csv$診断名_その他詳細 <- input_csv$診断名_その他詳細
      # 30カラム目は空値
      output_csv$col30 <- NA
      # 該当JPLSG臨床研究 　31カラム目
      output_csv$該当JPLSG臨床研究 <- input_csv$field2
      # 32カラム目は空値
      output_csv$col32 <- NA
      # 該当臨床研究 　33カラム目
      output_csv$該当臨床研究 <- input_csv$field3
      # 34カラム目は空値
      output_csv$col34 <- NA
      # 不参加理由 　35カラム目
      output_csv$不参加理由 <- input_csv$field4
    } else {
      # 対象外シートとする
      output_csv$シート名 <- NA
    }
  }
  # csv出力
  write.csv(output_csv, paste(outputpath, file_list[i], sep="/"), quote=F, na='', row.names=F)
}
