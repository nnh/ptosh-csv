# 変更前ファイルの列を変更後と同一にした比較ファイルを出力する
prtpath <- "C:/Users/MarikoOhtsuka/Desktop/HL14"
diffpath <- paste0(prtpath, "/diff")

constbef <- "_170821_1241.csv"
constaft <- "_171027_1305.csv"
# diffフォルダ内のファイル名を取得
diff_list <- list.files(diffpath)
kAllocation <- "allocation"
kTrialTitle  <- "HL-14"
# 出力フォルダが存在しなければ作成
diffeditpath <- paste0(prtpath, "/diffedit")
if (!(file.exists(diffeditpath))){
  dir.create(diffeditpath)
}


for (i in 1:length(diff_list)){
  # Allocation判定
  if (length(grep(kAllocation, diff_list[i])) > 0) {
    allocation_F <- T
  } else {
    allocation_F <- F
  }
  # Shift-JISで列もデータとして取り込む
  res <- try(input_csv <- read.csv(paste(diffpath, diff_list[i], sep="/"), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F), silent=T)
  # 空ファイルは処理スキップ
  if(class(res) != "try-error") {
    # 入力ファイルに1行目と同じ内容で列名をセット
    colnames(input_csv) <- input_csv[1, ]
    input_csv <- input_csv[-1, ]
    output_csv <- data.frame("バージョン" = rep("1", nrow(input_csv)))
    input_csv <- input_csv[ ,colnames(input_csv) != "バージョン"]
    output_csv$作成日 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "作成日"]
    # "最終更新日"
    output_csv$最終更新日 <- input_csv$最終更新日
    input_csv <- input_csv[ ,colnames(input_csv) != "最終更新日"]
    # "試験名"
    output_csv$試験名 <- kTrialTitle
    input_csv <- input_csv[ ,colnames(input_csv) != "試験名"]
    # "シート名"
    output_csv$シート名 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "シート名英数字別名"]
    # "シート作成時施設名"
    output_csv$シート作成時施設名 <- input_csv$シート作成時施設名
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時施設名"]
    # "現施設名"
    output_csv$現施設名 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "現施設名"]
    # シート作成時診療科名"
    output_csv$シート作成時診療科名 <- input_csv$シート作成時診療科名
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時診療科名"]
    # "シート作成時団体別施設コード"
    # 空列にする
    output_csv$シート作成時団体別施設コード <- input_csv$シート作成時団体別施設コード
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時団体別施設コード"]
    # "診察券番号"
    # 空列にする
    output_csv$診察券番号 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "診察券番号"]
    # "生年月日"
    output_csv$生年月日 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "生年月日"]
    # "診断日"
    # 空列にする
    output_csv$診断日 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "診断日"]
    # "シート作成時担当医"
    output_csv$シート作成時担当医 <- input_csv$シート作成時担当医
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時担当医"]
    # "現担当医"
    # 空列にする
    output_csv$現担当医 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "現担当医"]
    # "登録コード"
    output_csv$登録コード <- input_csv$登録コード
    input_csv <- input_csv[ ,colnames(input_csv) != "登録コード"]
    # "患者イニシャル"
    output_csv$患者イニシャル <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "患者イニシャル"]
    # "患者カナ"
    output_csv$患者カナ <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "患者カナ"]
    # "症例登録番号"
    output_csv$症例登録番号 <- input_csv$症例登録番号
    input_csv <- input_csv[ ,colnames(input_csv) != "症例登録番号"]
    # "性別"
    output_csv$性別 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "性別"]
    # "住所"
    # 空列にする
    output_csv$住所 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "住所"]
    # "生死"
    output_csv$生死 <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "生死"]
    # "死亡日"
    output_csv$死亡日 <-  NA
    input_csv <- input_csv[ ,colnames(input_csv) != "死亡日"]
    # "最終確認日"
    output_csv$最終確認日 <-  NA
    input_csv <- input_csv[ ,colnames(input_csv) != "最終確認日"]
    # "シート作成時施設コード"
    # "再入力依頼"
    # この2列はinput_csvに残し、あとで削除
    output_csv$シート作成時施設コード <- input_csv$シート作成時施設コード
    output_csv$再入力依頼 <- input_csv$再入力依頼
    # "ログインユーザー"
    # NA
    output_csv$ログインユーザー <- NA
    input_csv <- input_csv[ ,colnames(input_csv) != "ログインユーザー"]

    # allocationシート固有処理
    if (allocation_F) {
      # "グループ（郡）"
      # "割付ラベル"
      output_csv[ ,'グループ（郡）'] <- NA
      output_csv$割付ラベル <- NA
      input_csv <- input_csv[ ,colnames(input_csv) != "グループ（郡）"]
      input_csv <- input_csv[ ,colnames(input_csv) != "割付ラベル"]
    }

    # 上記列以降はそのまま残す
    if (ncol(input_csv) > 2) {
      # シート作成時施設コード、再入力依頼以外の列が残っていれば結合対象
      input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時施設コード"]
      input_csv <- input_csv[ ,colnames(input_csv) != "再入力依頼"]
      output_csv <- cbind(output_csv,input_csv)
    }
    # csv出力
    write.csv(output_csv, paste(diffeditpath, diff_list[i], sep="/"), na='""', row.names=F)

  }
}
