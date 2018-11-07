# テスト用データ作成スクリプト
# rawdataから空ファイルとヘッダーのみのファイルを削除する
# ------
# テスト内容
# 上記のrawdataで修正前のプログラムを実行（エンコード明示対応済みのプログラム）
# 本来のrawdataで修正後のプログラムを実行
# outputデータが等しいことを確認する

# 団体名、試験名、入力ファイル格納場所を指定
# *********************************
kOrganization  <- "JPLSG"
kTrialTitle  <- "IntReALL-SR-2010"
prtpath <- "/Users/admin/Documents/GitHub/ptosh-csv"
# *********************************
rawdatapath <- paste0(prtpath, "/before_rawdata")
# rawdataフォルダ内のファイル名を取得
file_list <- list.files(rawdatapath)
# allocationシートにはグループと割付ラベルの列を追加する
kAllocation <- "allocation"

# registrationシート読み込み
registration_index <- grep(paste(kOrganization, "registration", sep="_"), file_list)
if (length(registration_index) > 0) {
  # na.strings = "" の指定で文字列"NA"を残す
  registration_csv <- read.csv(paste(rawdatapath, file_list[registration_index], sep="/"), as.is=T, na.strings=""
                               , fileEncoding="CP932")
  file_list <- file_list[ - registration_index]
}
# 団体名_YYMMDD_HHMM.csv読み込み
base_index <- grep(paste0("^", kOrganization, "_[0-9]{6}_[0-9]{4}"), file_list)
if (length(base_index) > 0) {
  base_csv <- read.csv(paste(rawdatapath, file_list[base_index], sep="/"), as.is=T, fileEncoding="CP932")
  file_list <- file_list[ - base_index]
  # 試験名_YYMMDD_HHMM.csvを削除
  base2_index <- grep(paste0("^", kTrialTitle, "_[0-9]{6}_[0-9]{4}"), file_list)
  if (length(base2_index) > 0) {
    file_list <- file_list[ - base2_index]
  }
} else {
  # 団体名_YYMMDD_HHMM.csvが存在しない場合は試験名_YYMMDD_HHMM.csvを採用
  base_index <- grep(paste0("^", kTrialTitle, "_[0-9]{6}_[0-9]{4}"), file_list)
  if (length(base_index) > 0) {
    base_csv <- read.csv(paste(rawdatapath, file_list[base_index], sep="/"), as.is=T, fileEncoding="CP932")
    file_list <- file_list[ - base_index]
  }
}

# 試験名で始まらないCSVは処理対象外とする
file_list <- file_list[grep(paste0("^", kTrialTitle, "_"), file_list)]
fl <- ''
for (i in 1:length(file_list)) {
  res <- try(input_csv <- read.csv(paste(rawdatapath, file_list[i], sep="/"), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F), silent=T)
  # 空もしくはヘッダだけのファイル名を抽出する
  if(class(res) == "try-error" || nrow(input_csv) < 2)  {
    fl <- c(fl, file_list[i])
    }
}
# ファイルを削除する
for (i in 2:length(fl)) {
  file.remove(paste(rawdatapath, fl[i], sep="/"))
}
