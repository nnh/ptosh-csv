# 団体名、試験名、入力ファイル格納場所を指定
# *********************************
kOrganization  <- "JPLSG"
kTrialTitle  <- "ALL-B12"
prtpath <- "//aronas/Datacenter/Users/ohtsuka/ALL-B12"
# *********************************
rawdatapath <- paste0(prtpath, "/rawdata")
# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}
# rawdataフォルダ内のファイル名を取得
file_list <- list.files(rawdatapath)
# allocationシートにはグループと割付ラベルの列を追加する
kAllocation <- "allocation"
# 必要パッケージのインストール
target_packages = c("tidyverse")
installed_package <- target_packages %in% installed.packages()
if(length(target_packages[!installed_package]) > 0){
  install.packages(target_packages[!installed_package])
}
lapply(target_packages, require, character.only=T)
# registrationシート読み込み
registration_index <- grep(paste(kOrganization, "registration", sep="_"), file_list)
if (length(registration_index) > 0) {
  registration_csv <- read_csv(paste(rawdatapath, file_list[registration_index], sep="/"), show_col_types=F)
  file_list <- file_list[ - registration_index]
}
# 団体名_YYMMDD_HHMM.csv読み込み
base_index <- grep(paste0("^", kOrganization, "_[0-9]{6}_[0-9]{4}"), file_list)
if (length(base_index) > 0) {
  base_csv <- read_csv(paste(rawdatapath, file_list[base_index], sep="/"), show_col_types=F)
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
    base_csv <- read_csv(paste(rawdatapath, file_list[base_index], sep="/"), show_col_types=F)
    file_list <- file_list[ - base_index]
  }
}

# 試験名で始まらないCSVは処理対象外とする
  file_list <- file_list[grep(paste0("^", kTrialTitle, "_"), file_list)]

for (i in 1:length(file_list)){
  # 列もデータとして取り込む
  input_csv <- read_csv(paste(rawdatapath, file_list[i], sep="/"), col_names=F, show_col_types=F)
  # 空ファイルは処理スキップ
  if(nrow(input_csv) > 1)  {
  # 入力ファイルに1行目と同じ内容で列名をセット
    colnames(input_csv) <- input_csv[1, ]
    input_csv <- input_csv[-1, ]
    # 1列目"バージョン"
    # "1"固定の行を挿入
    output_csv <- data.frame("バージョン" = rep("1", nrow(input_csv)))
    # registrationシートから読み込む情報退避用データフレーム
    df_registration <- output_csv
    df_registration$生年月日 <- NA
    df_registration$患者イニシャル <- NA
    df_registration$患者カナ <- NA
    df_registration$性別 <- NA
    df_registration$生死 <- NA
    df_registration$死亡日 <- NA
    df_registration$最終確認日 <- NA

    # Allocation判定
    if (length(grep(kAllocation, file_list[i])) > 0) {
      allocation_F <- T
    } else {
      allocation_F <- F
    }

    # 別シートからの情報取り込み
    # registrationシートから情報取得
    # "生年月日"
    # registration$field20
    # "患者イニシャル"
    # registration$field22
    # "患者カナ"
    # registration$field23
    # "性別"
    # registration$field21 0->male,1->female
    # baseシートから情報取得
    # "生死"
    # 団体名_YYYYMMDD$生死
    # "死亡日"
    # 団体名_YYYYMMDD$死亡日
    # "最終確認日"
    # 団体名_YYYYMMDD$最終確認日

    # 登録コードの列がなければスキップ
    reg_index <- grep("登録コード", colnames(input_csv))
    if (length(reg_index) > 0) {
      reg_colnames <- colnames(registration_csv)
      for (j in 1:nrow(output_csv)){
        wk_registration <- subset(registration_csv, 登録コード == as.numeric(input_csv[j,"登録コード"]))
        # 各列が存在しない場合はスキップ
        if ("生年月日" %in% reg_colnames){
          df_registration[j,"生年月日"] <- gsub("-", "/", as.character(wk_registration$生年月日))
        }
        if ("英文イニシャル" %in% reg_colnames){
          df_registration[j,"患者イニシャル"] <- as.character(wk_registration$英文イニシャル)
        }
        if ("和文名前の一文字目" %in% reg_colnames){
          df_registration[j,"患者カナ"] <- as.character(wk_registration$和文名前の一文字目)
        }
        if ("性別" %in% reg_colnames){
          df_registration[j,"性別"] <- as.character(wk_registration$性別)
        }
        wk_base <-  subset(base_csv, 登録コード == as.numeric(input_csv[j,"登録コード"]))
        if ("生死" %in% reg_colnames){
          df_registration[j,"生死"] <- as.character(wk_base$生死)
        }
        if ("死亡日" %in% reg_colnames){
          df_registration[j,"死亡日"] <- gsub("-", "/", as.character(wk_base$死亡日))
        }
        if ("最終確認日" %in% reg_colnames){
          df_registration[j,"最終確認日"] <- gsub("-", "/", as.character(wk_base$最終確認日))
        }
      }
    }

    # 作成日
    output_csv$作成日 <- input_csv$作成日
    input_csv <- input_csv[ ,colnames(input_csv) != "作成日"]
    # "最終更新日"
    output_csv$最終更新日 <- input_csv$最終更新日
    input_csv <- input_csv[ ,colnames(input_csv) != "最終更新日"]
    # "試験名"
    output_csv$試験名 <- kTrialTitle
    # "シート名"
    output_csv$シート名 <- input_csv$シート名英数字別名
    input_csv <- input_csv[ ,colnames(input_csv) != "シート名英数字別名"]
    # "シート作成時施設名"
    output_csv$シート作成時施設名 <- input_csv$シート作成時施設名
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時施設名"]
    # "現施設名"
    # 空列にする
    output_csv$現施設名 <- NA
    # シート作成時診療科名"
    output_csv$シート作成時診療科名 <- input_csv$シート作成時診療科名
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時診療科名"]
    # "シート作成時団体別施設コード"
    output_csv$シート作成時団体別施設コード <- input_csv$シート作成時団体別施設コード
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時団体別施設コード"]
    # "診察券番号"
    # 空列にする
    output_csv$診察券番号 <- NA
    # "生年月日"
    output_csv$生年月日 <- df_registration$生年月日
    # "診断日"
    # 空列にする
    output_csv$診断日 <- NA
    # "シート作成時担当医"
    output_csv$シート作成時担当医 <- input_csv$シート作成時担当医
    input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時担当医"]
    # "現担当医"
    # 空列にする
    output_csv$現担当医 <- NA
    # "登録コード"
    output_csv$登録コード <- input_csv$登録コード
    input_csv <- input_csv[ ,colnames(input_csv) != "登録コード"]
    # "患者イニシャル"
    output_csv$患者イニシャル <- df_registration$患者イニシャル
    # "患者カナ"
    output_csv$患者カナ <- df_registration$患者カナ
    # "症例登録番号"
    output_csv$症例登録番号 <- input_csv$症例登録番号
    input_csv <- input_csv[ ,colnames(input_csv) != "症例登録番号"]
    # "性別"
    output_csv$性別 <- df_registration$性別
    # "住所"
    # 空列にする
    output_csv$住所 <- NA
    # "生死"
    output_csv$生死 <- df_registration$生死
    # "死亡日"
    output_csv$死亡日 <- df_registration$死亡日
    # "最終確認日"
    output_csv$最終確認日 <- df_registration$最終確認日
    # "シート作成時施設コード"
    # "再入力依頼"
    # この2列はinput_csvに残し、あとで削除
    output_csv$シート作成時施設コード <- input_csv$シート作成時施設コード
    output_csv$再入力依頼 <- input_csv$再入力依頼
    # "ログインユーザー"
    # NA
    output_csv$ログインユーザー <- NA

    # allocationシート固有処理
    if (allocation_F) {
      # "グループ（郡）"
      # "割付ラベル"
      output_csv[ ,'グループ（郡）'] <- NA
      output_csv$割付ラベル <- NA
    }

    # 上記列以降はそのまま残す
    if (ncol(input_csv) > 2) {
      # シート作成時施設コード、再入力依頼以外の列が残っていれば結合対象
      input_csv <- input_csv[ ,colnames(input_csv) != "シート作成時施設コード"]
      input_csv <- input_csv[ ,colnames(input_csv) != "再入力依頼"]
      output_csv <- cbind(output_csv, input_csv)
    }
    # csv出力
    write_excel_csv(output_csv, paste(outputpath, file_list[i], sep="/"),  na='""')
    print(paste0(file_list[i], "を出力しました"))
  } else {
    print(paste0(file_list[i], "のデータは0行のため出力対象外です"))
  }
}
