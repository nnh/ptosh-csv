# 列名がレイアウト変更前後で等しいことを確認する
prtpath <- "C:/Users/MarikoOhtsuka/Documents/github/ptosh-csv"
rawdatapath <- paste0(prtpath, "/rawdata")
outputpath <- paste0(prtpath, "/output")
diffpath <- paste0(prtpath, "/diff")
constbef <- "_170821_1241.csv"
constaft <- "_171027_1305.csv"
# rawdataフォルダ内のファイル名を取得
aftfile_list <- list.files(outputpath)
beffile_list <- list.files(diffpath)
wkdf <- data.frame(1,2)
# allocationシートにはグループと割付ラベルの列を追加する
for (i in 1:length(beffile_list)){
  # Shift-JISで列もデータとして取り込む
  wk_bef <- gsub(constbef, constaft, beffile_list[i])
  i_csv <- read.csv(paste(outputpath, wk_bef, sep="/"), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F)
  j_csv <- read.csv(paste(diffpath, beffile_list[i], sep="/"), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F)
  if (ncol(i_csv) == ncol(j_csv)) {
    wkdf <- rbind(wkdf,c("COL_LENGTH_OK", beffile_list[i]))
  } else {
    wkdf <- rbind(wkdf,c("COL_LENGTH_NG", beffile_list[i]))
  }
  for (j in 1:ncol(j_csv)){
    if (colnames(i_csv)[j] != colnames(j_csv)[j]) {
      wkdf <- rbind(wkdf,c("COL_name_NG", beffile_list[i]))
    }
  }
}
