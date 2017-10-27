# 列名がレイアウト変更前後で等しいことを確認する
prtpath <- "C:/Users/MarikoOhtsuka/Documents/github/PtoshCSV"
rawdatapath <- paste0(prtpath, "/rawdata")
outputpath <- paste0(prtpath, "/output")
diffpath <- paste0(prtpath, "/diff")
# rawdataフォルダ内のファイル名を取得
aftfile_list <- list.files(diffpath)
beffile_list <- list.files(outputpath)
wkdf <- data.frame(1,2)
# allocationシートにはグループと割付ラベルの列を追加する
for (i in 1:length(aftfile_list)){
  # Shift-JISで列もデータとして取り込む
  i_csv <- read.csv(paste(outputpath, aftfile_list[i], sep="/"), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F)
  j_csv <- read.csv(paste(diffpath, aftfile_list[i], sep="/"), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F)
  if (ncol(i_csv) == ncol(j_csv)) {
    wkdf <- rbind(wkdf,c("COL_LENGTH_OK", aftfile_list[i]))
  } else {
    wkdf <- rbind(wkdf,c("COL_LENGTH_NG", aftfile_list[i]))
  }
  for (j in 1:ncol(j_csv)){
    if (colnames(i_csv)[j] != colnames(j_csv)[j]) {
      wkdf <- rbind(wkdf,c("COL_name_NG", aftfile_list[i]))
    }
  }
}
