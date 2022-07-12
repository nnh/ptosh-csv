#' test20220711
#' test script
#' @file 20220711test.R
#' @author Mariko Ohtsuka
#' @date 2022.7.11
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(RUnit)
# ------ constants ------
# ------ functions ------
TestFunction <- function(){
  # 修正前のプログラムでの出力結果と修正後のプログラムでの出力結果を比較する
  input_bef_path <- '~/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2022/20220711/before/output'
  input_aft_path <- "~/Downloads/test20220711/output"
  input_bef <- list.files(input_bef_path)
  input_aft <- list.files(input_aft_path)
  print("ファイル数チェック 開始")
  if (checkEquals(input_bef, input_aft)){
    print('OK')
  } else {
    print('ファイル数チェック　エラー')
    return()
  }
  print("ファイル数チェック 終了")
  print("CSV読み込みチェック 開始")
  res <- input_bef %>% map(~ {
    target_file_name <- .
    bef_input <- ReadCsv(input_bef_path, target_file_name, 'bef')
    aft_input <- ReadCsv(input_aft_path, target_file_name, 'aft')
    if (is.null(bef_input) || is.null(aft_input)){
      return(target_file_name)
    } else {
      return(NA)
    }
  })
  read_error_list <- res %>% unlist() %>% na.omit()
  if (length(read_error_list) > 0){
    print("CSV読み込みチェック エラー")
    return(read_error_list)
  } else{
    print("OK")
  }
  print("CSV読み込みチェック 終了")
  print("ファイル内行列数チェック 開始")
  res <- input_bef %>% map(~ {
    target_file_name <- .
    bef_input <- ReadCsv(input_bef_path, target_file_name, 'bef')
    aft_input <- ReadCsv(input_aft_path, target_file_name, 'aft')
    if (bef_input %>% nrow() != aft_input %>% nrow()){
      print(target_file_name)
      stop('行数エラー')
    }
    if (bef_input %>% ncol() != aft_input %>% ncol()){
      print(target_file_name)
      stop('列数エラー')
    }
  })
  print("ファイル内行列数チェック 終了")
  print("ファイル内容チェック 開始")
  res <- input_bef %>% map(~ {
    target_file_name <- .
    bef_input <- ReadCsv(input_bef_path, target_file_name, 'bef')
    aft_input <- ReadCsv(input_aft_path, target_file_name, 'aft')
    # オモテ
    for (row in 1:nrow(bef_input)){
      for (col in 1:ncol(bef_input)){
        if (is.na(bef_input[row, col]) && is.na(aft_input[row, col])){
          # OK
        } else {
          if (bef_input[row, col] != aft_input[row, col]){
            # 修正前の方の文字列内の改行コードを変換して再度チェック
            # ex)"abc\r\ndef" -> "abc\ndef"
            temp <- aft_input[row, col] %>% str_remove_all('\\r')
            if (temp != bef_input[row, col]){
              print(target_file_name)
              print(str_c('行：', row))
              print(str_c('列：', col))
              print(str_c("「", bef_input[row, col], "」"))
              print(str_c("「", aft_input[row, col], "」"))
              stop('ファイル内容エラー')
            }
          }
        }
      }
    }
    # ウラ
    for (row in 1:nrow(aft_input)){
      for (col in 1:ncol(aft_input)){
        if (is.na(bef_input[row, col]) && is.na(aft_input[row, col])){
          # OK
        } else {
          if (bef_input[row, col] != aft_input[row, col]){
            # 修正前の方の文字列内の改行コードを変換して再度チェック
            # ex)"abc\r\ndef" -> "abc\ndef"
            temp <- aft_input[row, col] %>% str_remove_all('\\r')
            if (temp != bef_input[row, col]){
              print(target_file_name)
              print(str_c('行：', row))
              print(str_c('列：', col))
              print(str_c("「", bef_input[row, col], "」"))
              print(str_c("「", aft_input[row, col], "」"))
              stop('ファイル内容エラー')
            }
          }
        }
      }
    }
  })
  print("ファイル内容チェック 終了")
}
ReadCsv <- function(path, filename, message){
  res <- tryCatch(
    suppressMessages(
      read_csv(str_c(path, '/', filename), show_col_types=F, col_types=cols())
    ),
                        error=function(e){
                          print(str_c('*** ', message, '_csv_read_error start ***'))
                          print(e)
                          print(filename)
                          print(str_c('*** ', message, '_csv_read_error end ***'))
                          return(NULL)
                        },
                        warning=function(e){
                          print(str_c('*** ', message, '_csv_read_warning start ***'))
                          print(e)
                          print(filename)
                          print(str_c('*** ', message, '_csv_read_warning end ***'))
                          return(NULL)
                        }
  )
  return(res)
}
# ------ main ------
res <- TestFunction()
