
# Split a df in half, and bind them back together to make one table with 
# twice the number of columns, but half the rows. Adds an extra blank 
# row to the second df if needed. 
split_table <- function(x) {
  split1 <- x |> 
    head(ceiling(nrow(x) / 2))
  colnames(split1) <- paste(colnames(split1), "_1", sep = "")
  
  split2 <- x |> 
    tail(floor(nrow(x) / 2))
  colnames(split2) <- paste(colnames(split2), "_2", sep = "")
  
  if (nrow(split1) != nrow(split2)) {
    split2 <- add_row(split2)
  }
  
  return(final <- bind_cols(split1, split2))
}


# Use with split_table(). After splitting a table in half, you can strip off
# the last two characters of the columns with the "_1" and "_2". 
split_cols_label <- function(x) {
  stopifnot("x should be a gt" = inherits(x, "gt_tbl"))
  
  x |> 
    cols_label_with(
      fn = ~ stringr::str_remove(., "_1"), 
    ) |> 
    cols_label_with(
      fn = ~ stringr::str_remove(., "_2")
    ) 
}


# demo of split_table() and split_cols_label()

library(tidyverse)
library(gt)

tst <- tibble(rank = 1:19, upper = LETTERS[1:19], lower = letters[1:19])

split_tst <- split_table(tst)

gt(split_tst) |> 
  split_cols_label()


# Strip out specific text in displayed column labels


strip_cols_label <- function(x, string) {
  stopifnot("x should be a gt" = inherits(x, "gt_tbl"))
  
    x |> 
      cols_label_with(
        fn = ~ stringr::str_remove(., string)
        ) 
}

replace_cols_label <- function(x, string, replacement) {
  stopifnot("x should be a gt" = inherits(x, "gt_tbl"))
  
  x |> 
    cols_label_with(
      fn = ~ stringr::str_replace(., string, replacement)
    ) 
}



