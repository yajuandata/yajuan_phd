# 加载必要的包
library(data.table)


compare_and_print_csv <- function(file1, file2) {
    compare_csv <- function(file1, file2) {
        # 读取两个文件
        df1 <- fread(file1)
        df2 <- fread(file2)

        # 比较两个文件的行数和列数是否相同
        if (nrow(df1) != nrow(df2) || ncol(df1) != ncol(df2)) {
            return(list(success = FALSE, message = "行数或列数不同"))
        }

        # 比较两个文件的列名是否相同
        if (!all(names(df1) == names(df2))) {
            return(list(success = FALSE, message = "列名不同"))
        }

        # 初始化一个列表来收集不同的值
        differences <- list()

        # 比较两个文件的每个单元格的值是否相同
        for (i in 1:nrow(df1)) {
            for (j in 1:ncol(df1)) {
                val1 <- df1[i, ..j]
                val2 <- df2[i, ..j]
                if (!is.na(val1) && !is.na(val2) && val1 != val2) { # 忽略NA值
                    differences <- c(differences, list(paste("在第", i, "行，第", j, "列处不同，值分别为：", val1, "和", val2)))
                }
            }
        }

        # 如果有不同的值，返回这些值和它们的位置
        if (length(differences) > 0) {
            return(list(success = FALSE, message = differences))
        }

        # 如果以上比较都相同，则返回TRUE
        return(list(success = TRUE, message = "两个文件完全相同"))
    }

    # 比较两个csv文件是否相同
    result <- compare_csv(file1, file2)

    # 输出结果
    if (result$success) {
        print(result$message)
    } else {
        print(paste("发现", length(result$message), "处不同："))
        print(result$message)
    }
}

# 使用函数
compare_and_print_csv("../yajuan_data/out/ps_baseline.csv", "../yajuan_data/out/ps_baseline2.csv")
