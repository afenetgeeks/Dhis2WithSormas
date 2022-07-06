#' Connecting
#'
#' @description connecting to Dhis2
#'
#' @noRd
#' @import odbc RMariaDB DBI
#' @export
#'


updated_remote_database <- function(connection, dataset, aws_datatable, starting_index){

  i <- starting_index

  dataset_length <-  nrow(dataset)


  if(starting_index == 1){

    dbRemoveTable(conn = connection, name = aws_datatable)

    while (i < dataset_length) {

      if ( (dataset_length - i) < 1000){

        dbWriteTable(conn = connection,name =  aws_datatable, value = dataset[i:dataset_length,], append = TRUE)

        print(c(i, dataset_length))
        break

      }else{

        dbWriteTable(conn =connection, name = aws_datatable, value = dataset[i:(i+999),], append = TRUE)

        print(c(i, i+ 999))
      }

      i <- i + 1000
    }

  } else{

    while (i < dataset_length) {

      if ( (dataset_length - i) < 1000){

        dbWriteTable(conn = connection,name =  aws_datatable, value = dataset[i:dataset_length,], append = TRUE)

        print(c(i, dataset_length))
        break

      }else{

        dbWriteTable(conn =connection, name = aws_datatable, value = dataset[i:(i+999),], append = TRUE)

        print(c(i, i+ 999))
      }

      i <- i + 1000
    }

  }

}



