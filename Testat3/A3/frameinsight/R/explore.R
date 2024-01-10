#' Explore
#'
#' Explorative Datenanalyse für einen Datenframe
#'
#'describe
#' Diese Funktion erstellt eine Zusammenfassung für einen Datenframe.
#'
#' @param df Der Datenframe, für den eine Zusammenfassung erstellt werden soll.
#'
#' @return
#' Ein Listenobjekt mit folgenden Informationen:
#'   - num_columns: Anzahl der Spalten im Datenframe,
#'   - num_rows: Anzahl der Zeilen im Datenframe,
#'   - columns_info: Eine Liste mit Informationen zu jeder Spalte, einschließlich ihres Datentyps und statistischer Informationen (falls numerisch).
#'
#' @examples
#' # Beispiel mit einem Datenframe namens 'data'
#' data <- data.frame(
#'   age = c(25, 30, 22, 40, 35),
#'   income = c(50000, 60000, 75000, 80000, 70000),
#'   sex = c("f", "m", "f", "m", "f"),
#'   height = c(170, 180, 165, 175, 160),
#'   weight = c(65, 80, 55, 70, 50)
#' )
#' describe(data)
#'
#' @export
describe = function(df) {
  num_columns = ncol(df)

  num_rows = nrow(df)

  columns_info = list()
  for (col_name in colnames(df)) {
    col_type = class(df[[col_name]])
    col_info = list(type = col_type)

    if (col_type %in% c("integer", "numeric")) {
        col_stats = c(mean = mean(df[[col_name]], na.rm = TRUE),
                     median = median(df[[col_name]], na.rm = TRUE),
                     sd = sd(df[[col_name]], na.rm = TRUE))
        col_info = append(col_info, list(col_stats=col_stats))
    }

    columns_info[[col_name]] = col_info
  }

  result = list(
    num_columns = num_columns,
    num_rows = num_rows,
    columns_info = columns_info
  )

  return(result)
}

#'explore
#'
#'Überprüfung auf fehlende Werte (NA) in einer Spalte
#' @param col Die zu überprüfende Spalte (Vector).
#'
#' @return
#' Die Funktion gibt einen logischen Wert zurück: TRUE, wenn es fehlende Werte gibt, ansonsten FALSE.
#'
#' @examples
#' # Beispiel mit einer Datenframe-Spalte namens 'age'
#' data <- data.frame(age = c(25, 30, NA, 40, 22))
#' has_NA(data$age)  # Gibt TRUE zurück, da es einen fehlenden Wert gibt
#'
#' # Beispiel mit einer Datenframe-Spalte namens 'income'
#' data <- data.frame(income = c(50000, 60000, 75000, 80000))
#' has_NA(data$income)  # Gibt FALSE zurück, da keine fehlenden Werte vorhanden sind
#'
#' 

#' @return
has_NA = function(col)


data = data.frame(
  age = sample(18:60, n_rows, replace = TRUE),
  income = rnorm(n_rows, mean = 50000, sd = 10000),
  sex = sample(c("f", "m", "d"), n_rows, replace = TRUE),
  height = rnorm(n_rows, mean = 175, sd = 10),
  weight = rnorm(n_rows, mean = 70, sd = 5)
)

describe(data)
