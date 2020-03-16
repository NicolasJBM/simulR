#' List of products classified per family.
#'
#' @format List of products classified per family.
#' \itemize{
#'   \item case: id of the case.
#'   \item resource: product about which decisions are made.
#'   \item destination: related account number.
#'   \item destiantion_label: label of the related account.
#'   \item parameter: decision made: "price", "discount", "dso", "commission", "advertising" and "priority".
#'   \item initialization: value of the parameter in the initialization period.
#'   \item first_period: value of the parameter in the first period of operations.
#' }
#' @docType data
#' @keywords datasets
#' @name case_sales
#' @usage data("case_sales")
"case_sales"
