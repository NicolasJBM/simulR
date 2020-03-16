#' Resources consumed at different phases.
#'
#' @format Resources consumed by each product.
#' \itemize{
#'   \item case: id of the case.
#'   \item activity: name of the activity.
#'   \item phase: whether resources are consumed for "sales", "prroduction", or "support".
#'   \item output_standard_quantity: quantity produced.
#'   \item output_unit: unit of the quantity produced.
#'   \item output: kind of output produced.
#'   \item input_standard_quantity: quantity of resource consumed to make this quantity of that output.
#'   \item input_unit: unit of the resource consumed.
#'   \item input: kind of resource consumed.
#'   \item per_unit: quantity of input per unit or output.
#' }
#' @docType data
#' @keywords datasets
#' @name case_activity
#' @usage data("case_activity")
"case_activity"
