#' List of presources consumed with their price
#'
#' @format List of presources consumed with their price
#' \itemize{
#'   \item case
#'   \item resource_full: Full name of the resource.
#'   \item resource_sing: Singular name of the unit of the resource.
#'   \item resource_plur: Plural name of the unit of the resource.
#'   \item resource_type: Type of resource (materials, labor time or machine time).
#'   \item resource_unit: unit in which the resource is expressed.
#'   \item resource_price: price at which each unit or the resource can be procured.
#'   \item resource_capacity: maximum number of minutes worked by one unit of production capacity in a day.
#'   \item resource_quantity: number of units of production capacity or inventory available at the beginning.
#' }
#' @docType data
#' @keywords datasets
#' @name case_resources
#' @usage data("case_resources")
"case_resources"
