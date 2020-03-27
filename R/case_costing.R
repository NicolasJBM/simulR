#' Information useful for costing.
#'
#' @format Resources consumed by each product.
#' \itemize{
#'   \item case: id of the case.
#'   \item object_pool: account associated with the cost oobject or cost pool.
#'   \item object_pool_label: Name of the cost object or cost pool.
#'   \item allocation_base: name of the allocation base associated with the cost pool or cost object (for standard costing).
#'   \item standard_cost_reciprocal: allocation rate (using the reciprocal method) or standard cost associated with the cost pool or cost object (for standard costing).
#'   \item standard_cost_sequential: allocation rate (using the sequential method) or standard cost associated with the cost pool or cost object (for standard costing).
#'   \item standard_cost_direct: allocation rate (using the direct method) or standard cost associated with the cost pool or cost object (for standard costing).
#' }
#' @docType data
#' @keywords datasets
#' @name case_costing
#' @usage data("case_costing")
"case_costing"
