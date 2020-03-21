#' List of general parameters for a simulation.
#'
#' @format List of general parameters for a simulation.
#' \itemize{
#'   \item case: id of the case.
#'   \item environment_type: indication about the type of environment.
#'   \item value_added_tax: percentage of VAT on purchases.
#'   \item labor_tax: percentage of tax applied on wages.
#'   \item income_tax: percentage of EBT for the State.
#'   \item base_volume: daily volume serving as reference.
#'   \item sensitivity_price: sales' sensitivity to price differences.
#'   \item sensitivity_quality: sales' sensitivity to quality (proxied by standard prime costs) differences.
#'   \item sensitivity_dso: sales sensitivity to differences in credit terms.
#'   \item sensitivity_advertising: sales sensitivity to differences in advertising expenses.
#'   \item sensitivity_commissions: sales sensitivity to differences in sales commissions.
#'   \item sensitivity_demand: how discriminant differences in price, cost, dso, advertising and commissions are.
#'   \item sensitivity_shape: whether the demand is distributed following a "linear", "constant" or "logistic" law.
#'   \item downward_adjustment_cost: percentage of the yearly expense paid as penalty for decreasing capacity.
#'   \item upward_adjustment_cost: percentage of the yearly expense paid as cost of increasing capacity.
#' }
#' @docType data
#' @keywords datasets
#' @name case_environments
#' @usage data("case_environments")
"case_environments"
