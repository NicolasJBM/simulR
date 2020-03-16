#' List of accounts where transactions can be recorded for the case.
#'
#' @format List of accounts where transactions can be recorded for the case.
#' \itemize{
#'   \item case: id of the case
#'   \item account: 5-digit account number. The first three digits indicate the nature, the last two the destination.
#'   \item account_label: case-specific label for the account.
#'   \item account_generic: generic label for the account.
#'   \item account_subcategory: subcategory of the financial statements.
#'   \item account_category: category of the financial statements.
#'   \item account_statement: which financial statements.
#' }
#' @docType data
#' @keywords datasets
#' @name case_accounts
#' @usage data("case_accounts")
"case_accounts"
