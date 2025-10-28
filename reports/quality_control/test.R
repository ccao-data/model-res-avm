card_data <- rbind(comp_chars, baseline_chars)
dt <- as.data.table(card_data)[order(meta_pin, meta_card_num)]
setnames(dt, "meta_year", "year")

key_cols <- c("meta_pin", "meta_card_num")
cols <- setdiff(names(dt), key_cols)

# Create tolerance list
base_tol <- c(
  acs5_median_age_total = 5,
  acs5_median_household_renter_occupied_gross_rent = 400,
  acs5_median_household_total_occupied_year_built = 5,
  acs5_median_income_household_past_year = 5000,
  acs5_median_income_per_capita_past_year = 5000,
  ccao_n_years_exe_homeowner = 1,
  loc_latitude = 0.0001,
  loc_longitude = 0.0001,
  other_tax_bill_rate = 0.5,
  prox_airport_dnl_total = 1,
  prox_num_foreclosure_per_1000_pin_past_5_years = 5,
  time_sale_day = 31,
  time_sale_day_of_week = 7,
  time_sale_year = 1,
  year = 1
)

tol <- setNames(rep(NA_real_, length(cols)), cols)
tol[names(base_tol)] <- base_tol[names(base_tol) %in% cols]
tol[grepl("dist_ft$", cols)] <- 5
tol[grepl("^acs5_percent_", cols)] <- 0.05

tol_defined <- !is.na(tol) # TRUE only where a tolerance exists

# Group-wise checks: numeric path ONLY if tolerance is defined ---
group_matches <- dt[
  ,
  as.list(mapply(
    function(v, nm) {
      t <- tol[[nm]]

      # if both values are NA -> match
      if (all(is.na(v))) {
        return(TRUE)
      }

      if (tol_defined[[nm]]) {
        # try numeric path (only if coercion yields no NAs)
        nv <- suppressWarnings(as.numeric(v))
        if (anyNA(nv)) {
          # Since we qualify if both is NA we return a match,
          # either NA will return a FALSE match here
          return(FALSE)
        } else {
          r <- range(nv)
          return((r[2] - r[1]) <= t)
        }
      } else {
        # exact match after normalization
        sv <- tolower(trimws(as.character(v)))
        # "both NA" handled above; here NA present in only one side -> non-match
        if (anyNA(sv)) {
          return(FALSE)
        }
        return(uniqueN(sv) <= 1)
      }
    },
    .SD, names(.SD),
    SIMPLIFY = FALSE
  )),
  by = key_cols,
  .SDcols = cols
]
