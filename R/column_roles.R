#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


# This returns a `data_column` with NULL values removed using the
# sampling procedure of `first_n` or `random_n`
get_non_null_col_sample <- function(data_column,
                                    sample_type = c("first_n", "random_n"),
                                    sample_n = 1000,
                                    sample_min_n = 1) {
  
  sample_type <- match.arg(sample_type)
  
  # Stop function if there isn't exactly one column in `data_column`
  if (ncol(data_column) != 1) {
    stop("The table given as `data_column` must have exactly one column.")
  }
  
  # Filter out all NA/NULL values from the `data_column`
  data_column <- 
    dplyr::filter(data_column, dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
  
  # Obtain a subsample of non-NULL values in the column
  if (sample_type == "first_n") {
    data_column <- dplyr::slice_head(data_column, n = sample_n)
  } else {
    data_column <- dplyr::slice_sample(data_column, n = sample_n)
  }
  
  # Count the values in the column and return `NULL` if it the
  # count is below the threshold set `sample_min_n`
  data_column_count <- dplyr::pull(dplyr::count(data_column), n)
  
  if (data_column_count < sample_min_n) {
    return(NULL)
  }
  
  data_column
}

# This provides the cardinality of a column and we may choose
# include the exclude any NA group from this count
get_column_cardinality <- function(data_column,
                                   include_na = TRUE) {
  
  data_column_groups <-
    data_column %>%
    dplyr::rename(a__ = 1) %>%
    dplyr::group_by(a__) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()
  
  data_column_has_any_na <-
    data_column_groups %>%
    dplyr::filter(is.na(a__)) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    nrow() %>%
    as.logical()
  
  data_column_groups_n <-
    data_column_groups %>%
    dplyr::count() %>%
    dplyr::pull(n)
  
  if (data_column_has_any_na && !include_na) {
    data_column_groups_n <- data_column_groups_n - 1L
  }
  
  data_column_groups_n
}

# Check if a column of numerical values is fully integer-like
is_column_integerlike <- function(data_column) {
  
  data_vals <- 
    data_column %>%
    dplyr::rename(a__ = 1) %>%
    dplyr::group_by(a__) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::pull()
  
  for (i in seq_along(data_vals)) {
    
    if (!rlang::is_integerish(data_vals[i])) {
      return(FALSE)
    }
  }
  
  TRUE
}

# Get the types for a column; this provides one or more R types and also
# the SQL type if the `data_column` comes from a database source
get_column_types <- function(data_column) {
  
  column_info <- get_tbl_information(tbl = data_column)
  
  column_info["tbl_src_details"] <- NULL
  column_info["db_tbl_name"] <- NULL
  
  names(column_info) <- c("tbl_src", "name", "r_col_type", "db_col_type")

  column_info
}

get_string_length_stats <- function(data_column) {
  
  data_column %>%
    dplyr::rename(a__ = 1) %>%
    dplyr::mutate(str_length__ = nchar(a__)) %>%
    dplyr::group_by() %>%
    dplyr::summarize(
      min = min(str_length__, na.rm = TRUE),
      mean = mean(str_length__, na.rm = TRUE),
      max = max(str_length__, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    as.list()
}

col_name_possibly__id <- function(col_name) {
  
  patterns <- 
    c(
      "^(GU|gu|Gu|gU|UU|uu|Uu|uU)?(i|I)(d|D)$",
      "^(GU|gu|Gu|gU|UU|uu|Uu|uU)?(i|I)(d|D)[ \\._]",
      "[ \\._](GU|gu|Gu|gU|UU|uu|Uu|uU)?(i|I)(d|D)$",
      "[ \\._](GU|gu|Gu|gU|UU|uu|Uu|uU)?(i|I)(d|D)[ \\._]",
      "[a-z]ID$",
      "^Id[A-Z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__cat <- function(col_name) {
  
  patterns <- 
    c(
      "^(CATG?|Catg?|catg?|TYPE|Type|type)$",
      "^(CATG?|Catg?|catg?|TYPE|Type|type)[ \\._]",
      "[ \\._](CATG?|Catg?|catg?|TYPE|Type|type)$",
      "[ \\._](CATG?|Catg?|catg?|TYPE|Type|type)[ \\._]",
      "[a-z](CATG?|Catg?|TYPE|Type)$",
      "[A-Z](catg?|type)$",
      "^(Catg?|catg?|type|Type)[A-Z]",
      "^(CATG?|TYPE)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__country <- function(col_name) {
  
  patterns <- 
    c(
      "^(COUNTRY|Country|country|CTRY|Ctry|ctry)$",
      "^(COUNTRY|Country|country|CTRY|Ctry|ctry)[ \\._]",
      "[ \\._](COUNTRY|Country|country|CTRY|Ctry|ctry)$",
      "[ \\._](COUNTRY|Country|country|CTRY|Ctry|ctry)[ \\._]",
      "[a-z](COUNTRY|Country|CTRY|Ctry)$",
      "[A-Z](country|ctry)$",
      "^(Country|country|Ctry|ctry)[A-Z]",
      "^(COUTNRY|CTRY)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__subd <- function(col_name) {
  
  # kw_lower <-
  #   c("state", "province", "prov", "r.gio", "division", "district",
  #     "distrito", "land", "pref")
  # 
  # kw_upper <- toupper(kw_lower)
  # 
  # kw_title <- tools::toTitleCase(kw_lower)
  # 
  # kw_all_pipe <- 
  #   paste0("(", paste(c(kw_lower, kw_upper, kw_title), collapse = "|"), ")")
  # 
  # kw_ut_pipe <- 
  #   paste0("(", paste(c(kw_upper, kw_title), collapse = "|"), ")")
  # 
  # kw_lt_pipe <- 
  #   paste0("(", paste(c(kw_lower, kw_title), collapse = "|"), ")")
  #
  # kw_u_pipe <- 
  #   paste0("(", paste(kw_upper, collapse = "|"), ")")
  #
  # 
  # patterns <- 
  #   c(
  #     paste0("^", kw_all_pipe, "$"),
  #     paste0("^", kw_all_pipe, "[ \\._]"),
  #     paste0("[ \\._]", kw_all_pipe, "$"),
  #     paste0("[ \\._]", kw_all_pipe, "[ \\._]"),
  #     paste0("[a-z]", kw_ut_pipe, "$"),
  #     paste0("^", kw_lt_pipe, "[A-Z]")
  #   )
  
  patterns <- 
    c(
      "^(state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref)$",
      "^(state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref)[ \\._]",
      "[ \\._](state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref)$",
      "[ \\._](state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref)[ \\._]",
      "[a-z](STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref)$",
      "[A-Z](state|province|prov|r.gio|division|district|distrito|land|pref)$",
      "^(state|province|prov|r.gio|division|district|distrito|land|pref|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref)[A-Z]",
      "^(STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__ind <- function(col_name) {
  
  patterns <- 
    c(
      "^((i|I)(n|N)(d|D)|(i|I)(s|S))$",
      "^((i|I)(n|N)(d|D)|(i|I)(s|S))[ \\._]",
      "[ \\._]((i|I)(n|N)(d|D)|(i|I)(s|S))$",
      "[ \\._]((i|I)(n|N)(d|D)|(i|I)(s|S))[ \\._]",
      "[a-z](IS|IND)$",
      "[A-Z](is|ind)$",
      "^(Is|Ind)[A-Z]",
      "^(IS|IND)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__count <- function(col_name) {
  
  patterns <- 
    c(
      "^((n|N)|(c|C)(t|T)|(c|C)(o|O)(u|U)(n|N)(t|T))$",
      "^((n|N)|(c|C)(t|T)|(c|C)(o|O)(u|U)(n|N)(t|T))[ \\._]",
      "[ \\._]((n|N)|(c|C)(t|T)|(c|C)(o|O)(u|U)(n|N)(t|T))$",
      "[ \\._]((n|N)|(c|C)(t|T)|(c|C)(o|O)(u|U)(n|N)(t|T))[ \\._]",
      "[a-z](N|C(t|T|ount|OUNT))$",
      "[A-Z](n|ct|count)$",
      "^(n|Ct|Count)[A-Z]",
      "^(N|CT|COUNT)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__amount <- function(col_name) {
  
  patterns <- 
    c(
      "^((a|A)(m|M)(t|T)|(a|A)(m|M)(o|O)(u|U)(n|N)(t|T))$",
      "^((a|A)(m|M)(t|T)|(a|A)(m|M)(o|O)(u|U)(n|N)(t|T))[ \\._]",
      "[ \\._]((a|A)(m|M)(t|T)|(a|A)(m|M)(o|O)(u|U)(n|N)(t|T))$",
      "[ \\._]((a|A)(m|M)(t|T)|(a|A)(m|M)(o|O)(u|U)(n|N)(t|T))[ \\._]",
      "[a-z](Amt|Amount|AMT|AMOUNT)$",
      "[A-Z](amt|amount)$",
      "^(Amt|Amount|amt|amount)[A-Z]",
      "^(AMT|AMOUNT)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__value <- function(col_name) {
  
  patterns <- 
    c(
      "^((v|V)(a|A)(l|L)|(v|V)(a|A)(l|L)(u|U)(e|E))$",
      "^((v|V)(a|A)(l|L)|(v|V)(a|A)(l|L)(u|U)(e|E))[ \\._]",
      "[ \\._]((v|V)(a|A)(l|L)|(v|V)(a|A)(l|L)(u|U)(e|E))$",
      "[ \\._]((v|V)(a|A)(l|L)|(v|V)(a|A)(l|L)(u|U)(e|E))[ \\._]",
      "[a-z](Val|Value|VAL|VALUE)$",
      "[A-Z](val|value)$",
      "^(Val|Value|val|value)[A-Z]",
      "^(VAL|VALUE)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__date <- function(col_name) {
  
  patterns <- 
    c(
      "^((d|D)(t|T)|(d|D)(a|A)(t|T)(e|E))$",
      "^((d|D)(t|T)|(d|D)(a|A)(t|T)(e|E))[ \\._]",
      "[ \\._]((d|D)(t|T)|(d|D)(a|A)(t|T)(e|E))$",
      "[ \\._]((d|D)(t|T)|(d|D)(a|A)(t|T)(e|E))[ \\._]",
      "[a-z](Dt|Date|DT|DATE)$",
      "[A-Z](dt|date)$",
      "^(Dt|Date|dt|date)[A-Z]",
      "^(DT|DATE)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__datetime <- function(col_name) {
  
  patterns <- 
    c(
      "^(time|Time|TIME|datetime|Datetime|DATETIME|timestamp|Timestamp|TIMESTAMP)$",
      "^(time|Time|TIME|datetime|Datetime|DATETIME|timestamp|Timestamp|TIMESTAMP)[ \\._]",
      "[ \\._](time|Time|TIME|datetime|Datetime|DATETIME|timestamp|Timestamp|TIMESTAMP)$",
      "[ \\._](time|Time|TIME|datetime|Datetime|DATETIME|timestamp|Timestamp|TIMESTAMP)[ \\._]",
      "[a-z](Time|TIME|Datetime|DATETIME|Timestamp|TIMESTAMP)$",
      "^(time|Time|datetime|Datetime|timestamp|Timestamp)[A-Z]",
      "^(TIME|DATETIME|TIMESTAMP)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__latitude <- function(col_name) {
  
  patterns <- 
    c(
      "^((l|L)(a|A)(t|T)|(l|L)(a|A)(t|T)(i|I)(t|T)(u|U)(d|D)(e|E))$",
      "^((l|L)(a|A)(t|T)|(l|L)(a|A)(t|T)(i|I)(t|T)(u|U)(d|D)(e|E))[ \\._]",
      "[ \\._]((l|L)(a|A)(t|T)|(l|L)(a|A)(t|T)(i|I)(t|T)(u|U)(d|D)(e|E))$",
      "[ \\._]((l|L)(a|A)(t|T)|(l|L)(a|A)(t|T)(i|I)(t|T)(u|U)(d|D)(e|E))[ \\._]",
      "[a-z](L(at|atitude|AT|ATITUDE))$",
      "[A-Z](l(at|atitude))$",
      "^(Lat|lat|Latitude|latitude)[A-Z]",
      "^(LAT|LATITUDE)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__longitude <- function(col_name) {
  
  patterns <- 
    c(
      "^((l|L)(o|O)(n|N)(g|G)?|(l|L)(o|O)(n|N)(g|G)(i|I)(t|T)(u|U)(d|D)(e|E))$",
      "^((l|L)(o|O)(n|N)(g|G)?|(l|L)(o|O)(n|N)(g|G)(i|I)(t|T)(u|U)(d|D)(e|E))[ \\._]",
      "[ \\._]((l|L)(o|O)(n|N)(g|G)?|(l|L)(o|O)(n|N)(g|G)(i|I)(t|T)(u|U)(d|D)(e|E))$",
      "[ \\._]((l|L)(o|O)(n|N)(g|G)?|(l|L)(o|O)(n|N)(g|G)(i|I)(t|T)(u|U)(d|D)(e|E))[ \\._]",
      "[a-z](L(on|ong|ongitude|ON|ONG|ONGITUDE))$",
      "[A-Z](l(on|ong|ongitude))$",
      "^(Lon|lon|Longitude|longitude)[A-Z]",
      "^(LON|LONG|LONGITUDE)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}




check_patterns <- function(x, patterns) {
  
  for (pat in patterns) {
    
    if (grepl(pat, x)) {
      return(TRUE)
    } 
  }
  
  FALSE
}


get_column_role_string <- function(data_column) {
  
  # This is to be used on string-based column to try and
  # get at the role of the column
  
  column_name <- colnames(data_column)
  row_count <- get_table_total_rows(data_column)
  cardinality <- get_column_cardinality(data_column)
  column_samp <- get_non_null_col_sample(data_column)
  length_range <- get_string_length_range(column_samp)
  
  if (col_name_possibly__id(column_name)) {
    
    # If the column prominently has 'ID' in it's name, it's
    # best to believe it's an ID column of some sort
    
    return("id.string")
  }
  
  if (col_name_possibly_cat(column_name)) {
    
    # This is a category or type column judging by the name;
    # the naming is all-important in this case
  }
  
  if (col_name_possibly__country(column_name)) {
    
    # We might assume this is a column containing country names;
    # it's useful to further probe how this is represented since
    # there are a few standardized ways to refer to countries
  }
  
  if (col_name_possibly__subd(column_name)) {
    
    # The guess here is that this column may containing subdivisions;
    # of one or maybe more countries; this warrant further investigation
    # since even first-level subdivisions are standardized to great extent
  }
  
  if (col_name_possibly__currency_type(column_name)) {
    
    # 
  }

  if (col_name_possibly__url(column_name)) {
    
    # This could very well be a URL and it's easy to check for that
  }
    
  if (col_name_possibly__email(column_name)) {
    
    # This could very well be an email address; this isn't too
    # difficult to verify
  }
  
  if (col_name_possibly__ip(column_name)) {
    
    # An IP address? Might possibly be the case. Let's check on that
  }
  
  if (col_name_possibly__path(column_name)) {
    
    # A path or directory is what this column may or may not have
    
  }
  
  
  if (cardinality < 50 && row_count > 200) {
    return("string.categorical")
  }
  
  return("string")
}
