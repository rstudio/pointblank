#------------------------------------------------------------------------------#
# 
#                 _         _    _      _                _    
#                (_)       | |  | |    | |              | |   
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |                                                        
#  |_|                                                        
#  
#  This file is part of the 'rstudio/pointblank' project.
#  
#  Copyright (c) 2017-2023 pointblank authors
#  
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
# 
#------------------------------------------------------------------------------#


get_column_roles <- function(data) {
  
  if (!is_a_table_object(data)) {
    return(NULL)
  }
  
  col_types <- get_column_types(data)
  
  col_roles <- character(length(col_types$name))
  
  for (i in seq_along(col_types$name)) {
    
    col_type_i <- col_types$r_col_type[i]
    col_name_i <- col_types$name[i]
    data_column <- data %>% dplyr::select({{ col_name_i }})
    
    col_role_i <- 
      switch(
        col_type_i,
        character = get_column_role_character(data_column),
        numeric = get_column_role_numeric(data_column),
        factor = "string.categorical",
        ordered = "string.ordinal",
        integer = "integer.discrete",
        integer64 = "integer.discrete",
        logical = "boolean.logical.categorical",
        POSIXct = "datetime.continuous",
        Date = "date.discrete",
        list = "list_object",
        NA_character_
      )
    
    col_roles[i] <- col_role_i 
  }
  
  col_roles
}

# This returns a `data_column` with NULL values removed using the
# sampling procedure of `first_n` or `random_n`
get_non_null_col_sample <- function(
    data_column,
    sample_type = c("first_n", "random_n"),
    sample_n = 1000,
    sample_min_n = 1,
    make_distinct = FALSE
) {
  
  sample_type <- match.arg(sample_type)
  
  # Stop function if there isn't exactly one column in `data_column`
  if (ncol(data_column) != 1) {
    
    stop(
      "The table given as `data_column` must have exactly one column.",
      call. = FALSE
    )
  }
  
  # Filter out all NA/NULL values from the `data_column`
  data_column <- 
    data_column %>%
    dplyr::rename(a__ = 1) %>%
    dplyr::filter(!is.na(a__))
  
  # Obtain a subsample of non-NULL values in the column
  if (sample_type == "first_n") {
    data_column <- utils::head(data_column, sample_n)
  } else {
    data_column <- dplyr::slice_sample(data_column, n = sample_n)
  }
  
  if (make_distinct) {
    data_column <- dplyr::distinct(data_column)
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
get_column_cardinality <- function(
    data_column,
    include_na = TRUE
) {
  
  data_column_groups <-
    data_column %>%
    dplyr::rename(a__ = 1) %>%
    dplyr::distinct()

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
    dplyr::pull(n) %>%
    as.integer()
  
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
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull(a__)

  rlang::is_integerish(data_vals)
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

# nolint start

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

col_name_possibly__country <- function(col_name) {
  
  patterns <- 
    c(
      "^(COUNTRY|Country|country|CTRY|Ctry|ctry|(iso|ISO)2|(iso|ISO)3)$",
      "^(COUNTRY|Country|country|CTRY|Ctry|ctry|(iso|ISO)2|(iso|ISO)3)[ \\._]",
      "[ \\._](COUNTRY|Country|country|CTRY|Ctry|ctry|(iso|ISO)2|(iso|ISO)3)$",
      "[ \\._](COUNTRY|Country|country|CTRY|Ctry|ctry|(iso|ISO)2|(iso|ISO)3)[ \\._]",
      "[a-z](COUNTRY|Country|CTRY|Ctry|ISO2|ISO3)$",
      "[A-Z](country|ctry|iso2|iso3)$",
      "^(Country|country|Ctry|ctry|(iso|ISO)2|(iso|ISO)3)[A-Z]",
      "^(COUTNRY|CTRY|(iso|ISO)2|(iso|ISO)3)[a-z]"
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
      "^(state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref|NAME|name)$",
      "^(state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref|NAME|name)[ \\._]",
      "[ \\._](state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref|NAME|name)$",
      "[ \\._](state|province|prov|r.gio|division|district|distrito|land|pref|STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref|NAME|name)[ \\._]",
      "[a-z](STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref|NAME|name)$",
      "[A-Z](state|province|prov|r.gio|division|district|distrito|land|pref)$",
      "^(state|province|prov|r.gio|division|district|distrito|land|pref|State|Province|Prov|R.gio|Division|District|Distrito|Land|Pref)[A-Z]",
      "^(STATE|PROVINCE|PROV|R.GIO|DIVISION|DISTRICT|DISTRITO|LAND|PREF)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__currency_type <- function(col_name) {
  
  patterns <- 
    c(
      "^(CURRENCY|Currency|currency|CURR?|Curr?|curr?)$",
      "^(CURRENCY|Currency|currency|CURR?|Curr?|curr?|CY|cy)[ \\._]",
      "[ \\._](CURRENCY|Currency|currency|CURR?|Curr?|curr?|CY|cy)$",
      "[ \\._](CURRENCY|Currency|currency|CURR?|Curr?|curr?|CY|cy)[ \\._]",
      "[a-z](CURRENCY|Currency|CURR?|Curr?|CY)$",
      "[A-Z](currency|curr?|cy)$",
      "^(Currency|currency|Curr?|curr?|cy)[A-Z]",
      "^(CURRENCY|CURR?|CY)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__url <- function(col_name) {
  
  patterns <- 
    c(
      "^(URL|url|ADDR|Addr|addr|WEB|Web|web|SITE|Site|site|LINK|Link|link|PAGE|Page|page|WWW|www|Net|net)$",
      "^(URL|url|ADDR|Addr|addr|WEB|Web|web|SITE|Site|site|LINK|Link|link|PAGE|Page|page|WWW|www|Net|net)[ \\._]",
      "[ \\._](URL|url|ADDR|Addr|addr|WEB|Web|web|SITE|Site|site|LINK|Link|link|PAGE|Page|page|WWW|www|Net|net)$",
      "[ \\._](URL|url|ADDR|Addr|addr|WEB|Web|web|SITE|Site|site|LINK|Link|link|PAGE|Page|page|WWW|www|Net|net)[ \\._]",
      "[a-z](URL|ADDR|Addr|WEB|Web|SITE|Site|LINK|Link|PAGE|Page|WWW|Net)$",
      "[A-Z](url|addr|web|site|link|page|www|net)$",
      "^(url|Addr|addr|Web|web|Site|site|Link|link|Page|page|www|Net|net)[A-Z]",
      "^(URL|ADDR|WEB|SITE|LINK|PAGE|WWW)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__email <- function(col_name) {
  
  patterns <- 
    c(
      "^(E?-?MAIL|E?-?mail|e?-?mail|ADDR|Addr|addr)$",
      "^(E?-?MAIL|E?-?mail|e?-?mail|ADDR|Addr|addr)[ \\._]",
      "[ \\._](E?-?MAIL|E?-?mail|e?-?mail|ADDR|Addr|addr)$",
      "[ \\._](E?-?MAIL|E?-?mail|e?-?mail|ADDR|Addr|addr)[ \\._]",
      "[a-z](E?-?MAIL|E?-?mail|ADDR|Addr)$",
      "[A-Z](e?-?mail|addr)$",
      "^(E?-?mail|e?-?mail|Addr|addr)[A-Z]",
      "^(E?-?MAIL|ADDR)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

col_name_possibly__ip <- function(col_name) {
  
  patterns <- 
    c(
      "^(IP|ip|IPv[46]|ipv[46]|HOST|Host|host|NET|Net|net)$",
      "^(IP|ip|IPv[46]|ipv[46]|HOST|Host|host|NET|Net|net)[ \\._]",
      "[ \\._](IP|ip|IPv[46]|ipv[46]|HOST|Host|host|NET|Net|net)$",
      "[ \\._](IP|ip|IPv[46]|ipv[46]|HOST|Host|host|NET|Net|net)[ \\._]",
      "[a-z](IP|IPv[46]|HOST|Host|NET|Net)$",
      "[A-Z](ip|ipv[46]|host|net)$",
      "^(ip|IPv[46]|ipv[46]|Host|host|Net|net)[A-Z]",
      "^(IP|IPv[46]|ipv[46]|HOST|NET)[a-z]"
    )
  
  check_patterns(col_name, patterns)
}

# nolint end

check_patterns <- function(x, patterns) {
  
  for (pat in patterns) {
    
    if (grepl(pat, x)) {
      return(TRUE)
    } 
  }
  
  FALSE
}

get_column_role_character <- function(data_column) {
  
  column_name <- colnames(data_column)
  row_count <- get_table_total_rows(data_column)
  cardinality <- get_column_cardinality(data_column)
  column_samp <- get_non_null_col_sample(data_column)
  
  if (is.null(column_samp)) {
    return("string")
  }
  
  length_stats <- get_string_length_stats(column_samp)
  
  # Get the distinct non-null items from the column sample
  column_items <- 
    column_samp %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  if (col_name_possibly__country(column_name)) {
    
    # We might assume this is a column containing country names;
    # it's useful to further probe how this is represented since
    # there are a few standardized ways to refer to countries
    
    if (any(column_items %in% countries$name)) {
      
      prop_column_items <-
        sum(column_items %in% countries$name) / length(column_items)
      
      if (prop_column_items >= 0.5) {
        return("country:iso3166-1-esn.string.categorical")
      }
    }
    
    if (any(column_items %in% countries$alpha_3)) {
      
      prop_column_items <-
        sum(column_items %in% countries$alpha_3) / length(column_items)
      
      if (prop_column_items >= 0.9) {
        return("country:iso3166-1-a-3.string.categorical")
      }
    }
    
    if (any(column_items %in% countries$alpha_2)) {
      
      prop_column_items <-
        sum(column_items %in% countries$alpha_2) / length(column_items)
      
      if (prop_column_items >= 0.75) {
        return("country:iso3166-1-a-2.string.categorical")
        
      }
    }
  }
  
  if (col_name_possibly__subd(column_name)) {
    
    # The guess here is that this column may containing subdivisions;
    # of one or maybe more countries; this warrant further investigation
    # since even first-level subdivisions are standardized to great extent

    # Extract all fully-alphabetical subdivision names to
    # serve as a large set
    subd_names <- 
      subdivisions %>% 
      dplyr::select(subd_name) %>%
      dplyr::filter(grepl("[A-Z][A-Z]", subd_name)) %>% 
      dplyr::pull()
    
    # If any of the column items matches any one of the
    # `subd_names` then further inspect to see if there's a
    # country-specific match
    if (any(column_items %in% subd_names)) {
      
      frac <- c()
      
      for (co in names(subd_list_main)) {
        
        # obtain fractional amounts of subdivision names in each main country
        frac_co <- 
          sum(subd_list_main[[co]] %in% column_items) /
          length(subd_list_main[[co]])
        
        frac_co <- stats::setNames(frac_co, co)
        
        frac <- c(frac, frac_co)
      }
      
      # Sort `frac` by decreasing value
      frac <- sort(frac, decreasing = TRUE)
      
      # Get the non-zero values
      frac <- frac[frac != 0]
      
      # Get the length of non-zero values
      frac_length <- length(frac)
      
      if (frac_length == 1) {
        c_code <- names(frac)
        return(
          paste0("country_subd:iso3166-2[", c_code, "].string.categorical")
        )
      }
      
      # If there's no country-specific matching, we're still
      # somewhat sure this column has to do with country subdivisions
      return("country_subd:iso3166-2.string.categorical")
    }
    
    # Extract all subdivision short names to serve as a large set
    subd_names <- 
      subdivisions %>%
      dplyr::filter(country_alpha_3 %in% names(subd_list_main)) %>%
      dplyr::pull(name_en)
    
    # If any of the column items matches any one of the
    # `subd_names` then further inspect to see if there's a
    # country-specific match
    if (any(column_items %in% subd_names) && 
        sum(column_items %in% subd_names) > 1) {
      
      frac <- c()
      
      for (co in names(subd_list_main)) {
        
        subd_list_co <- 
          subdivisions %>%
          dplyr::filter(country_alpha_3 == {{ co }}) %>%
          dplyr::pull(name_en)
        
        # obtain fractional amounts of subdivision names in each main country
        frac_co <- 
          sum(subd_list_co %in% column_items) /
          length(subd_list_co)
        
        frac_co <- stats::setNames(frac_co, co)
        
        frac <- c(frac, frac_co)
      }
      
      # Sort `frac` by decreasing value
      frac <- sort(frac, decreasing = TRUE)
      
      # Get the non-zero values
      frac <- frac[frac != 0]
      
      # Get the length of non-zero values
      frac_length <- length(frac)
      
      if (frac_length == 1) {
        c_code <- names(frac)
        return(
          paste0("country_subd:iso3166-2-sn[", c_code, "].string.categorical")
        )
      }
      
      # If there's no country-specific matching, we're still
      # somewhat sure this column has to do with country subdivisions
      return("country_subd:iso3166-2-sn.string.categorical")
    }
  }
  
  if (col_name_possibly__currency_type(column_name)) {
    
    # Perhaps this column contains currency types and the only way to
    # be more certain is to check against 3-letter and 3-digit currency
    # codes
    
    prop_column_items_a <-
      sum(column_items %in% currencies$currency_a) / 
      length(currencies$currency_a)
    
    if (prop_column_items_a >= 0.9) {
      return("currency:iso4217-a.string.categorical")
    }
    
    prop_column_items_n <-
      sum(column_items %in% currencies$currency_n) / 
      length(currencies$currency_n)
    
    if (prop_column_items_n >= 0.9) {
      return("currency:iso4217-num.string.categorical")
    }
  }

  if (col_name_possibly__url(column_name)) {
    
    # This could very well be a URL and it's easy to check for that
    
    # Check that values in the column follow URL formatting rules
    prop_column_items_url <-
      sum(grepl(regex_url(), column_items)) / 
      length(column_items)
    
    if (prop_column_items_url >= 0.8) {
      return("url.string")
    }
  }
    
  if (col_name_possibly__email(column_name)) {
    
    # This could very well be an email address; this isn't too
    # difficult to verify
    
    # Check that values in the column follow email formatting rules
    prop_column_items_email <-
      sum(grepl(regex_email(), column_items)) / 
      length(column_items)
    
    if (prop_column_items_email >= 0.8) {
      return("email.string")
    }
  }
  
  if (col_name_possibly__ip(column_name)) {
    
    # An IP address? Might possibly be the case. Let's check on that
    
    # Check column for IPv4 addresses
    prop_column_items_ipv4 <-
      sum(grepl(regex_ipv4_address(), column_items)) / 
      length(column_items)
    
    if (prop_column_items_ipv4 >= 0.9) {
      return("ip.string")
    }
    
    # Check column for IPv6 addresses
    prop_column_items_ipv6 <-
      sum(grepl(regex_ipv6_address(), column_items)) / 
      length(column_items)
    
    if (prop_column_items_ipv6 >= 0.1) {
      return("ip.string")
    }
  }
  
  if (col_name_possibly__id(column_name)) {
    
    # If the column prominently has 'ID' in it's name, it's
    # best to believe it's an ID column of some sort
    
    return("id.string")
  }
  
  if (col_name_possibly__cat(column_name)) {
    
    # This is a category or type column judging by the name;
    # the naming is all-important in this case
    
    return("string.categorical")
  }
  
  if (cardinality < 50 && row_count > 200) {
    return("string.categorical")
  }
  
  if (col_name_possibly__datetime(column_name)) {
    
    # Determine if the column sample has strings that are
    # parsable as datetime values

    parsable <- FALSE
    
    for (s_fmt in strptime_8601_formats) {
      
      parsed <- strptime(column_items, format = s_fmt)
      
      prop_parsed <- sum(!is.na(parsed)) / length(parsed)
      
      if (prop_parsed > 0.8) {
        parsable <- TRUE
        break
      }
    }
    
    if (parsable) {
      return("datetime.string")
    }
  }
  
  if (col_name_possibly__date(column_name)) {
    
    # Determine if the column sample has strings that are
    # parsable as datetime values
    
    parsable <- FALSE
    
    for (s_fmt in strptime_date_formats) {
      
      parsed <- strptime(column_items, format = s_fmt)
      
      prop_parsed <- sum(!is.na(parsed)) / length(parsed)
      
      if (prop_parsed > 0.8) {
        parsable <- TRUE
        break
      }
    }
    
    if (parsable) {
      return("date.string")
    }
    
  }
  
  return("string")
}

get_column_role_numeric <- function(data_column) {
  
  column_name <- colnames(data_column)
  row_count <- get_table_total_rows(data_column)
  cardinality <- get_column_cardinality(data_column)
  column_samp <- get_non_null_col_sample(data_column)
  
  if (is.null(column_samp)) {
    return("numeric")
  }
  
  # Initialize the role fragment
  role <- ""
  
  # Get the distinct non-null items from the column sample
  column_items <- 
    column_samp %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  if (col_name_possibly__latitude(column_name)) {
    
    # Determine if the column sample has values that mostly lie
    # within the valid range of latitude values
    
    within_bounds <- column_items <= 90 & column_items >= -90
    
    prop_within_bounds <- sum(!is.na(within_bounds)) / length(within_bounds)
    
    if (prop_within_bounds > 0.8) {
      role <- "geo:latitude."
    }
  }
  
  if (col_name_possibly__longitude(column_name)) {
    
    # Determine if the column sample has values that mostly lie
    # within the valid range of longitude values
    
    within_bounds <- column_items <= 180 & column_items >= -180
    
    prop_within_bounds <- sum(!is.na(within_bounds)) / length(within_bounds)
    
    if (prop_within_bounds > 0.8) {
      role <- "geo:longitude."
    }
  }
  
  if (row_count > 200 && is_column_integerlike(data_column = column_samp)) {
    
    return(paste0(role, "numeric.discrete"))
    
  } else {
    return(paste0(role, "numeric.continuous"))
  }
  
  return(paste0(role, "numeric"))
}

strptime_8601_formats <-
  c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S",
    "%Y/%m/%d %H:%M:%S"
  )

strptime_date_formats <-
  c(
    "%Y-%m-%d",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )
