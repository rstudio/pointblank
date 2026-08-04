test_that("cdisc_rule_loader loads the bundled catalog", {

  skip_if_not_installed("jsonlite")

  rules <- cdisc_load_rules("sdtmig", "3-4")
  expect_true(length(rules) > 400)
  expect_true(all(vapply(rules, function(r) nzchar(r$core_id), logical(1))))

  # Every rule has a rule_type

  rule_types <- unique(vapply(rules, function(r) r$rule_type, character(1)))
  expect_true("RECORD_CHECK" %in% rule_types)
  expect_true("VARIABLE_METADATA_CHECK" %in% rule_types)
})

test_that("cdisc_rule_loader filters by rule_types", {

  skip_if_not_installed("jsonlite")

  rules <- cdisc_load_rules("sdtmig", "3-4", rule_types = "RECORD_CHECK")
  types <- unique(vapply(rules, function(r) r$rule_type, character(1)))
  expect_equal(types, "RECORD_CHECK")
  expect_true(length(rules) > 0)
})

test_that("cdisc_rule_loader errors on missing catalog", {

  skip_if_not_installed("jsonlite")

  expect_error(
    cdisc_load_rules("sdtmig", "99-99"),
    "No bundled rule catalog"
  )
})

test_that("cdisc_rule_from_list fills defaults", {

  minimal <- list(core_id = "TEST-001", rule_type = "RECORD_CHECK")
  rule <- cdisc_rule_from_list(minimal)
  expect_equal(rule$core_id, "TEST-001")
  expect_equal(rule$executability, "Fully Executable")
  expect_equal(rule$sensitivity, "Error")
  expect_equal(rule$description, "")
  expect_equal(rule$domains, list())
  expect_equal(rule$operations, list())
  expect_equal(rule$conditions, list())
})

test_that("cdisc_rules_available returns valid pairs", {

  skip_if_not_installed("jsonlite")

  avail <- cdisc_rules_available()
  expect_true(length(avail) > 0)
  expect_true(all(vapply(avail, function(p) nzchar(p$standard), logical(1))))
})

test_that("cdisc_catalog_path constructs correct path", {
  path <- cdisc_catalog_path("sdtmig", "3-4")
  expect_true(grepl("sdtmig-3-4\\.json$", path))

  # Dots converted to hyphens
  path2 <- cdisc_catalog_path("sdtmig", "3.4")
  expect_true(grepl("sdtmig-3-4\\.json$", path2))
})

# ── Controlled Terminology ───────────────────────────────────────────────────

test_that("cdisc_ct_load loads the bundled CT package", {

  skip_if_not_installed("jsonlite")

  ct <- cdisc_ct_load_default()
  expect_s3_class(ct, "cdisc_ct")
  expect_true(length(ct$codelists) > 0)
  expect_true(length(ct$packages) == 1)
})

test_that("cdisc_ct_get_codelist is case-insensitive", {

  skip_if_not_installed("jsonlite")

  ct <- cdisc_ct_load_default()
  sex_terms <- cdisc_ct_get_codelist(ct, "sex")
  sex_terms_upper <- cdisc_ct_get_codelist(ct, "SEX")
  expect_identical(sex_terms, sex_terms_upper)
})

test_that("cdisc_ct_get_codelist returns NULL for unknown codelist", {

  ct <- structure(list(codelists = list(), packages = "test"), class = "cdisc_ct")
  expect_null(cdisc_ct_get_codelist(ct, "NONEXISTENT"))
})

test_that("cdisc_ct_available returns character vector", {
  avail <- cdisc_ct_available()
  expect_type(avail, "character")
})

# ── Evaluator ────────────────────────────────────────────────────────────────

test_that("cdisc_evaluate_conditions returns all-FALSE for empty conditions", {

  df <- data.frame(X = 1:3)
  result <- cdisc_evaluate_conditions(df, list())
  expect_equal(result, c(FALSE, FALSE, FALSE))
})

test_that("evaluator handles equal_to operator", {

  df <- data.frame(SEX = c("M", "F", "M", "X"), stringsAsFactors = FALSE)
  cond <- list(name = "SEX", operator = "equal_to", value = "X")
  result <- cdisc_evaluate_conditions(df, cond)
  expect_equal(which(result), 4L)
})

test_that("evaluator handles is_null / is_not_null", {

  df <- data.frame(AGE = c(25, NA, 30))
  result_null <- cdisc_evaluate_conditions(
    df, list(name = "AGE", operator = "is_null")
  )
  expect_equal(which(result_null), 2L)

  result_not_null <- cdisc_evaluate_conditions(
    df, list(name = "AGE", operator = "is_not_null")
  )
  expect_equal(which(result_not_null), c(1L, 3L))
})

test_that("evaluator handles is_in / not_in", {

  df <- data.frame(SEX = c("M", "F", "U", "X"), stringsAsFactors = FALSE)
  cond_in <- list(name = "SEX", operator = "is_in", value = list("M", "F"))
  result <- cdisc_evaluate_conditions(df, cond_in)
  expect_equal(which(result), c(1L, 2L))

  cond_not_in <- list(name = "SEX", operator = "not_in", value = list("M", "F"))
  result2 <- cdisc_evaluate_conditions(df, cond_not_in)
  expect_equal(which(result2), c(3L, 4L))
})

test_that("evaluator handles contains / not_contains", {

  df <- data.frame(TERM = c("Headache", "Nausea", "Head injury"),
                   stringsAsFactors = FALSE)
  cond <- list(name = "TERM", operator = "contains", value = "Head")
  result <- cdisc_evaluate_conditions(df, cond)
  expect_equal(which(result), c(1L, 3L))
})

test_that("evaluator handles starts_with / ends_with", {

  df <- data.frame(CODE = c("AE001", "AE002", "LB001"),
                   stringsAsFactors = FALSE)
  cond <- list(name = "CODE", operator = "starts_with", value = "AE")
  result <- cdisc_evaluate_conditions(df, cond)
  expect_equal(which(result), c(1L, 2L))
})

test_that("evaluator handles matches_regex", {

  df <- data.frame(VAL = c("abc123", "def", "ghi456"),
                   stringsAsFactors = FALSE)
  cond <- list(name = "VAL", operator = "matches_regex", value = "\\d+")
  result <- cdisc_evaluate_conditions(df, cond)
  expect_equal(which(result), c(1L, 3L))
})

test_that("evaluator handles equal_to_column", {

  df <- data.frame(A = c(1, 2, 3), B = c(1, 9, 3))
  cond <- list(name = "A", operator = "equal_to_column", value = "B")
  result <- cdisc_evaluate_conditions(df, cond)
  expect_equal(which(result), c(1L, 3L))
})

test_that("evaluator handles all/any/not composition", {

  df <- data.frame(X = c(1, 2, 3, 4, 5))

  cond_all <- list(
    all = list(
      list(name = "X", operator = "greater_than", value = 2),
      list(name = "X", operator = "less_than", value = 5)
    )
  )
  result <- cdisc_evaluate_conditions(df, cond_all)
  expect_equal(which(result), c(3L, 4L))

  cond_any <- list(
    any = list(
      list(name = "X", operator = "equal_to", value = 1),
      list(name = "X", operator = "equal_to", value = 5)
    )
  )
  result2 <- cdisc_evaluate_conditions(df, cond_any)
  expect_equal(which(result2), c(1L, 5L))

  cond_not <- list(
    not = list(name = "X", operator = "equal_to", value = 3)
  )
  result3 <- cdisc_evaluate_conditions(df, cond_not)
  expect_equal(which(result3), c(1L, 2L, 4L, 5L))
})

test_that("evaluator errors on unknown column", {

  df <- data.frame(X = 1:3)
  cond <- list(name = "NONEXISTENT", operator = "equal_to", value = 1)
  expect_error(cdisc_evaluate_conditions(df, cond), "Column not found")
})

test_that("evaluator handles factor columns", {

  df <- data.frame(SEX = factor(c("M", "F", "M")))
  cond <- list(name = "SEX", operator = "equal_to", value = "F")
  result <- cdisc_evaluate_conditions(df, cond)
  expect_equal(which(result), 2L)
})

test_that("cdisc_is_iso8601 validates dates correctly", {

  expect_true(cdisc_is_iso8601("2024"))
  expect_true(cdisc_is_iso8601("2024-01"))
  expect_true(cdisc_is_iso8601("2024-01-15"))
  expect_true(cdisc_is_iso8601("2024-01-15T10:30"))
  expect_true(cdisc_is_iso8601("2024-01-15T10:30:00"))
  expect_true(cdisc_is_iso8601("2024-01-15T10:30:00Z"))
  expect_true(cdisc_is_iso8601("2024-01-15T10:30:00+05:30"))
  expect_true(cdisc_is_iso8601("2024-01-15T10:30:00.123"))

  expect_false(cdisc_is_iso8601("not-a-date"))
  expect_false(cdisc_is_iso8601(""))
  expect_false(cdisc_is_iso8601(NA_character_))
  expect_false(cdisc_is_iso8601("01-15-2024"))
  expect_false(cdisc_is_iso8601("2024/01/15"))
})

# ── Operations ───────────────────────────────────────────────────────────────

test_that("codelist_check operation works", {

  ct <- structure(
    list(
      codelists = list(SEX = c("M", "F", "U", "UNDIFFERENTIATED")),
      packages = "test"
    ),
    class = "cdisc_ct"
  )

  df <- data.frame(
    SEX = c("M", "F", "X", NA, ""),
    stringsAsFactors = FALSE
  )

  result <- .cdisc_op_codelist_check(
    df, list(column = "SEX", codelist = "SEX"), ct, list()
  )

  expect_equal(result[["_pb_SEX_valid"]], c(TRUE, TRUE, FALSE, TRUE, TRUE))
})

test_that("codelist_check is case-insensitive", {

  ct <- structure(
    list(codelists = list(SEX = c("M", "F")), packages = "test"),
    class = "cdisc_ct"
  )

  df <- data.frame(SEX = c("m", "f", "X"), stringsAsFactors = FALSE)
  result <- .cdisc_op_codelist_check(
    df, list(column = "SEX", codelist = "SEX"), ct, list()
  )
  expect_equal(result[["_pb_SEX_valid"]], c(TRUE, TRUE, FALSE))
})

test_that("iso8601_check operation works", {

  df <- data.frame(
    RFSTDTC = c("2024-01-15", "not-a-date", "", NA),
    stringsAsFactors = FALSE
  )
  result <- .cdisc_op_iso8601_check(
    df, list(column = "RFSTDTC"), NULL, list()
  )
  expect_equal(
    result[["_pb_RFSTDTC_iso8601"]],
    c(TRUE, FALSE, TRUE, TRUE)
  )
})

test_that("consistency_check operation works", {

  df <- data.frame(
    DOMAIN = c("DM", "DM", "DM", "AE"),
    stringsAsFactors = FALSE
  )
  result <- .cdisc_op_consistency_check(
    df, list(column = "DOMAIN"), NULL, list()
  )
  expect_equal(
    result[["_pb_DOMAIN_consistent"]],
    c(TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("unique_per_subject operation works", {

  df <- data.frame(
    USUBJID = c("S1", "S1", "S2", "S2"),
    AESEQ = c(1, 1, 1, 2),
    stringsAsFactors = FALSE
  )
  result <- .cdisc_op_unique_per_subject(
    df, list(column = "AESEQ"), NULL, list()
  )
  # S1 has duplicate AESEQ=1, S2 is fine
  expect_equal(result[["_pb_AESEQ_unique"]], c(FALSE, FALSE, TRUE, TRUE))
})

test_that("column_presence operation works", {

  df <- data.frame(SEX = c("M"), stringsAsFactors = FALSE)
  result <- .cdisc_op_column_presence(
    df, list(column = "SEX"), NULL, list()
  )
  expect_true(result[["_pb_SEX_present"]])

  result2 <- .cdisc_op_column_presence(
    df, list(column = "NONEXISTENT"), NULL, list()
  )
  expect_false(result2[["_pb_NONEXISTENT_present"]])
})

test_that("has_required_variables operation works", {

  df <- data.frame(STUDYID = "S1", DOMAIN = "DM", stringsAsFactors = FALSE)
  result <- .cdisc_op_has_required_variables(
    df, list(variables = list("STUDYID", "DOMAIN", "MISSING")), NULL, list()
  )
  expect_true(result[["_pb_STUDYID_present"]])
  expect_true(result[["_pb_DOMAIN_present"]])
  expect_false(result[["_pb_MISSING_present"]])
})

test_that("valid_variable_order operation works", {

  df <- data.frame(STUDYID = "S1", DOMAIN = "DM", USUBJID = "U1",
                   stringsAsFactors = FALSE)
  result <- .cdisc_op_valid_variable_order(
    df, list(expected_order = list("STUDYID", "DOMAIN", "USUBJID")),
    NULL, list()
  )
  expect_true(result[["_pb_variable_order_valid"]])

  # Wrong order
  df2 <- data.frame(DOMAIN = "DM", STUDYID = "S1", USUBJID = "U1",
                    stringsAsFactors = FALSE)
  result2 <- .cdisc_op_valid_variable_order(
    df2, list(expected_order = list("STUDYID", "DOMAIN", "USUBJID")),
    NULL, list()
  )
  expect_false(result2[["_pb_variable_order_valid"]])
})

test_that("variable_type_check operation works", {

  df <- data.frame(AGE = c(25, 30), SEX = c("M", "F"),
                   stringsAsFactors = FALSE)
  result <- .cdisc_op_variable_type_check(
    df, list(column = "AGE", expected_type = "numeric"), NULL, list()
  )
  expect_true(all(result[["_pb_AGE_type_valid"]]))

  result2 <- .cdisc_op_variable_type_check(
    df, list(column = "SEX", expected_type = "character"), NULL, list()
  )
  expect_true(all(result2[["_pb_SEX_type_valid"]]))

  result3 <- .cdisc_op_variable_type_check(
    df, list(column = "AGE", expected_type = "character"), NULL, list()
  )
  expect_true(all(!result3[["_pb_AGE_type_valid"]]))
})

test_that("define-XML stub operations always return TRUE", {

  df <- data.frame(SEX = c("M", "F"), stringsAsFactors = FALSE)

  r1 <- .cdisc_op_define_var_declared(
    df, list(column = "SEX"), NULL, list()
  )
  expect_true(all(r1[["_pb_SEX_in_define"]]))

  r2 <- .cdisc_op_define_required_check(
    df, list(column = "SEX"), NULL, list()
  )
  expect_true(all(r2[["_pb_SEX_mandatory_ok"]]))

  r3 <- .cdisc_op_define_codelist_check(
    df, list(column = "SEX"), NULL, list()
  )
  expect_true(all(r3[["_pb_SEX_define_valid"]]))

  r4 <- .cdisc_op_define_type_check(
    df, list(column = "SEX"), NULL, list()
  )
  expect_true(all(r4[["_pb_SEX_define_type_ok"]]))
})

# ── Result structures ────────────────────────────────────────────────────────

test_that("cdisc_conformance_result has correct class", {

  result <- cdisc_conformance_result(
    standard = "sdtmig", version = "3-4",
    ct_packages = "test",
    rule_results = list(
      cdisc_rule_result("R1", "RECORD_CHECK", "DM", CDISC_STATUS_PASS)
    )
  )
  expect_s3_class(result, "cdisc_conformance_result")
})

test_that("cdisc_all_passed works", {

  result_pass <- cdisc_conformance_result(
    "sdtmig", "3-4", "test",
    list(
      cdisc_rule_result("R1", "RECORD_CHECK", "DM", CDISC_STATUS_PASS),
      cdisc_rule_result("R2", "RECORD_CHECK", "AE", CDISC_STATUS_PASS)
    )
  )
  expect_true(cdisc_all_passed(result_pass))

  result_fail <- cdisc_conformance_result(
    "sdtmig", "3-4", "test",
    list(
      cdisc_rule_result("R1", "RECORD_CHECK", "DM", CDISC_STATUS_PASS),
      cdisc_rule_result("R2", "RECORD_CHECK", "AE", CDISC_STATUS_FAIL,
                        n_issues = 1L)
    )
  )
  expect_false(cdisc_all_passed(result_fail))
})

test_that("cdisc_status_counts works", {

  result <- cdisc_conformance_result(
    "sdtmig", "3-4", "test",
    list(
      cdisc_rule_result("R1", "RECORD_CHECK", "DM", CDISC_STATUS_PASS),
      cdisc_rule_result("R2", "RECORD_CHECK", "AE", CDISC_STATUS_FAIL),
      cdisc_rule_result("R3", "RECORD_CHECK", "LB", CDISC_STATUS_PASS)
    )
  )
  counts <- cdisc_status_counts(result)
  expect_equal(as.integer(counts[["pass"]]), 2L)
  expect_equal(as.integer(counts[["fail"]]), 1L)
})

test_that("cdisc_issues returns data frame", {

  result <- cdisc_conformance_result(
    "sdtmig", "3-4", "test",
    list(
      cdisc_rule_result("R1", "RECORD_CHECK", "DM", CDISC_STATUS_PASS),
      cdisc_rule_result("R2", "RECORD_CHECK", "AE", CDISC_STATUS_FAIL,
                        n_issues = 3L, message = "Bad values")
    )
  )
  issues <- cdisc_issues(result)
  expect_s3_class(issues, "data.frame")
  expect_equal(nrow(issues), 1L)
  expect_equal(issues$rule_id, "R2")
  expect_equal(issues$n_issues, 3L)
})

test_that("cdisc_findings_df returns empty data frame when no findings", {

  result <- cdisc_conformance_result(
    "sdtmig", "3-4", "test",
    list(cdisc_rule_result("R1", "RECORD_CHECK", "DM", CDISC_STATUS_PASS))
  )
  df <- cdisc_findings_df(result)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that("print.cdisc_conformance_result works", {

  result <- cdisc_conformance_result(
    "sdtmig", "3-4", "test",
    list(
      cdisc_rule_result("R1", "RECORD_CHECK", "DM", CDISC_STATUS_PASS),
      cdisc_rule_result("R2", "RECORD_CHECK", "AE", CDISC_STATUS_FAIL,
                        n_issues = 2L)
    )
  )
  output <- capture.output(print(result))
  expect_true(any(grepl("SDTMIG", output)))
  expect_true(any(grepl("FAIL", output)))
  expect_true(any(grepl("2 issues", output)))
})

# ── Engine (synthetic rules) ─────────────────────────────────────────────────

test_that("engine dispatches RECORD_CHECK correctly", {

  ct <- structure(
    list(codelists = list(SEX = c("M", "F")), packages = "test"),
    class = "cdisc_ct"
  )
  engine <- list(standard = "sdtmig", version = "3-4", ct = ct)

  rule <- cdisc_rule_from_list(list(
    core_id = "TEST-001",
    rule_type = "RECORD_CHECK",
    domains = list("DM"),
    operations = list(
      list(operator = "codelist_check", params = list(column = "SEX", codelist = "SEX"))
    ),
    conditions = list(
      name = "_pb_SEX_valid", operator = "equal_to", value = FALSE
    ),
    actions = list(params = list(message = "Invalid SEX value"))
  ))

  datasets <- list(
    DM = data.frame(
      USUBJID = c("S001", "S002", "S003"),
      SEX = c("M", "X", "F"),
      stringsAsFactors = FALSE
    )
  )

  result <- .cdisc_evaluate_rule(rule, engine, datasets)
  expect_equal(result$status, CDISC_STATUS_FAIL)
  expect_equal(result$n_issues, 1L)
  expect_equal(length(result$row_findings), 1L)
  expect_equal(result$row_findings[[1]]$usubjid, "S002")
  expect_equal(result$row_findings[[1]]$row, 2L)
})

test_that("engine dispatches DOMAIN_PRESENCE_CHECK correctly", {

  ct <- structure(
    list(codelists = list(), packages = "test"),
    class = "cdisc_ct"
  )
  engine <- list(standard = "sdtmig", version = "3-4", ct = ct)

  rule <- cdisc_rule_from_list(list(
    core_id = "TEST-002",
    rule_type = "DOMAIN_PRESENCE_CHECK",
    actions = list(params = list(required_domains = list("DM", "AE")))
  ))

  datasets <- list(DM = data.frame(X = 1))

  result <- .cdisc_evaluate_rule(rule, engine, datasets)
  expect_equal(result$status, CDISC_STATUS_FAIL)
  expect_equal(result$n_issues, 1L)
  expect_true(grepl("AE", result$message))
})

test_that("engine handles VARIABLE_METADATA_CHECK", {

  ct <- structure(
    list(codelists = list(), packages = "test"),
    class = "cdisc_ct"
  )
  engine <- list(standard = "sdtmig", version = "3-4", ct = ct)

  rule <- cdisc_rule_from_list(list(
    core_id = "TEST-003",
    rule_type = "VARIABLE_METADATA_CHECK",
    domains = list("DM"),
    operations = list(
      list(operator = "column_presence", params = list(column = "RFSTDTC"))
    ),
    conditions = list(
      name = "_pb_RFSTDTC_present", operator = "equal_to", value = FALSE
    )
  ))

  # DM without RFSTDTC -> violation
  datasets <- list(DM = data.frame(USUBJID = "S001", stringsAsFactors = FALSE))
  result <- .cdisc_evaluate_rule(rule, engine, datasets)
  expect_equal(result$status, CDISC_STATUS_FAIL)

  # DM with RFSTDTC -> pass
  datasets2 <- list(
    DM = data.frame(USUBJID = "S001", RFSTDTC = "2024-01-01",
                    stringsAsFactors = FALSE)
  )
  result2 <- .cdisc_evaluate_rule(rule, engine, datasets2)
  expect_equal(result2$status, CDISC_STATUS_PASS)
})

test_that("engine returns NOT_SUPPORTED for unknown rule types", {

  ct <- structure(
    list(codelists = list(), packages = "test"),
    class = "cdisc_ct"
  )
  engine <- list(standard = "sdtmig", version = "3-4", ct = ct)

  rule <- cdisc_rule_from_list(list(
    core_id = "TEST-004",
    rule_type = "UNKNOWN_TYPE"
  ))

  result <- .cdisc_evaluate_rule(rule, engine, list())
  expect_equal(result$status, CDISC_STATUS_NOT_SUPPORTED)
})

test_that("engine handles Partially Executable rules with missing inputs", {

  ct <- structure(
    list(codelists = list(), packages = "test"),
    class = "cdisc_ct"
  )
  engine <- list(standard = "sdtmig", version = "3-4", ct = ct)

  rule <- cdisc_rule_from_list(list(
    core_id = "TEST-005",
    rule_type = "RECORD_CHECK",
    executability = "Partially Executable",
    datasets = list("DEFINE"),
    domains = list("DM")
  ))

  datasets <- list(DM = data.frame(X = 1))
  result <- .cdisc_evaluate_rule(rule, engine, datasets)
  expect_equal(result$status, CDISC_STATUS_NOT_APPLICABLE)
  expect_true(grepl("DEFINE", result$message))
})

# ── End-to-end smoke test ────────────────────────────────────────────────────

test_that("validate_sdtmig runs end-to-end with known violations", {

  skip_if_not_installed("jsonlite")

  dm <- data.frame(
    STUDYID = rep("STUDY01", 4),
    DOMAIN = rep("DM", 4),
    USUBJID = c("S001", "S002", "S003", "S004"),
    SUBJID = c("001", "002", "003", "004"),
    SEX = c("M", "F", "INVALID", "M"),
    AGE = c(25, 30, 45, 50),
    RFSTDTC = c("2024-01-15", "not-a-date", "2024-03-20", "2024"),
    ARMCD = c("A", "B", "A", "B"),
    ARM = c("Treatment A", "Treatment B", "Treatment A", "Treatment B"),
    ACTARMCD = c("A", "B", "A", "B"),
    ACTARM = c("Treatment A", "Treatment B", "Treatment A", "Treatment B"),
    COUNTRY = c("USA", "USA", "GBR", "GBR"),
    stringsAsFactors = FALSE
  )

  result <- validate_sdtmig(list(DM = dm))

  expect_s3_class(result, "cdisc_conformance_result")
  expect_equal(result$standard, "sdtmig")
  expect_equal(result$version, "3-4")

  # Should have run many rules
  expect_true(length(result$rule_results) > 100)

  # Should have found some issues (at minimum the invalid SEX and bad date)
  total_issues <- cdisc_n_total_issues(result)
  expect_true(total_issues > 0)

  # Should not all pass
  expect_false(cdisc_all_passed(result))

  # The issues data frame should have rows
  issues <- cdisc_issues(result)
  expect_true(nrow(issues) > 0)

  # The findings data frame should have rows
  findings <- cdisc_findings_df(result)
  expect_true(nrow(findings) > 0)

  # Status counts should include both pass and fail
  counts <- cdisc_status_counts(result)
  expect_true("pass" %in% names(counts))
  expect_true("fail" %in% names(counts))
})

test_that("validate_sdtmig accepts dot-separated version", {

  skip_if_not_installed("jsonlite")

  dm <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "S001",
    stringsAsFactors = FALSE
  )

  result <- validate_sdtmig(list(DM = dm), version = "3.4")
  expect_s3_class(result, "cdisc_conformance_result")
})

test_that("validate_sdtmig with rule_types filter", {

  skip_if_not_installed("jsonlite")

  dm <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "S001",
    stringsAsFactors = FALSE
  )

  result <- validate_sdtmig(
    list(DM = dm),
    rule_types = "DOMAIN_PRESENCE_CHECK"
  )
  types <- unique(
    vapply(result$rule_results, function(r) r$rule_type, character(1))
  )
  expect_equal(types, "DOMAIN_PRESENCE_CHECK")
})
