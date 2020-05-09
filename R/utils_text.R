reporting_languages <- c("en", "fr", "de", "it", "es")

#
# Text for autobriefs
#

precondition_text <- 
  c(
    "en" = "Precondition applied",
    "fr" = "Condition pr\u00E9alable",
    "de" = "Voraussetzung angewendet",
    "it" = "Prerequisito applicato",
    "es" = "Condici\u00F3n previa aplicada"
  )

column_computed_text <-
  c(
    "en" = "computed column",
    "fr" = "colonne calcul\u00E9e",
    "de" = "berechnete Spalte",
    "it" = "colonna calcolata",
    "es" = "columna calculada"
  )

values_text <- 
  c(
    "en" = "and {num_omitted} more",
    "fr" = "et {num_omitted} de plus",
    "de" = "und {num_omitted} mehr",
    "it" = "e altri {num_omitted}",
    "es" = "y {num_omitted} m\u00E1s"
  )

compare_expectation_text <- 
  c(
    "en" = "Expect that values in {column_text} {column_computed_text} should be {operator} {values_text}.",
    "fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} soient {operator} {values_text}.",
    "de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} {operator} {values_text} sein sollten.",
    "it" = "Aspettatevi che i valori in {column_text} {column_computed_text} dovrebbero essere {operator} {values_text}.",
    "es" = "Espere que los valores en {column_text} {column_computed_text} sean {operator} {values_text}."
  )

compare_failure_text <- 
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should have been {operator} {values_text}."#,
    #"fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} soient {operator} {values_text}.",
    #"de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} {operator} {values_text} sein sollten.",
    #"it" = "Aspettatevi che i valori in {column_text} {column_computed_text} dovrebbero essere {operator} {values_text}.",
    #"es" = "Espere que los valores en {column_text} {column_computed_text} sean {operator} {values_text}."
  )

in_set_expectation_text <-
  c(
    "en" = "Expect that values in {column_text} {column_computed_text} should be in the set of {values_text}.",
    "fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} soient dans l'ensemble de {values_text}.",
    "de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} in der Menge von {values_text} enthalten sein sollten.",
    "it" = "Aspettatevi che i valori in {column_text} {column_computed_text} siano nell'insieme di {values_text}.",
    "es" = "Espere que los valores en {column_text} {column_computed_text} est\u00E9n en el conjunto de {values_text}."
  )

in_set_failure_text <-
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should have been in the set of {values_text}."#,
    #"fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} soient dans l'ensemble de {values_text}.",
    #"de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} in der Menge von {values_text} enthalten sein sollten.",
    #"it" = "Aspettatevi che i valori in {column_text} {column_computed_text} siano nell'insieme di {values_text}.",
    #"es" = "Espere que los valores en {column_text} {column_computed_text} est\u00E9n en el conjunto de {values_text}."
  )

not_in_set_expectation_text <-
  c(
    "en" = "Expect that values in {column_text} {column_computed_text} should not be in the set of {values_text}.",
    "fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} ne soient pas dans l'ensemble de {values_text}.",
    "de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} nicht in der Menge von {values_text} enthalten sein sollten.",
    "it" = "Aspettatevi che i valori in {column_text} {column_computed_text} non debbano essere nel set di {values_text}.",
    "es" = "Espere que los valores en {column_text} {column_computed_text} no est\u00E9n en el conjunto de {values_text}."
  )

not_in_set_failure_text <-
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should not have been in the set of {values_text}."#,
    #"fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} ne soient pas dans l'ensemble de {values_text}.",
    #"de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} nicht in der Menge von {values_text} enthalten sein sollten.",
    #"it" = "Aspettatevi che i valori in {column_text} {column_computed_text} non debbano essere nel set di {values_text}.",
    #"es" = "Espere que los valores en {column_text} {column_computed_text} no est\u00E9n en el conjunto de {values_text}."
  )

between_expectation_text <- 
  c(
    "en" = "Expect that values in {column_text} {column_computed_text} should be between {value_1} and {value_2}.",
    "fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} soient comprises entre {value_1} et {value_2}.",
    "de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} zwischen {value_1} und {value_2} liegen sollten.",
    "it" = "Aspettati che i valori in {column_text} {column_computed_text} siano compresi tra {value_1} e {value_2}.",
    "es" = "Espere que los valores en {column_text} {column_computed_text} est\u00E9n entre {value_1} y {value_2}."
  )

between_failure_text <- 
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should have been between {value_1} and {value_2}."#,
    #"fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} soient comprises entre {value_1} et {value_2}.",
    #"de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} zwischen {value_1} und {value_2} liegen sollten.",
    #"it" = "Aspettati che i valori in {column_text} {column_computed_text} siano compresi tra {value_1} e {value_2}.",
    #"es" = "Espere que los valores en {column_text} {column_computed_text} est\u00E9n entre {value_1} y {value_2}."
  )

not_between_expectation_text <- 
  c(
    "en" = "Expect that values in {column_text} {column_computed_text} should not be between {value_1} and {value_2}.",
    "fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} ne soient pas comprises entre {value_1} et {value_2}.",
    "de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} nicht zwischen {value_1} und {value_2} liegen sollten.",
    "it" = "Aspettatevi che i valori in {column_text} {column_computed_text} non debbano essere compresi tra {value_1} e {value_2}.",
    "es" = "Espere que los valores en {column_text} {column_computed_text} no est\u00E9n entre {value_1} y {value_2}."
  )

not_between_failure_text <- 
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should not have been between {value_1} and {value_2}."#,
    #"fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} ne soient pas comprises entre {value_1} et {value_2}.",
    #"de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} nicht zwischen {value_1} und {value_2} liegen sollten.",
    #"it" = "Aspettatevi che i valori in {column_text} {column_computed_text} non debbano essere compresi tra {value_1} e {value_2}.",
    #"es" = "Espere que los valores en {column_text} {column_computed_text} no est\u00E9n entre {value_1} y {value_2}."
  )

null_expectation_text <- 
  c(
    "en" = "Expect that all values in {column_text} {column_computed_text} should be NULL.",
    "fr" = "Attendez-vous \u00E0 ce que toutes les valeurs de {column_text} {column_computed_text} soient NULL.",
    "de" = "Erwarten Sie, dass alle Werte in {column_text} {column_computed_text} NULL sein sollten.",
    "it" = "Aspettatevi che tutti i valori in {column_text} {column_computed_text} siano NULL.",
    "es" = "Espere que todos los valores en {column_text} {column_computed_text} sean NULL."
  )

null_failure_text <- 
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should have been NULL."#,
    #"fr" = "Attendez-vous \u00E0 ce que toutes les valeurs de {column_text} {column_computed_text} soient NULL.",
    #"de" = "Erwarten Sie, dass alle Werte in {column_text} {column_computed_text} NULL sein sollten.",
    #"it" = "Aspettatevi che tutti i valori in {column_text} {column_computed_text} siano NULL.",
    #"es" = "Espere que todos los valores en {column_text} {column_computed_text} sean NULL."
  )

not_null_expectation_text <- 
  c(
    "en" = "Expect that all values in {column_text} {column_computed_text} should not be NULL.",
    "fr" = "Attendez-vous \u00E0 ce que toutes les valeurs de {column_text} {column_computed_text} ne soient pas NULL.",
    "de" = "Erwarten Sie, dass alle Werte in {column_text} {column_computed_text} nicht NULL sein sollten.",
    "it" = "Aspettatevi che tutti i valori in {column_text} {column_computed_text} non debbano essere NULL.",
    "es" = "Espere que todos los valores en {column_text} {column_computed_text} no sean NULL."
  )

not_null_failure_text <- 
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should not have been NULL."#,
    #"fr" = "Attendez-vous \u00E0 ce que toutes les valeurs de {column_text} {column_computed_text} ne soient pas NULL.",
    #"de" = "Erwarten Sie, dass alle Werte in {column_text} {column_computed_text} nicht NULL sein sollten.",
    #"it" = "Aspettatevi che tutti i valori in {column_text} {column_computed_text} non debbano essere NULL.",
    #"es" = "Espere que todos los valores en {column_text} {column_computed_text} no sean NULL."
  )

regex_expectation_text <- 
  c(
    "en" = "Expect that values in {column_text} {column_computed_text} should match the regular expression: {values_text}.",
    "fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} correspondent \u00E0 l'expression r\u00E9guli\u00E8re: {values_text}.",
    "de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} mit dem regul\u00E4ren Ausdruck {values_text} \u00FCbereinstimmen.",
    "it" = "Aspettati che i valori in {column_text} {column_computed_text} debbano corrispondere all'espressione regolare: {values_text}.",
    "es" = "Espere que los valores en {column_text} {column_computed_text} coincidan con la expresi\u00F3n regular: {values_text}."
  )

regex_failure_text <- 
  c(
    "en" = "Exceedance of failed test units where values in {column_text} should have matched the regular expression: {values_text}."#,
    #"fr" = "Attendez-vous \u00E0 ce que les valeurs de {column_text} {column_computed_text} correspondent \u00E0 l'expression r\u00E9guli\u00E8re: {values_text}.",
    #"de" = "Erwarten Sie, dass die Werte in {column_text} {column_computed_text} mit dem regul\u00E4ren Ausdruck {values_text} \u00FCbereinstimmen.",
    #"it" = "Aspettati che i valori in {column_text} {column_computed_text} debbano corrispondere all'espressione regolare: {values_text}.",
    #"es" = "Espere que los valores en {column_text} {column_computed_text} coincidan con la expresi\u00F3n regular: {values_text}."
  )

conjointly_expectation_text <-
  c(
    "en" = "Expect conjoint 'pass' units across the following expressions: {values_text}.",
    "fr" = "Attendez-vous \u00E0 des unit\u00E9s de \u00ABpass\u00BB conjointes dans les expressions suivantes: {values_text}.",
    "de" = "Erwarten Sie gemeinsame 'Pass'-Einheiten f\u00FCr die folgenden Ausdr\u00FCcke: {values_text}.",
    "it" = "Aspettatevi unit\u00E1 'pass' congiunte tra le seguenti espressioni: {values_text}.",
    "es" = "Espere unidades conjuntas de 'pass' en las siguientes expresiones: {values_text}."
  )

conjointly_failure_text <-
  c(
    "en" = "Exceedance of failed test units where there should have been conjoint 'pass' units across the following expressions: {values_text}."#,
    #"fr" = "Attendez-vous \u00E0 des unit\u00E9s de \u00ABpass\u00BB conjointes dans les expressions suivantes: {values_text}.",
    #"de" = "Erwarten Sie gemeinsame 'Pass'-Einheiten f\u00FCr die folgenden Ausdr\u00FCcke: {values_text}.",
    #"it" = "Aspettatevi unit\u00E1 'pass' congiunte tra le seguenti espressioni: {values_text}.",
    #"es" = "Espere unidades conjuntas de 'pass' en las siguientes expresiones: {values_text}."
  )

col_exists_expectation_text <-
  c(
    "en" = "Expect that column {column_text} exists.",
    "fr" = "Attendez-vous \u00E0 ce que la colonne {column_text} existe.",
    "de" = "Erwarten Sie, dass die Spalte {column_text} vorhanden ist.",
    "it" = "Aspettati che la colonna {column_text} esista.",
    "es" = "Espere que exista la columna {column_text}."
  )

col_exists_failure_text <-
  c(
    "en" = "Failure to validate that column {column_text} exists."#,
    #"fr" = "Attendez-vous \u00E0 ce que la colonne {column_text} existe.",
    #"de" = "Erwarten Sie, dass die Spalte {column_text} vorhanden ist.",
    #"it" = "Aspettati che la colonna {column_text} esista.",
    #"es" = "Espere que exista la columna {column_text}."
  )

col_is_expectation_text <-
  c(
    "en" = "Expect that column {column_text} is of type: {col_type}.",
    "fr" = "Attendez-vous \u00E0 ce que la colonne {column_text} soit de type: {col_type}.",
    "de" = "Erwarten Sie, dass die Spalte {column_text} vom Typ {col_type} ist.",
    "it" = "Aspettati che la colonna {column_text} sia di tipo: {col_type}.",
    "es" = "Espere que la columna {column_text} sea del tipo: {col_type}."
  )

col_is_failure_text <-
  c(
    "en" = "Failure to validate that column {column_text} is of type: {col_type}."#,
    #"fr" = "Attendez-vous \u00E0 ce que la colonne {column_text} soit de type: {col_type}.",
    #"de" = "Erwarten Sie, dass die Spalte {column_text} vom Typ {col_type} ist.",
    #"it" = "Aspettati che la colonna {column_text} sia di tipo: {col_type}.",
    #"es" = "Espere que la columna {column_text} sea del tipo: {col_type}."
  )

all_row_distinct_expectation_text <-
  c(
    "en" = "Expect entirely distinct rows across all columns.",
    "fr" = "Attendez-vous \u00E0 des lignes enti\u00E8rement distinctes dans toutes les colonnes.",
    "de" = "Erwarten Sie in allen Spalten v\u00F6llig unterschiedliche Zeilen.",
    "it" = "Aspettati righe completamente distinte su tutte le colonne.",
    "es" = "Espere filas completamente distintas en todas las columnas."
  )

all_row_distinct_failure_text <-
  c(
    "en" = "Exceedance of failed test units where there weren't distinct rows across all columns."#,
    #"fr" = "Attendez-vous \u00E0 des lignes enti\u00E8rement distinctes dans toutes les colonnes.",
    #"de" = "Erwarten Sie in allen Spalten v\u00F6llig unterschiedliche Zeilen.",
    #"it" = "Aspettati righe completamente distinte su tutte le colonne.",
    #"es" = "Espere filas completamente distintas en todas las columnas."
  )

across_row_distinct_expectation_text <-
  c(
    "en" = "Expect entirely distinct rows across {column_text}.",
    "fr" = "Attendez-vous \u00E0 des lignes enti\u00E8rement distinctes sur {column_text}.",
    "de" = "Erwarten Sie v\u00F6llig unterschiedliche Zeilen in {column_text}.",
    "it" = "Aspettati righe completamente distinte su {column_text}.",
    "es" = "Espere filas completamente distintas en {column_text}."
  )

across_row_distinct_failure_text <-
  c(
    "en" = "Exceedance of failed test units where there weren't distinct rows across selected columns."#,
    #"fr" = "Attendez-vous \u00E0 des lignes enti\u00E8rement distinctes sur {column_text}.",
    #"de" = "Erwarten Sie v\u00F6llig unterschiedliche Zeilen in {column_text}.",
    #"it" = "Aspettati righe completamente distinte su {column_text}.",
    #"es" = "Espere filas completamente distintas en {column_text}."
  )

col_schema_match_expectation_text <-
  c(
    "en" = "Expect that column schemas match.",
    "fr" = "Attendez-vous \u00E0 ce que les sch\u00E9mas de colonnes correspondent.",
    "de" = "Erwarten Sie, dass die Spaltenschemata \u00FCbereinstimmen.",
    "it" = "Aspettati che gli schemi di colonna corrispondano.",
    "es" = "Espere que los esquemas de columna coincidan."
  )

col_schema_match_failure_text <-
  c(
    "en" = "Failure to validate that column schemas match."#,
    #"fr" = "Attendez-vous \u00E0 ce que les sch\u00E9mas de colonnes correspondent.",
    #"de" = "Erwarten Sie, dass die Spaltenschemata \u00FCbereinstimmen.",
    #"it" = "Aspettati che gli schemi di colonna corrispondano.",
    #"es" = "Espere que los esquemas de columna coincidan."
  )

#
# Text for agent report
#

pointblank_validation_title_text <-
  c(
    "en" = "Pointblank Validation",
    "fr" = "Validation Pointblank",
    "de" = "Pointblank-Validierung",
    "it" = "Convalida Pointblank",
    "es" = "Validaci\u00F3n de Pointblank"
  )

pointblank_validation_plan_text <- 
  c(
    "en" = "Pointblank Validation Plan",
    "fr" = "Plan de validation de Pointblank",
    "de" = "Pointblank-Validierungsplan",
    "it" = "Piano di convalida Pointblank",
    "es" = "Plan de validaci\u00F3n de Pointblank"
  )

no_interrogation_performed_text <- 
  c(
    "en" = "No Interrogation Performed",
    "fr" = "Aucune interrogation effectu\u00E9e",
    "de" = "Keine Abfrage durchgef\u00FChrt",
    "it" = "Nessuna interrogazione eseguita",
    "es" = "No se realizan interrogatorios"
  )

report_col_step <-
  c(
    "en" = "STEP",
    "fr" = "\u00C9TAPE",
    "de" = "SCHRITT",
    "it" = "INDICE",
    "es" = "\u00CDNDICE"
  )

report_col_steps <-
  c(
    "en" = "STEPS",
    "fr" = "\u00C9TAPES",
    "de" = "SCHRITTE",
    "it" = "INDICI",
    "es" = "\u00CDNDICES"
  )

report_col_columns <-
  c(
    "en" = "COLUMNS",
    "fr" = "COLONNES",
    "de" = "SPALTEN",
    "it" = "COLONNE",
    "es" = "COLUMNAS"
  )

report_col_values <-
  c(
    "en" = "VALUES",
    "fr" = "VALEURS",
    "de" = "WERTE",
    "it" = "VALORI",
    "es" = "VALORES"
  )

report_col_units <-
  c(
    "en" = "UNITS",
    "fr" = "UNIT\u00C9S",
    "de" = "EINH.",
    "it" = "UNIT\u00C0",
    "es" = "UNIDADES"
  )

report_column_schema <-
  c(
    "en" = "SCHEMA",
    "fr" = "SCH\u00C9MA",
    "de" = "SCHEMA",
    "it" = "SCHEMA",
    "es" = "ESQUEMA"
  )

report_r_col_types <-
  c(
    "en" = "R TYPES",
    "fr" = "TYPES R",
    "de" = "R-TYPEN",
    "it" = "TIPI R",
    "es" = "TIPOS R"
  )

report_r_sql_types <-
  c(
    "en" = "SQL TYPES",
    "fr" = "TYPES SQL",
    "de" = "SQL-TYPEN",
    "it" = "TIPI SQL",
    "es" = "TIPOS SQL"
  )
