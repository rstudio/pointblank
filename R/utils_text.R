reporting_languages <- c("en", "fr", "de", "it", "es")

#
# Text for autobriefs
#

precondition_text <- 
  c(
    "en" = "Precondition applied",
    "fr" = "Condition pr\u00E9alable",
    "de" = "Voraussetzung angewandt",
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
    "en" = "Exceedance of failed test units where values in {column_text} should have been {operator} {values_text}.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} auraient d\u00FB \u00EAtre {operator} {values_text}.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte in {column_text} {operator} {values_text} sein sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} avrebbero dovuto essere {operator} {values_text}.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} deber\u00EDan haber sido {operator} {values_text}."
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
    "en" = "Exceedance of failed test units where values in {column_text} should have been in the set of {values_text}.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} auraient d\u00FB \u00EAtre dans l'ensemble de {values_text}.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen Werte in {column_text} in der Menge von {values_text} enthalten sein sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} avrebbero dovuto essere nel set di {values_text}.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} deber\u00EDan haber estado en el conjunto de {values_text}."
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
    "en" = "Exceedance of failed test units where values in {column_text} should not have been in the set of {values_text}.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} n'auraient pas d\u00FB \u00EAtre dans l'ensemble de {values_text}.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte in {column_text} nicht in der Menge von {values_text} enthalten sein sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} non avrebbero dovuto essere nel set di {values_text}.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} no deber\u00EDan haber estado en el conjunto de {values_text}."
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
    "en" = "Exceedance of failed test units where values in {column_text} should have been between {value_1} and {value_2}.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} auraient d\u00FB \u00EAtre comprises entre {value_1} et {value_2}.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte in {column_text} zwischen {value_1} und {value_2} liegen sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} avrebbero dovuto essere compresi tra {value_1} e {value_2}.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} deber\u00EDan haber estado entre {value_1} y {value_2}."
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
    "en" = "Exceedance of failed test units where values in {column_text} should not have been between {value_1} and {value_2}.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} n'auraient pas d\u00FB \u00EAtre comprises entre {value_1} et {value_2}.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte in {column_text} nicht zwischen {value_1} und {value_2} liegen sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} non avrebbero dovuto essere compresi tra {value_1} e {value_2}.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} no deber\u00EDan haber estado entre {value_1} y {value_2}."
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
    "en" = "Exceedance of failed test units where values in {column_text} should have been NULL.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} auraient d\u00FB \u00EAtre NULL.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte in {column_text} NULL sein sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} avrebbero dovuto essere NULL.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} deber\u00EDan sido NULL."
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
    "en" = "Exceedance of failed test units where values in {column_text} should not have been NULL.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} n'auraient pas d\u00FB \u00EAtre NULL.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte in {column_text} nicht NULL sein sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} non avrebbero dovuto essere NULL.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} no deber\u00EDan sido NULL."
  )

col_vals_expr_expectation_text <- 
  c(
    "en" = "Expect that values should agree with the given R expression.",
    "fr" = "Attendez-vous \u00E0 ce que les valeurs soient en accord avec l'expression R donn\u00E9e.",
    "de" = "Erwarten Sie, dass die Werte mit dem angegebenen R-Ausdruck \u00FCbereinstimmen.",
    "it" = "Aspettatevi che i valori siano in accordo con l'espressione R fornita.",
    "es" = "Espere que los valores deben estar de acuerdo con la expresi\u00F3n R dada."
  )

col_vals_expr_failure_text <- 
  c(
    "en" = "Exceedance of failed test units where values should have agreed with the given R expression.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs auraient d\u00FB correspondre \u00E0 l'expression R donn\u00E9e.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte mit dem angegebenen R-Ausdruck \u00FCbereinstimmen sollten.",
    "it" = "Superamento di unit\u00E0 di test fallite in cui i valori avrebbero dovuto concordare con l'espressione R fornita.",
    "es" = "Superaci\u00F3n de las unidades de prueba fallidas donde los valores deber\u00EDan haber estado de acuerdo con la expresi\u00F3n R dada."
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
    "en" = "Exceedance of failed test units where values in {column_text} should have matched the regular expression: {values_text}.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 o\u00F9 les valeurs dans {column_text} auraient d\u00FB correspondre \u00E0 l'expression r\u00E9guli\u00E8re: {values_text}.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen die Werte in {column_text} mit dem regul\u00E4ren Ausdruck {values_text} \u00FCbereinstimmen sollten.",
    "it" = "Superamento delle unit\u00E0 di test non riuscite in cui i valori in {column_text} avrebbero dovuto corrispondere all'espressione regolare: {values_text}.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde los valores en {column_text} deber\u00EDan coincidir con la expresi\u00F3n regular: {values_text}."
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
    "en" = "Exceedance of failed test units where there should have been conjoint 'pass' units across the following expressions: {values_text}.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 l\u00E0 o\u00F9 il aurait d\u00FB y avoir des unit\u00E9s de \u00ABpass\u00BB conjointes dans les expressions suivantes: {values_text}.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen \u00FCber die folgenden Ausdr\u00FCcke hinweg 'Pass'-Einheiten zusammengesetzt sein sollten: {values_text}.",
    "it" = "Superamento delle unit\u00E0 di test fallite in cui avrebbero dovuto esserci unit\u00E0 di 'pass' congiunte tra le seguenti espressioni: {values_text}.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde deber\u00EDan haber unidades conjuntas de 'pass' en las siguientes expresiones: {values_text}."
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
    "en" = "Failure to validate that column {column_text} exists.",
    "fr" = "\u00C9chec de validation de l'existence de la colonne {column_text}.",
    "de" = "Fehler beim \u00DCberpr\u00FCfen, ob die Spalte {column_text} vorhanden ist.",
    "it" = "Impossibile convalidare l'esistenza della colonna {column_text}.",
    "es" = "Error al validar que la columna {column_text} existe."
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
    "en" = "Failure to validate that column {column_text} is of type: {col_type}.",
    "fr" = "\u00C9chec de validation de la colonne {column_text} de type: {col_type}.",
    "de" = "Fehler beim \u00DCberpr\u00FCfen dieser Spalte {column_text} vom Typ {col_type}.",
    "it" = "Impossibile convalidare che la colonna {column_text} sia di tipo: {col_type}.",
    "es" = "No validar esa columna {column_text} es de tipo: {col_type}."
  )

all_row_distinct_expectation_text <-
  c(
    "en" = "Expect entirely distinct rows across all columns.",
    "fr" = "Attendez-vous \u00E0 des lignes enti\u00E8rement distinctes dans toutes les colonnes.",
    "de" = "Erwarten Sie in allen Spalten einzigartige Zeilen.",
    "it" = "Aspettati righe completamente distinte su tutte le colonne.",
    "es" = "Espere filas completamente distintas en todas las columnas."
  )

all_row_distinct_failure_text <-
  c(
    "en" = "Exceedance of failed test units where there weren't distinct rows across all columns.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 l\u00E0 o\u00F9 il n'y avait pas des lignes distinctes dans toutes les colonnes.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen nicht in allen Spalten einzigartige Zeilen vorhanden waren.",
    "it" = "Superamento di unit\u00E0 di test fallite in cui non c'erano righe distinte su tutte le colonne.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde no hab\u00EDa filas distintas en todas las columnas."
  )

across_row_distinct_expectation_text <-
  c(
    "en" = "Expect entirely distinct rows across {column_text}.",
    "fr" = "Attendez-vous \u00E0 des lignes enti\u00E8rement distinctes sur {column_text}.",
    "de" = "Erwarten Sie einzigartige Zeilen in {column_text}.",
    "it" = "Aspettati righe completamente distinte su {column_text}.",
    "es" = "Espere filas completamente distintas en {column_text}."
  )

across_row_distinct_failure_text <-
  c(
    "en" = "Exceedance of failed test units where there weren't distinct rows across selected columns.",
    "fr" = "D\u00E9passement des unit\u00E9s de test ayant \u00E9chou\u00E9 l\u00E0 o\u00F9 il n'y avait pas des lignes distinctes dans les colonnes s\u00E9lectionn\u00E9es.",
    "de" = "\u00DCberschreitung fehlgeschlagener Testeinheiten, bei denen in ausgew\u00E4hlten Spalten keine eindeutigen Zeilen vorhanden waren.",
    "it" = "Superamento delle unit\u00E0 di test fallite in cui non c'erano righe distinte tra le colonne selezionate.",
    "es" = "Superaci\u00F3n de unidades de prueba fallidas donde no hab\u00EDa filas distintas en las columnas seleccionadas."
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
    "en" = "Failure to validate that column schemas match.",
    "fr" = "\u00C9chec de validation de la correspondance des sch\u00E9mas de colonne.",
    "de" = "Fehler beim \u00DCberpr\u00FCfen der \u00FCbereinstimmen dieser Spaltenschemata.",
    "it" = "Impossibile convalidare gli schemi di colonna corrispondenti.",
    "es" = "Error al validar que coincidan los esquemas de columna."
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
