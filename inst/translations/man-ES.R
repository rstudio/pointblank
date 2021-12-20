# Generated file
# ES translations for man pages


# action_levels------------------------------------------------------------
#' Establecer niveles de acción: umbrales de falla y funciones para invocar
#' 
#' @description
#' La función `action_levels()` trabaja con el argumento `actions` que está
#' presente en la función [create_agent()] y en cada función de paso de
#' validación (que también tiene un argumento `actions`). Con él, podemos
#' proporcionar niveles de umbral *falla* para cualquier combinación de
#' estados de `warn`, `stop` o `notify`.
#' 
#' Podemos reaccionar ante cualquier entrada de un estado proporcionando las
#' funciones correspondientes al argumento `fns`. Se someterán a evaluación en
#' el momento en que se ingrese al estado de coincidencia. Si se proporciona a
#' [create_agent()], las políticas se aplicarán a cada paso de validación,
#' actuando de forma predeterminada para la validación en su conjunto.
#' 
#' Las llamadas de `action_levels()` también podrían aplicarse directamente a
#' cualquier paso de validación y esto actuará como una anulación si se
#' establece también en [create_agent()]. Se requiere el uso de
#' `action_levels()` para tener efectos secundarios útiles (es decir,
#' advertencias, errores de lanzamiento) en el caso de funciones de validación
#' que operan directamente sobre los datos (p. Ej., `mtcars %>%
#' col_vals_lt("mpg", 35)`). Hay dos funciones auxiliares que son convenientes
#' cuando se usan funciones de validación directamente en datos (el flujo de
#' trabajo sin `agent`): `warn_on_fail()` y `stop_on_fail()`. Estos ayudantes
#' advierten o se detienen (el umbral de falla predeterminado para cada uno se
#' establece en `1`) y lo hacen con advertencias informativas o mensajes de
#' error. El ayudante `stop_on_fail()` se aplica de forma predeterminada cuando
#' se utilizan funciones de validación directamente en los datos (se proporciona
#' más información sobre esto en *Detalles*).
#'
#' @details
#' El resultado de la `action_levels()` llamada actionsse interpretará de forma
#' ligeramente diferente si se usa un agente o se usan funciones de validación
#' directamente en una tabla de datos. Por conveniencia, cuando se trabaja
#' directamente con datos, cualquier valor suministrado `warn_at` o `stop_at` se
#' le asignará automáticamente un stock `warning()` o `stop()` función. Por
#' ejemplo, el uso `small_table %>% col_is_integer("date")` proporcionará un
#' mensaje de detención detallado de forma predeterminada, que indica el motivo
#' del error. Si tuviera que suministrar el `fns` for `stop` o `warn`
#' manualmente, las funciones de stock se anularían. Además, si actionses `NULL`
#' en este flujo de trabajo (el predeterminado), pointblank usará un `stop_at`
#' valor de `1` (que proporciona un mensaje de error detallado y específico del
#' contexto si hay unidades que *fallan*). Podemos suprimir absolutamente este
#' comportamiento de parada automática en cada paso de validación mediante la
#' configuración `active = FALSE`. En este caso de datos interactivos, no se
#' proporciona una función de stock para `notify_at`. El notifyestado de falla
#' se usa con menos frecuencia en este flujo de trabajo que en el basado en
#' agente .
#'
#' Cuando se utiliza un agente , a menudo opta por no utilizar ninguna función
#' en la `fns` que el `warn`, `stop` y `notify` serán reportados en los estados
#' de insuficiencia cuando se usa `create_agent_report()` (y, por lo general eso
#' es suficiente). En cambio, usar el `end_fns` argumento es una mejor opción,
#' ya que ese esquema proporciona datos útiles sobre todo el interrogatorio, lo
#' que permite un control más preciso de los efectos secundarios y reduce la
#' posibilidad de duplicar los efectos secundarios.
#' 
#' @param warn_at,stop_at,notify_at El número de umbral o fracción de unidades
#'   de prueba que pueden proporcionar una *fallar* resultado antes de entrar en
#'   las `warn`, `stop` o `notify` estados de fallo. Si este es un valor decimal
#'   entre `0` y `1` entonces es un umbral de falla proporcional (por ejemplo,
#'   `0.15` indica que si se encuentra que el 15% por ciento de las unidades de
#'   prueba *fallan*, entonces se ingresa el estado de falla designado). En `1`
#'   su lugar, se pueden usar valores absolutos a partir de, y esto constituye
#'   un umbral de falla absoluto (por ejemplo, `10` significa que si se
#'   encuentra que 10 de las unidades de prueba fallan , se ingresa el estado de
#'   falla).
#' @param fns Una lista nombrada de funciones que se emparejará con los estados
#'   de falla apropiados. La sintaxis para esta lista implica el uso de nombres
#'   de estado fracaso desde el conjunto de `warn`, `stop` y `notify`. Las
#'   funciones correspondientes a los estados de falla se proporcionan como
#'   fórmulas (por ejemplo `list(warn = ~ warning("Too many failures."))`). Se
#'   puede usar una serie de expresiones para cada estado nombrado encerrando el
#'   conjunto de declaraciones con `{ }`.
#' 
#' @examples 
#' # Para estos ejemplos, usaremos el conjunto
#' # de datos `small_table` incluido
#' small_table
#' 
#' # Crea un objeto `action_levels` con valores
#' # fraccionarios para el `warn_at`,
#' # `stop_at` y `notify_at` estados
#' al <- 
#'   action_levels(
#'     warn_at = 0.2,
#'     stop_at = 0.8,
#'     notify_at = 0.5
#'   )
#'   
#' # Un resumen de la configuración de `al`
#' # El objeto se muestra imprimiéndolo
#' al
#' 
#' # Cree un agente a quemarropa y
#' # aplicar el objeto `al` a las `actions`;
#' # agregue dos pasos de validación y
#' # interrogar a la `small_table`
#' agent_1 <-
#'   create_agent(
#'     tbl = small_table,
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     vars(a), value = 2
#'   ) %>%
#'   col_vals_lt(
#'     vars(d), value = 20000
#'   ) %>%
#'   interrogate()
#' 
#' # El informe del agente mostrará
#' # que se ha entrado en el estado de advertencia
#' # para el primer paso de validación pero no
#' # el segundo; podemos confirmar esto
#' # en la consola inspeccionando el
#' # Componente `warn` en la x-list del agente
#' x_list <- get_agent_x_list(agent_1)
#' x_list$warn
#' 
#' # Aplicar el objeto `action_levels`
#' # para el agente significa que toda
#' # validación los pasos heredarán esta
#' # configuración, pero podemos anular
#' # esto aplicando otro objeto similar a
#' # la validación paso en su lugar (esta
#' # vez usando el `warn_on_fail()`
#' # abreviatura)
#' agent_2 <-
#'   create_agent(
#'     tbl = small_table,
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     vars(a), value = 2,
#'     actions = warn_on_fail(warn_at = 0.5)
#'   ) %>%
#'   col_vals_lt(
#'     vars(d), value = 20000
#'   ) %>%
#'   interrogate()
#'
#' # En este caso, la primera validación
#' # el paso tiene una falla menos estricta
#' # umbral para el estado de advertencia y es
#' # lo suficientemente alto como para que la
#' # condición no sea ingresó; esto se puede
#' # confirmar en el consola a través de la
#' # inspección de la componente x-list `warn`
#' x_list <- get_agent_x_list(agent_2)
#' x_list$warn
#'
#' if (interactive()) {
#'
#' # En el contexto del uso de la validación
#' # funciona directamente en los datos (es
#' # decir, no participación de un agente)
#' # queremos desencadenar advertencias y
#' # generar errores; los siguiente dará una
#' # advertencia si se ejecuta (devolviendo el
#' # datos de `small_table`)
#' small_table %>%
#'   col_vals_gt(
#'     vars(a), value = 2,
#'     actions = warn_on_fail(warn_at = 2)
#'   )
#' 
#' # Con el mismo oleoducto, no suministrando
#' # cualquier cosa para `actions` (es` NULL`
#' # por predeterminado) tendrá el mismo
#' # efecto que usando `stop_on_fail (stop_at = 1)`
#' small_table %>%
#'   col_vals_gt(vars(a), value = 2)
#' 
#' small_table %>%
#'   col_vals_gt(
#'     vars(a), value = 2,
#'     actions = stop_on_fail(stop_at = 1)
#'   )
#' 
#' # Esto se debe a que el `stop_on_fail()`
#' # la llamada se inyecta automáticamente
#' # de forma predeterminada caso (cuando
#' # se opera con datos) para su conveniencia;
#' # detrás de escena un agente secreto' utiliza
#' # 'acciones encubiertas': todo para que
#' # puedas escribir menos
#' 
#' }
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-5
#' 
#' @name action_levels

# activate_steps-----------------------------------------------------------
#' Activar uno o más de los pasos de validación de un *agent*
#'
#' @description 
#' Si es necesario activar ciertos pasos de validación después de la creación
#' del plan de validación para un *agent*, use la función `active_steps()`.
#' Este es equivalente a usar `active = TRUE` para los pasos de validación
#' seleccionados (`active` es un argumento en todas las funciones de
#' validación). Esto reemplazará a cualquier función que puede haber sido
#' definida para el argumento `active` durante la creación de los pasos de
#' validación específicos.
#'
#' @param agent Un objeto de agente de clase `ptblank_agent`.
#' @param i El número de paso de validación, que se asigna a cada paso de
#'   validación en el orden de definición.
#'
#' @return Un objeto `ptblank_agent`.
#' 
#' @examples 
#' # Cree un agente que tenga objeto
#' # `small_table` como el tabla de 
#' # destino, agregue algunos inactivos
#' # pasos de validación, y luego use
#' # `interrogar()`
#' agent_1 <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>%
#'   col_exists(
#'     vars(date),
#'     active = FALSE
#'   ) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}",
#'     active = FALSE
#'   ) %>%
#'   interrogate()
#' 
#' # En lo anterior, los datos son no
#' # realmente interrogado porque el
#' # ajuste `active` fue `FALSE` en
#' # todos los pasos; nosotros puede
#' # cambiar esto de forma selectiva
#' # con `active_steps()`
#' agent_2 <-
#'   agent_1 %>%
#'   activate_steps(i = 1) %>%
#'   interrogate()
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-6
#' 
#' @seealso Para el comportamiento opuesto, use la función [deactivate_steps()].
#'
#' @export

# affix_date---------------------------------------------------------------
#' Ponga la fecha actual en un nombre de archivo
#' 
#' @description
#' Esta función ayuda a fijar la fecha actual a un nombre de archivo. Esto es
#' útil al escribir objetos *agent* y *informante* en el disco como parte de un
#' proceso continuo. La fecha puede ser en términos de hora UTC o del sistema
#' local. tiempo. La fecha se puede colocar al final del nombre del archivo
#' (antes del extensión de archivo) o al principio con un delimitador
#' personalizable.
#' 
#' Las funciones [x_write_disk()], [yaml_write()] permiten la escritura de
#' objetos **pointblank** al disco. Además, la función [log4r_step()] tiene
#' el argumento `append_to` que acepta nombres de archivo, y es razonable que un
#' La serie de archivos de registro se puede diferenciar por un componente de
#' fecha en el nombre esquema. La modificación de la cadena del nombre del
#' archivo tiene efecto inmediatamente, pero no en el momento de escribir un
#' archivo en el disco. En la mayoría de los casos, especialmente cuando usando
#' `affix_date()` con las funciones de escritura de archivos antes mencionadas,
#' el archivo las marcas de tiempo deben aproximarse a los componentes de tiempo
#' fijados a los nombres de archivo.
#' 
#' @param filename El nombre de archivo que se va a modificar.
#' @param position Dónde colocar la fecha formateada. Esto podría estar al
#'   `"end"` del nombre del archivo (el predeterminado) o al `"start"`.
#' @param format Una cadena de formato [base::strptime()] para formatear la
#'   fecha. Por predeterminado, esto es `"%Y-%m-%d"` que expresa la fecha de
#'   acuerdo con la ISO 8601 estándar (como 'AAAA-MM-DD'). Consulte la
#'   documentación en [base::strptime()] para las especificaciones de conversión
#'   si planea usar un cadena de formato diferente.
#' @param delimiter Los caracteres delimitadores que se utilizarán para separar
#'   la fecha. cadena del nombre del archivo original.
#' @param utc_time Una opción para utilizar la hora UTC actual para establecer
#'   la fecha (la predeterminada, con `TRUE`), o, utilizar el local del sistema
#'   tiempo (`FALSE`).
#'   
#' @return Un vector de caracteres.
#'   
#' @examples 
#' # Tomando el nombre genérico de `pb_file`
#' # para un archivo, le agregamos la fecha
#' # actual como sufijo
#' affix_date(filename = "pb_file")
#' 
#' # Las extensiones de archivo no se
#' # interpondrán en el camino:
#' affix_date(filename = "pb_file.rds")
#' 
#' # La fecha se puede utilizar como prefijo
#' affix_date(
#'   filename = "pb_file",
#'   position = "start"
#' )
#' 
#' # El patrón de fecha se puede cambiar
#' # y así puede el delimitador
#' affix_date(
#'   filename = "pb_file.yml",
#'   format = "%Y%m%d",
#'   delimiter = "-"
#' )
#' 
#' if (interactive()) {
#' 
#' # Podemos usar una convención de
#' # nomenclatura de archivos que
#' # involucran fechas al escribir la
#' # salida archivos inmediatamente
#' # después de interrogar; útil al
#' # interrogar directamente de YAML
#' # en un proceso programado
#' yaml_agent_interrogate(
#'   filename = system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ) %>% 
#'   x_write_disk(
#'     filename = affix_date(
#'       filename = "small_table_agent.rds",
#'       delimiter = "-"
#'     ),
#'     keep_tbl = TRUE,
#'     keep_extracts = TRUE
#'   )
#' 
#' }
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-3
#' 
#' @seealso La función [affix_datetime()] proporciona las mismas características
#'   excepto que produce una cadena de fecha y hora de forma predeterminada.
#' 
#' @export

# affix_datetime-----------------------------------------------------------
#' Ponga la fecha y hora actual en un nombre de archivo
#' 
#' @description
#' Esta función ayuda a fijar la fecha y hora actual a un nombre de archivo.
#' Este es útil al escribir objetos *agent* y *informant* en el disco
#' como parte de un proceso continuo. La cadena de fecha y hora puede basarse en
#' la hora UTC actual. o la hora del sistema local. La fecha y hora se puede
#' colocar al final de el nombre del archivo (antes de la extensión del archivo)
#' o al principio con un delimitador personalizable. Opcionalmente, la
#' información de la zona horaria puede ser incluido. Si la fecha y hora se basa
#' en la hora del sistema local, el sistema del usuario La zona horaria se
#' muestra con el formato `<time>(+/-)hhmm`. Si usa la hora UTC, entonces se
#' adopta el formato `<time>Z`.
#' 
#' Las funciones [x_write_disk()], [yaml_write()] permiten la escritura de
#' objetos **pointblank** al disco. La modificación de la cadena de nombre de
#' archivo toma efecto inmediatamente, pero no en el momento de escribir un
#' archivo en el disco. En la mayoría casos, especialmente cuando se usa
#' `affix_datetime()` con el mencionado funciones de escritura de archivos, las
#' marcas de tiempo del archivo deben aproximarse al tiempo componentes
#' adheridos a los nombres de archivo.
#' 
#' @inheritParams affix_date
#' @param position Dónde colocar la fecha y hora formateada. Esto podría ser al
#'   `"end"` del nombre del archivo (predeterminado) o al `"start"`.
#' @param format Una cadena de formato [base::strptime()] para formatear el
#'   fecha y hora. De forma predeterminada, es `"%Y-%m-%dT%H:%M:%S"` que expresa
#'   la fecha según la norma ISO 8601. Por ejemplo, si la corriente fecha-hora
#'   es `2020-12-04 13:11:23`, la cadena formateada se convertiría en
#'   `"2020-12-04T13:11:23"`. Consulte la documentación en [base::strptime()]
#'   para las especificaciones de conversión si planea utilizar una cadena de
#'   formato diferente.
#' @param delimiter Los caracteres delimitadores que se utilizarán para separar
#'   la cadena de fecha y hora del nombre del archivo original.
#' @param utc_time Una opción para usar la hora UTC actual para establecer la
#'   fecha y hora (la predeterminada, con `TRUE`), o usar la hora local del
#'   sistema (`FALSE`).
#' @param add_tz ¿Debería proporcionarse la zona horaria (como un desfase de
#'   UTC)? Si es `TRUE`, la compensación UTC se proporcionará como `<time>Z`
#'   (si `utc_time = TRUE`) o `<time>(+/-)hhmm`. De forma predeterminada, es
#'   `FALSE`.
#'
#' @return Un vector de caracteres.
#' 
#' @examples 
#' # Tomando el nombre genérico de `pb_file`
#' # para un archivo, le agregamos la fecha y
#' # hora actual como sufijo
#' affix_datetime(filename = "pb_file")
#' 
#' # Las extensiones de archivo no se
#' # interpondrán en el camino:
#' affix_datetime(filename = "pb_file.rds")
#' 
#' # La fecha y hora se puede utilizar
#' # como prefijo
#' affix_datetime(
#'   filename = "pb_file",
#'   position = "start"
#' )
#' 
#' # El patrón de fecha y hora se puede
#' # cambiar y así puede el delimitador
#' affix_datetime(
#'   filename = "pb_file.yml",
#'   format = "%Y%m%d_%H%M%S",
#'   delimiter = "-"
#' )
#' 
#' # Se puede incluir información sobre
#' # la zona horaria
#' affix_datetime(
#'   filename = "pb_file.yml",
#'   add_tz = TRUE
#' )
#' 
#' if (interactive()) {
#' 
#' # Podemos usar una convención de
#' # nomenclatura de archivos que
#' # involucran fechas y horas al
#' # escribir la salida archivos
#' # inmediatamente después de interrogar;
#' # útil al interrogar directamente
#' # de YAML en un proceso programado
#' yaml_agent_interrogate(
#'   filename = system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ) %>% 
#'   x_write_disk(
#'     filename = affix_datetime(
#'       filename = "small_table_agent.rds",
#'       delimiter = "-"
#'     ),
#'     keep_tbl = TRUE,
#'     keep_extracts = TRUE
#'   )
#' 
#' }
#'
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-4
#'
#' @seealso La función [affix_date()] proporciona las mismas características
#'   excepto que produce una cadena de fecha por defecto.
#' 
#' @export

# col_exists---------------------------------------------------------------
#' ¿Existe realmente una o más columnas?
#'
#' @description
#' La función de validación `col_exists()`, la expectativa `expect_col_exists()`
#' función, y la función de prueba `test_col_exists()` comprueban si uno o
#' existen más columnas en la tabla de destino. El único requisito es la
#' especificación de los nombres de las columnas. La función de validación se
#' puede utilizar directamente en un dato. tabla o con un objeto * agent *
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con datos mesa. Los tipos de tablas
#' de datos que se pueden utilizar incluyen marcos de datos, tibbles, tablas de
#' base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada paso de
#' validación o expectativa operará en una sola unidad de prueba, que es si la
#' columna existe o no.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que se produce mejor con la función [action_levels()]. Lee esa
#' función documentación para la verdad sobre cómo crear reacciones por encima
#' del umbral niveles de falla en la validación. La esencia básica es que
#' querrás al menos un nivel de umbral único (especificado como la fracción de
#' unidades de prueba falló, o un valor absoluto), a menudo usando el argumento
#' `warn_at`. Utilizando `action_levels(warn_at = 1)` o `action_levels(stop_at =
#' 1)` son buenas opciones dependiendo de la situación (el primero produce una
#' advertencia, el otro `stop()`).
#'
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_exists()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_exists()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_exists(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_exists()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_exists:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_exists()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param columns Una o más columnas de la tabla en foco. Esto se puede
#'   proporcionar como un vector de nombres de columnas usando `c()` o nombres
#'   de columnas desnudos encerrados entre [vars()].
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con
#' # dos columnas: `a` y` b`
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3)
#'   )
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valide que las columnas `a` y` b`
#' # existan en la tabla `tbl`; esto hace
#' # dos pasos de validación distintos ya
#' # que se proporcionaron dos columnas
#' # a `vars()`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_exists(vars(a, b)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% col_exists(vars(a, b))
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_exists(tbl, vars(a))
#' expect_col_exists(tbl, vars(b))
#' 
#' # D: Usando la función de prueba
#' 
#' # Con el formulario `test_*()`,
#' # deberíamos obtener un único valor
#' # lógico que se nos devuelva (incluso
#' # si hay varias columnas probadas,
#' # como es el caso a continuación)
#' tbl %>% test_col_exists(vars(a, b))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-29
#' @name col_exists

# col_is_character---------------------------------------------------------
#' ¿Las columnas contienen datos de caracteres / cadenas?
#'
#' @description
#' La función de validación `col_is_character()`, la función de expectativa
#' `expect_col_is_character()` y la función de prueba `test_col_is_character()`
#' comprueban si una o más columnas de una tabla son del tipo carácter. Como
#' muchas de las funciones de tipo `col_is_*()` en **pointblank**, el único
#' requisito es una especificación de los nombres de las columnas. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre una sola unidad de prueba, que
#' es si la columna es una columna de tipo carácter o no.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_is_*()`, usar `action_levels(warn_at = 1)` o
#' `action_levels(stop_at = 1)` son buenas opciones dependiendo de la situación
#' (la primera produce una advertencia, la otra `stop()`).
#'
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#'
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_is_character()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_is_character()` como paso de validación se expresa en código R y en la
#' representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_is_character(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_is_character()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_is_character:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_is_character()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con una
#' # columna numérica (`a`) y una
#' # columna de caracteres (`b`)
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = LETTERS[1:6]
#'   )
#'   
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que la columna `b` tenga la
#' # clase `character`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_is_character(vars(b)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% col_is_character(vars(b))
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_is_character(tbl, vars(b))
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>% test_col_is_character(vars(b))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-22
#' 
#' @name col_is_character

# col_is_date--------------------------------------------------------------
#' ¿Las columnas contienen objetos R `Date`?
#'
#' @description
#' La función de validación `col_is_date()`, la función de expectativa
#' `expect_col_is_date()` y la función de prueba `test_col_is_date()` comprueban
#' si una o más columnas en una tabla es del tipo **R** `Date`. Como muchas de
#' las funciones de tipo `col_is_*()` en **pointblank**, el único requisito es
#' una especificación de los nombres de las columnas. La función de validación
#' se puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre una sola unidad de prueba, que
#' es si la columna es una columna de tipo `Date` o no.
#' 
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_is_*()`, usar `action_levels(warn_at = 1)` o
#' `action_levels(stop_at = 1)` son buenas opciones dependiendo de la situación
#' (la primera produce una advertencia, la otra `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#'
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_is_date()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_is_date()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_is_date(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_is_date()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_is_date:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_is_date()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # The `small_table` dataset in the
#' # package has a `date` column; the
#' # following examples will validate
#' # that that column is of the `Date`
#' # class
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que la columna `date`
#' # tenga la clase `Date`
#' agent <-
#'   create_agent(small_table) %>%
#'   col_is_date(vars(date)) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_is_date(vars(date)) %>%
#'   dplyr::slice(1:5)
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_is_date(
#'   small_table, vars(date)
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_is_date(vars(date))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-26
#' 
#' @name col_is_date

# col_is_factor------------------------------------------------------------
#' ¿Las columnas contienen objetos de `factor` R?
#'
#' @description
#' La función de validación `col_is_factor()`, la función de expectativa
#' `expect_col_is_factor()` y la función de prueba `test_col_is_factor()`
#' comprueban si una o más columnas en una tabla son del tipo factor. Como
#' muchas de las funciones de tipo `col_is_*()` en **pointblank**, el único
#' requisito es una especificación de los nombres de las columnas. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre una sola unidad de prueba, que
#' es si la columna es una columna de tipo factor o no.
#' 
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_is_*()`, usar `action_levels(warn_at = 1)` o
#' `action_levels(stop_at = 1)` son buenas opciones dependiendo de la situación
#' (la primera produce una advertencia, la otra `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_is_factor()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_is_factor()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_is_factor(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_is_factor()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_is_factor:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_is_factor()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'
#' @examples
#' # Modifiquemos la columna `f` en la
#' # tabla `small_table` para que los
#' # valores sean factores en lugar de
#' # tener la clase `character`; los
#' # siguientes ejemplos validarán que la
#' # columna `f` se mutó con éxito y
#' # ahora consta de factores
#' tbl <- 
#'   small_table %>%
#'   dplyr::mutate(f = factor(f))
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que la columna `f` en el
#' # objeto `tbl` sea de la clase `factor`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_is_factor(vars(f)) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>%
#'   col_is_factor(vars(f)) %>%
#'   dplyr::slice(1:5)
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_is_factor(tbl, vars(f))
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>% test_col_is_factor(vars(f))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-28
#' 
#' @name col_is_factor

# col_is_integer-----------------------------------------------------------
#' ¿Las columnas contienen valores enteros?
#'
#' @description
#' La función de validación `col_is_integer()`, la función de expectativa
#' `expect_col_is_integer()` y la función de prueba `test_col_is_integer()`
#' comprueban si una o más columnas de una tabla son de tipo entero. Como muchas
#' de las funciones de tipo `col_is_*()` en **pointblank**, el único requisito
#' es una especificación de los nombres de las columnas. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre una sola unidad de prueba, que
#' es si la columna es una columna de tipo entero o no.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_is_*()`, usar `action_levels(warn_at = 1)` o
#' `action_levels(stop_at = 1)` son buenas opciones dependiendo de la situación
#' (la primera produce una advertencia, la otra `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_is_integer()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_is_integer()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_is_integer(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_is_integer()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_is_integer:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_is_integer()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#' 
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos aquí, usaremos
#' # una tabla simple con una columna de
#' # caracteres (`a`) y una columna de
#' # números enteros (`b`)
#' tbl <-
#'   dplyr::tibble(
#'     a = letters[1:6],
#'     b = 2:7
#'   )
#'   
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que la columna `b` tenga la
#' # clase `integer`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_is_integer(vars(b)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% col_is_integer(vars(b))
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_is_integer(tbl, vars(b))
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>% test_col_is_integer(vars(b))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-24
#' 
#' @name col_is_integer

# col_is_logical-----------------------------------------------------------
#' ¿Las columnas contienen valores lógicos?
#'
#' @description
#' La función de validación `col_is_logical()`, la función de expectativa
#' `expect_col_is_logical()` y la función de prueba `test_col_is_logical()`
#' verifican si una o más columnas en una tabla son lógicas (`TRUE`/` FALSE`).
#' Como muchas de las funciones de tipo `col_is_*()` en **pointblank**, el único
#' requisito es una especificación de los nombres de las columnas. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre una sola unidad de prueba, que
#' es si la columna es una columna de tipo lógico o no.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_is_*()`, usar `action_levels(warn_at = 1)` o
#' `action_levels(stop_at = 1)` son buenas opciones dependiendo de la situación
#' (la primera produce una advertencia, la otra `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_is_logical()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_is_logical()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_is_logical(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_is_logical()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_is_logical:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_is_logical()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete
#' # tiene una columna `e` que tiene valores
#' # lógicos; los siguientes ejemplos
#' # validarán que esa columna es de la
#' # clase `logical`
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que la columna `e` tenga la
#' # clase `logical`
#' agent <-
#'   create_agent(small_table) %>%
#'   col_is_logical(vars(e)) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_is_logical(vars(e)) %>%
#'   dplyr::slice(1:5)
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_is_logical(
#'   small_table, vars(e)
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_is_logical(vars(e))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-25
#' 
#' @name col_is_logical

# col_is_numeric-----------------------------------------------------------
#' ¿Las columnas contienen valores numéricos?
#'
#' @description
#' La función de validación `col_is_numeric()`, la función de expectativa
#' `expect_col_is_numeric()` y la función de prueba `test_col_is_numeric()`
#' verifican si una o más columnas en una tabla son de tipo numérico. Como
#' muchas de las funciones de tipo `col_is_*()` en **pointblank**, el único
#' requisito es una especificación de los nombres de las columnas. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre una sola unidad de prueba, que
#' es si la columna es de tipo numérico o no.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_is_*()`, usar `action_levels(warn_at = 1)` o
#' `action_levels(stop_at = 1)` son buenas opciones dependiendo de la situación
#' (la primera produce una advertencia, la otra `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_is_numeric()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_is_numeric()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_is_numeric(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_is_numeric()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_is_numeric:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_is_numeric()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#' 
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete
#' # tiene una columna `d` que se sabe
#' # que es numérica; los siguientes
#' # ejemplos validarán que esa columna
#' # es de hecho de la clase `numeric`
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que la columna `d` tenga
#' # la clase `numeric`
#' agent <-
#'   create_agent(small_table) %>%
#'   col_is_numeric(vars(d)) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_is_numeric(vars(d)) %>%
#'   dplyr::slice(1:5)
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_is_numeric(
#'   small_table, vars(d)
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_is_numeric(vars(d))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-23
#' 
#' @name col_is_numeric

# col_is_posix-------------------------------------------------------------
#' ¿Las columnas contienen fechas `POSIXct`?
#'
#' @description
#' La función de validación `col_is_posix()`, la función de expectativa
#' `expect_col_is_posix()` y la función de prueba `test_col_is_posix()`
#' comprueban si una o más columnas en una tabla es del tipo de fecha y hora R
#' `POSIXct`. Como muchas de las funciones de tipo `col_is_*()` en
#' **pointblank**, el único requisito es una especificación de los nombres de
#' las columnas. La función de validación se puede usar directamente en una
#' tabla de datos o con un objeto *agent* (técnicamente, un objeto
#' `ptblank_agent`) mientras que las funciones de expectativa y prueba solo se
#' pueden usar con una tabla de datos. Los tipos de tablas de datos que se
#' pueden utilizar incluyen marcos de datos, tibbles, tablas de base de datos
#' (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada paso de validación o
#' expectativa operará sobre una sola unidad de prueba, que es si la columna es
#' una columna de tipo `POSIXct` o no.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_is_*()`, usar `action_levels(warn_at = 1)` o
#' `action_levels(stop_at = 1)` son buenas opciones dependiendo de la situación
#' (la primera produce una advertencia, la otra `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML: 
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_is_posix()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_is_posix()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_is_posix(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_is_posix()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_is_posix:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_is_posix()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete
#' # tiene una columna `date_time`; los
#' # siguientes ejemplos validarán que esa
#' # columna es de las clases `POSIXct`
#' # y `POSIXt`
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valide que la columna `date_time` sea
#' # de hecho una columna de fecha y hora
#' agent <-
#'   create_agent(small_table) %>%
#'   col_is_posix(vars(date_time)) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba defectuosas (1)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_is_posix(vars(date_time)) %>%
#'   dplyr::slice(1:5)
#' 
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_is_posix(
#'   small_table, vars(date_time)
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_is_posix(vars(date_time))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-27
#' 
#' @name col_is_posix

# col_schema---------------------------------------------------------------
#' Genere un esquema de columna de tabla manualmente o con una tabla de
#' referencia
#' 
#' Un objeto de esquema de columna de tabla, como puede ser creado por
#' `col_schema()`, es necesario cuando se usa la función de validación
#' [col_schema_match()] (que verifica si el objeto de tabla en estudio coincide
#' con un esquema de columna conocido). El objeto `col_schema` se puede crear
#' proporcionando cuidadosamente los nombres de las columnas y sus tipos como un
#' conjunto de argumentos con nombre, o podríamos proporcionar un objeto de
#' tabla, que podría ser de `data.frame`, `tbl_df`, `tbl_dbi`, o `tbl_spark`.
#' Hay una opción adicional, que es solo para validar el esquema de
#' un objeto `tbl_dbi` o `tbl_spark`: podemos validar el esquema en función de
#' los tipos de columna R (por ejemplo, `"numeric"`, `"character"`, etc.),
#' tipos de columna SQL (por ejemplo, `"double"`, `"varchar"`, etc.), o
#' tipos de columna Spark SQL (`"DoubleType"`, `"StringType"`, etc.). Esto
#' es genial si queremos validar los esquemas de columna de la tabla tanto en el
#' lado del servidor como cuando se recopilan y cargan datos tabulares en R.
#' 
#' @param ... Un conjunto de argumentos con nombre donde los nombres se refieren
#'   a nombres de columna y los valores son uno o más tipos de columna.
#' @param .tbl Una opción para usar un objeto de tabla para definir el esquema.
#'   Si se proporciona esto, se ignorarán los valores proporcionados a `...`.
#' @param .db_col_types Determina si los tipos de columna se refieren a tipos de
#'   columna R (`"r"`) o tipos de columna SQL (`"sql"`).
#'   
#' @examples 
#' # Cree una tabla simple con dos
#' # columnas: una `integer` y la
#' # otra `character`
#' tbl <- 
#'   dplyr::tibble(
#'     a = 1:5,
#'     b = letters[1:5]
#'   )
#' 
#' # Cree un objeto de esquema de
#' # columna que describa las columnas
#' # y sus tipos (en el orden esperado)
#' schema_obj <- 
#'   col_schema(
#'     a = "integer",
#'     b = "character"
#'   )
#' 
#' # Valide que el objeto de esquema
#' # `schema_obj` defina exactamente los
#' # nombres de columna y los tipos de
#' # columna de la tabla `tbl`
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_schema_match(schema_obj) %>%
#'   interrogate()
#' 
#' # Determine si este paso de validación
#' # pasó usando `all_passed()`
#' all_passed(agent)
#' 
#' # Alternativamente, podemos crear un
#' # objeto de esquema de columna a partir
#' # de un objeto `tbl_df`
#' schema_obj <-
#'   col_schema(
#'     .tbl = dplyr::tibble(
#'       a = integer(0),
#'       b = character(0)
#'     )
#'   )
#'
#' # Esto debería proporcionar los mismos
#' # resultados de interrogación que en
#' # el ejemplo anterior
#' create_agent(tbl = tbl) %>%
#'   col_schema_match(schema_obj) %>%
#'   interrogate() %>%
#'   all_passed()
#'   
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-1
#' 
#' @export

# col_schema_match---------------------------------------------------------
#' ¿Las columnas de la tabla (y sus tipos) coinciden con un esquema predefinido?
#'
#' @description
#' La función de validación `col_schema_match()`, la función de expectativa
#' `expect_col_schema_match()` y la función de prueba `test_col_schema_match()`
#' funcionan en conjunto con un objeto `col_schema` (generado a través de la
#' función [col_schema()]) para determinar si el esquema esperado coincide con
#' el de la tabla de destino. La función de validación se puede usar
#' directamente en una tabla de datos o con un objeto *agent* (técnicamente, un
#' objeto `ptblank_agent`) mientras que las funciones de expectativa y prueba
#' solo se pueden usar con una tabla de datos. Los tipos de tablas de datos que
#' se pueden utilizar incluyen marcos de datos, tibbles, tablas de base de datos
#' (`tbl_dbi`) y Spark DataFrames (`tbl_spark`).
#' 
#' El paso de validación o expectativa opera sobre una sola unidad de prueba,
#' que es si el esquema coincide con el de la tabla (dentro de las restricciones
#' impuestas por las opciones `complete`, `in_order` y `is_exact`). Si la tabla
#' de destino es un objeto `tbl_dbi` o `tbl_spark`, podemos optar por validar el
#' esquema de columna que se basa en los tipos de columna R (por ejemplo,
#' `"numeric"`, `"character"`, etc.), Tipos de columna SQL (p. Ej., `"Double"`,
#' `"varchar"`, etc.) o tipos de Spark SQL (p. Ej., `"DoubleType"`,
#' `"StringType"`, etc.). Esa opción se define en la función [col_schema()] (es
#' el argumento `.db_col_types`).
#' 
#' Hay opciones para hacer que la verificación de esquemas sea menos estricta
#' (de forma predeterminada, esta validación opera con el nivel más alto de
#' rigor). Con la opción `complete` establecida en `FALSE`, podemos proporcionar
#' un objeto `col_schema` con una inclusión parcial de columnas. El uso de
#' `in_order` establecido en` FALSE` significa que no es necesario que las
#' columnas definidas en el objeto `schema` estén en el mismo orden que en la
#' tabla de destino. Finalmente, la opción `is_exact` establecida en `FALSE`
#' significa que no es necesario proporcionar todas las clases / tipos de
#' columna para una columna en particular. Incluso puede ser `NULL`, omitiendo
#' la verificación del tipo de columna.
#'
#' @section Actions:
#' A menudo, querremos especificar "acciones" para la validación. Este
#' argumento, presente en cada función de validación, toma un objeto de lista
#' especialmente diseñado que es mejor producido por la función
#' [action_levels()]. Lea la documentación de esa función para obtener
#' información sobre cómo crear reacciones a niveles de falla por encima del
#' umbral en la validación. La esencia básica es que querrá al menos un nivel de
#' umbral único (especificado como la fracción de unidades de prueba fallidas o
#' un valor absoluto), a menudo utilizando el argumento `warn_at`. El uso de
#' `action_levels(warn_at = 1)` o `action_levels(stop_at = 1)` son buenas
#' opciones dependiendo de la situación (el primero produce una advertencia, el
#' otro `stop()`).
#'
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_schema_match()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_schema_match()` como paso de validación se expresa en código R y en la
#' representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_schema_match(
#'     schema = col_schema(
#'       a = "integer",
#'       b = "character"
#'     ), 
#'     complete = FALSE,
#'     in_order = FALSE,
#'     is_exact = FALSE,
#'     actions = action_levels(stop_at = 1),
#'     label = "El paso `col_schema_match()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_schema_match:
#'     schema:
#'       a: integer
#'       b: character
#'     complete: false
#'     in_order: false
#'     is_exact: false
#'     actions:
#'       stop_count: 1.0
#'     label: El paso `col_schema_match()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos ya que solo el argumento
#' `schema` requiere un valor. Los argumentos con valores predeterminados no se
#' escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#' 
#' @inheritParams col_vals_gt
#' @param schema Un esquema de tabla de tipo `col_schema` que se puede generar
#'   usando la función [col_schema()].
#' @param complete Este es un requisito para tener en cuenta todas las columnas
#'   de la tabla en el `schema` proporcionado. De forma predeterminada, es
#'   `TRUE`, por lo que todos los nombres de columna de la tabla de destino
#'   deben estar presentes en el objeto de esquema. Esta restricción se puede
#'   relajar usando `FALSE`, donde podemos proporcionar un subconjunto de
#'   columnas de tabla en el esquema.
#' @param in_order Este es un requisito estricto para hacer cumplir el orden de
#'   las columnas en el `schema` proporcionado. De forma predeterminada, es
#'   `TRUE` y el orden de las columnas tanto en el esquema como en la tabla de
#'   destino debe coincidir. Al establecerlo en `FALSE`, se elimina este
#'   requisito de orden estricto.
#' @param is_exact Esta opción determina si la verificación de los tipos de
#'   columna debe ser exacta o incluso realizada. Por ejemplo, las columnas en
#'   los marcos de datos R pueden tener varias clases (por ejemplo, una columna
#'   de fecha y hora puede tener las clases `POSIXct` y `POSIXt`). Si usa
#'   `is_exact == FALSE`, el tipo de columna en el esquema definido por el
#'   usuario para un valor de fecha y hora se puede establecer como `"POSIXct"`
#'   *o* `"POSIXt"` y pasar la validación (con esta columna, por lo menos). Esto
#'   se puede ir un paso más allá y el uso de `NULL` para un tipo de columna en
#'   el esquema definido por el usuario omitirá la verificación de validación de
#'   un tipo de columna. De forma predeterminada, `is_exact` se establece en
#'   `TRUE`.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con dos
#' # columnas: una `integral` (`a`) y la
#' # otra `caracteres` (`b`); los
#' # siguientes ejemplos validarán que
#' # las columnas de la tabla coinciden
#' # con un esquema creado por
#' # `col_schema()`.
#' tbl <- 
#'   dplyr::tibble(
#'     a = 1:5,
#'     b = letters[1:5]
#'   )
#'   
#' tbl
#' 
#' # Crear un objeto de esquema de
#' # columnas con la función de ayuda
#' # `col_schema()` que describa las
#' # columnas y sus tipos (en el
#' # orden esperado)
#' schema_obj <- 
#'   col_schema(
#'     a = "integer",
#'     b = "character"
#'   )
#'   
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que el objeto de esquema
#' # `schema_obj` define exactamente los
#' # nombres y tipos de columnas
#' agent <-
#'   create_agent(tbl) %>%
#'   col_schema_match(schema_obj) %>%
#'   interrogate()
#' 
#' # Determinar si esta validación no tenía
#' # unidades de prueba que fallaran (hay
#' # una única unidad de prueba gobernada
#' # por si hay una coincidencia)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% col_schema_match(schema_obj)
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_schema_match(tbl, schema_obj)
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>% test_col_schema_match(schema_obj)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-30
#' 
#' @name col_schema_match

# col_vals_between---------------------------------------------------------
#' ¿Los datos de la columna se encuentran entre dos valores especificados o los
#' datos de otras columnas?
#'
#' @description
#' La función de validación `col_vals_between()`, la función de expectativa
#' `expect_col_vals_between()` y la función de prueba `test_col_vals_between()`
#' comprueban si los valores de las columnas de una tabla están dentro de un
#' rango. El rango especificado con tres argumentos: `left`, `right` e
#' `inclusive`. Los valores `left` y `right` especifican los límites superior e
#' inferior. Los límites se pueden especificar como valores literales simples o
#' como nombres de columna dados en `vars()`. El argumento `inclusive`, como un
#' vector de dos valores lógicos relacionados con "izquierda" y "derecha",
#' establece si cada límite es inclusivo o no. El valor predeterminado es
#' `c(TRUE, TRUE)`, donde ambos extremos son inclusivos (es decir, `[izquierda,
#' derecha]`). Para versiones parcialmente ilimitadas de esta función, podemos
#' usar las funciones de validación [col_vals_lt()], [col_vals_lte()],
#' [col_vals_gt()] o [col_vals_gte()]. La función de validación se puede usar
#' directamente en una tabla de datos o con un objeto *agent* (técnicamente, un
#' objeto `ptblank_agent`) mientras que las funciones de expectativa y prueba
#' solo se pueden usar con una tabla de datos. Los tipos de tablas de datos que
#' se pueden utilizar incluyen marcos de datos, tibbles, tablas de base de datos
#' (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada paso de validación o
#' expectativa operará sobre el número de unidades de prueba que es igual al
#' número de filas en la tabla (después de que se hayan aplicado las
#' `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_between()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_between()` como paso de validación se expresa en código R y en la
#' representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_between(
#'     columns = vars(a),
#'     left = 1,
#'     right = 2,
#'     inclusive = c(TRUE, FALSE),
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_between()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_between:
#'     columns: vars(a)
#'     left: 1.0
#'     right: 2.0
#'     inclusive:
#'     - true
#'     - false
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_between()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos suelen ser más cortos, ya que solo los argumentos de
#' las `columns`, `left` y `right` requieren valores. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param left El límite inferior del rango. La validación incluye este valor
#'   límite (si el primer elemento en `inclusive` es `TRUE`) además de valores
#'   mayores que `left`. Puede ser un valor único o una columna compatible dada
#'   en `vars()`.
#' @param right El límite superior del rango. La validación incluye este valor
#'   límite (si el segundo elemento en `inclusive` es `TRUE`) además de valores
#'   inferiores a `right`. Puede ser un valor único o una columna compatible
#'   dada en `vars()`.
#' @param inclusive Un valor lógico de dos elementos que indica si los límites
#'   `left` y` right` deben ser inclusivos. De forma predeterminada, ambos
#'   límites son inclusivos.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # El conjunto de datos `small_table` en
#' # el paquete tiene una columna de valores
#' # numéricos en `c` (hay algunas `NA` en
#' # esa columna); los siguientes ejemplos
#' # validarán los valores en esa
#' # columna numérica
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valide que los valores en la columna
#' # `c` estén todos entre `1` y `9`;
#' # debido a que hay valores `NA`,
#' # elegiremos dejar que pasen la
#' # validación configurando `na_pass = TRUE`
#' agent <-
#'   create_agent(small_table) %>%
#'   col_vals_between(
#'     vars(c), 1, 9, na_pass = TRUE
#'   ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 13
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_vals_between(
#'     vars(c), 1, 9, na_pass = TRUE
#'   ) %>%
#'   dplyr::pull(c)
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_between(
#'   small_table, vars(c), 1, 9,
#'   na_pass = TRUE
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_vals_between(
#'     vars(c), 1, 9,
#'     na_pass = TRUE
#'   )
#'
#' # Una nota adicional sobre los límites
#' # de esta función: son inclusivos por
#' # defecto (es decir, se aprobarán valores
#' # de exactamente `1` y `9`); podemos
#' # modificar la inclusividad de los límites
#' # superior e inferior con la opción
#' # `inclusive`, que es un vector lógico
#' # de longitud 2
#' 
#' # Al probar con el límite superior no
#' # inclusivo, obtenemos `FALSO` ya que dos
#' # valores son `9` y ahora quedan fuera
#' # del límite superior (o derecho)
#' small_table %>%
#'   test_col_vals_between(
#'     vars(c), 1, 9,
#'     inclusive = c(TRUE, FALSE),
#'     na_pass = TRUE
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-7
#' 
#' @seealso El análogo a esta función: [col_vals_not_between()].
#' 
#' @name col_vals_between

# col_vals_decreasing------------------------------------------------------
#' ¿Los datos de las columnas están disminuyendo por filas?
#'
#' @description
#' La función de validación `col_vals_decreasing()`, la función de expectativa
#' `expect_col_vals_decreasing()` y la función de prueba
#' `test_col_vals_decreasing()` comprueban si los valores de las columnas en una
#' tabla están disminuyendo cuando se mueve hacia abajo en una tabla. Hay
#' opciones para permitir valores `NA` en la columna de destino, permitir fases
#' estacionarias (donde los valores consecutivos no cambian), e incluso para
#' permitir movimientos crecientes hasta un cierto umbral. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_decreasing()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_decreasing()` como paso de validación se expresa en código R y en
#' la representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_decreasing(
#'     columns = vars(a),
#'     allow_stationary = TRUE,
#'     increasing_tol = 0.5,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_decreasing()`.",
#'     active = FALSE
#'   ) %>% yaml_agent_string()
#' 
#' # Representación YAML
#' steps:
#' - col_vals_decreasing:
#'     columns: vars(a)
#'     allow_stationary: true
#'     increasing_tol: 0.5
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_decreasing()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#' 
#' @inheritParams col_vals_gt
#' @param allow_stationary Una opción para permitir pausas en valores
#'   decrecientes. Por ejemplo, si los valores de las unidades de prueba son
#'   `[85, 82, 82, 80, 77]`, la tercera unidad (`82`, que aparece por segunda
#'   vez) fallará cuando `allow_stationary` sea `FALSE` (el valor predeterminado
#'   ). El uso de `allow_stationary = TRUE` dará como resultado que todas las
#'   unidades de prueba en `[85, 82, 82, 80, 77]` se marquen con aprobado.
#' @param increasing_tol Un valor de umbral opcional que permite el movimiento
#'   de valores numéricos en la dirección positiva. De forma predeterminada, es
#'   `NULL`, pero se utiliza un valor numérico para establecer el umbral
#'   absoluto de recorrido positivo permitido en las unidades de prueba
#'   numéricas. Tenga en cuenta que establecer un valor aquí también tiene el
#'   efecto de establecer `allow_stationary` en `TRUE`.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @family validation functions
#' @section Function ID:
#' 2-14
#' 
#' @seealso La función análoga que se mueve en la dirección opuesta:
#' [col_vals_increasing()].
#' 
#' @name col_vals_decreasing

# col_vals_equal-----------------------------------------------------------
#' ¿Son los datos de la columna iguales a un valor fijo o los datos de otra
#' columna?
#' 
#' @description
#' La función de validación `col_vals_equal()`, la función de expectativa
#' `expect_col_vals_equal()` y la función de prueba `test_col_vals_equal()`
#' comprueban si los valores de las columnas en una tabla son iguales a un
#' `value` especificado. El `value` se puede especificar como un valor literal
#' único o como un nombre de columna dado en `vars()`. La función de validación
#' se puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_equal()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_equal()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_equal(
#'     columns = vars(a),
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_equal()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_equal:
#'     columns: vars(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_equal()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param value Un valor utilizado para esta prueba de igualdad. Puede ser un
#'   valor único o una columna compatible dada en `vars()`. Cualquier valor de
#'   columna igual a lo que se especifica aquí pasará la validación.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con tres
#' # columnas numéricas (`a`,` b` y `c`)
#' # y tres columnas de caracteres
#' # (`d`, `e` y `f`)
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 5, 5, 5, 5, 5),
#'     b = c(1, 1, 1, 2, 2, 2),
#'     c = c(1, 1, 1, 2, 2, 2),
#'     d = LETTERS[c(1:3, 5:7)],
#'     e = LETTERS[c(1:6)],
#'     f = LETTERS[c(1:6)]
#'   )
#'   
#' tbl
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valide que los valores en la columna
#' # `a` sean todos iguales al valor de `5`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_equal(vars(a), 5) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 6
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% 
#'   col_vals_equal(vars(a), 5) %>%
#'   dplyr::pull(a)
#'   
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_equal(tbl, vars(a), 5)
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' test_col_vals_equal(tbl, vars(a), 5)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-3
#' 
#' @seealso The analogue to this function: [col_vals_not_equal()].
#' 
#' @name col_vals_equal

# col_vals_expr------------------------------------------------------------
#' ¿Coinciden los datos de la columna con una expresión de predicado?
#'
#' @description
#' La función de validación `col_vals_expr()`, la función de expectativa
#' `expect_col_vals_expr()` y la función de prueba `test_col_vals_expr()`
#' comprueban si los valores de las columnas en una tabla concuerdan con una
#' expresión de predicado definida por el usuario. La función de validación se
#' puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_expr()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_expr()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_expr(
#'     expr = ~ a %% 1 == 0,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_expr()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_expr:
#'     expr: ~a%%1 == 0
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_expr()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos ya que solo el argumento
#' `expr` requiere un valor. Los argumentos con valores predeterminados no se
#' escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'   
#' @inheritParams col_vals_gt
#' @param expr Una expresión para usar en esta prueba. Esto puede ser en forma
#'   de una llamada realizada con la función `expr()` o como una fórmula **R**
#'   unilateral (usando un `~` inicial).
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con tres
#' # columnas numéricas (`a`, `b` y `c`)
#' # y tres columnas de caracteres
#' # (`d`, `e` y `f`)
#' tbl <-
#'   dplyr::tibble(
#'     a = c(1, 2, 1, 7, 8, 6),
#'     b = c(0, 0, 0, 1, 1, 1),
#'     c = c(0.5, 0.3, 0.8, 1.4, 1.9, 1.2),
#'   )
#'   
#' tbl
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valide que los valores en la columna
#' # `a` sean de tipo entero usando el
#' # operador de módulo R y esperando `0`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_expr(expr(a %% 1 == 0)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 6
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% 
#'   col_vals_expr(expr(a %% 1 == 0)) %>%
#'   dplyr::pull(a)
#'   
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_expr(tbl, ~ a %% 1 == 0)
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' test_col_vals_expr(tbl, ~ a %% 1 == 0)
#' 
#' # Variaciones
#' 
#' # Podemos hacer cosas más complejas
#' # aprovechando las funciones `case_when()`
#' # y `between()` (disponibles para su uso
#' # en el paquete pointblank)
#' tbl %>%
#'   test_col_vals_expr(~ case_when(
#'     b == 0 ~ a %>% between(0, 5) & c < 1,
#'     b == 1 ~ a > 5 & c >= 1
#'   ))
#' 
#' # Si solo desea probar un subconjunto de
#' # filas, entonces la declaración
#' # `case_when()` no necesita ser exhaustiva;
#' # todas las filas que no caigan en los
#' # casos se recortarán (lo que nos dará
#' # menos unidades de prueba en general)
#' tbl %>%
#'   test_col_vals_expr(~ case_when(
#'     b == 1 ~ a > 5 & c >= 1
#'   ))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-19
#' 
#' @seealso Estas funciones reexportadas (de **rlang** y **dplyr**) funcionan
#'   bien dentro de `col_vals_expr()` y sus variantes: [rlang::expr()],
#'   [dplyr::between()] y [dplyr::case_when()].
#' 
#' @name col_vals_expr

# col_vals_gt--------------------------------------------------------------
#' ¿Los datos de la columna son mayores que un valor fijo o los datos de otra
#' columna?
#'
#' @description
#' La función de validación `col_vals_gt()`, la función de expectativa
#' `expect_col_vals_gt()` y la función de prueba `test_col_vals_gt()` comprueban
#' si los valores de las columnas en una tabla son *mayores que* un `value`
#' especificado (la comparación exacta utilizada en esta función es `col_val >
#' value`). El `value` se puede especificar como un valor literal único o como
#' un nombre de columna dado en `vars()`. La función de validación se puede usar
#' directamente en una tabla de datos o con un objeto *agent* (técnicamente, un
#' objeto `ptblank_agent`) mientras que las funciones de expectativa y prueba
#' solo se pueden usar con una tabla de datos. Los tipos de tablas de datos que
#' se pueden utilizar incluyen marcos de datos, tibbles, tablas de base de datos
#' (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada paso de validación o
#' expectativa operará sobre el número de unidades de prueba que es igual al
#' número de filas en la tabla (después de que se hayan aplicado las
#' `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_gt()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_gt()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_gt(
#'     columns = vars(a),
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_gt()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_gt:
#'     columns: vars(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_gt()`.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns` and
#' `value` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'
#' @param x Un data.frame, tibble (`tbl_df` o `tbl_dbi`), Spark DataFrame
#'   (`tbl_spark`), o un *agent* objeto de clase `ptblank_agent` que se crea
#'   con [create_agent()].
#' @param object Un data.frame, tibble (`tbl_df` o `tbl_dbi`) o Spark DataFrame
#'   (`tbl_spark`) que sirve como tabla de destino para la función de
#'   expectativa o la función de prueba.
#' @param columns La columna (o un conjunto de columnas, proporcionado como un
#'   vector de caracteres) a la que se debe aplicar esta validación.
#' @param value Un valor utilizado para esta comparación. Puede ser un valor
#'   único o una columna compatible dada en `vars()`. Cualquier valor de columna
#'   mayor que el especificado aquí pasará la validación.
#' @param na_pass ¿Debería considerarse que los valores de `NA` encontrados
#'   pasan unidades de prueba? Esto es por defecto `FALSE`. Ajústelo en `TRUE`
#'   para darle un pase a `NA`.
#' @param preconditions Una expresión opcional para mutar la tabla de entrada
#'   antes de continuar con la validación. Esto se puede proporcionar como una
#'   fórmula R unilateral usando un `~` inicial (por ejemplo,
#'   `~ . %>% dplyr::mutate(col = col + 10)` o como una función (por ejemplo,
#'   `function (x) dplyr::mutate(x, col = col + 10)`. Consulte la sección
#'   *Preconditions* para obtener más información.
#' @param segments Una expresión opcional o un conjunto de expresiones
#'   (contenidas en una lista) que sirven para segmentar la tabla de destino por
#'   valores de columna. Cada expresión se puede dar de una de dos maneras: (1)
#'   como nombres de columna, o (2) como una fórmula de dos lados donde el LHS
#'   contiene un nombre de columna y el RHS contiene los valores de columna para
#'   segmentar. Consulte la sección *Segments* para obtener más detalles sobre
#'   esto.
#' @param actions Una lista que contiene los niveles de umbral para que el paso
#'   de validación pueda reaccionar en consecuencia al superar los niveles
#'   establecidos. Esto se creará con la función auxiliar [action_levels()].
#' @param step_id Uno o más identificadores opcionales para los pasos de
#'   validación únicos o múltiples generados al llamar a una función de
#'   validación. El uso de ID de pasos sirve para distinguir los pasos de
#'   validación entre sí y brinda la oportunidad de proporcionar una etiqueta
#'   más significativa en comparación con el índice de pasos. De forma
#'   predeterminada, es `NULL`, y **pointblank** generará automáticamente el
#'   valor de ID de paso (basado en el índice de paso) en este caso. Se pueden
#'   proporcionar uno o más valores, y el número exacto de valores de ID debe
#'   (1) coincidir con el número de pasos de validación que producirá la llamada
#'   a la función de validación (influenciado por el número de `columns`
#'   proporcionadas), (2) ser un ID cadena no utilizada en ningún paso de
#'   validación anterior, y (3) ser un vector con valores únicos.
#' @param threshold Un valor de umbral de falla simple para usar con las
#'   variantes de función expectativa (`expect_`) y prueba (`test_`). De
#'   forma predeterminada, se establece en `1`, lo que significa que cualquier
#'   unidad de falla en la validación de datos da como resultado una falla
#'   general de la prueba. Los números enteros más allá de `1` indican que
#'   cualquier unidad defectuosa hasta ese valor de umbral absoluto dará como
#'   resultado una **thatthat** prueba o evalúe como `TRUE`. Asimismo,
#'   los valores fraccionarios (entre `0` y `1`) actúan como un umbral de falla
#'   proporcional, donde `0.15` significa que el `15` por ciento de las unidades
#'   de prueba que fallan dan como resultado una falla general de la prueba.
#' @param label Una etiqueta opcional para el paso de validación. Esta etiqueta
#'   aparece en el informe del *agent* y, para una mejor apariencia, debe ser
#'   breve.
#' @param brief Una descripción opcional basada en texto para el paso de
#'   validación. Si no se proporciona nada aquí, el objeto *agent* genera un
#'   *autobrief*, utilizando el lenguaje proporcionado en el argumento `lang` de
#'   [create_agent()] (que por defecto es `"en"` o inglés). El *autobrief*
#'   incorpora detalles del paso de validación, por lo que a menudo es la opción
#'   preferida en la mayoría de los casos (donde un `label` podría ser más
#'   adecuada para describir sucintamente la validación).
#' @param active Un valor lógico que indica si el paso de validación debe estar
#'   activo. Si la función de validación está trabajando con un objeto *agent*,
#'   `FALSE` hará que el paso de validación esté inactivo (aún informando su
#'   presencia y manteniendo los índices de los pasos sin cambios). Si la
#'   función de validación operará directamente en los datos (sin participación
#'   de *agent*), entonces cualquier paso con `active = FALSE` simplemente
#'   pasará los datos sin validación alguna. Aparte de un vector lógico, una
#'   fórmula R unilateral que usa un `~` inicial se puede usar con `.` (que
#'   sirve como la tabla de datos de entrada) para evaluar a un solo valor
#'   lógico. Con este enfoque, la función **pointblank** [has_columns()] se
#'   puede utilizar para determinar si se debe activar un paso de validación
#'   sobre la base de una o más columnas existentes en la tabla (por ejemplo,
#'   `~ . %>% has_columns(vars(d, e))`). El valor predeterminado de `active` es
#'   `TRUE`.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos,
#' # utilizaremos una tabla sencilla
#' # con tres columnas numéricas
#' # (`a`, `b` y `c`) y tres columnas
#' # de caracteres (`d`, `e` y `f`)
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 5, 5, 5, 5, 5),
#'     b = c(1, 1, 1, 2, 2, 2),
#'     c = c(1, 1, 1, 2, 3, 4),
#'     d = LETTERS[a],
#'     e = LETTERS[b],
#'     f = LETTERS[c]
#'   )
#'   
#' tbl
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valida que los valores de la columna
#' # `a` son todos mayores que el valor
#' # de `4`.
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_gt(vars(a), value = 4) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 6
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% 
#'   col_vals_gt(vars(a), value = 4) %>%
#'   dplyr::pull(a)
#'   
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_gt(
#'   tbl, vars(a),
#'   value = 4
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' test_col_vals_gt(
#'   tbl, vars(a), 
#'   value = 4
#' )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-6
#' 
#' @seealso La función análoga con un límite cerrado a la izquierda:
#'   [col_vals_gte()].
#' 
#' @name col_vals_gt

# col_vals_gte-------------------------------------------------------------
#' ¿Los datos de la columna son mayores o iguales a un valor fijo o los datos de
#' otra columna?
#'
#' @description
#' La función de validación `col_vals_gte()`, la función de expectativa
#' `expect_col_vals_gte()` y la función de prueba `test_col_vals_gte()`
#' comprueban si los valores de las columnas en una tabla son *mayores o iguales
#' a* un `value` especificado (el La comparación exacta utilizada en esta
#' función es `col_val> = value`). El `value` se puede especificar como un valor
#' literal único o como un nombre de columna dado en `vars()`. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_gte()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_gte()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_gte(
#'     columns = vars(a),
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_gte()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_gte:
#'     columns: vars(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_gte()`.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns` and
#' `value` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'   
#' @inheritParams col_vals_gt
#' @param value Un valor utilizado para esta comparación. Puede ser un valor
#'   único o una columna compatible dada en `vars()`. Cualquier valor de columna
#'   mayor o igual a lo especificado aquí pasará la validación.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos,
#' # utilizaremos una tabla sencilla
#' # con tres columnas numéricas
#' # (`a`, `b` y `c`) y tres columnas
#' # de caracteres (`d`, `e` y `f`)
#' tbl <-
#'   dplyr::tibble(
#'       a = c(5, 5, 5, 5, 5, 5),
#'       b = c(1, 1, 1, 2, 2, 2),
#'       c = c(1, 1, 1, 2, 3, 4),
#'       d = LETTERS[a],
#'       e = LETTERS[b],
#'       f = LETTERS[c]
#'   )
#'   
#' tbl
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validate that values in column `a`
#' # are all greater than or equal to the
#' # value of `5`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_gte(vars(a), 5) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 6
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% 
#'   col_vals_gte(vars(a), 5) %>%
#'   dplyr::pull(a)
#'   
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_gte(tbl, vars(a), 5)
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' test_col_vals_gte(tbl, vars(a), 5)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-5
#' 
#' @seealso The analogous function with a left-open bound: [col_vals_gt()].
#' 
#' @name col_vals_gte

# col_vals_in_set----------------------------------------------------------
#' ¿Los datos de la columna forman parte de un conjunto de valores específico?
#'
#' @description
#' La función de validación `col_vals_in_set()`, la función de expectativa
#' `expect_col_vals_in_set()` y la función de prueba `test_col_vals_in_set()`
#' comprueban si los valores de las columnas en una tabla son parte de un `set`
#' de valores especificado. La función de validación se puede usar directamente
#' en una tabla de datos o con un objeto *agent* (técnicamente, un objeto
#' `ptblank_agent`) mientras que las funciones de expectativa y prueba solo se
#' pueden usar con una tabla de datos. Los tipos de tablas de datos que se
#' pueden utilizar incluyen marcos de datos, tibbles, tablas de base de datos
#' (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada paso de validación o
#' expectativa operará sobre el número de unidades de prueba que es igual al
#' número de filas en la tabla (después de que se hayan aplicado las
#' `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_in_set()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_in_set()`
#' como paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_in_set(
#'     columns = vars(a),
#'     set = c(1, 2, 3, 4),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_in_set()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_in_set:
#'     columns: vars(a)
#'    set:
#'    - 1.0
#'    - 2.0
#'    - 3.0
#'    - 4.0
#'    preconditions: ~. %>% dplyr::filter(a < 10)
#'    segments: b ~ c("group_1", "group_2")
#'    actions:
#'      warn_fraction: 0.1
#'      stop_fraction: 0.2
#'    label: El paso `col_vals_in_set()`.
#'    active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns`, and
#' `set` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'   
#' @inheritParams col_vals_gt
#' @param set A vector of numeric or string-based elements, where column values
#'   found within this `set` will be considered as passing.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete
#' # se utilizará para validar que los
#' # valores de las columnas forman
#' # parte de un conjunto determinado
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que los valores de la
#' # columna `f` forman parte del
#' # conjunto de valores que contienen
#' # `low`, `mid` y `high`
#' agent <-
#'   create_agent(small_table) %>%
#'   col_vals_in_set(
#'     vars(f), c("low", "mid", "high")
#'   ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 13
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_vals_in_set(
#'     vars(f), c("low", "mid", "high")
#'   ) %>%
#'   dplyr::pull(f) %>%
#'   unique()
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_in_set(
#'   small_table,
#'   vars(f), c("low", "mid", "high")
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_vals_in_set(
#'     vars(f), c("low", "mid", "high")
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-9
#' 
#' @seealso El análogo de esta función: [col_vals_not_in_set()].
#' 
#' @name col_vals_in_set

# col_vals_increasing------------------------------------------------------
#' ¿Los datos de la columna aumentan por fila?
#'
#' @description
#' La función de validación `col_vals_increasing()`, la función de expectativa
#' `expect_col_vals_increasing()` y la función de prueba
#' `test_col_vals_increasing()` comprueban si los valores de las columnas en una
#' tabla están aumentando cuando se mueve hacia abajo en una tabla. Hay opciones
#' para permitir valores `NA` en la columna de destino, permitir fases
#' estacionarias (donde los valores consecutivos no cambian), e incluso para
#' permitir movimientos decrecientes hasta un cierto umbral. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_increasing()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_increasing()` como paso de validación se expresa en código R y en
#' la representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_increasing(
#'     columns = vars(a),
#'     allow_stationary = TRUE,
#'     decreasing_tol = 0.5,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_increasing()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_increasing:
#'     columns: vars(a)
#'     allow_stationary: true
#'     decreasing_tol: 0.5
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_increasing()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#' 
#' @inheritParams col_vals_gt
#' @param allow_stationary An option to allow pauses in decreasing values. For
#'   example if the values for the test units are `[80, 82, 82, 85, 88]` then
#'   the third unit (`82`, appearing a second time) would be marked with *fail*
#'   when `allow_stationary` is `FALSE` (the default). Using `allow_stationary =
#'   TRUE` will result in all the test units in `[80, 82, 82, 85, 88]` to be
#'   marked with *pass*.
#' @param decreasing_tol An optional threshold value that allows for movement of
#'   numerical values in the negative direction. By default this is `NULL` but
#'   using a numerical value with set the absolute threshold of negative travel
#'   allowed across numerical test units. Note that setting a value here also
#'   has the effect of setting `allow_stationary` to `TRUE`.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @family validation functions
#' @section Function ID:
#' 2-13
#' 
#' @seealso The analogous function that moves in the opposite direction:
#' [col_vals_decreasing()].
#' 
#' @name col_vals_increasing

# col_vals_lt--------------------------------------------------------------
#' ¿Los datos de la columna son menores que un valor fijo o los datos de otra
#' columna?
#'
#' @description
#' La función de validación `col_vals_lt()`, la función de expectativa
#' `expect_col_vals_lt()` y la función de prueba `test_col_vals_lt()` comprueban
#' si los valores de las columnas en una tabla son *menores que* un `value`
#' especificado (la comparación exacta utilizada en esta función es
#' `col_val < value`). El `value` se puede especificar como un valor literal
#' único o como un nombre de columna dado en `vars()`. La función de validación
#' se puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_lt()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_lt()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_lt(
#'     columns = vars(a),
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_lt()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_lt:
#'     columns: vars(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_lt()`.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns` and
#' `value` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#' 
#' @inheritParams col_vals_gt
#' @param value A value used for this comparison. This can be a single value or
#'   a compatible column given in `vars()`. Any column values less than what is
#'   specified here will pass validation.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos, utilizaremos
#' # una tabla sencilla con tres columnas
#' # numéricas (`a`, `b` y `c`) y tres
#' # columnas de caracteres (`d`, `e`
#' # y `f`)
#' tbl <-
#'   dplyr::tibble(
#'       a = c(5, 5, 5, 5, 5, 5),
#'       b = c(1, 1, 1, 2, 2, 2),
#'       c = c(1, 1, 1, 2, 3, 4),
#'       d = LETTERS[a],
#'       e = LETTERS[b],
#'       f = LETTERS[c]
#'   )
#'   
#' tbl
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valida que los valores de la columna
#' # `c` son todos menores que el valor
#' # de `5`.
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_lt(vars(c), 5) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 6
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% 
#'   col_vals_lt(vars(c), 5) %>%
#'   dplyr::pull(c)
#'   
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_lt(tbl, vars(c), 5)
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' test_col_vals_lt(tbl, vars(c), 5)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-1
#' 
#' @seealso The analogous function with a right-closed bound: [col_vals_lte()].
#' 
#' @name col_vals_lt

# col_vals_lte-------------------------------------------------------------
#' ¿Los datos de la columna son menores o iguales a un valor fijo o los datos de
#' otra columna?
#'
#' @description
#' La función de validación `col_vals_lte()`, la función de expectativa
#' `expect_col_vals_lte()` y la función de prueba `test_col_vals_lte()`
#' comprueban si los valores de las columnas en una tabla son *menores o iguales
#' a* un `value` especificado (la comparación exacta utilizada en esta función
#' es `col_val < value`). El `value` se puede especificar como un valor literal
#' único o como un nombre de columna dado en `vars()`. La función de validación
#' se puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_lte()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_lte()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_lte(
#'     columns = vars(a),
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_lte()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_lte:
#'     columns: vars(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_lte()`.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns` and
#' `value` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#' 
#' @inheritParams col_vals_gt
#' @param value A value used for this comparison. This can be a single value or
#'   a compatible column given in `vars()`. Any column values less than or equal
#'   to what is specified here will pass validation.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos, utilizaremos
#' # una tabla sencilla con tres columnas
#' # numéricas (`a`, `b` y `c`) y tres
#' # columnas de caracteres (`d`, `e`
#' # y `f`)
#' tbl <-
#'   dplyr::tibble(
#'       a = c(5, 5, 5, 5, 5, 5),
#'       b = c(1, 1, 1, 2, 2, 2),
#'       c = c(1, 1, 1, 2, 3, 4),
#'       d = LETTERS[a],
#'       e = LETTERS[b],
#'       f = LETTERS[c]
#'   )
#'   
#' tbl
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validate that values in column `c`
#' # are all less than or equal to the
#' # value of `4`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_lte(vars(c), 4) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 6
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% 
#'   col_vals_lte(vars(c), 4) %>%
#'   dplyr::pull(c)
#'   
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_lte(tbl, vars(c), 4)
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' test_col_vals_lte(tbl, vars(c), 4)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-2
#' 
#' @seealso The analogous function with a right-open bound: [col_vals_lt()].
#' 
#' @name col_vals_lte

# col_vals_make_set--------------------------------------------------------
#' ¿Un conjunto de valores se tiene en cuenta por completo en una columna de
#' valores?
#'
#' @description
#' La función de validación `col_vals_make_set()`, la función de expectativa
#' `expect_col_vals_make_set()` y la función de prueba
#' `test_col_vals_make_set()` comprueban si los valores de `set` se ven al menos
#' una vez en una columna de la tabla. Un criterio necesario aquí es que no se
#' deben ver *valores adicionales* (fuera de los definidos en el `set`) (este
#' requisito se relaja en la función de validación [col_vals_make_subset()] y en
#' sus variantes de expectativa y prueba). La función de validación se puede
#' usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Each
#' validation step or expectation will operate over the number of test units
#' that is equal to the number of elements in the `set` plus a test unit
#' reserved for detecting column values outside of the `set` (any outside value
#' seen will make this additional test unit fail).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_make_set()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_make_set()` como paso de validación se expresa en código R y en la
#' representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_make_set(
#'     columns = vars(a),
#'     set = c(1, 2, 3, 4),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_make_set()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_make_set:
#'     columns: vars(a)
#'    set:
#'    - 1.0
#'    - 2.0
#'    - 3.0
#'    - 4.0
#'    preconditions: ~. %>% dplyr::filter(a < 10)
#'    segments: b ~ c("group_1", "group_2")
#'    actions:
#'      warn_fraction: 0.1
#'      stop_fraction: 0.2
#'    label: El paso `col_vals_make_set()`.
#'    active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns`, and
#' `set` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'   
#' @inheritParams col_vals_gt
#' @param set A vector of elements that is expected to be equal to the set of
#'   unique values in the target column.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete se
#' # utilizará para validar que los valores
#' # de las columnas forman parte de un
#' # conjunto determinado
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que los valores de la columna
#' # `f` comprenden los valores de `low`,
#' # `mid` y `high`, y que no haya
#' # otros valores
#' agent <-
#'   create_agent(small_table) %>%
#'   col_vals_make_set(
#'     vars(f), c("low", "mid", "high")
#'   ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 4
#' # unidades de prueba)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_vals_make_set(
#'     vars(f), c("low", "mid", "high")
#'   ) %>%
#'   dplyr::pull(f) %>%
#'   unique()
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_make_set(
#'   small_table,
#'   vars(f), c("low", "mid", "high")
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_vals_make_set(
#'     vars(f), c("low", "mid", "high")
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-11
#' 
#' @name col_vals_make_set

# col_vals_make_subset-----------------------------------------------------
#' ¿Es un conjunto de valores un subconjunto de una columna de valores?
#'
#' @description
#' La función de validación `col_vals_make_subset()`, la función de expectativa
#' `expect_col_vals_make_subset()` y la función de prueba
#' `test_col_vals_make_subset()` comprueban si todos los valores de `set` se ven
#' al menos una vez en una columna de tabla. La función de validación se puede
#' usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de elementos en el `set`.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_make_subset()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_make_subset()` como paso de validación se expresa en código R y en
#' la representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_make_subset(
#'     columns = vars(a),
#'     set = c(1, 2, 3, 4),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_make_subset()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_make_subset:
#'     columns: vars(a)
#'    set:
#'    - 1.0
#'    - 2.0
#'    - 3.0
#'    - 4.0
#'    preconditions: ~. %>% dplyr::filter(a < 10)
#'    segments: b ~ c("group_1", "group_2")
#'    actions:
#'      warn_fraction: 0.1
#'      stop_fraction: 0.2
#'    label: El paso `col_vals_make_subset()`.
#'    active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns`, and
#' `set` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'   
#' @inheritParams col_vals_gt
#' @param set A vector of elements that is expected to be a subset of the unique
#'   values in the target column.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete se
#' # utilizará para validar que los valores
#' # de las columnas forman parte de un
#' # conjunto determinado
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que el conjunto de valores
#' # distintos de la columna `f` contenga
#' # al menos el subconjunto definido como
#' # `low` y `high` (la columna tiene
#' # realmente ambos y algunos
#' # valores `mid`)
#' agent <-
#'   create_agent(small_table) %>%
#'   col_vals_make_subset(
#'     vars(f), c("low", "high")
#'   ) %>%
#'   interrogate()
#'   
#' # Determina si esta validación no tiene
#' # unidades de prueba fallidas (hay 2
#' # unidades de prueba, una por elemento
#' # en el `set`)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_vals_make_subset(
#'     vars(f), c("low", "high")
#'   ) %>%
#'   dplyr::pull(f) %>%
#'   unique()
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_make_subset(
#'   small_table,
#'   vars(f), c("low", "high")
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_vals_make_subset(
#'     vars(f), c("low", "high")
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-12
#' 
#' @name col_vals_make_subset

# col_vals_not_between-----------------------------------------------------
#' ¿Los datos de la columna se encuentran fuera de dos valores especificados o
#' los datos de otras columnas?
#' 
#' @description
#' La función de validación `col_vals_not_between()`, la función de expectativa
#' `expect_col_vals_not_between()` y la función de prueba
#' `test_col_vals_not_between()` comprueban si los valores de las columnas en
#' una tabla *no* caen dentro de un rango. El rango especificado con tres
#' argumentos: `left`, `right` e `inclusive`. Los valores `left` y `right`
#' especifican los límites superior e inferior. Los límites se pueden
#' especificar como valores literales simples o como nombres de columna dados en
#' `vars()`. El argumento `inclusive`, como un vector de dos valores lógicos
#' relacionados con `left` y `right`, establece si cada límite es inclusivo o
#' no. El valor predeterminado es `c(TRUE, TRUE)`, donde ambos extremos son
#' inclusivos (es decir, `[izquierda, derecha]`). Para versiones parcialmente
#' ilimitadas de esta función, podemos usar las funciones de validación
#' [col_vals_lt()], [col_vals_lte()], [col_vals_gt()] o [col_vals_gte()]. La
#' función de validación se puede usar directamente en una tabla de datos o con
#' un objeto *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las
#' funciones de expectativa y prueba solo se pueden usar con una tabla de datos.
#' Los tipos de tablas de datos que se pueden utilizar incluyen marcos de datos,
#' tibbles, tablas de base de datos (`tbl_dbi`) y Spark DataFrames
#' (`tbl_spark`). Cada paso de validación o expectativa operará sobre el número
#' de unidades de prueba que es igual al número de filas en la tabla (después de
#' que se hayan aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_not_between()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_not_between()` como paso de validación se expresa en código R y en
#' la representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_not_between(
#'     columns = vars(a),
#'     left = 1,
#'     right = 2,
#'     inclusive = c(TRUE, FALSE),
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_not_between()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_not_between:
#'     columns: vars(a)
#'     left: 1.0
#'     right: 2.0
#'     inclusive:
#'     - true
#'     - false
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_not_between()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que sólo las `columns`,
#' `left` y `right` requieren valores. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param left,right Los valores límite inferior (o izquierdo) y superior (o
#'   derecho) del rango. Pueden expresarse como valores individuales, columnas
#'   compatibles dadas en `vars()`, o una combinación de ambos. Por defecto,
#'   cualquier valor de columna mayor o igual que `left` *y* menor o igual que
#'   `right` fallará la validación. La inclusividad de los límites puede ser
#'   modificada por la opción `inclusive`.
#'   
#' @inheritParams col_vals_between
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete
#' # tiene una columna de valores
#' # numéricos en `c` (hay algunos `NA`
#' # en esa columna); los siguientes
#' # ejemplos validarán los valores de
#' # esa columna numérica
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valida que los valores de la
#' # columna `c` estén todos entre `10`
#' # y `20`; como hay valores `NA`,
#' # elegiremos que pasen la validación
#' # poniendo `na_pass = TRUE`.
#' agent <-
#'   create_agent(small_table) %>%
#'   col_vals_not_between(
#'     vars(c), 10, 20, na_pass = TRUE
#'   ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 13
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_vals_not_between(
#'     vars(c), 10, 20, na_pass = TRUE
#'   ) %>%
#'   dplyr::pull(c)
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_not_between(
#'   small_table, vars(c), 10, 20,
#'   na_pass = TRUE
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_vals_not_between(
#'     vars(c), 10, 20,
#'     na_pass = TRUE
#'   )
#'
#' # Una nota adicional sobre los límites
#' # de esta función: son inclusivos por
#' # defecto; podemos modificar la
#' # inclusividad de los límites superior
#' # e inferior con la opción `inclusive`,
#' # que es un vector lógico de longitud 2
#' 
#' # Al cambiar el límite inferior para
#' # que sea `9` y hacerlo no inclusivo,
#' # obtenemos `TRUE` ya que aunque dos
#' # valores son `9` y quedan fuera del
#' # límite inferior (o izquierdo) (y
#' # cualquier valor `no entre` cuenta
#' # como unidades de prueba que pasan)
#' small_table %>%
#'   test_col_vals_not_between(
#'     vars(c), 9, 20,
#'     inclusive = c(FALSE, TRUE),
#'     na_pass = TRUE
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-8
#' 
#' @seealso El análogo a esta función: [col_vals_between()].
#' 
#' @name col_vals_not_between

# col_vals_not_equal-------------------------------------------------------
#' ¿Los datos de la columna no son iguales a un valor fijo o los datos de otra
#' columna?
#' 
#' @description
#' La función de validación `col_vals_not_equal()`, la función de expectativa
#' `expect_col_vals_not_equal()` y la función de prueba
#' `test_col_vals_not_equal()` comprueban si los valores de las columnas en una
#' tabla *no son* iguales a un `value` especificado. La función de validación se
#' puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_not_equal()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_not_equal()` como paso de validación se expresa en código R y en la
#' representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_not_equal(
#'     columns = vars(a),
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_not_equal()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_not_equal:
#'     columns: vars(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_not_equal()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que sólo los argumentos
#' `columns` y `value` requieren valores. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#' 
#' @inheritParams col_vals_gt
#' @param value Un valor utilizado para esta prueba de desigualdad. Puede ser un
#'   valor único o una columna compatible dada en `vars()`. Cualquier valor de
#'   columna que no sea igual a lo que se especifica aquí pasará la validación.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'
#' @examples
#' # Para todos los ejemplos, utilizaremos
#' # una tabla sencilla con tres columnas
#' # numéricas (`a`, `b` y `c`) y tres
#' # columnas de caracteres (`d`, `e`
#' # y `f`)
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 5, 5, 5, 5, 5),
#'     b = c(1, 1, 1, 2, 2, 2),
#'     c = c(1, 1, 1, 2, 2, 2),
#'     d = LETTERS[c(1:3, 5:7)],
#'     e = LETTERS[c(1:6)],
#'     f = LETTERS[c(1:6)]
#'   )
#'   
#' tbl
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que los valores de la columna
#' # `a` son todos *no* iguales al valor
#' # de `6`.
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_not_equal(vars(a), 6) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 6
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>% 
#'   col_vals_not_equal(vars(a), 6) %>%
#'   dplyr::pull(a)
#'   
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_not_equal(tbl, vars(a), 6)
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' test_col_vals_not_equal(tbl, vars(a), 6)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-4
#' 
#' @seealso The analogue to this function: [col_vals_equal()].
#' 
#' @name col_vals_not_equal

# col_vals_not_in_set------------------------------------------------------
#' ¿Los datos no forman parte de un conjunto específico de valores?
#'
#' @description
#' La función de validación `col_vals_not_in_set()`, la función de expectativa
#' `expect_col_vals_not_in_set()` y la función de prueba
#' `test_col_vals_not_in_set()` comprueban si los valores de columna en una
#' tabla *no son parte* de un `conjunto` de valores especificado. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_not_in_set()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_not_in_set()` como paso de validación se expresa en código R y en
#' la representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_not_in_set(
#'     columns = vars(a),
#'     set = c(1, 2, 3, 4),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_not_in_set()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_not_in_set:
#'     columns: vars(a)
#'    set:
#'    - 1.0
#'    - 2.0
#'    - 3.0
#'    - 4.0
#'    preconditions: ~. %>% dplyr::filter(a < 10)
#'    segments: b ~ c("group_1", "group_2")
#'    actions:
#'      warn_fraction: 0.1
#'      stop_fraction: 0.2
#'    label: El paso `col_vals_not_in_set()`.
#'    active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos ya que sólo los argumentos
#' `columns` y `set` requieren valores. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#' 
#' @inheritParams col_vals_gt
#' @param set Un vector de elementos numéricos o de cadena, donde los valores de
#'   las columnas encontrados dentro de este `set` serán considerados como
#'   fallidos.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # La tabla `small_table` del paquete se
#' # utilizará para validar que los valores
#' # de las columnas no forman parte de un
#' # conjunto determinado
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valida que los valores de la columna
#' # `f` no contengan ninguno de los valores
#' # `lows`, `mids` y `highs`.
#' agent <-
#'   create_agent(small_table) %>%
#'   col_vals_not_in_set(
#'     vars(f), c("lows", "mids", "highs")
#'   ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 13
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_vals_not_in_set(
#'     vars(f), c("lows", "mids", "highs")
#'   ) %>%
#'   dplyr::pull(f) %>%
#'   unique()
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_not_in_set(
#'   small_table,
#'   vars(f), c("lows", "mids", "highs")
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_vals_not_in_set(
#'     vars(f), c("lows", "mids", "highs")
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-10
#' 
#' @seealso The analogue to this function: [col_vals_in_set()].
#' 
#' @name col_vals_not_in_set

# col_vals_not_null--------------------------------------------------------
#' ¿Los datos de la columna no son `NULL`/`NA`?
#'
#' @description
#' La función de validación `col_vals_not_null()`, la función de expectativa
#' `expect_col_vals_not_null()` y la función de prueba
#' `test_col_vals_not_null()` comprueban si los valores de columna en una tabla
#' *no son* valores de `NA` o, en el contexto de la base de datos, *no* valores
#' `NULL`. La función de validación se puede usar directamente en una tabla de
#' datos o con un objeto *agent* (técnicamente, un objeto `ptblank_agent`)
#' mientras que las funciones de expectativa y prueba solo se pueden usar con
#' una tabla de datos. Los tipos de tablas de datos que se pueden utilizar
#' incluyen marcos de datos, tibbles, tablas de base de datos (`tbl_dbi`) y
#' Spark DataFrames (`tbl_spark`). Cada paso de validación o expectativa operará
#' sobre el número de unidades de prueba que es igual al número de filas en la
#' tabla (después de que se hayan aplicado las `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_not_null()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_not_null()` como paso de validación se expresa en código R y en la
#' representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_not_null(
#'     vars(a),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_not_null()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_not_null:
#'     columns: vars(a)
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_not_null()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con
#' # cuatro columnas: `a`, `b`, `c`
#' # y `d`
#' tbl <-
#'   dplyr::tibble(
#'     a = c( 5,  7,  6,  5,  8),
#'     b = c( 7,  1,  0,  0,  0),
#'     c = c(NA, NA, NA, NA, NA),
#'     d = c(35, 23, NA, NA, NA)
#'   )
#'   
#' tbl
#'   
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que todos los valores de la
#' # columna `b` son *no* NA (serían no
#' # `NULL` en un contexto de base de datos,
#' # lo que no es el caso aquí)
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_not_null(vars(b)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 5
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>%
#'   col_vals_not_null(vars(b)) %>%
#'   dplyr::pull(b)
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_not_null(tbl, vars(b))
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>% test_col_vals_not_null(vars(b))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-16
#' 
#' @seealso The analogue to this function: [col_vals_null()].
#' 
#' @name col_vals_not_null

# col_vals_null------------------------------------------------------------
#' ¿Son los datos de la columna `NULL`/`NA`?
#'
#' @description
#' La función de validación `col_vals_null()`, la función de expectativa
#' `expect_col_vals_null()` y la función de prueba `test_col_vals_null()`
#' comprueban si los valores de las columnas en una tabla son valores `NA` o, en
#' el contexto de la base de datos, `NULL` valores. La función de validación se
#' puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#' 
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_null()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_null()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_null(
#'     vars(a),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_null()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_null:
#'     columns: vars(a)
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_null()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo el argumento de
#' las `columns` requiere un valor. Los argumentos con valores predeterminados
#' no se escribirán en YAML cuando se use [yaml_write()] (aunque es aceptable
#' incluirlos con sus valores predeterminados al generar el YAML por otros
#' medios). También es posible obtener una vista previa de la transformación de
#' un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con
#' # cuatro columnas: `a`, `b`, `c`
#' # y `d`
#' tbl <-
#'   dplyr::tibble(
#'     a = c( 5,  7,  6,  5,  8),
#'     b = c( 7,  1,  0,  0,  0),
#'     c = c(NA, NA, NA, NA, NA),
#'     d = c(35, 23, NA, NA, NA)
#'   )
#'   
#' tbl
#'   
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que todos los valores de la
#' # columna `c` son NA (serían `NULL` en
#' # un contexto de base de datos,
#' # lo que no es el caso aquí)
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_null(vars(c)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 5
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>%
#'   col_vals_null(vars(c)) %>%
#'   dplyr::pull(c)
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_null(tbl, vars(c))
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>% test_col_vals_null(vars(c))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-15
#' 
#' @seealso The analogue to this function: [col_vals_not_null()].
#' 
#' @name col_vals_null

# col_vals_regex-----------------------------------------------------------
#' ¿Las cadenas en los datos de la columna coinciden con un patrón de
#' expresiones regulares?
#' 
#' @description
#' La función de validación `col_vals_regex()`, la función de expectativa
#' `expect_col_vals_regex()` y la función de prueba `test_col_vals_regex()`
#' comprueban si los valores de las columnas en una tabla corresponden a una
#' expresión coincidente de `regex`. La función de validación se puede usar
#' directamente en una tabla de datos o con un objeto *agent* (técnicamente, un
#' objeto `ptblank_agent`) mientras que las funciones de expectativa y prueba
#' solo se pueden usar con una tabla de datos. Los tipos de tablas de datos que
#' se pueden utilizar incluyen marcos de datos, tibbles, tablas de base de datos
#' (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada paso de validación o
#' expectativa operará sobre el número de unidades de prueba que es igual al
#' número de filas en la tabla (después de que se hayan aplicado las
#' `preconditions`).
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_regex()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `col_vals_regex()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_regex(
#'     columns = vars(a),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}",
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_regex()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_regex:
#'     columns: vars(a)
#'     regex: '[0-9]-[a-z]{3}-[0-9]{3}'
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_regex()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos ya que sólo los argumentos
#' `columns` y `regex` requieren valores. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param regex Un patrón de expresión regular para comprobar si coincide con la
#'   columna de destino. Cualquier coincidencia de regex con los valores de las
#'   `columns` de destino pasará la validación.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @examples
#' # La tabla `small_table` del paquete
#' # tiene una columna `b` basada en
#' # caracteres con valores que se
#' # adhieren a un patrón muy particular;
#' # los siguientes ejemplos validarán
#' # que esa columna cumple con una
#' # regex patrón
#' small_table
#' 
#' # Este es el patrón regex que se
#' # utilizará en el resto de los ejemplos
#' pattern <- "[0-9]-[a-z]{3}-[0-9]{3}"
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que todos los valores de la
#' # columna `b` coinciden con el
#' # regex `pattern`
#' agent <-
#'   create_agent(small_table) %>%
#'   col_vals_regex(vars(b), pattern) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 13
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' small_table %>%
#'   col_vals_regex(vars(b), pattern) %>%
#'   dplyr::slice(1:5)
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_regex(
#'   small_table,
#'   vars(b), pattern
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' small_table %>%
#'   test_col_vals_regex(
#'     vars(b), pattern
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-17
#' 
#' @name col_vals_regex

# col_vals_within_spec-----------------------------------------------------
#' ¿Los valores de los datos de la columna se ajustan a una especificación?
#' 
#' @description
#' La función de validación `col_vals_within_spec()`, la función de expectativa
#' `expect_col_vals_within_spec()` y la función de prueba
#' `test_col_vals_within_spec()` comprueban si los valores de columna en una
#' tabla corresponden a un tipo de especificación (`spec`) (los detalles son
#' disponible en la sección *Especificaciones*) La función de validación se
#' puede usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#' 
#' @section Especificaciones:
#' Se debe utilizar un tipo de especificación con el argumento `spec`. Se trata
#' de una palabra clave basada en caracteres que corresponde al tipo de datos en
#' las `columns` especificadas. Se pueden utilizar las siguientes palabras
#' clave:
#' 
#' - `"isbn"`: El número de libro estándar internacional (ISBN) es un
#' identificador numérico único para libros, panfletos, kits educativos,
#' microformas y publicaciones digitales/electrónicas. La especificación se ha
#' formalizado en ISO 2108. Esta palabra clave puede utilizarse para validar los
#' ISBN de 10 o 13 dígitos.
#' - `"VIN"`: Un número de identificación del vehículo (VIN) es un código único
#' (que incluye un número de serie) utilizado por la industria del automóvil
#' para identificar vehículos de motor, motocicletas, scooters y ciclomotores
#' individuales, según lo estipulado ISO 3779 e ISO 4030.
#' - `"postal_code[<country_code>]"`: Un código postal (también conocido como
#' código postal, PIN o ZIP, dependiendo de la región) es una serie de letras,
#' dígitos o ambos (a veces incluyendo espacios/puntuación) incluidos en una
#' dirección postal para ayudar a clasificar el correo. Dado que la codificación
#' varía según el país, es necesario proporcionar un código de país en los
#' formatos de 2 (ISO 3166-1 alpha-2) o 3 letras (ISO 3166-1 alpha-3) junto con
#' las palabras clave (por ejemplo, para los códigos postales de Alemania, se
#' puede utilizar `"postal_code[DE]"` o `"postal_code[DEU]"`). El puede
#' utilizarse el alias de la palabra clave `"zip"` para los códigos postales de
#' Estados Unidos.
#' - `"credit_card"`: Se puede validar un número de tarjeta de crédito y esta
#' comprobación funciona en una gran variedad de emisores de tipo de crédito
#' (donde los números de tarjeta se asignan de acuerdo con la norma ISO/IEC
#' 7812). Los números pueden tener varias longitudes (normalmente, son de 14 a
#' 19 dígitos) y la validación clave que se realiza aquí es el uso del algoritmo
#' Luhn.
#' - `"iban[<country_code>]"`: El número internacional de cuenta bancaria (IBAN)
#' es un sistema de identificación de cuentas bancarias en diferentes países con
#' el fin de mejorar las transacciones transfronterizas. Los valores IBAN se
#' validan mediante la conversión a valores enteros y la realización de una
#' operación básica de mod-97 (como se describe en la norma ISO 7064) sobre
#' ellos. Dado que la longitud y la codificación varían en función del país, es
#' necesario proporcionar un código de país en formato de 2 (ISO 3166-1 alpha-2)
#' o 3 letras (ISO 3166-1 alpha-3) junto con las palabras clave (por ejemplo,
#' para los IBAN de Alemania, se puede utilizar `"iban[DE]"` o `"iban[DEU]"`).
#' - `"swift"`: Los códigos de identificación comercial (también conocidos como
#' SWIFT-BIC, BIC o código SWIFT) se definen en un formato estándar descrito por
#' la norma ISO 9362. Estos códigos son identificadores únicos para
#' instituciones financieras y no financieras. SWIFT son las siglas de Society
#' for Worldwide Interbank Financial Telecommunication. Estos números se
#' utilizan al transferir dinero entre bancos, especialmente importante para las
#' transferencias internacionales.
#' - `"phone"`, `"email"`, `"url"`, `"ipv4"`, `"ipv6"`, `"mac"`: Los números de
#' teléfono, las direcciones de correo electrónico, las URL de Internet, las
#' direcciones IPv4 o IPv6 y las direcciones MAC pueden validarse con sus
#' respectivas palabras clave. Estas validaciones utilizan coincidencias basadas
#' en regex para determinar la validez.
#' 
#' Sólo debe proporcionarse un único valor `spec` por llamada a la función.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna, el resultado será una expansión de
#' pasos de validación para ese número de nombres de columna (por ejemplo,
#' `vars(col_a, col_b)` dará lugar a la entrada de dos pasos de validación).
#' Aparte de los nombres de las columnas en comillas y en `vars()`,
#' **tidyselect** funciones auxiliares están disponibles para especificando
#' columnas. Ellos son: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, y `everything()`.
#'
#' @section Valores faltantes:
#' Esta función de validación admite el manejo especial de valores `NA`. El
#' argumento `na_pass` determinará si un valor de `NA` que aparece en una unidad
#' de prueba debe pasar o no. El valor predeterminado de `na_pass = FALSE`
#' significa que cualquier `NA` encontrado acumulará unidades de prueba
#' fallidas.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()` en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `col_vals_within_spec()` se representa en
#' YAML (bajo la clave de nivel superior `steps` como un miembro de la lista),
#' la sintaxis sigue de cerca la firma de la función de validación. A
#' continuación se muestra un ejemplo de cómo una llamada compleja de
#' `col_vals_within_spec()` como paso de validación se expresa en código R y en
#' la representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   col_vals_within_spec(
#'     columns = vars(a),
#'     spec = "email",
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(b < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `col_vals_within_spec()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - col_vals_within_spec:
#'     columns: vars(a)
#'     spec: email
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(b < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `col_vals_within_spec()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos ya que sólo los argumentos
#' `columns` y `spec` requieren valores. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param spec Una cadena de especificación. Los ejemplos son `"email"`,
#'   `"url"`, y `"postal[USA]"`. Todas las opciones se explican en la sección
#'   *Especificaciones*.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#' 
#' @examples
#' # La tabla `specifications` del paquete
#' # tiene columnas de datos de caracteres
#' # que corresponden a cada una de las
#' # especificaciones que se pueden probar;
#' # los siguientes ejemplos validarán que
#' # la columna `email_addresses` tiene 5
#' # valores correctos (esto es cierto si
#' # obtenemos un subconjunto de los datos:
#' # las primeras cinco filas)
#' spec_slice <- specifications[1:5, ]
#' 
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Validar que todos los valores de la
#' # columna `email_addresses` son correctos
#' agent <-
#'   create_agent(spec_slice) %>%
#'   col_vals_within_spec(
#'     vars(email_addresses),
#'     spec = "email"
#'   ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 5
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' spec_slice %>%
#'   col_vals_within_spec(
#'     vars(email_addresses),
#'     spec = "email"
#'   ) %>%
#'   dplyr::select(email_addresses)
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_col_vals_within_spec(
#'   spec_slice,
#'   vars(email_addresses),
#'   spec = "email"
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' spec_slice %>%
#'   test_col_vals_within_spec(
#'     vars(email_addresses),
#'     spec = "email"
#'   )
#' 
#' @family validation functions
#' @section Function ID:
#' 2-18
#' 
#' @name col_vals_within_spec

# conjointly---------------------------------------------------------------
#' Realice múltiples validaciones por filas para la validez conjunta
#'
#' @description 
#' La función de validación `conjointly()`, la función de expectativa
#' `expect_conjointly()` y la función de prueba `test_conjointly()` comprueban
#' si las unidades de prueba en cada índice (típicamente cada fila) pasan todas
#' las validaciones múltiples. Podemos usar funciones de validación que validan
#' las unidades de fila (la serie `col_vals_*()`), verificar la existencia de la
#' columna ([col_exists()]), o validar el tipo de columna (la serie
#' `col_is_*()`). Debido a la restricción impuesta sobre las funciones de
#' validación permitidas, el conjunto de unidades de prueba está compuesto por
#' filas de la tabla (después de que se hayan aplicado las `preconditions`
#' comunes) o son unidades de prueba únicas (para aquellas funciones que validan
#' columnas).
#' 
#' Cada una de las funciones usadas en un paso de validación `conjointly()`
#' (compuesto usando múltiples llamadas a funciones de validación) finalmente
#' realiza una prueba por filas de si todas las subvalidaciones reportaron un
#' *aprobado* para las mismas unidades de prueba. En la práctica, un ejemplo de
#' validación conjunta es probar si los valores de la columna `a` son mayores
#' que un valor específico, mientras que los valores adyacentes en la columna
#' `b` se encuentran dentro de un rango específico. Las funciones de validación
#' que serán parte de la validación conjunta deben ser suministradas como
#' fórmulas **R** unilaterales (usando un `~` inicial y con un `.` como el
#' objeto de datos). La función de validación se puede usar directamente en una
#' tabla de datos o con un objeto *agent* (técnicamente, un objeto
#' `ptblank_agent`) mientras que las funciones de expectativa y prueba solo se
#' pueden usar con una tabla de datos.
#'
#' @section Nombres de columnas:
#' Si proporciona varios nombres de columna en cualquiera de los pasos de
#' validación proporcionados, el resultado será una expansión de los pasos de
#' subvalidación a ese número de nombres de columna. Aparte de los nombres de
#' las columnas entre comillas y en `vars()`, las funciones auxiliares
#' **tidyselect** están disponibles para especificar columnas. Son:
#' `starts_with()`, `ends_with()`, `contains()`, `matches()` y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' Si hay varias `columns` especificadas, el número potencial de pasos de
#' validación será `m` columnas multiplicadas por `n` segmentos resueltos.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()`s en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `conjointly()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `conjointly()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   conjointly(
#'     ~ col_vals_lt(., vars(a), 8),
#'     ~ col_vals_gt(., vars(c), vars(a)),
#'     ~ col_vals_not_null(., vars(b)),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
#'     label = "El paso `conjointly()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - conjointly:
#'     fns:
#'     - ~col_vals_lt(., vars(a), 8)
#'     - ~col_vals_gt(., vars(c), vars(a))
#'     - ~col_vals_not_null(., vars(b))
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `conjointly()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo son necesarias
#' las expresiones para los pasos de validación. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param ... Una colección de fórmulas unilaterales que constan de funciones de
#'   validación que validan unidades de fila (la serie `col_vals_*()`),
#'   existencia de columna ([col_exists()]) o tipo de columna (la serie
#'   `col_is_*()`). Un ejemplo de esto es `~ col_vals_gte(., vars(a), 5.5),
#'   ~ col_vals_not_null(., vars(b)`).
#' @param .list Permite el uso de una lista como alternativa de entrada a `...`.
#'
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'
#' @examples
#' # Para todos los ejemplos aquí, usaremos
#' # una tabla simple con tres columnas
#' # numéricas (`a`, `b` y `c`); esta es
#' # una tabla muy básica, pero será más
#' # útil cuando se expliquen las cosas
#' # más adelante
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 2, 6),
#'     b = c(3, 4, 6),
#'     c = c(9, 8, 7)
#'   )
#'   
#' tbl
#'   
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # Valida una serie de cosas fila por fila
#' # utilizando funciones de validación del
#' # tipo `col_vals*` (todas tienen el mismo
#' # número de unidades de prueba): (1) los
#' # valores de `a` son menores que `8`,
#' # (2) los valores de `c` son mayores que
#' # los valores adyacentes de `a`, y (3) no
#' # hay valores `NA` en `b`.
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   conjointly(
#'     ~ col_vals_lt(., vars(a), value = 8),
#'     ~ col_vals_gt(., vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., vars(b))
#'     ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 3
#' # unidades de prueba, una para cada fila)
#' all_passed(agent)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent)`
#' 
#' # ¿Que esta pasando? Piense en que hay
#' # tres validaciones paralelas, cada una
#' # de las cuales produce una columna de
#' # valores `TRUE` o `FALSE` ("pasa" o
#' # "falla") y alinéelos uno al lado del
#' # otro, cualquier fila con cualquier
#' # valor `FALSE` da como resultado una
#' # unidad de prueba conjunta "fallida"
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>%
#'   conjointly(
#'     ~ col_vals_lt(., vars(a), value = 8),
#'     ~ col_vals_gt(., vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., vars(b))
#'   )
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_conjointly(
#'   tbl,
#'   ~ col_vals_lt(., vars(a), value = 8),
#'   ~ col_vals_gt(., vars(c), value = vars(a)),
#'   ~ col_vals_not_null(., vars(b))
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>%
#'   test_conjointly(
#'     ~ col_vals_lt(., vars(a), value = 8),
#'     ~ col_vals_gt(., vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., vars(b))
#'   )
#'
#' @family validation functions
#' @section Function ID:
#' 2-31
#'
#' @name conjointly

# create_agent-------------------------------------------------------------
#' Crear un objeto **pointblank** *agent*
#'
#' @description
#' La función `create_agent()` crea un objeto *agent*, que se utiliza en un
#' flujo de trabajo de *informes de calidad de datos*. El objetivo general de
#' este flujo de trabajo es generar información de informes útil para evaluar el
#' nivel de calidad de los datos para la tabla de destino. Podemos proporcionar
#' tantas funciones de validación como el usuario desee escribir, aumentando así
#' el nivel de cobertura de validación para esa tabla. El *agent* asignado por
#' la llamada `create_agent()` toma funciones de validación, que se expanden a
#' los pasos de validación (cada uno está numerado). Este proceso se conoce como
#' desarrollo de un *plan de validación*.
#'
#' Las funciones de validación, cuando se llaman en un *agent*, son
#' simplemente instrucciones hasta el punto en que se llama a la función
#' [interrogate()]. Eso inicia el proceso del *agent* actuando sobre el *
#' plan de validación * y obteniendo resultados para cada paso. Una vez que se
#' completa el proceso de interrogación, podemos decir que el *agent* tiene
#' inteligencia. Llamar al *agent* en sí mismo dará como resultado una tabla
#' de informes. También se puede acceder a este informe de la interrogación con
#' la función [get_agent_report()], donde hay más opciones de informe.
#'
#' @section Data Products Obtained from an Agent:
#' Se puede obtener un objeto de lista muy detallado, conocido como x-list,
#' usando la función [get_agent_x_list()] en el *agent*. Esta fuente de
#' información puede tomarse como un todo o desglosarse por el número de paso
#' (con el argumento `i`).
#'
#' A veces es útil ver qué filas fallaron. Al usar la función
#' [get_data_extracts()] en el *agent*, obtenemos una lista de tibbles (para
#' aquellos pasos que tienen extracciones de datos) o un tibble si el paso de
#' validación se especifica con el argumento `i`.
#'
#' Los datos de destino se pueden dividir en partes que representan las
#' porciones de 'pasa' y 'falla' con la función [get_sundered_data()]. Un
#' requisito principal es un agente al que se le haya llamado a [interrogate()].
#' Además, los pasos de validación considerados para esta división de datos
#' deben ser aquellos que operan en valores en una columna (por ejemplo, las
#' funciones `col_vals_*()` o [conjointly()]). Con estos pasos de validación en
#' consideración, las filas sin unidades de prueba fallidas en todos los pasos
#' de validación comprenden la pieza de datos 'aprobada', y las filas con al
#' menos una unidad de prueba fallida en la misma serie de validaciones
#' constituyen la pieza 'fallida'.
#'
#' Si solo necesitamos saber si todas las validaciones pasaron por completo (es
#' decir, todos los pasos no tenían unidades de prueba fallidas), la función
#' [all_passed()] podría usarse en el *agent*. Sin embargo, en la práctica, no
#' es frecuente que todos los pasos de validación de datos estén libres de
#' unidades defectuosas.
#' 
#' Si bien la impresión de un *agent* mostrará el informe *agent* en el Viewer,
#' podemos usar alternativamente [get_agent_report()] para aprovechar otras
#' opciones (por ejemplo, cambiar el idioma, modificar la disposición de las
#' filas del informe, etc.) y devolver el informe como objetos independientes.
#' Por ejemplo, con la opción `display_table = TRUE` (la predeterminada),
#' [get_agent_report()] devolverá un objeto de tabla **gt** (`"gt_tbl"`). Si
#' `display_table` se establece en `FALSE`, obtendremos un marco de datos en su
#' lugar.
#' 
#' @section YAML: 
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]) . Aquí hay un ejemplo de cómo se expresa una
#' llamada compleja de `create_agent()` en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' create_agent(
#'   read_fn = ~ small_table,
#'   tbl_name = "small_table",
#'   label = "Un ejemplo.",
#'   actions = action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   ), 
#'   end_fns = list(
#'     ~ beepr::beep(2),
#'     ~ Sys.sleep(1)
#'   ), 
#'   embed_report = TRUE,
#'   lang = "fr", 
#'   locale = "fr_CA"
#' )
#' 
#' # Representación YAML
#' type: agent
#' read_fn: ~small_table
#' tbl_name: small_table
#' label: Un ejemplo.
#' lang: fr
#' locale: fr_CA
#' actions:
#'   warn_fraction: 0.1
#' stop_fraction: 0.25
#' notify_fraction: 0.35
#' end_fns:
#' - ~beepr::beep(2)
#' - ~Sys.sleep(1)
#' embed_report: true
#' ```
#' 
#' En la práctica, este bloque de YAML será más corto ya que los argumentos con
#' valores predeterminados no se escribirán en YAML cuando se use [yaml_write()]
#' (aunque es aceptable incluirlos con su valor predeterminado al generar el
#' YAML por otros medios). El único requisito para escribir la representación
#' YAML de un *agent* es tener especificado `read_fn` (cualquier tabla
#' proporcionada a `tbl` es ignorado).
#'
#' Lo que sigue típicamente a este fragmento de YAML es una parte de "pasos", y
#' eso corresponde a la adición de pasos de validación a través de funciones de
#' validación. Los artículos de ayuda para cada función de validación tienen una
#' sección *YAML* que describe cómo una función de validación determinada se
#' traduce a YAML.
#'
#' Si necesita obtener una vista previa de la transformación de un *agent* a
#' YAML (sin enviar nada al disco), use la función [yaml_agent_string()]. Si ya
#' tiene un archivo `.yml` que contiene un *agent*, puede echar un vistazo a las
#' expresiones R que se utilizan para regenerar ese agente con
#' [yaml_agent_show_exprs()].
#' 
#' @section Escribir un agente en el disco:
#' Se puede escribir un objeto *agent* en el disco con la función
#' [x_write_disk()]. Esto puede resultar útil para mantener un historial de
#' validaciones y generar vistas de la calidad de los datos a lo largo del
#' tiempo. Los agentes se almacenan en formato RDS serializado y se pueden
#' recuperar fácilmente con la función [x_read_disk()].
#'
#' Se recomienda que las fórmulas de preparación de tablas se proporcionen al
#' argumento `read_fn` de `create_agent()`. De esta manera, cuando se lee un
#' *agent* desde el disco a través de [x_read_disk()], se puede reutilizar para
#' acceder a la tabla de destino (que puede cambiar, de ahí la necesidad de usar
#' una expresión para esto).
#' 
#' @section Combinación de varios agentes en un objeto *multiagent*:
#' Varios objetos *agent* pueden formar parte de un objeto *agent* múltiple, y
#' se pueden usar dos funciones para esto: [create_multiagent()] y
#' [read_disk_multiagent()]. Al recopilar varios agentes que han realizado
#' interrogaciones en el pasado, podemos obtener un informe *multiagent* que
#' muestra cómo evolucionó la calidad de los datos con el tiempo. Este caso de
#' uso es interesante para el control y la gestión de la calidad de los datos, y
#' los informes (que se pueden personalizar con [get_multiagent_report()]) son
#' sólidos frente a los cambios en los pasos de validación para una tabla de
#' destino determinada.
#'
#' @param tbl La tabla de entrada. Puede ser un marco de datos, un tibble, un
#'   objeto `tbl_dbi` o un objeto `tbl_spark`. Alternativamente, se puede usar
#'   una función para leer en la tabla de datos de entrada con el argumento
#'   `read_fn` (en cuyo caso, `tbl` puede ser `NULL`).
#' @param read_fn Una fórmula de preparación de tablas que se usa para acceder a
#'   la tabla de destino. Incluso si se proporciona un `tbl`, esta fórmula se
#'   invocará para obtener los datos (es decir, el `read_fn` tiene prioridad).
#'   Hay dos formas de especificar un `read_fn`: (1) con una expresión de
#'   fórmula del lado derecho (RHS) (p. Ej., `~ {<código de lectura de la
#'   tabla>}`) o (2) como una función (p. Ej., `function () {<código de lectura
#'   de la tabla>}`).
#' @param tbl_name Un nombre opcional para asignar al objeto de la tabla de
#'   entrada. Si no se proporciona ningún valor, se generará un nombre en
#'   función de la información disponible. Este nombre de tabla se mostrará en
#'   el área de encabezado del informe del agente generado al imprimir el
#'   *agent* o llamar a [get_agent_report()].
#' @param label Una etiqueta opcional para el plan de validación. Si no se
#'   proporciona ningún valor, se generará una etiqueta basada en la hora actual
#'   del sistema. Markdown se puede usar aquí para hacer que la etiqueta sea más
#'   atractiva visualmente (aparecerá en el área de encabezado del informe del
#'   agente).
#' @param actions Una opción para incluir una lista con niveles de umbral para
#'   que todos los pasos de validación puedan reaccionar en consecuencia al
#'   exceder los niveles establecidos. Esto se creará con la función auxiliar
#'   [action_levels()]. Si se utiliza una lista de niveles de acción para un
#'   paso de validación específico, se anulará el conjunto predeterminado
#'   especificado aquí.
#' @param end_fns Una lista de expresiones que deben invocarse al final de un
#'   interrogatorio. Cada expresión debe tener la forma de una fórmula R
#'   unilateral, por lo que en general se debe usar esta construcción: `end_fns
#'   = list (~ <declaraciones R>, ~ <declaraciones R>, ...)`. Un ejemplo de una
#'   función incluida en **pointblank** que se puede utilizar con sensatez
#'   aquí es [email_blast()], que envía un correo electrónico del informe de
#'   validación (basado en una condición de envío).
#' @param embed_report Una opción para incrustar un informe de validación basado
#'   en **gt** en el objeto `ptblank_agent`. Si es `FALSE` (el valor
#'   predeterminado), el objeto de la tabla no se generará ni estará disponible
#'   con el *agent* al regresar del interrogatorio.
#' @param lang El idioma que se utilizará para la creación automática de
#'   resúmenes (descripciones breves para cada paso de validación) y para el
#'   *informe del agente* (una tabla de resumen que proporciona el plan de
#'   validación y los resultados de la interrogación. De forma predeterminada,
#'   `NULL` creará inglés (`"en"`) texto. Otras opciones incluyen francés
#'   (`"fr"`), alemán (`"de"`), italiano (`"it"`), español (`"es"`), portugués
#'   (`"pt"`), turco (`"tr"`), chino (`"zh"`), ruso (`"ru"`), polaco (`"pl"`),
#'   danés (`"da"`) , Sueco (`"sv"`) y holandés (`"nl"`).
#' @param locale Un ID de configuración regional opcional que se utilizará para
#'   formatear valores en la tabla de resumen *informe del agente* de acuerdo
#'   con las reglas de la configuración regional. Los ejemplos incluyen
#'   `"en_US"` para inglés (Estados Unidos) y `"fr_FR"` para francés (Francia);
#'   más simplemente, puede ser un identificador de idioma sin una designación
#'   de país, como "es" para español (España, igual que `"es_ES"`).
#'   
#' @return Un objeto `ptblank_agent`.
#'   
#' @examples
#' # Analicemos un análisis de la calidad de
#' # los datos de una tabla extremadamente pequeña;
#' # en realidad se llama `small_table` y podemos
#' # encontrarlo como un conjunto de datos en
#' # este paquete
#' small_table
#' 
#' # Debemos pensar en lo que es tolerable en
#' # términos de calidad de los datos, así que
#' # designemos umbrales de falla proporcionales
#' # a los estados `warn`, `stop` y `notify`
#' # usando `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Ahora cree un objeto `agent` en blanco y
#' # asígnele el objeto` al` (que sirve como valor
#' # predeterminado para todos los pasos de
#' # validación que se pueden anular); los umbrales
#' # estáticos proporcionados por `al` harán que
#' # los informes sean un poco más útiles
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   )
#'
#' # Luego, como con cualquier objeto `agent`,
#' # podemos agregar pasos al plan de validación
#' # usando tantas funciones de validación como
#' # queramos; luego, usamos `interrogate()`
#' # para realizar físicamente las validaciones
#' # y recopilar información
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5) %>%
#'   col_vals_equal(
#'     vars(d), value = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   col_vals_between(
#'     vars(c),
#'     left = vars(a), right = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   interrogate()
#'   
#' # Llamar a un objeto *agent* en la consola
#' # imprime el informe del agente; pero
#' # podemos obtener un objeto `gt_tbl`
#' # directamente con `get_agent_report(agent)`
#' report <- get_agent_report(agent)
#' class(report)
#' 
#' # ¿Qué puedes hacer con el informe? Imprima
#' # desde un fragmento de código R Markdown,
#' # úselo en un correo electrónico **blastula**,
#' # colóquelo en una página web o modifíquelo
#' # con el paquete **gt**
#' 
#' # Por el informe sabemos que el Paso 4
#' # tenía dos unidades de prueba (filas,
#' # en realidad) que fallaron; podemos ver
#' # esas filas con `get_data_extracts()`
#' agent %>% get_data_extracts(i = 4)
#' 
#' # Podemos obtener una 'x-list' para toda
#' # la validación (8 pasos), o solo para el
#' # cuarto paso con `get_agent_x_list()`
#' xl_step_4 <-
#'   agent %>% get_agent_x_list(i = 4)
#'  
#' # Y luego podemos examinar las diferentes
#' # partes de la lista; obtengamos la fracción
#' # de unidades de prueba que fallaron
#' xl_step_4$f_failed
#' 
#' # Simplemente imprimiendo la lista x nos
#' # dirá qué hay disponible allí
#' xl_step_4
#' 
#' # Una 'x-list' que no sea específica de
#' # ningún paso tendrá mucha más información
#' # y una estructura ligeramente diferente; ver
#' # `help(get_agent_x_list)` para más información
#' # get_agent_x_list(agent)
#' 
#' @section Figures:
#' \if{html}{\figure{man_create_agent_1.png}{options: width=100\%}}
#'  
#' @family Planning and Prep
#' @section Function ID:
#' 1-2
#'   
#' @export

# create_informant---------------------------------------------------------
#' Crear un objeto **pointblank** *informant*
#'
#' @description
#' La función `create_informant()` crea un objeto *informant*, que se utiliza en
#' un flujo de trabajo de *gestión de información*. El objetivo general de este
#' flujo de trabajo es registrar, recopilar y generar información útil en tablas
#' de datos. Podemos proporcionar cualquier información que sea útil para
#' describir una tabla de datos en particular. El objeto *informant* creado por
#' la función `create_informant()` toma funciones enfocadas en información:
#' [info_columns()], [info_tabular()], [info_section()], y [info_snippet()].
#'
#' La serie de funciones `info_*()` permite una acumulación progresiva de
#' información sobre la tabla de destino. Las funciones [info_columns()] y
#' [info_tabular()] facilitan la entrada de *texto de información* que concierne
#' a las columnas de la tabla y la tabla propiamente dicha; la función
#' [info_section()] permite la creación de secciones arbitrarias que pueden
#' tener múltiples subsecciones llenas de *texto de información* adicional. El
#' sistema permite valores dinámicos seleccionados de la tabla de destino
#' mediante [info_snippet()], para obtener extractos de texto con nombre de
#' consultas y el uso de `{<snippet_name>}` en el *texto de información*. Para
#' hacer el uso de [info_snippet()] más conveniente para consultas comunes, se
#' proporciona un conjunto de funciones `snip_*()` en el paquete ([snip_list()],
#' [snip_stats()], [snip_lowest()], y [snip_highest()]) aunque puedes usar tus
#' propias expresiones.
#' 
#' Debido a que los fragmentos necesitan consultar la tabla de destino para
#' devolver fragmentos de *texto de información*, la función [incorporate()]
#' debe usarse para iniciar esta acción. Esto también es necesario para que el
#' *informant* actualice otros elementos de metadatos, como los recuentos de
#' filas y columnas. Una vez que se complete el proceso de incorporación, se
#' actualizarán los fragmentos y otros metadatos. Llamar al *informant* en sí
#' mismo dará como resultado una tabla de informes. También se puede acceder a
#' este informe con la función [get_informant_report()], donde hay más opciones
#' de informe.
#' 
#' @section YAML: 
#' Se puede escribir un informante **pointblank** en YAML con [yaml_write()] y
#' el YAML resultante se puede usar para regenerar un informante (con
#' [yaml_read_informant()]) o realizar la acción 'incorporar' usando la tabla de
#' destino (a través de [yaml_informant_incorporate()]). Aquí hay un ejemplo de
#' cómo una llamada compleja de `create_informant()` se expresa en código R y en
#' la representación YAML correspondiente.
#' 
#' ```
#' # Código R
#' create_informant(
#'   read_fn = ~ small_table,
#'   tbl_name = "small_table",
#'   label = "Un ejemplo.",
#'   lang = "fr", 
#'   locale = "fr_CA"
#' )
#' 
#' # Representación YAML
#' type: informant
#' read_fn: ~small_table
#' tbl_name: small_table
#' info_label: Un ejemplo.
#' lang: fr
#' locale: fr_CA
#' table:
#'   name: small_table
#'   _columns: 8
#'   _rows: 13.0
#'   _type: tbl_df
#' columns:
#'   date_time:
#'     _type: POSIXct, POSIXt
#'   date:
#'     _type: Date
#'   a:
#'     _type: integer
#'   b:
#'     _type: character
#'   c:
#'     _type: numeric
#'   d:
#'     _type: numeric
#'   e:
#'     _type: logical
#'   f:
#'     _type: character
#' ```
#' 
#' El YAML generado incluye algunas claves de nivel superior donde `type` y
#' `read_fn` son obligatorios, y dos secciones de metadatos: `table` y
#' `columns`. Las claves que comienzan con un carácter de subrayado son las que
#' se actualizan cada vez que se llama a [incorporate()] en un *informant*. La
#' sección de metadatos `table` puede tener múltiples subsecciones con *texto de
#' información*. De manera similar, la sección de metadatos `columns` puede
#' tener múltiples subsecciones, siempre que sean elementos secundarios de cada
#' una de las claves de columna (en el ejemplo de YAML anterior, `date_time` y
#' `date` son claves de columna y coinciden con los nombres de columna de la
#' tabla). Se pueden agregar secciones adicionales, pero deben tener nombres de
#' clave en el nivel superior que no dupliquen el conjunto predeterminado (es
#' decir, `type`,` table`, `columns`, etc. se tratan como claves reservadas).
#' 
#' @section Escribir un objeto *informant* en el disco:
#' Se puede escribir un objeto *informant* en el disco con la función
#' [x_write_disk()]. Los informantes se almacenan en formato RDS serializado y
#' se pueden recuperar fácilmente con la función [x_read_disk()].
#'
#' Se recomienda que las fórmulas de preparación de tablas se proporcionen al
#' argumento `read_fn` de `create_informant()`. De esta manera, cuando se lee un
#' *informante* desde el disco a través de [x_read_disk()], se puede reutilizar
#' para acceder a la tabla de destino (que puede cambiar, de ahí la necesidad de
#' usar una expresión para esto).
#'
#' @param tbl La tabla de entrada. Puede ser un marco de datos, un tibble, un
#'   objeto `tbl_dbi` o un objeto `tbl_spark`. Alternativamente, se puede usar
#'   una función para leer en la tabla de datos de entrada con el argumento
#'   `read_fn` (en cuyo caso, `tbl` puede ser `NULL`).
#' @param read_fn Una función que se usa para leer los datos. Incluso si se
#'   proporciona un `tbl`, esta función se invocará para obtener los datos (es
#'   decir, el `read_fn` tiene prioridad). Hay dos formas de especificar un
#'   `read_fn`: (1) usando una función (por ejemplo, `function () {<código de
#'   lectura de la tabla>}`) o, (2) con una expresión de fórmula R.
#' @param agent Un objeto *agent* a quemarropa. Este objeto se puede utilizar
#'   en lugar de proporcionar una tabla en `tbl` o una fórmula de preparación de
#'   tablas en `read_fn`.
#' @param tbl_name Un nombre opcional para asignar al objeto de la tabla de
#'   entrada. Si no se proporciona ningún valor, se generará un nombre en
#'   función de la información disponible.
#' @param label Una etiqueta opcional para el informe de información. Si no se
#'   proporciona ningún valor, se generará una etiqueta basada en la hora actual
#'   del sistema. Markdown se puede utilizar aquí para hacer que la etiqueta sea
#'   más atractiva visualmente (aparecerá en el área de encabezado del informe
#'   de información).
#' @param lang El idioma que se utilizará para el informe de información (una
#'   tabla de resumen que proporciona toda la información disponible para la
#'   tabla. De forma predeterminada, `NULL` creará texto en inglés (`"en"`).
#'   Otras opciones incluyen francés (`"fr"`), Alemán (`"de"`), italiano
#'   (`"it"`), español (`"es"`), portugués (`"pt"`), turco (`"tr"`), chino
#'   (`"zh"`), ruso (`"ru"`), polaco (`"pl"`), danés (`"da"`), sueco (`"sv"`) y
#'   holandés (`"nl"`).
#' @param locale Un ID de configuración regional opcional que se utilizará para
#'   dar formato a los valores en el informe de información de acuerdo con las
#'   reglas de la configuración regional. Los ejemplos incluyen `"en_US"` para
#'   inglés (Estados Unidos) y `"fr_FR"` para francés (Francia); más
#'   simplemente, puede ser un identificador de idioma sin una designación de
#'   país, como "es" para español (España, igual que `"es_ES"`).
#'   
#' @return Un objeto `ptblank_informant`.
#' 
#' @examples 
#' # Veamos cómo podemos generar información
#' # útil para una mesa realmente pequeña; en
#' # realidad se llama `small_table` y podemos
#' # encontrarlo como un conjunto de datos en
#' # este paquete
#' small_table
#' 
#' # Cree un objeto `informant` en blanco con
#' # `create_informant()` y el conjunto de
#' # datos `small_table`
#' informant <- 
#'   create_informant(
#'     read_fn = ~small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   )
#' 
#' # Esta función crea información sin ninguna
#' # ayuda adicional al perfilar el objeto de
#' # tabla proporcionado; agrega las secciones:
#' # (1) 'tabla' y (2) 'columnas' y podemos
#' # imprimir el objeto para ver el informe
#' # de información
#' 
#' # Alternativamente, podemos obtener el mismo
#' # informe usando `get_informant_report()`
#' report <- get_informant_report(informant)
#' class(report)
#' 
#' @section Figures:
#' \if{html}{\figure{man_create_informant_1.png}{options: width=100\%}}
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-3
#' 
#' @export

# create_multiagent--------------------------------------------------------
#' Crear un objeto **pointblank** *multiagent*
#'
#' @description 
#' Varios *agents* pueden formar parte de un solo objeto llamado *multiagent*.
#' Esto puede ser útil cuando se reúnen varios agentes que han realizado
#' interrogaciones en el pasado (quizás guardados en el disco con
#' [x_write_disk()]). Cuando formamos parte de un *multiagent*, podemos obtener
#' un informe que muestra cómo la calidad de los datos evolucionó con el tiempo.
#' Esto puede ser de interés cuando es importante monitorear la calidad de los
#' datos e incluso la evolución del propio plan de validación. La tabla de
#' informes, generada imprimiendo un objeto `ptblank_multiagent` o usando la
#' función [get_multiagent_report()], está, por defecto, organizada por el
#' tiempo de interrogación y reconoce automáticamente qué pasos de validación
#' son equivalentes entre interrogaciones.
#'
#' @param ... Uno o más objetos de *agent* **pointblank**.
#' @param lang El idioma que se utilizará para cualquier informe que se generará
#'   a partir del *multiagente*. De forma predeterminada, `NULL` creará texto en
#'   inglés (`"en"`). Otras opciones incluyen francés (`"fr"`), alemán (`"de"`),
#'   italiano (`"it"`), español (`"es"`), portugués (`"pt"`), turco (`"tr"`),
#'   chino (`"zh"`), ruso (`"ru"`), polaco (`"pl"`), danés (`"da"`), sueco
#'   (`"sv"`) y holandés (`"nl"`).
#' @param locale Un ID de configuración regional opcional que se utilizará para
#'   formatear valores en las salidas de informes de acuerdo con las reglas de
#'   la configuración regional. Los ejemplos incluyen `"en_US"` para inglés
#'   (Estados Unidos) y `"fr_FR"` para francés (Francia); más simplemente, puede
#'   ser un identificador de idioma sin una designación de país, como "es" para
#'   español (España, igual que `"es_ES"`).
#'   
#' @return Un objeto `ptblank_multiagent`.
#' 
#' @examples 
#' if (interactive()) {
#' 
#' # Repasemos varios análisis teóricos de
#' # la calidad de los datos de una tabla
#' # extremadamente pequeña; esa tabla se
#' # llama `small_table` y podemos
#' # encontrarla como un conjunto de datos
#' # en este paquete
#' small_table
#' 
#' # Para establecer límites de falla y
#' # condiciones de señal, designamos
#' # umbrales de falla proporcionales a
#' # los estados `warn`, `stop` y
#' # `notify` usando `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.05,
#'     stop_at = 0.10,
#'     notify_at = 0.20
#'   )
#' 
#' # Crearemos cuatro agentes diferentes
#' # y tendremos pasos de validación
#' # ligeramente diferentes en cada uno
#' # de ellos; en el primero, `agent_1`,
#' # se crean ocho pasos de validación
#' # diferentes y el agente interrogará
#' # a la `small_table`
#' agent_1 <-
#'   create_agent(
#'     read_fn = ~ small_table,
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     vars(date_time),
#'     value = vars(date),
#'     na_pass = TRUE
#'   ) %>%
#'   col_vals_gt(
#'     vars(b), 
#'     value = vars(g),
#'     na_pass = TRUE
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_equal(
#'     vars(d), 
#'     value = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   col_vals_between(
#'     vars(c), 
#'     left = vars(a), right = vars(d)
#'   ) %>%
#'   col_vals_not_between(
#'     vars(c),
#'     left = 10, right = 20,
#'     na_pass = TRUE
#'   ) %>%
#'   rows_distinct(vars(d, e, f)) %>%
#'   col_is_integer(vars(a)) %>%
#'   interrogate()
#' 
#' # El segundo agente, `agent_2`,
#' # conserva todos los pasos de `agent_1`
#' # y agrega dos más (el último de los
#' # cuales está inactivo)
#' agent_2 <- 
#'   agent_1 %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), 
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}",
#'     active = FALSE
#'   ) %>%
#'   interrogate()
#' 
#' # El tercer agente, `agent_3`, agrega
#' # un solo paso de validación, elimina
#' # el quinto y desactiva el primero
#' agent_3 <- 
#'   agent_2 %>%
#'   col_vals_in_set(
#'     vars(f),
#'     set = c("low", "mid", "high")
#'   ) %>%
#'   remove_steps(i = 5) %>%
#'   deactivate_steps(i = 1) %>%
#'   interrogate()
#' 
#' # El cuarto y último agente, `agent_4`,
#' # reactiva los pasos 1 y 10, y elimina
#' # el sexto paso
#' agent_4 <-
#'   agent_3 %>%
#'   activate_steps(i = 1) %>%
#'   activate_steps(i = 10) %>%
#'   remove_steps(i = 6) %>%
#'   interrogate()
#' 
#' # Si bien todos los agentes son
#' # ligeramente diferentes entre sí, aún
#' # podemos obtener un informe combinado
#' # de ellos creando un objeto 'multiagent'
#' multiagent <-
#'   create_multiagent(
#'     agent_1, agent_2, agent_3, agent_4
#'   )
#' 
#' # Llamar a un objeto `multiagent` en
#' # la consola imprime el informe
#' # multiagente; pero podemos obtener un
#' # objeto `gt_tbl` con la función
#' # `get_multiagent_report()`
#' report <- get_multiagent_report(multiagent)
#' 
#' class(report)
#' 
#' }
#' 
#' @section Figures:
#' \if{html}{\figure{man_create_multiagent_1.png}{options: width=100\%}}
#' 
#' @family The multiagent
#' @section Function ID:
#' 10-1
#'
#' @export

# db_tbl-------------------------------------------------------------------
#' Obtener una tabla de una base de datos
#' 
#' @description 
#' Si su tabla de destino está en una base de datos, la función `db_tbl()` es
#' una forma práctica de acceder a ella. Esta función simplifica el proceso de
#' obtener un objeto `tbl_dbi`, que generalmente implica una combinación de
#' construir una conexión a una base de datos y usar la función `dplyr::tbl()`
#' con la conexión y el nombre de la tabla (o una referencia a una tabla en un
#' esquema). Puede usar `db_tbl()` como base para obtener una tabla de base de
#' datos para el parámetro `read_fn` en [create_agent()] o [create_informant()].
#' Esto se puede hacer usando un `~` antes de la llamada `db_tbl()` (por
#' ejemplo, `read_fn = ~ db_tbl (...)`). Otra gran opción es proporcionar una
#' fórmula de preparación de tablas que involucre `db_tbl()` a [tbl_store()]
#' para que tenga acceso a las tablas de la base de datos a través de nombres
#' únicos a través de un almacén de tablas.
#'
#' El nombre de usuario y la contraseña se proporcionan a través de variables de
#' entorno. Si lo desea, estos se pueden proporcionar directamente encerrando
#' esos valores en [I()].
#' 
#' @param table El nombre de la tabla, o una referencia a una tabla en un
#'   esquema (vector de dos elementos con los nombres de esquema y tabla).
#'   Alternativamente, esto se puede proporcionar como una tabla de datos para
#'   copiar en una conexión de base de datos en memoria. Esto solo funciona si:
#'   (1) el `db` es` "sqlite"` o `"duckdb" `, (2) el `dbname` fue elegido como
#'   `":memory:"`, y (3) el `data_tbl` es un marco de datos o un objeto tibble.
#' @param dbname El nombre de la base de datos.
#' @param dbtype Ya sea una función de controlador apropiada (por ejemplo,
#'   `RPostgres::Postgres()`) o un nombre corto para el tipo de base de datos.
#'   Los nombres válidos son: `"postgresql"`, `"postgres"`, o `"pgsql"`
#'   (PostgreSQL, usando la función del controlador `RPostgres::Postgres()`);
#'   `"mysql"` (MySQL, usando `RMySQL::MySQL()`); `"duckdb"` (DuckDB, usando
#'   `duckdb::duckdb()`); y `"sqlite"` (SQLite, usando `RSQLite::SQLite()`).
#' @param host,port El host de la base de datos y el número de puerto opcional.
#' @param user,password Las variables de entorno utilizadas para acceder al
#'   nombre de usuario y la contraseña de la base de datos.
#'   
#' @return Un objeto `tbl_dbi`.
#' 
#' @examples 
#' # Puede usar una tabla de base de
#' # datos en memoria y proporcionarle
#' # una tabla en memoria también:
#' small_table_duckdb <- 
#'   db_tbl(
#'     table = small_table,
#'     dbname = ":memory:",
#'     dbtype = "duckdb"
#'   )
#'
#' if (interactive()) {
#'
#' # También es posible obtener un
#' # archivo remoto y guardarlo en una
#' # base de datos en memoria; use el
#' # combo `file_tbl()` + `db_tbl()`
#' all_revenue_large_duckdb <-
#'   db_tbl(
#'     table = file_tbl(
#'       file = from_github(
#'         file = "all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'       )
#'     ),
#'     dbname = ":memory:",
#'     dbtype = "duckdb"
#'   )
#' 
#' # Para las bases de datos remotas,
#' # es muy similar; aquí hay un ejemplo
#' # que accede a la tabla `rna` (en la
#' # base de datos pública de RNA Central)
#' # usando `db_tbl()`
#' rna_db_tbl <- 
#'   db_tbl(
#'     table = "rna",
#'     dbname = "pfmegrnargs",
#'     dbtype = "postgres", 
#'     host = "hh-pgsql-public.ebi.ac.uk",
#'     port = 5432,
#'     user = I("reader"),
#'     password = I("NWDMCE5xdipIjRrp")
#'   )
#' 
#' # El uso de `I()` para el nombre de
#' # usuario y la contraseña significa que
#' # está pasando los valores reales pero,
#' # normalmente, querrá usar los nombres
#' # de las variables de entorno (envvars)
#' # para acceder de forma segura a los
#' # valores de nombre de usuario y
#' # contraseña apropiados cuando se
#' # conecte a una base de datos:
#' example_db_tbl <- 
#'   db_tbl(
#'     table = "<table_name>",
#'     dbname = "<database_name>",
#'     dbtype = "<database_type_shortname>", 
#'     host = "<connection_url>",
#'     port = "<connection_port>",
#'     user = "<DB_USER_NAME>",
#'     password = "<DB_PASSWORD>"
#'   )
#'
#' # Las variables de entorno se pueden
#' # crear editando el archivo de usuario
#' # `.Renviron` y la función
#' # `usethis::edit_r_environ()` hace que
#' # esto sea bastante fácil de hacer
#' 
#' # El almacenamiento de fórmulas de
#' # preparación de tablas en una tienda
#' # de tablas facilita el trabajo con
#' # tablas de base de datos en blanco;
#' # aquí se explica cómo generar un
#' # almacén de tablas con dos entradas
#' # con nombre para la preparación de
#' # la tabla
#' tbls <-
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = pointblank::small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     small_high_duck ~ db_tbl(
#'       table = pointblank::small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ) %>%
#'       dplyr::filter(f == "high")
#'   )
#' 
#' # Ahora es fácil acceder a cualquiera
#' # de estas tablas (la segunda es una
#' # versión mutada) a través de la
#' # función `tbl_get()`
#' tbl_get("small_table_duck", store = tbls)
#' tbl_get("small_high_duck", store = tbls)
#' 
#' # Las fórmulas de preparación de tablas
#' # en `tbls` también podrían usarse en
#' # funciones con el argumento `read_fn`;
#' # esto es gracias a la función
#' # `tbl_source()`
#' agent <- 
#'   create_agent(
#'     read_fn = ~ tbl_source(
#'       "small_table_duck",
#'       store = tbls
#'     )
#'   )
#' 
#' informant <- 
#'   create_informant(
#'     read_fn = ~ tbl_source(
#'       "small_high_duck",
#'       store = tbls
#'     )
#'   )
#' 
#' }
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-6
#'
#' @export

# deactivate_steps---------------------------------------------------------
#' Desactive uno o más de los pasos de validación de un *agent*
#'
#' @description
#' Si fuera necesario desactivar uno o más pasos de validación después de la
#' creación del plan de validación para un *agent*, la función
#' `deactivate_steps()` será útil para ello. Esto tiene el mismo efecto que usar
#' la opción `active = FALSE` (`active` es un argumento en todas las funciones
#' de validación) para los pasos de validación seleccionados. Tenga en cuenta
#' que esto edita directamente el paso de validación, eliminando cualquier
#' función que pueda haber sido definida para si el paso debería estar activo o
#' no.
#'
#' @param agent Un objeto de agente de clase `ptblank_agent`.
#' @param i El número de paso de validación, que se asigna a cada paso de
#'   validación en el orden de definición.
#'
#' @return Un objeto `ptblank_agent`.
#' 
#' @examples 
#' # Cree un agente que tenga el objeto
#' # `small_table` como tabla de destino,
#' # agregue algunos pasos de validación
#' # y luego use `interrogate()`
#' agent_1 <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>%
#'   col_exists(vars(date)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]"
#'   ) %>%
#'   interrogate()
#'   
#' # El segundo paso de validación
#' # se está reconsiderando ahora y
#' # puede eliminarse gradualmente o
#' # mejorarse; en el período
#' # intermedio se decidió que el paso
#' # debería desactivarse por ahora
#' agent_2 <-
#'   agent_1 %>%
#'   deactivate_steps(i = 2) %>%
#'   interrogate()
#'
#' @family Object Ops
#' @section Function ID:
#' 9-7
#'
#' @seealso For the opposite behavior, use the [activate_steps()] function.
#'
#' @export

# draft_validation---------------------------------------------------------
#' Redacte un archivo inicial **pointblank** validation .R / .Rmd con una tabla
#' de datos
#' 
#' @description
#' Genere un borrador del plan de validación en un nuevo archivo .R o .Rmd
#' usando una tabla de datos de entrada. Con este flujo de trabajo, se escaneará
#' la tabla de datos para conocer los datos de su columna y se escribirá un
#' conjunto de pasos de validación de inicio (que constituyen un plan de
#' validación). Es mejor utilizar un extracto de datos que contenga al menos
#' 1000 filas y que esté relativamente libre de datos falsos.
#'
#' Una vez en el archivo, es posible modificar los pasos de validación para que
#' se ajusten mejor a las expectativas del dominio en particular. Si bien la
#' inferencia de columna se utiliza para generar planes de validación
#' razonables, es difícil inferir los valores aceptables sin experiencia en el
#' dominio. Sin embargo, usar `draft_validation()` podría ayudarlo a comenzar
#' en el piso 10 para abordar los problemas de calidad de los datos y, en
#' cualquier caso, es mejor que comenzar con una vista de editor de código
#' vacía.
#' 
#' @param tbl La tabla de entrada. Puede ser un marco de datos, tibble, un
#'   objeto `tbl_dbi` o un objeto `tbl_spark`.
#' @param tbl_name Un nombre opcional para asignar al objeto de la tabla de
#'   entrada. Si no se proporciona ningún valor, se generará un nombre en
#'   función de la información disponible. Este nombre de tabla se mostrará en
#'   el área de encabezado del informe del agente generado al imprimir el
#'   *agent* o llamar a [get_agent_report()].
#' @param file_name Un nombre opcional para el archivo .R o .Rmd. Debe ser un
#'   nombre sin extensión. Por defecto, esto se toma del `tbl_name` pero si no
#'   se proporciona nada para eso, el nombre contendrá el texto
#'   `"draft_validation_"` seguido de la fecha y hora actuales.
#' @param path Aquí se puede especificar una ruta si no se debe intentar colocar
#'   el archivo generado en el directorio de trabajo.
#' @param lang El idioma que se utilizará al crear comentarios para los pasos de
#'   validación generados automáticamente. De forma predeterminada, `NULL`
#'   creará texto en inglés (`"en"`). Otras opciones incluyen francés (`"fr"`),
#'   alemán (`"de"`), italiano (`"it"`), español (`"es"`), portugués (`"pt"`),
#'   turco (`"tr"`), chino (`"zh"`), ruso (`"ru"`), polaco (`"pl"`), danés
#'   (`"da"`), sueco (`"sv"` ) y holandés (`"nl"`).
#' @param output_type Una opción para elegir qué tipo de salida se debe generar.
#'   De forma predeterminada, este es un script .R (`"R"`) pero podría ser
#'   alternativamente un documento R Markdown (`"Rmd"`).
#' @param add_comments ¿Debería haber comentarios que expliquen las
#'   características del plan de validación en el documento generado? De forma
#'   predeterminada, es `TRUE`.
#' @param overwrite ¿Debería sobrescribirse un archivo con el mismo nombre? De
#'   forma predeterminada, es `FALSE`.
#' @param quiet ¿Debería la función *no* informar cuando se escribe el archivo?
#'   Por defecto, esto es `FALSE`.
#'   
#' @return Devuelve de forma invisible `TRUE` si el archivo se ha escrito.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # Proyecto de plan de validación
#' # para la tabla `dplyr::storms`
#' draft_validation(tbl = dplyr::storms)
#' 
#' }
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-11
#' 
#' @export

# email_blast--------------------------------------------------------------
#' Envíe un correo electrónico en un paso de validación o al final de un
#' interrogatorio
#' 
#' @description
#' La función `email_blast()` es útil para enviar un mensaje de correo
#' electrónico que explica el resultado de una validación de **pointblank**. Se
#' alimenta de los paquetes **blastula** y **glue**. Esta función debe ser
#' invocada como parte del argumento `end_fns` de [create_agent()]. También es
#' posible invocar `email_blast()` como parte del argumento `fns` de la función
#' [action_levels()] (es decir, para enviar múltiples mensajes de correo
#' electrónico en la granularidad de diferentes pasos de validación que superen
#' los umbrales de fallo).
#'
#' Para manejar mejor el envío de correos electrónicos con `email_blast()`, se
#' puede utilizar la función análoga [email_create()] con un objeto agente
#' **pointblank** o una lista x obtenida mediante la función
#' [get_agent_x_list()].
#' 
#' @section YAML: 
#' Un objeto agente **pointblank** puede escribirse en YAML con [yaml_write()] y
#' el YAML resultante puede utilizarse para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (mediante
#' [yaml_agent_interrogate()]). He aquí un ejemplo de cómo se expresa el uso de
#' `email_blast()` dentro del argumento `end_fns` de [create_agent()] en código
#' R y en la correspondiente representación YAML.
#' 
#' ```
#' # Código R
#' create_agent(
#'   read_fn = ~ small_table,
#'   tbl_name = "small_table",
#'   label = "Un ejemplo.",
#'   actions = al,
#'   end_fns = list(
#'     ~ email_blast(
#'       x,
#'       to = "joe_public@example.com",
#'       from = "pb_notif@example.com",
#'       msg_subject = "Table Validation",
#'       credentials = blastula::creds_key(
#'         id = "smtp2go"
#'       ),
#'     )
#'   )
#' ) %>%
#'   col_vals_gt(vars(a), 1) %>%
#'   col_vals_lt(vars(a), 7) 
#' 
#' # Representación YAML
#' type: agent
#' read_fn: ~small_table
#' tbl_name: small_table
#' label: Un ejemplo.
#' lang: en
#' locale: en
#' actions:
#'   warn_count: 1.0
#'   notify_count: 2.0
#' end_fns: ~email_blast(x, to = "joe_public@example.com", 
#'   from = "pb_notif@example.com", msg_subject = "Table Validation",
#'   credentials = blastula::creds_key(id = "smtp2go"),
#'   )
#' embed_report: true
#' steps:
#'   - col_vals_gt:
#'     columns: vars(a)
#'     value: 1.0
#'   - col_vals_lt:
#'     columns: vars(a)
#'     value: 7.0
#' ```
#' 
#' @param x Una referencia al objeto x-list preparado internamente por el
#'   agente. Esta versión de la x-list es la misma que la generada a través de
#'   `get_agent_x_list(<agent>)` excepto que esta versión se genera
#'   internamente y por lo tanto sólo está disponible en un contexto de
#'   evaluación interna.
#' @param to,from Las direcciones de correo electrónico de los destinatarios y
#'   del remitente.
#' @param credentials Un objeto de lista de credenciales producido por
#'   cualquiera de las funciones [blastula::creds()],
#'   [blastula::creds_anonymous()], [blastula::creds_key()] o
#'   [blastula::creds_file()]. Consulte la documentación de **blastula** para
#'   obtener información sobre cómo utilizar estas funciones.
#' @param msg_subject La línea de asunto del mensaje de correo electrónico.
#' @param msg_header,msg_body,msg_footer Contenido para la cabecera, el cuerpo y
#'   el pie de página del mensaje de correo electrónico HTML.
#' @param send_condition Una expresión que debe evaluarse como un vector lógico
#'   de longitud 1. Si se evalúa como `TRUE` entonces se enviará el correo
#'   electrónico, si `FALSE` entonces no ocurrirá. La expresión puede usar
#'   variables de la lista x (por ejemplo, `x$notify`, `x$type`, etc.) y todas
#'   esas variables pueden ser exploradas usando la función
#'   [get_agent_x_list()]. La expresión por defecto es `~TRUE %in% x$notify`,
#'   que da como resultado `TRUE` si hay algún valor `TRUE` en el vector lógico
#'   `x$notify` (es decir, cualquier paso de validación da como resultado una
#'   condición condición de 'notify').
#'   
#' @examples
#' # Crear una lista `action_levels()`
#' # con valores absolutos para los
#' # estados `warn` y `notify` (con
#' # umbrales de 1 y 2 unidades
#' # de "fallo")
#' al <- 
#'   action_levels(
#'     warn_at = 1,
#'     notify_at = 2
#'   )
#'   
#' if (interactive()) {
#' 
#' # Validar que los valores de la
#' # columna `a` de `small_tbl` sean
#' # siempre > 1 y que sean siempre < 7;
#' # en primer lugar, aplicar la
#' # directiva `actions_levels()` a `actions`
#' # y configurar un `email_blast()` como
#' # uno de los `end_fns` (por defecto, el
#' # correo electrónico se enviará si hay
#' # un único estado `notify` en todos
#' # los pasos de validación)
#' agent <-
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al,
#'     end_fns = list(
#'       ~ email_blast(
#'         x,
#'         to = "joe_public@example.com",
#'         from = "pb_notif@example.com",
#'         msg_subject = "Table Validation",
#'         credentials = blastula::creds_key(
#'           id = "smtp2go"
#'         ),
#'       )
#'     )
#'   ) %>%
#'   col_vals_gt(vars(a), value = 1) %>%
#'   col_vals_lt(vars(a), value = 7) %>%
#'   interrogate()
#' 
#' }
#' 
#' # El ejemplo anterior no se ha ejecutado
#' # intencionadamente porque las
#' # credenciales de correo electrónico no
#' # están disponibles y las direcciones de
#' # correo electrónico `to` y `from`
#' # son inexistentes
#' 
#' # Para obtener un objeto de correo
#' # electrónico de blastula en lugar de
#' # enviar el mensaje de forma ansiosa,
#' # podemos utilizar la función función
#' # `email_create()`
#' email_object <-
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(a), value = 5) %>%
#'   col_vals_lt(vars(a), value = 7) %>%
#'   interrogate() %>%
#'   email_create()
#'   
#' @family Emailing
#' @section Function ID:
#' 4-1
#' 
#' @export 

# email_create-------------------------------------------------------------
#' Cree un objeto de correo electrónico a partir de un **pointblank** *agent* o
#' *informant*
#' 
#' @description
#' La función `email_create()` produce un objeto de mensaje de correo
#' electrónico que puede ser enviar utilizando el paquete **blastula**. La "x"
#' que necesitamos para esto puede ser ser un agente **pointblank**, la lista x
#' del *agente* (producida desde el *agente* con la función
#' [get_agent_x_list()]), o un **pointblank** *informante*. En todos los casos,
#' el mensaje de correo electrónico aparecerá en el Visor y una **blastula**
#' `email_message` object será devuelto.
#'
#' @param x A **pointblank** *agent*, an *agent* x-list, or a **pointblank**
#'   *informant*. The x-list object can be created with the [get_agent_x_list()]
#'   function. It is recommended that the option `i = NULL` be used with
#'   [get_agent_x_list()] if supplying an x-list as `x`. Furthermore, The option
#'   `generate_report = TRUE` could be used with [create_agent()] so that the
#'   agent report is available within the email.
#' @param msg_header,msg_body,msg_footer Content for the header, body, and
#'   footer components of the HTML email message.
#'   
#' @return A **blastula** `email_message` object.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # Crear una lista `action_levels()`
#' # con valores absolutos para los
#' # estados `warn`, y `notify` (con
#' # umbrales de 1 y 2 unidades
#' # de `fallo`)
#' al <- 
#'   action_levels(
#'     warn_at = 1,
#'     notify_at = 2
#'   )
#' 
#' # En un flujo de trabajo que
#' # implique un objeto `agente`,
#' # podemos hacer uso del argumento
#' # `end_fns` y enviar el informe por
#' # correo electrónico de forma
#' # programática con la función
#' # `email_blast()`, sin embargo, un
#' # flujo de trabajo alternativo es
#' # producir el objeto de correo
#' # electrónico y elegir enviarlo
#' # fuera de la API de pointblank; la
#' # función `email_create()` nos
#' # permite hacer esto con un
#' # objeto `agente`
#' email_object_1 <-
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(a), value = 1) %>%
#'   col_vals_lt(vars(a), value = 7) %>%
#'   interrogate() %>%
#'   email_create()
#' 
#' # Podemos ver el correo HTML
#' # simplemente imprimiendo
#' # `email_object`; debería debería
#' # aparecer en el Visor
#' 
#' # La función `email_create()` también
#' # puede utilizarse en una x-list de
#' # agentes para obtener el mismo
#' # objeto de mensaje de correo
#' # electrónico
#' email_object_2 <-
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(a), value = 5) %>%
#'   col_vals_lt(vars(b), value = 5) %>%
#'   interrogate() %>%
#'   get_agent_x_list() %>%
#'   email_create()
#' 
#' # Un informe producido por el
#' # informante puede convertirse en
#' # un objeto de mensaje de correo
#' # electrónico; creemos un informante
#' # y utilicemos `email_create()`
#' email_object_3 <-
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>%
#'   info_tabular(
#'     info = "A simple table in the
#'     *Examples* section of the function
#'     called `email_create()`."
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "Numbers. On the high side."
#'   ) %>%
#'   info_columns(
#'     columns = vars(b),
#'     info = "Lower numbers. Zeroes, even."
#'   ) %>%
#'   incorporate() %>%
#'   email_create()
#' 
#' }
#' 
#' @family Emailing
#' @section Function ID:
#' 4-2
#' 
#' @export 

# export_report------------------------------------------------------------
#' Exportar un *agent*, *informant*, *multiagent* o escaneo de tabla a HTML
#' 
#' @description 
#' El *agent*, el *informant*, el *multiagent* y el objeto de exploración de la
#' tabla pueden escribirse fácilmente como HTML con `export_report()`. Además,
#' cualquier objeto de informe del *agente*, *informante* y *multiagente*
#' (generados mediante [get_agent_report()], [get_informant_report()] y
#' [get_multiagent_report()]) puede ser proporcionado aquí para su exportación
#' en HTML. Cada documento HTML escrito en el disco es autocontenido y
#' fácilmente visualizable en un en un navegador web.
#'
#' @param x Un objeto *agent* de la clase `ptblank_agent`, un *informant* de
#'   la clase `ptblank_informant`, un *multiagente* de la clase
#'   `ptblank_multiagent`, un escaneo de tabla de la clase `ptblank_tbl_scan`,
#'   o, objetos de informe personalizados (`ptblank_agent_report`,
#'   `ptblank_informant_report`, `ptblank_multiagent_report.wide`,
#'   `ptblank_multiagent_report.long`).
#' @param filename El nombre del archivo que se creará en el disco para la
#'   exportación HTML del objeto proporcionado. Se recomienda incluir la
#'   extensión `".html"`.
#' @param path Una ruta opcional en la que se debe guardar el archivo (esto es
#'   se combina automáticamente con `filename`).
#' @param quiet ¿La función no debe informar cuando se escribe el archivo? En
#'   por defecto es `FALSE`.
#'   
#' @return Devuelve invisiblemente `TRUE` si el archivo ha sido escrito.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # A: Escribir un informe de agente como HTML
#' 
#' # Vamos a recorrer el proceso de
#' # (1) desarrollar un agente con un plan de
#' # validación (que se utilizará para el
#' # análisis de la calidad de los datos de
#' # la tabla `small_table`), (2) interrogar
#' # al agente con la función `interrogate()`,
#' # y (3) escribir el agente y toda su
#' # información en un archivo
#' 
#' # La creación de un objeto `action_levels`
#' # es un paso común del flujo de trabajo al
#' # crear un agente pointblank; designamos
#' # los umbrales de fallo a los estados
#' # `warn`, `stop` y `notify` utilizando
#' # `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Ahora cree un objeto pointblank `agent`
#' # y déle el objeto `al` (que sirve como
#' # valor por defecto para todos los pasos
#' # de validación que pueden ser anulados);
#' # el datos serán referenciados en un
#' # `read_fn`.
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`export_report()`",
#'     actions = al
#'   )
#' 
#' # Luego, como con cualquier objeto
#' # agente, podemos añadir pasos al plan
#' # de validación utilizando tantas
#' # funciones de validación como que
#' # queramos; entonces, `interrogate()`
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5) %>%
#'   interrogate()
#'
#' # El informe del agente se puede
#' # escribir en un archivo HTML con
#' # `export_report()`
#' export_report(
#'   agent,
#'   filename = "agent-small_table.html"
#' )
#' 
#' # Si estás escribiendo constantemente
#' # informes de agente cuando compruebas
#' # periódicamente los datos, podríamos
#' # hacer uso de `affix_date()` o
#' # `affix_datetime()` dependiendo de la
#' # granularidad que necesites; aquí
#' # tienes un ejemplo que escribe el
#' # fichero con el formato
#' # 'agent-small_table-YYYY-mm-dd_HH-MM-SS'
#' export_report(
#'   agent,
#'   filename = affix_datetime(
#'     "agent-small_table.html"
#'   )
#' )
#' 
#' # B: Escribir un informe de informante
#' #    como HTML
#' 
#' # Vamos a repasar el proceso de (1)
#' # crear un objeto informante que
#' # describa mínimamente la tabla
#' # `small_table`, (2) asegurarnos de que
#' # se capturan los datos de la tabla de
#' # destino utilizando la función
#' # `incorporate()`, y (3) escribir el
#' # informe informante en HTML
#' 
#' # Crear un objeto `informant` en
#' # blanco con `create_informant()` y
#' # el conjunto de datos `small_table`;
#' # `incorporate()` para que los
#' # fragmentos de información se integren
#' # en el texto
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`export_report()`"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "high_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "low_a",
#'     fn = snip_lowest(column = "a")
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "From {low_a} to {high_a}."
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`."
#'   ) %>%
#'   incorporate()
#'
#' # El informe del informante puede
#' # escribirse en un archivo HTML con
#' # `export_report()`; hagámoslo con
#' # `affix_date()` para que el nombre del
#' # archivo tenga una marca de fecha
#' export_report(
#'   informant,
#'   filename = affix_date(
#'     "informant-small_table.html"
#'   )
#' )
#' 
#' # C: Escribir una exploración de l
#' #    tabla como HTML
#' 
#' # Podemos obtener un informe qu
#' # describa todos los datos de la
#' # tabla `storms`
#' tbl_scan <- scan_data(tbl = dplyr::storms)
#' 
#' # El objeto de escaneo de la tabla
#' # puede ser escrito en un archivo
#' # HTML con `export_report()`
#' export_report(
#'   tbl_scan,
#'   filename = "tbl_scan-storms.html"
#' )
#' 
#' }
#'
#' @family Object Ops
#' @section Function ID:
#' 9-3
#' 
#' @export

# file_tbl-----------------------------------------------------------------
#' Obtener una tabla de un archivo local o remoto
#' 
#' @description 
#' Si su tabla de destino se encuentra en un archivo, almacenado local o
#' remotamente, la función `file_tbl()` puede hacer posible el acceso a la misma
#' en una sola llamada a la función. Los tipos de archivo compatibles con esta
#' función son: CSV (`.csv`), TSV (`.tsv`), RDA (`.rda`) y RDS (`.rds`). Esta
#' función genera un objeto `tbl_dbl` en memoria, que puede utilizarse como
#' tabla de destino para [create_agent()] y [create_informant()]. La opción
#' ideal para el acceso a los datos con `file_tbl()` es utilizar esta función
#' como parámetro `read_fn` en cualquiera de las funciones `create_*()` antes
#' mencionadas. Esto puede hacerse utilizando un `~` inicial (por ejemplo,.
#' `read_fn = ~file_tbl(...)`).
#'
#' En el caso de uso de datos remotos, podemos especificar una URL que empiece
#' por `http://`, `https://`, etc., y que termine con el archivo que contiene la
#' tabla de datos. Si los archivos de datos están disponibles en un repositorio
#' de GitHub, podemos utilizar la función [from_github()] para especificar el
#' nombre y la ubicación de los datos de la tabla en un repositorio.
#' 
#' @param file La ruta completa del archivo que lleva a una tabla de datos
#'   compatible, ya sea en el sistema del usuario o en una URL `http://`,
#'   `https://`, `ftp://` o `ftps://`. En el caso de un archivo alojado en un
#'   repositorio de GitHub, una llamada a la función [from_github()] se puede
#'   utilizar aquí.
#' @param type El tipo de archivo. Normalmente se infiere por la extensión del
#'   archivo y por defecto es `NULL` para indicar que la extensión dictará el
#'   tipo de lectura del archivo que se realiza internamente. Sin embargo, si no
#'   hay extensión (y las extensiones válidas son `.csv`, `.tsv`, `.rda` y
#'   `.rds`), podemos podemos proporcionar el tipo como `csv`, `tsv`, `rda`, o
#'   `rds`.
#' @param ... Opciones pasadas a la función `read_csv()` o `read_tsv()` de
#'   **readr**. Ambas funciones tienen los mismos argumentos y se utilizará una
#'   u otra internamente en función de la extensión del archivo o de un valor
#'   explícito dado a `type`.
#' @param keep En el caso de un archivo descargado, ¿debe almacenarse en el
#'   directorio de trabajo (`keep = TRUE`) o debe descargarse en un directorio
#'   temporal? Por defecto, esto es `FALSE`.
#' @param verify Si es `TRUE` (el valor por defecto) entonces se realizará una
#'   verificación del objeto de datos con la clase `data.frame`.
#'   
#' @return Un objeto `tbl_df`.
#' 
#' @examples 
#' # Un archivo CSV local puede obtenerse
#' # como un objeto tbl suministrando una
#' # ruta al archivo y algunas opciones
#' # de lectura CSV (las utilizadas por
#' # `readr::read_csv()`) a la función
#' # `file_tbl()`; para este ejemplo
#' # podríamos obtener una ruta a un
#' # archivo CSV en el paquete pointblank
#' # con `system.file()`:
#' csv_path <- 
#'   system.file(
#'     "data_files", "small_table.csv",
#'     package = "pointblank"
#'   )
#' 
#' # A continuación, utilice esa ruta
#' # en `file_tbl()` con la opción de
#' # especificar los tipos de columna
#' # en ese CSV  
#' tbl <- 
#'   file_tbl(
#'     file = csv_path,
#'     col_types = "TDdcddlc"
#'   )
#'   
#' # Ahora que tenemos un objeto `tbl`
#' # que es un tibble, se puede
#' # introducir en `create_agent()`
#' # para su validación
#' agent <- create_agent(tbl = tbl)
#'
#' # Una estrategia diferente es
#' # proporcionar la llamada a la
#' # función de lectura de datos
#' # directamente a `create_agent()`:
#' agent <- 
#'   create_agent(
#'     read_fn = ~ file_tbl(
#'       file = system.file(
#'         "data_files", "small_table.csv",
#'         package = "pointblank"
#'       ),
#'       col_types = "TDdcddlc"
#'     )
#'   ) %>%
#'   col_vals_gt(vars(a), value = 0)
#'
#' # Todas las instrucciones de lectura
#' # de archivos se encapsulan en el
#' # `read_fn` para que el agente obtenga
#' # siempre la versión más reciente del
#' # conjunto de datos (y la lógica puede
#' # traducirse a YAML, para uso posterior)
#' 
#' if (interactive()) {
#' 
#' # Se puede obtener un CSV de un
#' # repositorio público de GitHub
#' # utilizando la función de ayuda
#' # `from_github()`; creemos un agente
#' # que suministre una fórmula de
#' # preparación de tablas que obtenga el
#' # mismo archivo CSV del repositorio de
#' # GitHub para el paquete pointblank 
#' agent <- 
#'   create_agent(
#'     read_fn = ~ file_tbl(
#'       file = from_github(
#'         file = "inst/data_files/small_table.csv",
#'         repo = "rich-iannone/pointblank"
#'       ),
#'       col_types = "TDdcddlc"
#'     )
#'   ) %>%
#'   col_vals_gt(vars(a), value = 0) %>%
#'   interrogate()
#' 
#' # Esto interroga los datos que se
#' # obtuvieron del archivo fuente remoto,
#' # y, no hay nada que limpiar (por defecto,
#' # el archivo descargado va a un
#' # directorio temporal del sistema)
#' 
#' # El almacenamiento de fórmulas de
#' # preparación de tablas en un almacén
#' # de tablas facilita el trabajo con
#' # datos tabulares procedentes de
#' # archivos; a continuación se explica
#' # cómo generar un almacén de tablas con
#' # dos entradas con nombre para la tabla
#' # preparaciones
#' tbls <-
#'   tbl_store(
#'     small_table_file ~ file_tbl(
#'       file = system.file(
#'         "data_files", "small_table.csv",
#'         package = "pointblank"
#'       ),
#'       col_types = "TDdcddlc"
#'     ),
#'     small_high_file ~ file_tbl(
#'       file = system.file(
#'         "data_files", "small_table.csv",
#'         package = "pointblank"
#'       ),
#'       col_types = "TDdcddlc"
#'     ) %>%
#'       dplyr::filter(f == "high")
#'   )
#' 
#' # Ahora es fácil acceder a cualquiera
#' # de estas tablas (la segunda es una
#' # versión mutada) mediante la
#' # función `tbl_get()`
#' tbl_get("small_table_file", store = tbls)
#' tbl_get("small_high_file", store = tbls)
#' 
#' # Las fórmulas de preparación de
#' # tablas en `tbls` también pueden
#' # utilizarse en funciones con el
#' # argumento `read_fn`; esto es gracias
#' # a la función `tbl_source()`
#' agent <- 
#'   create_agent(
#'     read_fn = ~ tbl_source(
#'       "small_table_file",
#'       store = tbls
#'     )
#'   )
#' 
#' informant <- 
#'   create_informant(
#'     read_fn = ~ tbl_source(
#'       "small_high_file",
#'       store = tbls
#'     )
#'   )
#' 
#' }
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-7
#'
#' @export

# from_github--------------------------------------------------------------
#' Especificar un archivo para descargar desde GitHub
#' 
#' La función `from_github()` es útil para generar una URL válida que apunte a
#' un archivo de datos en un repositorio público de GitHub. Esta función se
#' puede utilizar en el argumento `file` de la función [file_tbl()] o en
#' cualquier otro lugar donde se necesiten URLs de GitHub para el contenido
#' bruto del usuario.
#' 
#' @param file El nombre del archivo al que se apunta en un repositorio de
#'   GitHub. Puede ser una ruta que lleve al archivo y lo incluya. Esto se
#'   combina con cualquier ruta dada en `subdir`.
#' @param repo La dirección del repositorio de GitHub en el formato
#'   `username/repo[/subdir][@ref|#pull|@*release]`.
#' @param subdir Una cadena de ruta que representa un subdirectorio en el
#'   repositorio de GitHub de GitHub. Se combina con cualquier componente de la
#'   ruta incluido en `file`.
#'   
#' @return Un vector de caracteres de longitud 1 que contiene una URL.
#' 
#' @examples
#' # Una URL válida a un archivo de datos
#' # en GitHub puede ser obtenida en el
#' # HEAD de la rama por defecto
#' # from_github(
#' #   file = "inst/data_files/small_table.csv",
#' #   repo = "rich-iannone/pointblank"
#' # )
#' 
#' # La ruta de acceso a la ubicación del
#' # archivo puede ser suministrada total
#' # o parcialmente a `subdir`.
#' # from_github(
#' #   file = "small_table.csv",
#' #   repo = "rich-iannone/pointblank",
#' #   subdir = "inst/data_files"
#' # )
#' 
#' # Podemos utilizar la primera llamada
#' # en combinación con `file_tbl()` y
#' # `create_agent()`; esto proporciona
#' # una fórmula de preparación de tablas
#' # que obtiene un archivo CSV del
#' # repositorio de GitHub para el
#' # paquete pointblank 
#' # agent <- 
#' #   create_agent(
#' #     read_fn = ~ file_tbl(
#' #       file = from_github(
#' #         file = "inst/data_files/small_table.csv",
#' #         repo = "rich-iannone/pointblank"
#' #       ),
#' #       col_types = "TDdcddlc"
#' #     )
#' #   ) %>%
#' #   col_vals_gt(vars(a), 0) %>%
#' #   interrogate()
#' 
#' # La función de ayuda `from_github()`
#' # es bastante potente y puede llegar
#' # a muchos diferentes archivos en
#' # un repositorio
#' 
#' # Un archivo de datos de GitHub puede
#' # obtenerse de un commit en el
#' # momento de la publicación
#' # from_github(
#' #   file = "inst/extdata/small_table.csv",
#' #   repo = "rich-iannone/pointblank@v0.2.1"
#' # )
#' 
#' # También se puede obtener un archivo
#' # de un repo en el momento de un commit
#' # específico (se puede utilizar el hash
#' # parcial o hash SHA-1 completo de
#' # la confirmación)
#' # from_github(
#' #   file = "data-raw/small_table.csv",
#' #   repo = "rich-iannone/pointblank@e04a71"
#' # )
#' 
#' # Un archivo también puede obtenerse
#' # de un *abierto* pull request
#' # from_github(
#' #   file = "data-raw/small_table.csv",
#' #   repo = "rich-iannone/pointblank#248"
#' # )
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-6
#' 
#' @export

# game_revenue-------------------------------------------------------------
#' Una tabla con datos de ingresos del juego
#'
#' Esta tabla es un subconjunto de la tabla `sj_all_revenue` del paquete de
#' datos **intendo**. Son las primeras 2000 filas de esa tabla donde los
#' registros de ingresos van desde `2015-01-01` hasta `2015-01-21`.
#'
#' @format Un tibble con 2.000 filas y 11 variables:
#' \describe{
#' \item{player_id}{Una columna de `character` con identificadores únicos para
#' cada usuario/jugador.}
#' \item{session_id}{Una columna de `character` que contiene identificadores
#' únicos para cada sesión de jugador.}
#' \item{session_start}{Una columna de fecha-hora que indica cuándo la sesión
#' (que contiene el evento de ingresos) comenzó.}
#' \item{time}{Una columna de fecha-hora que indica exactamente cuándo el
#' jugador (o evento de ingresos).}
#' \item{item_type}{Una columna de `character` que proporciona la clase del
#' artículo comprado.}
#' \item{item_name}{Una columna de `character` que proporciona el nombre del
#' artículo comprado.}
#' \item{item_revenue}{Una columna `numeric` con los importes de los ingresos
#' por artículo comprado.}
#' \item{session_duration}{Una columna `numeric` que indica la duración de la
#' sesión (en minutos) en la que se produjo la compra.}
#' \item{start_day}{Una columna `Date` que proporciona la fecha del primer
#' ingreso de el jugador que realiza la compra.}
#' \item{acquisition}{Una columna de `character` que proporciona el método de
#' adquisición para el jugador.}
#' \item{country}{Una columna de `character` que proporciona el probable país
#' de residencia del jugador.}
#' }
#'
#' @examples
#' # Aquí hay un vistazo a los datos
#' # disponibles en `game_revenue`
#' dplyr::glimpse(game_revenue)
#'
#' @family Datasets
#' @section Function ID:
#' 14-4
#'

# game_revenue_info--------------------------------------------------------
#' Una tabla con metadatos para el conjunto de datos `game_revenue`
#'
#' Esta tabla contiene metadatos para la tabla `game_revenue`. La primera
#' columna (llamada `column`) proporciona los nombres de las columnas de
#' `game_revenue`. La segunda columna (`info`) contiene las descripciones de
#' cada una de las columnas de ese conjunto de datos. Esta tabla tiene el
#' formato correcto para su uso en la función [info_columns_from_tbl()].
#'
#' @format Un tibble con 11 filas y 2 variables:
#' \describe{
#' \item{column}{Una columna de `character` con identificadores únicos para
#' cada usuario/jugador.}
#' \item{info}{Una columna de `character` que contiene identificadores únicos
#' para cada sesión de jugador.}
#' }
#'
#' @examples
#' # Aquí hay un vistazo a los datos
#' # disponibles en `game_revenue_info`
#' dplyr::glimpse(game_revenue_info)
#'
#' @family Datasets
#' @section Function ID:
#' 14-5
#'

# get_agent_report---------------------------------------------------------
#' Obtenga un informe resumido de una *agent*
#' 
#' @description 
#' Podemos obtener una tabla resumen informativa de un agente utilizando la
#' función `get_agent_report()`. La tabla puede ser proporcionada en dos formas
#' sustancialmente diferentes: como una tabla de visualización basada en **gt**
#' (el valor por defecto), o, como un tibble. La cantidad de campos con intel es
#' diferente dependiendo de si el agente realizó o no una interrogación (con la
#' función [interrogate()]). Básicamente, antes de llamar a [interrogate()], el
#' agente contendrá sólo el plan de validación (el número de filas que tenga
#' depende de cuántas funciones de validación se hayan suministrado como parte
#' de ese plan). Después de la interrogación, se proporciona información sobre
#' las unidades de prueba que pasan y que fallan, junto con indicadores sobre si
#' se han introducido ciertos estados de fallo (siempre que se hayan establecido
#' a través de `actions`). La variante de la tabla de visualización del informe
#' del agente, la forma por defecto, tendrá las siguientes columnas:
#' 
#' \itemize{
#' \item i (sin etiquetas): el número de paso de validación
#' \item STEP: el nombre de la función de validación utilizada para el paso
#' de validación
#' \item COLUMNS: los nombres de las columnas de destino utilizadas en el paso
#' de validación (si procede)
#' \item VALUES: los valores utilizados en el paso de validación, cuando sea
#' aplicable; esto podría ser como valores literales, como nombres de
#' columnas, una expresión, un conjunto de subvalidaciones (para un paso de
#' validación [conjointly()]), etc.
#' \item TBL: indica si había alguna condición previa que aplicar antes de la
#' interrogación; si no es así, aparece una aparece una flecha hacia la derecha
#' \item EVAL: un valor de carácter que denota el resultado de cada validación
#' evaluación de las funciones de paso durante la interrogación
#' \item *N*: el número total de unidades de prueba para la etapa de validación
#' \item PASS: el número de unidades de prueba que han recibido un *paso*.
#' \item FAIL: la fracción de unidades de prueba que recibieron un *paso*.
#' \item W, S, N: Indicadores que muestran si se han introducido los estados
#' de `warn`, `stop` o `notify`; los estados no establecidos aparecen como
#' guiones, los estados establecidos con umbrales aparecen como círculos sin
#' rellenar cuando no se han introducido y rellenos cuando se superan los
#' umbrales (los colores para W, S y N son ámbar, rojo y azul).
#' \item EXT: una columna que proporciona botones con extractos de datos para
#' cada paso de validación en el que están disponibles las filas fallidas
#' (como archivos CSV)
#' }
#' 
#' La versión pequeña de la tabla de visualización (obtenida con `size =
#' "small") omite las columnas `COLUMNS`, `TBL` y `EXT`. El ancho de la tabla
#' pequeña es de 575px; la tabla estándar tiene 875px de ancho.
#' 
#' Si se elige obtener un tibble (con `display_table = FALSE`), tendrá las
#' siguientes columnas:
#' 
#' \itemize{
#' \item i: el número de paso de validación
#' \item type: el nombre de la función de validación utilizada para el paso
#' de validación
#' \item columns: los nombres de las columnas de destino utilizadas en el paso
#' de validación (si procede)
#' \item values: los valores utilizados en el paso de validación, cuando sea
#' aplicable; esto podría ser como valores literales, como nombres de
#' columnas, una expresión, un conjunto de subvalidaciones (para un paso de
#' validación [conjointly()]), etc.
#' \item precon: indica si hay que aplicar alguna condición previa antes del
#' interrogatorio y, en caso afirmativo, el número de declaraciones utilizadas
#' \item active: un valor lógico que indica si un paso de validación se
#' establece como `"active"` durante una interrogación
#' \item eval: un valor de carácter que denota el resultado de cada validación
#' evaluación de las funciones de paso durante la interrogación
#' \item units: el número total de unidades de prueba para la etapa de
#' validación
#' \item n_pass: el número de unidades de prueba que han recibido un *paso*
#' \item f_pass: la fracción de unidades de prueba que recibieron un *paso*
#' \item W, S, N: valor lógico que indica si los estados `warn`, `stop` o
#' `notify` fueron introducidos
#' \item extract: un valor lógico que indica si un extracto de datos está
#' disponible para el paso de validación
#' }
#' 
#' @param agent Un objeto de agente de clase `ptblank_agent`.
#' @param arrange_by Una opción para organizar las filas de la tabla de informes
#'   por el número de paso de validación (`"i"`, el valor predeterminado), o,
#'   para organizar en orden descendente por severidad del estado de fallo (con
#'   `"severity"`).
#' @param keep Una opción para mantener `"all"` las filas de la tabla del
#'   informe (el valor predeterminado), o, mantener sólo aquellas filas que
#'   reflejan uno o más `"fail_states"`.
#' @param display_table ¿Debe generarse una tabla de visualización? Si es `TRUE`
#'   (por defecto), y si el paquete **gt** está instalado, se mostrará una tabla
#'   de visualización del informe en el Visor. Si es `FALSE`, o si **gt** no
#'   está está disponible, entonces se devolverá un tibble.
#' @param size El tamaño de la tabla de visualización, que puede ser
#'   `"standard"` (por defecto) o `"small"`. Esto sólo se aplica a una tabla de
#'   visualización (donde `display_table = TRUE`).
#' @param title Opciones para personalizar el título del informe. La opción por
#'   defecto es la palabra clave `":default:"`, que produce un texto de título
#'   genérico que hace referencia al paquete **pointblank** en el idioma regido
#'   por la opción `lang`. Otra opción de palabra clave es `":tbl_name:"`, que
#'   presenta el nombre de la tabla como título del informe. Si no se desea
#'   ningún título, se puede utilizar la opción de palabra clave `":none:"`.
#'   Aparte de las opciones de palabras clave, se puede proporcionar texto para
#'   el título y se pueden utilizar llamadas a `glue::glue()` para construir la
#'   cadena de texto. Si se proporciona texto, se interpretará como texto
#'   Markdown y se transformará internamente en HTML. Para evitar dicha
#'   transformación, utilice text en [I()] para indicar explícitamente que el
#'   texto suministrado no debe ser transformado.
#' @param lang El lenguaje que se utilizará para la creación automática de
#'   resúmenes (descripciones breves para cada paso de validación) y para el
#'   *agent report* (una tabla resumen que proporciona el plan de validación y
#'   los resultados de la interrogación. De forma predeterminada, `NULL` creará
#'   texto en inglés (`"en"`). Otras opciones incluyen francés (`"fr"`), alemán
#'   (`"de"`), italiano (`"it"`), español (`"es"`), portugués (`"pt"`), turco
#'   (`"tr"`), chino (`"zh"`), ruso (`"ru"`), polaco (`"pl"`), danés (`"da"`),
#'   sueco (`"sv"` ) y holandés (`"nl"`). Esta opción `lang` anulará cualquier
#'   previamente establecido (por ejemplo, por la llamada [create_agent()]).
#' @param locale Un identificador opcional de la configuración regional que se
#'   utilizará para formatear los valores en la tabla de resumen del *informe de
#'   agente* de acuerdo con las reglas de la configuración regional. Los
#'   ejemplos incluyen `"en_US"` para el inglés (Estados Unidos) y `"fr_FR"`
#'   para el francés (Francia); más simplemente, puede ser un identificador de
#'   idioma sin designación de país, como `"es"` para el español (España, igual
#'   que `"es_ES"`). Esta opción `locale` anulará cualquier valor de locale
#'   previamente establecido (por ejemplo, por la opción [create_agent()]).
#' 
#' @return Un objeto de tabla **gt** si `display_table = TRUE` o un tibble si
#'   `display_table = FALSE`.
#' 
#' @examples
#' # Crear una tabla simple con una
#' # columna de valores numéricos
#' tbl <- 
#'   dplyr::tibble(a = c(5, 7, 8, 5))
#' 
#' # Validar que los valores de la
#' # columna `a` sean siempre mayores
#' # que 4
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(vars(a), value = 4) %>%
#'   interrogate()
#' 
#' # Obtenga un informe basado en
#' # tibble del agente utilizando
#' # `get_agent_report()` y
#' # `display_table = FALSE`
#' agent %>%
#'   get_agent_report(display_table = FALSE)
#'   
#' # Ver un informe imprimiendo el
#' # objeto `agente` en cualquier momento,
#' # pero, devolver un objeto de tabla gt
#' # usando esto con `display_table = TRUE`
#' # (por defecto)
#' report <- get_agent_report(agent)
#' class(report)
#' 
#' # ¿Qué puedes hacer con el informe?
#' # Imprimirlo desde un código R Markdown,
#' # utilizarlo en un correo electrónico,
#' # ponerlo en una página web, o
#' # modificarlo aún más con el paquete
#' # **gt**
#' 
#' # El informe del agente como tabla
#' # de visualización **gt** viene en
#' # dos tamaños: "standard" (el
#' # predeterminado) y "small"
#' small_report <- 
#'   get_agent_report(
#'     agent = agent,
#'     size = "small"
#'   )
#' 
#' class(small_report)
#' 
#' # El informe estándar tiene 875px
#' # de ancho y el pequeño 575px
#' 
#' @family Interrogate and Report
#' @section Function ID:
#' 6-2
#' 
#' @export

# get_agent_x_list---------------------------------------------------------
#' Obtenga la **x-list** del agente
#'
#' @description 
#' The agent's **x-list** is a record of information that the agent possesses at
#' any given time. The **x-list** will contain the most complete information
#' after an interrogation has taken place (before then, the data largely
#' reflects the validation plan). The **x-list** can be constrained to a
#' particular validation step (by supplying the step number to the `i`
#' argument), or, we can get the information for all validation steps by leaving
#' `i` unspecified. The **x-list** is indeed an R `list` object that contains a
#' veritable cornucopia of information.
#'
#' For an **x-list** obtained with `i` specified for a validation step, the
#' following components are available:
#' \itemize{
#' \item `time_start`: the time at which the interrogation began
#' (`POSIXct [0 or 1]`)
#' \item `time_end`: the time at which the interrogation ended
#' (`POSIXct [0 or 1]`)
#' \item `label`: the optional label given to the agent (`chr [1]`)
#' \item `tbl_name`: the name of the table object, if available (`chr [1]`)
#' \item `tbl_src`: the type of table used in the validation (`chr [1]`)
#' \item `tbl_src_details`: if the table is a database table, this provides
#' further details for the DB table (`chr [1]`)
#' \item `tbl`: the table object itself
#' \item `col_names`: the table's column names (`chr [ncol(tbl)]`)
#' \item `col_types`: the table's column types (`chr [ncol(tbl)]`)
#' \item `i`: the validation step index (`int [1]`)
#' \item `type`: the type of validation, value is validation function name
#' (`chr [1]`)
#' \item `columns`: the columns specified for the validation function
#' (`chr [variable length]`)
#' \item `values`: the values specified for the validation function
#' (`mixed types [variable length]`)
#' \item `briefs`: the brief for the validation step in the specified `lang`
#' (`chr [1]`)
#' \item `eval_error`, `eval_warning`: indicates whether the evaluation of the
#' step function, during interrogation, resulted in an error or a warning
#' (`lgl [1]`)
#' \item `capture_stack`: a list of captured errors or warnings during
#' step-function evaluation at interrogation time (`list [1]`)
#' \item `n`: the number of test units for the validation step (`num [1]`)
#' \item `n_passed`, `n_failed`: the number of passing and failing test units
#' for the validation step (`num [1]`)
#' \item `f_passed`: the fraction of passing test units for the validation step,
#' `n_passed` / `n` (`num [1]`)
#' \item `f_failed`: the fraction of failing test units for the validation step,
#' `n_failed` / `n` (`num [1]`)
#' \item `warn`, `stop`, `notify`: a logical value indicating whether the level
#' of failing test units caused the corresponding conditions to be entered
#' (`lgl [1]`)
#' \item `lang`: the two-letter language code that indicates which
#' language should be used for all briefs, the agent report, and the reporting
#' generated by the [scan_data()] function (`chr [1]`) 
#' }
#' 
#' If `i` is unspecified (i.e., not constrained to a specific validation step)
#' then certain length-one components in the **x-list** will be expanded to the
#' total number of validation steps (these are: `i`, `type`, `columns`,
#' `values`, `briefs`, `eval_error`, `eval_warning`, `capture_stack`, `n`,
#' `n_passed`, `n_failed`, `f_passed`, `f_failed`, `warn`, `stop`, and
#' `notify`). The **x-list** will also have additional components when `i` is
#' `NULL`, which are:
#' \itemize{
#' \item `report_object`: a **gt** table object, which is also presented as the
#' default print method for a `ptblank_agent`
#' \item `email_object`: a **blastula** `email_message` object with a default
#' set of components
#' \item `report_html`: the HTML source for the `report_object`, provided as
#' a length-one character vector
#' \item `report_html_small`: the HTML source for a narrower, more condensed
#' version of `report_object`, provided as a length-one character vector; The
#' HTML has inlined styles, making it more suitable for email message bodies
#' }
#'
#' @param agent Un objeto de agente de clase `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of invocation. If `NULL` (the default), the **x-list**
#'   will provide information for all validation steps. If a valid step number
#'   is provided then **x-list** will have information pertaining only to that
#'   step.
#' 
#' @return Un objeto `list`.
#' 
#' @examples
#' # Create a simple data frame with
#' # a column of numerical values
#' tbl <- dplyr::tibble(a = c(5, 7, 8, 5))
#' 
#' # Create an `action_levels()` list
#' # with fractional values for the
#' # `warn`, `stop`, and `notify` states
#' al <-
#'   action_levels(
#'     warn_at = 0.2,
#'     stop_at = 0.8,
#'     notify_at = 0.345
#'   )
#' 
#' # Create an agent (giving it the
#' # `tbl` and the `al` objects),
#' # supply two validation step
#' # functions, then interrogate
#' agent <-
#'   create_agent(
#'     tbl = tbl,
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(a), value = 7) %>%
#'   col_is_numeric(vars(a)) %>%
#'   interrogate()
#'   
#' # Get the agent x-list
#' x <- get_agent_x_list(agent)
#' 
#' # Print the x-list object `x`
#' x
#' 
#' # Get the `f_passed` component
#' # of the x-list
#' x$f_passed
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-1
#' 
#' @export

# get_data_extracts--------------------------------------------------------
#' Recopile extractos de datos de un paso de validación
#'
#' @description
#' In an agent-based workflow (i.e., initiating with [create_agent()]), after
#' interrogation with [interrogate()], we can extract the row data that didn't
#' pass row-based validation steps with the `get_data_extracts()` function.
#' There is one discrete extract per row-based validation step and the amount of
#' data available in a particular extract depends on both the fraction of test
#' units that didn't pass the validation step and the level of sampling or
#' explicit collection from that set of units. These extracts can be collected
#' programmatically through `get_data_extracts()` but they may also be
#' downloaded as CSV files from the HTML report generated by the agent's print
#' method or through the use of [get_agent_report()].
#'
#' The availability of data extracts for each row-based validation step depends
#' on whether `extract_failed` is set to `TRUE` within the [interrogate()] call
#' (it is by default). The amount of *fail* rows extracted depends on the
#' collection parameters in [interrogate()], and the default behavior is to
#' collect up to the first 5000 *fail* rows.
#'
#' Row-based validation steps are based on those validation functions of the
#' form `col_vals_*()` and also include [conjointly()] and [rows_distinct()].
#' Only functions from that combined set of validation functions can yield data
#' extracts.
#'
#' @param agent Un objeto de agente de clase `ptblank_agent`. It should have had
#'   [interrogate()] called on it, such that the validation steps were carried
#'   out and any sample rows from non-passing validations could potentially be
#'   available in the object.
#' @param i The validation step number, which is assigned to each validation
#'   step by **pointblank** in the order of definition. If `NULL` (the default),
#'   all data extract tables will be provided in a list object.
#' 
#' @return A list of tables if `i` is not provided, or, a standalone table if
#'   `i` is given.
#' 
#' @examples
#' # Create a series of two validation
#' # steps focused on testing row values
#' # for part of the `small_table` object;
#' # `interrogate()` immediately
#' agent <-
#'   create_agent(
#'     read_fn = ~ small_table %>%
#'       dplyr::select(a:f),
#'     label = "`get_data_extracts()`"
#'   ) %>%
#'   col_vals_gt(vars(d), value = 1000) %>%
#'   col_vals_between(
#'     vars(c),
#'     left = vars(a), right = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   interrogate()
#' 
#' # Using `get_data_extracts()` with its
#' # defaults returns of a list of tables,
#' # where each table is named after the
#' # validation step that has an extract
#' # available
#' agent %>% get_data_extracts()
#' 
#' # We can get an extract for a specific
#' # step by specifying it in the `i`
#' # argument; let's get the failing rows
#' # from the first validation step
#' # (`col_vals_gt`)
#' agent %>% get_data_extracts(i = 1)
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-2
#' 
#' @export

# get_informant_report-----------------------------------------------------
#' Obtener un informe de información de la tabla de un objeto *informant*
#' 
#' @description 
#' We can get a table information report from an informant object that's
#' generated by the [create_informant()] function. The report is provided as a
#' **gt** based display table. The amount of information shown depends on the
#' extent of that added via the use of the `info_*()` functions or through
#' direct editing of a **pointblank** YAML file (an informant can be written
#' to **pointblank** YAML with `yaml_write(informant = <informant>, ...)`).
#' 
#' @param informant An informant object of class `ptblank_informant`.
#' @param size The size of the display table, which can be either `"standard"`
#'   (the default, with a width of 875px) or `"small"` (width of 575px).
#' @param title Options for customizing the title of the report. The default is
#'   the keyword `":default:"` which produces generic title text that refers to
#'   the **pointblank** package in the language governed by the `lang` option.
#'   Another keyword option is `":tbl_name:"`, and that presents the name of the
#'   table as the title for the report. If no title is wanted, then the
#'   `":none:"` keyword option can be used. Aside from keyword options, text can
#'   be provided for the title and `glue::glue()` calls can be used to construct
#'   the text string. If providing text, it will be interpreted as Markdown text
#'   and transformed internally to HTML. To circumvent such a transformation,
#'   use text in [I()] to explicitly state that the supplied text should not be
#'   transformed.
#' @param lang The language to use for the *information report* (a summary table
#'   that provides the validation plan and the results from the interrogation.
#'   De forma predeterminada, `NULL` creará texto en inglés (`"en"`). Otras
#'   opciones incluyen francés (`"fr"`), alemán (`"de"`), italiano (`"it"`),
#'   español (`"es"`), portugués (`"pt"`), turco (`"tr"`), chino (`"zh"`), ruso
#'   (`"ru"`), polaco (`"pl"`), danés (`"da"`), sueco (`"sv"` ) y holandés
#'   (`"nl"`). This `lang` option will override any previously set language
#'   setting (e.g., by the [create_agent()] call).
#' @param locale An optional locale ID to use for formatting values in the
#'   *information report* summary table according the locale's rules. Examples
#'   include `"en_US"` for English (United States) and `"fr_FR"` for French
#'   (France); more simply, this can be a language identifier without a country
#'   designation, like `"es"` for Spanish (Spain, same as `"es_ES"`). This
#'   `locale` option will override any previously set locale value (e.g., by the
#'   [create_agent()] call).
#' 
#' @return A **gt** table object.
#' 
#' @examples
#' # Generate an informant object using
#' # the `small_table` dataset
#' informant <- create_informant(small_table)
#' 
#' # This function creates some information
#' # without any extra help by profiling
#' # the supplied table object; it adds
#' # the sections 'table' and columns' and
#' # we can print the object to see the
#' # table information report
#' 
#' # Alternatively, we can get the same report
#' # by using `get_informant_report()`
#' report <- get_informant_report(informant)
#' class(report)
#' 
#' @family Incorporate and Report
#' @section Function ID:
#' 7-2
#' 
#' @export

# get_multiagent_report----------------------------------------------------
#' Obtenga un informe resumido utilizando varios agentes
#' 
#' @description 
#' We can get an informative summary table from a collective of agents by using
#' the `get_multiagent_report()` function. Information from multiple agent can
#' be provided in three very forms: (1) the *Long Display* (stacked reports),
#' (2) the *Wide Display* (a comparison report), (3) as a tibble with packed
#' columns.
#' 
#' @section The Long Display:
#' When displayed as `"long"` the multiagent report will stack individual agent
#' reports in a single document in the order of the agents in the multiagent
#' object.
#' 
#' Each validation plan (possibly with interrogation info) will be provided and
#' the output for each is equivalent to calling [get_agent_report()] on each
#' of the agents within the multiagent object.
#' 
#' @section The Wide Display:
#' When displayed as `"wide"` the multiagent report will show data from
#' individual agents as columns, with rows standing as validation steps common
#' across the agents.
#'
#' Each validation step is represented with an icon (standing in for the name of
#' the validation function) and the associated SHA1 hash. This is a highly
#' trustworthy way for ascertaining which validation steps are effectively
#' identical across interrogations. This way of organizing the report is
#' beneficial because different agents may have used different steps and we want
#' to track the validation results where the validation step doesn't change but
#' the target table does (i.e., new rows are added, existing rows are updated,
#' etc.).
#' 
#' The single table from this display mode will have the following columns:
#' 
#' - STEP: the SHA1 hash for the validation step, possibly shared among
#' several interrogations.
#' - *subsequent columns*: each column beyond `STEP` represents a separate
#' interrogation from an *agent* object. The time stamp for the completion of
#' each interrogation is shown as the column label.
#' 
#' @param multiagent A multiagent object of class `ptblank_multiagent`.
#' @param display_table Should a display table be generated? If `TRUE` (the
#'   default) a display table for the report will be shown in the Viewer. If
#'   `FALSE` then a tibble will be returned.
#' @param display_mode If we are getting a display table, should the agent data
#'   be presented in a `"long"` or `"wide"` form? The default is `"long"` but
#'   when comparing multiple runs where the target table is the same it might be
#'   preferable to choose `"wide"`.
#' @param title Options for customizing the title of the report when
#'   `display_table = TRUE`. The default is the keyword `":default:"` which
#'   produces generic title text. If no title is wanted, then the `":none:"`
#'   keyword option can be used. Another keyword option is `":tbl_name:"`, and
#'   that presents the name of the table as the title for the report (this can
#'   only be used when `display_mode = "long"`). Aside from keyword options,
#'   text can be provided for the title and `glue::glue()` calls can be used to
#'   construct the text string. If providing text, it will be interpreted as
#'   Markdown text and transformed internally to HTML. To circumvent such a
#'   transformation, use text in [I()] to explicitly state that the supplied
#'   text should not be transformed.
#' @param lang La lengua a utilizar para los formularios de informes largos o
#'   amplios. De forma predeterminada, `NULL` creará texto en inglés (`"en"`).
#'   Otras opciones incluyen francés (`"fr"`), alemán (`"de"`), italiano
#'   (`"it"`), español (`"es"`), portugués (`"pt"`), turco (`"tr"`), chino
#'   (`"zh"`), ruso (`"ru"`), polaco (`"pl"`), danés (`"da"`), sueco (`"sv"` ) y
#'   holandés (`"nl"`).
#' @param locale An optional locale ID to use for formatting values in the long
#'   or wide report forms (according the locale's rules). Examples include
#'   `"en_US"` for English (United States) and `"fr_FR"` for French (France);
#'   more simply, this can be a language identifier without a country
#'   designation, like `"es"` for Spanish (Spain, same as `"es_ES"`). This
#'   `locale` option will override any previously set locale values.
#' 
#' @return Un objeto de tabla **gt** si `display_table = TRUE` o un tibble si
#'   `display_table = FALSE`.
#' 
#' @examples 
#' if (interactive()) {
#' 
#' # Let's walk through several theoretical
#' # data quality analyses of an extremely
#' # small table; that table is called
#' # `small_table` and we can find it as a
#' # dataset in this package
#' small_table
#' 
#' # To set failure limits and signal
#' # conditions, we designate proportional
#' # failure thresholds to the `warn`, `stop`,
#' # and `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.05,
#'     stop_at = 0.10,
#'     notify_at = 0.20
#'   )
#' 
#' # We will create four different agents
#' # and have slightly different validation
#' # steps in each of them; in the first,
#' # `agent_1`, eight different validation
#' # steps are created and the agent will
#' # interrogate the `small_table`
#' agent_1 <-
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`get_multiagent_report()`",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     vars(date_time),
#'     value = vars(date),
#'     na_pass = TRUE
#'   ) %>%
#'   col_vals_gt(
#'     vars(b), 
#'     value = vars(g),
#'     na_pass = TRUE
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_equal(
#'     vars(d), 
#'     value = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   col_vals_between(
#'     vars(c), 
#'     left = vars(a), right = vars(d)
#'   ) %>%
#'   col_vals_not_between(
#'     vars(c),
#'     left = 10, right = 20,
#'     na_pass = TRUE
#'   ) %>%
#'   rows_distinct(vars(d, e, f)) %>%
#'   col_is_integer(vars(a)) %>%
#'   interrogate()
#' 
#' # The second agent, `agent_2`, retains
#' # all of the steps of `agent_1` and adds
#' # two more (the last of which is inactive)
#' agent_2 <- 
#'   agent_1 %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), 
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}",
#'     active = FALSE
#'   ) %>%
#'   interrogate()
#' 
#' # The third agent, `agent_3`, adds a single
#' # validation step, removes the fifth one,
#' # and deactivates the first
#' agent_3 <- 
#'   agent_2 %>%
#'   col_vals_in_set(
#'     vars(f),
#'     set = c("low", "mid", "high")
#'   ) %>%
#'   remove_steps(i = 5) %>%
#'   deactivate_steps(i = 1) %>%
#'   interrogate()
#' 
#' # The fourth and final agent, `agent_4`,
#' # reactivates steps 1 and 10, and removes
#' # the sixth step
#' agent_4 <-
#'   agent_3 %>%
#'   activate_steps(i = 1) %>%
#'   activate_steps(i = 10) %>%
#'   remove_steps(i = 6) %>%
#'   interrogate()
#' 
#' # While all the agents are slightly
#' # different from each other, we can still
#' # get a combined report of them by
#' # creating a 'multiagent'
#' multiagent <-
#'   create_multiagent(
#'     agent_1, agent_2, agent_3, agent_4
#'   )
#' 
#' # Calling `multiagent` in the console
#' # prints the multiagent report; but we
#' # can use some non-default option with
#' # the `get_multiagent_report()` function
#' 
#' # By default, `get_multiagent_report()`
#' # gives you a tall report with agent
#' # reports being stacked
#' report_1 <- 
#'   get_multiagent_report(multiagent)
#'   
#' # We can modify the title with that's
#' # more suitable or use a keyword like
#' # `:tbl_name:` to give us the target
#' # table name in each section
#' report_2 <- 
#'   get_multiagent_report(
#'     multiagent,
#'     title = ":tbl_name:"
#'   )
#' 
#' # We can opt for a wide display of
#' # the reporting info, and this is
#' # great when reporting on multiple
#' # validations of the same target
#' # table
#' report_3 <- 
#'   get_multiagent_report(
#'     multiagent,
#'     display_mode = "wide"
#'   )
#' }
#'
#' @family The multiagent
#' @section Function ID:
#' 10-3
#'
#' @export

# get_sundered_data--------------------------------------------------------
#' Divida los datos, dividiéndolos en partes de 'pasa' y 'falla'
#'
#' @description
#' Validation of the data is one thing but, sometimes, you want to use the best
#' part of the input dataset for something else. The `get_sundered_data()`
#' function works with an agent object that has intel (i.e., post
#' `interrogate()`) and gets either the 'pass' data piece (rows with no failing
#' test units across all row-based validation functions), or, the 'fail' data
#' piece (rows with at least one failing test unit across the same series of
#' validations). As a final option, we can have emit all the data with a new
#' column (called `.pb_combined`) which labels each row as passing or failing
#' across validation steps. These labels are `"pass"` and `"fail"` by default
#' but their values can be easily customized.
#' 
#' @details
#' There are some caveats to sundering. The validation steps considered for this
#' splitting has to be of the row-based variety (e.g., the `col_vals_*()`
#' functions or [conjointly()], but not `rows_distinct()`). Furthermore,
#' validation steps that experienced evaluation issues during interrogation are
#' not considered, and, validation steps where `active = FALSE` will be
#' disregarded. The collection of validation steps that fulfill the above
#' requirements for sundering are termed in-consideration validation steps.
#'
#' If using any `preconditions` for validation steps, we must ensure that all
#' in-consideration validation steps use the same specified `preconditions`
#' function. Put another way, we cannot split the target table using a
#' collection of in-consideration validation steps that use different forms of
#' the input table.
#'
#' @param agent Un objeto de agente de clase `ptblank_agent`. It should have had
#'   [interrogate()] called on it, such that the validation steps were actually
#'   carried out.
#' @param type The desired piece of data resulting from the splitting. Options
#'   for returning a single table are `"pass"` (the default) and `"fail"`. Each
#'   of these options return a single table with, in the `"pass"` case, only the
#'   rows that passed across all validation steps (i.e., had no failing test
#'   units in any part of a row for any validation step), or, the complementary
#'   set of rows in the `"fail"` case. Providing `NULL` returns both of the
#'   split data tables in a list (with the names of `"pass"` and `"fail"`). The
#'   option `"combined"` applies a categorical (pass/fail) label (settable in
#'   the `pass_fail` argument) in a new `.pb_combined` flag column. For this
#'   case the ordering of rows is fully retained from the input table.
#' @param pass_fail A vector for encoding the flag column with 'pass' and 'fail'
#'   values when `type = "combined"`. The default is `c("pass", "fail")` but
#'   other options could be `c(TRUE, FALSE)`, `c(1, 0)`, or `c(1L, 0L)`.
#' @param id_cols An optional specification of one or more identifying columns.
#'   When taken together, we can count on this single column or grouping of
#'   columns to distinguish rows. If the table undergoing validation is not a
#'   data frame or tibble, then columns need to be specified for `id_cols`.
#' @return A list of table objects if `type` is `NULL`, or, a single table if a
#'   `type` is given.
#' 
#' @examples
#' # Create a series of two validation
#' # steps focused on testing row values
#' # for part of the `small_table` object;
#' # `interrogate()` immediately
#' agent <-
#'   create_agent(
#'     read_fn = ~ small_table %>%
#'       dplyr::select(a:f),
#'     label = "`get_sundered_data()`"
#'   ) %>%
#'   col_vals_gt(vars(d), value = 1000) %>%
#'   col_vals_between(
#'     vars(c),
#'     left = vars(a), right = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   interrogate()
#' 
#' # Get the sundered data piece that
#' # contains only rows that passed both
#' # validation steps (the default piece);
#' # this yields 5 of 13 total rows
#' agent %>% get_sundered_data()
#' 
#' # Get the complementary data piece:
#' # all of those rows that failed either
#' # of the two validation steps;
#' # this yields 8 of 13 total rows
#' agent %>%
#'   get_sundered_data(type = "fail")
#'   
#' # We can get all of the input data
#' # returned with a flag column (called
#' # `.pb_combined`); this is done by
#' # using `type = "combined"` and that
#' # rightmost column will contain `"pass"`
#' # and `"fail"` values
#' agent %>%
#'   get_sundered_data(type = "combined")
#' 
#' # We can change the `"pass"` or `"fail"`
#' # text values to another type of coding
#' # with the `pass_fail` argument; one
#' # possibility is TRUE/FALSE
#' agent %>%
#'   get_sundered_data(
#'     type = "combined",
#'     pass_fail = c(TRUE, FALSE)
#'   )
#'
#' # ...and using `0` and `1` might be
#' # worthwhile in some situations
#' agent %>%
#'   get_sundered_data(
#'     type = "combined",
#'     pass_fail = 0:1
#'   )
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-3
#' 
#' @export

# get_tt_param-------------------------------------------------------------
#' Obtener un valor de parámetro de una tabla de resumen
#' 
#' @description The `get_tt_param()` function can help you to obtain a single
#' parameter value from a summary table generated by the `tt_*()` functions
#' [tt_summary_stats()], [tt_string_info()], [tt_tbl_dims()], or
#' [tt_tbl_colnames()]. The following parameters are to be used depending on the
#' input `tbl`:
#'
#' - from [tt_summary_stats()]: `"min"`, `"p05"`, `"q_1"`, `"med"`, `"q_3"`,
#' `"p95"`, `"max"`, `"iqr"`, `"range"`
#' - from [tt_string_info()]: `"length_mean"`, `"length_min"`, `"length_max"`
#' - from [tt_tbl_dims()]: `"rows"`, `"columns"`
#' - from [tt_tbl_colnames()]: any integer present in the `.param.` column
#' 
#' The [tt_summary_stats()] and [tt_string_info()] functions will generate
#' summary tables with columns that mirror the numeric and character columns
#' in their input tables, respectively. For that reason, a column name must be
#' supplied to the `column` argument in `get_tt_param()`.
#' 
#' @param tbl A summary table generated by either of the [tt_summary_stats()],
#'   [tt_string_info()], [tt_tbl_dims()], or [tt_tbl_colnames()] functions.
#' @param param The parameter name associated to the value that is to be gotten.
#'   These parameter names are always available in the first column (`.param.`)
#'   of a summary table obtained by [tt_summary_stats()], [tt_string_info()],
#'   [tt_tbl_dims()], or [tt_tbl_colnames()].
#' @param column The column in the summary table for which the data value should
#'   be obtained. This must be supplied for summary tables generated by
#'   [tt_summary_stats()] and [tt_string_info()] (the [tt_tbl_dims()] and
#'   [tt_tbl_colnames()] functions will always generate a two-column summary
#'   table).
#'   
#' @examples 
#' # Get summary statistics for the
#' # first quarter of the `game_revenue`
#' # dataset that's included in the package
#' stat_tbl <- 
#'   game_revenue %>%
#'   tt_time_slice(slice_point = 0.25) %>%
#'   tt_summary_stats()
#' 
#' # Based on player behavior for the first
#' # quarter of the year, test whether the
#' # maximum session duration during the
#' # rest of the year is never higher
#' game_revenue %>%
#'   tt_time_slice(
#'     slice_point = 0.25,
#'     keep = "right"
#'   ) %>%
#'   test_col_vals_lte(
#'     columns = vars(session_duration), 
#'     value = get_tt_param(
#'       tbl = stat_tbl,
#'       param = "max",
#'       column = "session_duration"
#'     )
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-7
#' 
#' @export

# has_columns--------------------------------------------------------------
#' Determinar si existen una o más columnas en una tabla
#' 
#' @description
#' This utility function can help you easily determine whether a column of a
#' specified name is present in a table object. This function works well enough
#' on a table object but it can also be used as part of a formula in any
#' validation function's `active` argument. Using `active = ~ . %>%
#' has_columns("column_1")` means that the validation step will be inactive if
#' the target table doesn't contain a column named `column_1`. We can also use
#' multiple columns in `vars()` so having `active = ~ . %>%
#' has_columns(vars(column_1, column_2))` in a validation step will make it
#' inactive at [interrogate()] time unless the columns `column_1` and `column_2`
#' are both present.
#' 
#' @param x The table object.
#' @param columns One or more column names that are to be checked for existence
#' in the table `x`.
#'   
#' @return A length-1 logical vector.
#' 
#' @examples 
#' # The `small_table` dataset in the
#' # package has the columns `date_time`,
#' # `date`, and the `a` through `f`
#' # columns
#' small_table
#' 
#' # With `has_columns()` we can check for
#' # column existence by using it directly
#' # with the table; a column name can be
#' # verified as present by using it in
#' # double quotes
#' small_table %>% has_columns("date")
#' 
#' # Multiple column names can be supplied;
#' # this is `TRUE` because both columns are
#' # present in `small_table`
#' small_table %>% has_columns(c("a", "b"))
#' 
#' # It's possible to supply column names
#' # in `vars()` as well
#' small_table %>% has_columns(vars(a, b))
#' 
#' # Because column `h` isn't present, this
#' # returns `FALSE` (all specified columns
#' # need to be present to obtain `TRUE`)
#' small_table %>% has_columns(vars(a, h))
#' 
#' # The `has_columns()` function can be
#' # useful in expressions that involve the
#' # target table, especially if it is
#' # uncertain that the table will contain
#' # a column that's involved in a validation
#' 
#' # In the following agent-based validation,
#' # the first two steps will be 'active'
#' # because all columns checked for in the
#' # expressions are present; the third step
#' # is inactive because column `j` isn't
#' # there (without the `active` statement we
#' # would get an evaluation failure in the
#' # agent report)
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table"
#'   ) %>%
#'   col_vals_gt(
#'     vars(c), value = vars(a),
#'     active = ~ . %>% has_columns(vars(a, c))
#'   ) %>%
#'   col_vals_lt(
#'     vars(h), value = vars(d),
#'     preconditions = ~ . %>% dplyr::mutate(h = d - a),
#'     active = ~ . %>% has_columns(vars(a, d))
#'   ) %>%
#'   col_is_character(
#'     vars(j),
#'     active = ~ . %>% has_columns("j")
#'   ) %>%
#'   interrogate() 
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-2
#'
#' @export

# incorporate--------------------------------------------------------------
#' Dado un objeto *informant*, actualice e incorpore fragmentos de tabla
#' 
#' @description 
#' When the *informant* object has a number of snippets available (by using
#' [info_snippet()]) and the strings to use them (by using the `info_*()`
#' functions and `{<snippet_name>}` in the text elements), the process of
#' incorporating aspects of the table into the info text can occur by
#' using the `incorporate()` function. After that, the information will be fully
#' updated (getting the current state of table dimensions, re-rendering the
#' info text, etc.) and we can print the *informant* object or use the
#' [get_informant_report()] function to see the information report.
#' 
#' @param informant An informant object of class `ptblank_informant`.
#' 
#' @return Un objeto `ptblank_informant`.
#' 
#' @examples 
#' if (interactive()) {
#' 
#' # Take the `small_table` and
#' # assign it to `test_table`; we'll
#' # modify it later
#' test_table <- small_table
#' 
#' # Generate an informant object, add
#' # two snippets with `info_snippet()`,
#' # add information with some other
#' # `info_*()` functions and then
#' # `incorporate()` the snippets into
#' # the info text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ test_table,
#'     tbl_name = "test_table"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "row_count",
#'     fn = ~ . %>% nrow()
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "col_count",
#'     fn = ~ . %>% ncol()
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to 10. (SIMPLE)"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. (CALC)"
#'   ) %>%
#'   info_section(
#'     section_name = "rows",
#'     row_count = "There are {row_count} rows available."
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `informant` object
#' # to see the information report
#' 
#' # Let's modify `test_table` to give
#' # it more rows and an extra column
#' test_table <- 
#'   dplyr::bind_rows(test_table, test_table) %>%
#'   dplyr::mutate(h = a + c)
#' 
#' # Using `incorporate()` will cause
#' # the snippets to be reprocessed, and,
#' # the strings to be updated
#' informant <-
#'   informant %>% incorporate()
#'   
#' # When printed again, we'll see that the
#' # row and column counts in the header
#' # have been updated to reflect the
#' # changed `test_table`
#' 
#' }
#' 
#' @family Incorporate and Report
#' @section Function ID:
#' 7-1
#' 
#' @export

# info_columns-------------------------------------------------------------
#' Agregar información que se centre en aspectos de las columnas de una tabla de
#' datos
#' 
#' @description
#' Upon creation of an *informant* object (with the [create_informant()]
#' function), there are two sections containing properties: (1) 'table' and (2)
#' 'columns'. The 'columns' section is initialized with the table's column names
#' and their types (as `_type`). Beyond that, it is useful to provide details
#' about the nature of each column and we can do that with the `info_columns()`
#' function. A single column (or multiple columns) is targeted, and then a
#' series of named arguments (in the form `entry_name = "The *info text*."`)
#' serves as additional information for the column or columns.
#' 
#' @section Info Text:
#' The *info text* that's used for any of the `info_*()` functions readily
#' accepts Markdown formatting, and, there are a few *Text Tricks* that can be
#' used to spice up the presentation. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label-style text by using: (1) double parentheses
#' around text for a rectangular border as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular border like `(((label
#' text)))`.
#'
#' CSS style rules can be applied to spans of *info text* with the following
#' form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' The are quite a few CSS style rules that can be used to great effect. Here
#' are a few you might like:
#' 
#' - `color: <a color value>;` (text color)
#' - `background-color: <a color value>;` (the text's background color)
#' - `text-decoration: (overline | line-through | underline);`
#' - `text-transform: (uppercase | lowercase | capitalize);`
#' - `letter-spacing: <a +/- length value>;`
#' - `word-spacing: <a +/- length value>;`
#' - `font-style: (normal | italic | oblique);`
#' - `font-weight: (normal | bold | 100-900);`
#' - `font-variant: (normal | bold | 100-900);`
#' - `border: <a color value> <a length value> (solid | dashed | dotted);`
#' 
#' In the above examples, 'length value' refers to a CSS length which can be
#' expressed in different units of measure (e.g., `12px`, `1em`, etc.). Some
#' lengths can be expressed as positive or negative values (e.g., for
#' `letter-spacing`). Color values can be expressed in a few ways, the most
#' common being in the form of hexadecimal color values or as CSS color names.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). The way that information on table
#' columns is represented in YAML works like this: *info text* goes into
#' subsections of YAML keys named for the columns, which are themselves part of
#' the top-level `columns` key. Here is an example of how several calls of
#' `info_columns()` are expressed in R code and how the result corresponds to
#' the YAML representation.
#' 
#' ```
#' # Código R
#' informant %>% 
#'   info_columns(
#'     columns = "date_time",
#'     info = "*info text* 1."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "*info text* 2."
#'   ) %>%
#'   info_columns(
#'     columns = "item_count",
#'     info = "*info text* 3. Statistics: {snippet_1}."
#'   ) %>%
#'   info_columns(
#'     columns = vars(date, date_time),
#'     info = "UTC time."
#'   )
#' 
#' # Representación YAML
#' columns:
#'   date_time:
#'     _type: POSIXct, POSIXt
#'     info: '*info text* 1. UTC time.'
#'   date:
#'     _type: Date
#'     info: '*info text* 2. UTC time.'
#'   item_count:
#'     _type: integer
#'     info: '*info text* 3. Statistics: {snippet_1}.'
#' ```
#' 
#' Subsections represented as column names are automatically generated when
#' creating an informant. Within these, there can be multiple subsections used
#' for holding *info text* on each column. The subsections used across the
#' different columns needn't be the same either, the only commonality that
#' should be enforced is the presence of the `_type` key (automatically updated
#' at every [incorporate()] invocation). 
#' 
#' It's safest to use single quotation marks around any *info text* if directly
#' editing it in a YAML file. Note that Markdown formatting and *info snippet*
#' placeholders (shown here as `{snippet_1}`, see [info_snippet()] for more
#' information) are preserved in the YAML. The Markdown to HTML conversion is
#' done when printing an informant (or invoking [get_informant_report()] on an
#' *informant*) and the processing of snippets (generation and insertion) is
#' done when using the [incorporate()] function. Thus, the source text is always
#' maintained in the YAML representation and is never written in processed form.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param columns The column or set of columns to focus on. Can be defined as a
#'   column name in quotes (e.g., `"<column_name>"`), one or more column names
#'   in `vars()` (e.g., `vars(<column_name>)`), or with a select helper (e.g.,
#'   `starts_with("date")`).
#' @param ... Information entries as a series of named arguments. The names
#'   refer to subsection titles within `COLUMN` -> `<COLUMN_NAME>` and the RHS
#'   contains the *info text* (informational text that can be written as
#'   Markdown and further styled with *Text Tricks*).
#' @param .add Should new text be added to existing text? This is `TRUE` by
#'   default; setting to `FALSE` replaces any existing text for a property.
#' 
#' @return Un objeto `ptblank_informant`.
#' 
#' @examples 
#' # Create a pointblank `informant`
#' # object with `create_informant()`;
#' # we specify a `read_fn` with the
#' # `~` followed by a statement that
#' # gets the `small_table` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   )
#' 
#' # We can add *info text* to describe
#' # the columns in the table with multiple
#' # `info_columns()` calls; the *info text*
#' # calls are additive to existing content
#' # in subsections
#' informant <-
#'   informant %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to 10. (SIMPLE)"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. (CALC)"
#'   )
#' 
#' # Upon printing the `informant` object, we see
#' # the additions made to the 'Columns' section
#' 
#' if (interactive()) {
#' 
#' # The `informant` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then, information can
#' # be directly edited or modified
#' yaml_write(
#'   informant = informant,
#'   filename = "informant.yml"
#' )
#' 
#' # The YAML file can then be read back
#' # into an informant object with the
#' # `yaml_read_informant()` function
#' informant <-
#'   yaml_read_informant(
#'     filename = "informant.yml"
#'   )
#' 
#' }
#' 
#' @section Figures:
#' \if{html}{\figure{man_info_columns_1.png}{options: width=100\%}}
#'
#' @family Information Functions
#' @section Function ID:
#' 3-2
#'
#' @export

# info_columns_from_tbl----------------------------------------------------
#' Agregar información de columna de otra tabla de datos
#' 
#' @description
#' The `info_columns_from_tbl()` function is a wrapper around the
#' [info_columns()] function and is useful if you wish to apply *info text* to
#' columns where that information already exists in a data frame (or in some
#' form that can readily be coaxed into a data frame). The form of the input
#' `tbl` (the one that contains column metadata) has a few basic requirements:
#' 
#' - the data frame must have two columns
#' - both columns must be of class `character`
#' - the first column should contain column names and the second should contain
#' the *info text*
#' 
#' Each column that matches across tables (i.e., the `tbl` and the target table
#' of the informant) will have a new entry for the `"info"` property. Empty or
#' missing info text will be pruned from `tbl`.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param tbl The two-column data frame which contains metadata about the target
#'   table in the informant object. 
#' @param .add Should new text be added to existing text? This is `TRUE` by
#'   default; setting to `FALSE` replaces any existing text for the `"info"`
#'   property.
#' 
#' @return Un objeto `ptblank_informant`.
#' 
#' @examples 
#' # Create a pointblank `informant`
#' # object with `create_informant()`;
#' # we specify a `read_fn` with the
#' # `~` followed by a statement that
#' # gets the `game_revenue` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~ game_revenue,
#'     tbl_name = "game_revenue",
#'     label = "Un ejemplo."
#'   )
#' 
#' # We can add *info text* to describe
#' # the columns in the table by using
#' # information in another table; the
#' # `info_columns_from_tbl()` takes a
#' # table object where the first column
#' # has the column names and the second
#' # contains the *info text* (the
#' # `game_revenue_info` dataset contains
#' # metadata for `game_revenue`)
#' informant <-
#'   informant %>%
#'   info_columns_from_tbl(
#'     tbl = game_revenue_info
#'   )
#' 
#' # We can continue to add more *info
#' # text* since the process is additive;
#' # the `info_columns_from_tbl()`
#' # function populates the `info`
#' # subsection
#' informant <-
#'   informant %>%
#'   info_columns(
#'     columns = "item_revenue",
#'     info = "Revenue reported in USD."
#'   ) %>%
#'   info_columns(
#'     columns = "acquisition",
#'     `top list` = "{top5_aq}"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "top5_aq",
#'     fn = snip_list(column = "acquisition")
#'   ) %>%
#'   incorporate()
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-3
#' 
#' @seealso The [info_columns()] function, which allows for manual entry of
#'   *info text*.
#'
#' @export

# info_section-------------------------------------------------------------
#' Agregue información que se centre en algún aspecto clave de la tabla de datos
#' 
#' @description 
#' While the [info_tabular()] and [info_columns()] functions allow us to
#' add/modify info text for specific sections, the `info_section()` makes it
#' possible to add sections of our own choosing and the information that make
#' sense for those sections. Define a `section_name` and provide a series of
#' named arguments (in the form `entry_name = "The *info text*."`) to build the
#' informational content for that section.
#' 
#' @section Info Text:
#' The *info text* that's used for any of the `info_*()` functions readily
#' accepts Markdown formatting, and, there are a few *Text Tricks* that can be
#' used to spice up the presentation. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label-style text by using: (1) double parentheses
#' around text for a rectangular border as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular border like `(((label
#' text)))`.
#'
#' CSS style rules can be applied to spans of *info text* with the following
#' form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' The are quite a few CSS style rules that can be used to great effect. Here
#' are a few you might like:
#' 
#' - `color: <a color value>;` (text color)
#' - `background-color: <a color value>;` (the text's background color)
#' - `text-decoration: (overline | line-through | underline);`
#' - `text-transform: (uppercase | lowercase | capitalize);`
#' - `letter-spacing: <a +/- length value>;`
#' - `word-spacing: <a +/- length value>;`
#' - `font-style: (normal | italic | oblique);`
#' - `font-weight: (normal | bold | 100-900);`
#' - `font-variant: (normal | bold | 100-900);`
#' - `border: <a color value> <a length value> (solid | dashed | dotted);`
#' 
#' In the above examples, 'length value' refers to a CSS length which can be
#' expressed in different units of measure (e.g., `12px`, `1em`, etc.). Some
#' lengths can be expressed as positive or negative values (e.g., for
#' `letter-spacing`). Color values can be expressed in a few ways, the most
#' common being in the form of hexadecimal color values or as CSS color names.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). Extra sections (i.e., neither the
#' `table` nor the `columns` sections) can be generated and filled with *info
#' text* by using one or more calls of `info_section()`. This is how it is
#' expressed in both R code and in the YAML representation.
#' 
#' ```
#' # Código R
#' informant %>% 
#'   info_section(
#'     section_name = "History",
#'     Changes = "
#' - Change 1
#' - Change 2
#' - Change 3",
#'     `Last Update` = "(2020-10-23) at 3:28 PM."
#'   ) %>%
#'   info_section(
#'     section_name = "Additional Notes",
#'     `Notes 1` = "Notes with a {snippet}.",
#'     `Notes 2` = "**Bold notes**."
#'   )
#' 
#' # Representación YAML
#' History:
#'   Changes: |2-
#'   
#'     - Change 1
#'     - Change 2
#'     - Change 3
#'   Last Update: (2020-10-23) at 3:28 PM.
#' Additional Notes:
#'   Notes 1: Notes with a {snippet}.
#'   Notes 2: '**Bold notes**.'
#' ```
#' 
#' Subsections represented as column names are automatically generated when
#' creating an informant. Within each of the top-level sections (i.e., `History`
#' and `Additional Notes`) there can be multiple subsections used for holding
#' *info text*.
#' 
#' It's safest to use single quotation marks around any *info text* if directly
#' editing it in a YAML file. Note that Markdown formatting and *info snippet*
#' placeholders (shown here as `{snippet}`, see [info_snippet()] for more
#' information) are preserved in the YAML. The Markdown to HTML conversion is
#' done when printing an informant (or invoking [get_informant_report()] on an
#' *informant*) and the processing of snippets (generation and insertion) is
#' done when using the [incorporate()] function. Thus, the source text is always
#' maintained in the YAML representation and is never written in processed form.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param section_name The name of the section for which this information
#'   pertains.
#' @param ... Information entries as a series of named arguments. The names
#'   refer to subsection titles within the section defined as `section_name` and
#'   the RHS is the *info text* (informational text that can be written as
#'   Markdown and further styled with *Text Tricks*).
#' 
#' @return Un objeto `ptblank_informant`.
#' 
#' @examples 
#' # Create a pointblank `informant`
#' # object with `create_informant()`;
#' # we specify a `read_fn` with the
#' # `~` followed by a statement that
#' # gets the `small_table` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   )
#' 
#' # The `informant` object has the 'table'
#' # and 'columns' sections; we can create
#' # entirely different sections with their
#' # own properties using `info_section()`
#' 
#' # We can add *info text* to sections
#' # entirely different than `table` and
#' # `columns` with `info_section()`
#' informant <-
#'   informant %>%
#'   info_section(
#'     section_name = "Notes",
#'     creation = "Dataset generated on (2020-01-15).",
#'     usage = "`small_table %>% dplyr::glimpse()`"
#'   ) %>%
#'   incorporate()
#' 
#' # Upon printing the `informant` object, we see
#' # the addition of the 'Notes' section and its
#' # own information
#' 
#' if (interactive()) {
#' 
#' # The `informant` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then, information can
#' # be directly edited or modified
#' yaml_write(
#'   informant = informant,
#'   filename = "informant.yml"
#' )
#' 
#' # The YAML file can then be read back
#' # into an informant object with the
#' # `yaml_read_informant()` function
#' informant <-
#'   yaml_read_informant(
#'     filename = "informant.yml"
#'   )
#' 
#' }
#' 
#' @section Figures:
#' \if{html}{\figure{man_info_section_1.png}{options: width=100\%}}
#'
#' @family Information Functions
#' @section Function ID:
#' 3-4
#'
#' @export

# info_snippet-------------------------------------------------------------
#' Genere un 'snippet' de texto útil a partir de la tabla de destino
#' 
#' @description 
#' Getting little snippets of information from a table goes hand-in-hand with
#' mixing those bits of info with your table info. Call `info_snippet()` to
#' define a snippet and how you'll get that from the target table. The snippet
#' definition is supplied either with a formula, or, with a
#' **pointblank**-supplied `snip_*()` function. So long as you know how to
#' interact with a table and extract information, you can easily define snippets
#' for a *informant* object. And once those snippets are defined, you can insert
#' them into the *info text* as defined through the other `info_*()` functions
#' ([info_tabular()], [info_columns()], and [info_section()]). Use curly braces
#' with just the `snippet_name` inside (e.g., `"This column has {n_cat}
#' categories."`).
#' 
#' @section Snip functions provided in **pointblank**:
#' For convenience, there are several `snip_*()` functions provided in the
#' package that work on column data from the *informant*'s target table. These
#' are:
#' 
#' - [snip_list()]: get a list of column categories
#' - [snip_stats()]: get an inline statistical summary
#' - [snip_lowest()]: get the lowest value from a column
#' - [snip_highest()] : get the highest value from a column
#' 
#' As it's understood what the target table is, only the `column` in each of
#' these functions is necessary for obtaining the resultant text.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). Snippets are stored in the YAML
#' representation and here is is how they are expressed in both R code and in
#' the YAML output (showing both the `meta_snippets` and `columns` keys to
#' demonstrate their relationship here).
#' 
#' ```
#' # Código R
#' informant %>% 
#'   info_columns(
#'     columns = "date_time",
#'     `Latest Date` = "The latest date is {latest_date}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "latest_date",
#'     fn = ~ . %>% dplyr::pull(date) %>% max(na.rm = TRUE)
#'   ) %>%
#'   incorporate()
#' 
#' # Representación YAML
#' meta_snippets:
#'   latest_date: ~. %>% dplyr::pull(date) %>% max(na.rm = TRUE)
#' ...
#' columns:
#'   date_time:
#'     _type: POSIXct, POSIXt
#'     Latest Date: The latest date is {latest_date}.
#'   date:
#'     _type: Date
#'   item_count:
#'     _type: integer
#' ```
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param snippet_name The name for snippet, which is used for interpolating the
#'   result of the snippet formula into *info text* defined by an `info_*()`
#'   function.
#' @param fn A formula that obtains a snippet of data from the target table.
#'   It's best to use a leading dot (`.`) that stands for the table itself and
#'   use pipes to construct a series of operations to be performed on the table
#'   (e.g., `~ . %>% dplyr::pull(column_2) %>% max(na.rm = TRUE)`). So long as
#'   the result is a length-1 vector, it'll likely be valid for insertion into
#'   some info text. Alternatively, a `snip_*()` function can be used here
#'   (these functions always return a formula that's suitable for all types of
#'   data sources).
#' 
#' @return Un objeto `ptblank_informant`.
#' 
#' @examples 
#' # Take the `small_table` and
#' # assign it to `test_table`; we'll
#' # modify it later
#' test_table <- small_table
#' 
#' # Generate an informant object, add
#' # two snippets with `info_snippet()`,
#' # add information with some other
#' # `info_*()` functions and then
#' # `incorporate()` the snippets into
#' # the info text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ test_table,
#'     tbl_name = "test_table",
#'     label = "Un ejemplo."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "row_count",
#'     fn = ~ . %>% nrow()
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "max_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to {max_a}. (SIMPLE)"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. (CALC)"
#'   ) %>%
#'   info_section(
#'     section_name = "rows",
#'     row_count = "There are {row_count} rows available."
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `informant` object
#' # to see the information report
#' 
#' # Let's modify `test_table` to give
#' # it more rows and an extra column
#' test_table <- 
#'   dplyr::bind_rows(test_table, test_table) %>%
#'   dplyr::mutate(h = a + c)
#' 
#' # Using `incorporate()` will cause
#' # the snippets to be reprocessed, and,
#' # the info text to be updated
#' informant <-
#'   informant %>% incorporate()
#'   
#' @section Figures:
#' \if{html}{\figure{man_info_snippet_1.png}{options: width=100\%}}
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-5
#' 
#' @export

# info_tabular-------------------------------------------------------------
#' Agregue información que se centre en aspectos de la tabla de datos en su
#' conjunto
#' 
#' @description 
#' When an *informant* object is created with the [create_informant()] function,
#' it has two starter sections: (1) 'table' and (2) 'columns'. The 'table'
#' section should contain a few properties upon creation, such as the supplied
#' table name (`name`) and table dimensions (as `_columns` and `_rows`). We can
#' add more table-based properties with the `info_tabular()` function. By
#' providing a series of named arguments (in the form `entry_name = "The *info
#' text*."`), we can add more information that makes sense for describing the
#' table as a whole.
#' 
#' @section Info Text:
#' The *info text* that's used for any of the `info_*()` functions readily
#' accepts Markdown formatting, and, there are a few *Text Tricks* that can be
#' used to spice up the presentation. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label-style text by using: (1) double parentheses
#' around text for a rectangular border as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular border like `(((label
#' text)))`.
#'
#' CSS style rules can be applied to spans of *info text* with the following
#' form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' The are quite a few CSS style rules that can be used to great effect. Here
#' are a few you might like:
#' 
#' - `color: <a color value>;` (text color)
#' - `background-color: <a color value>;` (the text's background color)
#' - `text-decoration: (overline | line-through | underline);`
#' - `text-transform: (uppercase | lowercase | capitalize);`
#' - `letter-spacing: <a +/- length value>;`
#' - `word-spacing: <a +/- length value>;`
#' - `font-style: (normal | italic | oblique);`
#' - `font-weight: (normal | bold | 100-900);`
#' - `font-variant: (normal | bold | 100-900);`
#' - `border: <a color value> <a length value> (solid | dashed | dotted);`
#' 
#' In the above examples, 'length value' refers to a CSS length which can be
#' expressed in different units of measure (e.g., `12px`, `1em`, etc.). Some
#' lengths can be expressed as positive or negative values (e.g., for
#' `letter-spacing`). Color values can be expressed in a few ways, the most
#' common being in the form of hexadecimal color values or as CSS color names.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). When `info_tabular()` is
#' represented in YAML, *info text* goes into subsections of the top-level
#' `table` key. Here is an example of how a call of `info_tabular()` is
#' expressed in R code and in the corresponding YAML representation.
#' 
#' ```
#' # Código R
#' informant %>% 
#'   info_tabular(
#'     section_1 = "*info text* 1.",
#'     `section 2` = "*info text* 2 and {snippet_1}"
#'   )
#' 
#' # Representación YAML
#' table:
#'   _columns: 23
#'   _rows: 205.0
#'   _type: tbl_df
#'   section_1: '*info text* 1.'
#'   section 2: '*info text* 2 and {snippet_1}'
#' ```
#' 
#' Subsection titles as defined in `info_tabular()` can be set in backticks if
#' they are not syntactically correct as an argument name without them (e.g.,
#' when using spaces, hyphens, etc.).
#' 
#' It's safest to use single quotation marks around any *info text* if directly
#' editing it in a YAML file. Note that Markdown formatting and *info snippet*
#' placeholders (shown here as `{snippet_1}`, see [info_snippet()] for more
#' information) are preserved in the YAML. The Markdown to HTML conversion is
#' done when printing an informant (or invoking [get_informant_report()] on an
#' *informant*) and the processing of snippets (generation and insertion) is
#' done when using the [incorporate()] function. Thus, the source text is always
#' maintained in the YAML representation and is never written in processed form.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param ... Information entries as a series of named arguments. The names
#'   refer to subsection titles within the `TABLE` section and the RHS is the
#'   *info text* (informational text that can be written as Markdown and further
#'   styled with *Text Tricks*).
#' 
#' @return Un objeto `ptblank_informant`.
#' 
#' @examples 
#' # Create a pointblank `informant`
#' # object with `create_informant()`;
#' # we specify a `read_fn` with the
#' # `~` followed by a statement that
#' # gets the `small_table` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   )
#' 
#' # We can add *info text* to describe
#' # the table with `info_tabular()`
#' informant <-
#'   informant %>%
#'   info_tabular(
#'     `Row Definition` = "A row has randomized values.",
#'     Source = c(
#'       "- From the **pointblank** package.",
#'       "- [https://rich-iannone.github.io/pointblank/]()"
#'      )
#'    )
#' 
#' # Upon printing the `informant` object, we see
#' # the additions made to the 'Table' section
#' 
#' if (interactive()) {
#' 
#' # The `informant` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then information can
#' # be directly edited or modified
#' yaml_write(
#'   informant = informant,
#'   filename = "informant.yml"
#' )
#' 
#' # The YAML file can then be read back
#' # into an informant object with the
#' # `yaml_read_informant()` function
#' informant <-
#'   yaml_read_informant(
#'     filename = "informant.yml"
#'   )
#' 
#' }
#'
#' @section Figures:
#' \if{html}{\figure{man_info_tabular_1.png}{options: width=100\%}}
#'
#' @family Information Functions
#' @section Function ID:
#' 3-1
#'
#' @export

# interrogate--------------------------------------------------------------
#' Dado un agente que tiene un plan de validación, realice un interrogatorio
#'
#' @description 
#' When the agent has all the information on what to do (i.e., a validation plan
#' which is a series of validation steps), the interrogation process can occur
#' according its plan. After that, the agent will have gathered intel, and we
#' can use functions like [get_agent_report()] and [all_passed()] to understand
#' how the interrogation went down.
#'
#' @param agent An agent object of class `ptblank_agent` that is created with
#'   [create_agent()].
#' @param extract_failed An option to collect rows that didn't pass a particular
#'   validation step. The default is `TRUE` and further options allow for fine
#'   control of how these rows are collected.
#' @param get_first_n If the option to collect non-passing rows is chosen, there
#'   is the option here to collect the first `n` rows here. Supply the number of
#'   rows to extract from the top of the non-passing rows table (the ordering of
#'   data from the original table is retained).
#' @param sample_n If the option to collect non-passing rows is chosen, this
#'   option allows for the sampling of `n` rows. Supply the number of rows to
#'   sample from the non-passing rows table. If `n` is greater than the number
#'   of non-passing rows, then all the rows will be returned.
#' @param sample_frac If the option to collect non-passing rows is chosen, this
#'   option allows for the sampling of a fraction of those rows. Provide a
#'   number in the range of `0` and `1`. The number of rows to return may be
#'   extremely large (and this is especially when querying remote databases),
#'   however, the `sample_limit` option will apply a hard limit to the returned
#'   rows.
#' @param sample_limit A value that limits the possible number of rows returned
#'   when sampling non-passing rows using the `sample_frac` option.
#'   
#' @return Un objeto `ptblank_agent`.
#'   
#' @examples
#' if (interactive()) {
#' 
#' # Create a simple table with two
#' # columns of numerical values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3)
#'   )
#' 
#' # Validate that values in column
#' # `a` from `tbl` are always > 5,
#' # using `interrogate()` carries out
#' # the validation plan and completes
#' # the whole process
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(vars(a), value = 5) %>%
#'   interrogate()
#' 
#' }
#' 
#' @family Interrogate and Report
#' @section Function ID:
#' 6-1
#' 
#' @export

# log4r_step---------------------------------------------------------------
#' Habilite el registro de condiciones de falla en el nivel del paso de
#' validación
#' 
#' @description 
#' The `log4r_step()` function can be used as an action in the [action_levels()]
#' function (as a list component for the `fns` list). Place a call to this
#' function in every failure condition that should produce a log (i.e., `warn`,
#' `stop`, `notify`). Only the failure condition with the highest severity for a
#' given validation step will produce a log entry (skipping failure conditions
#' with lower severity) so long as the call to `log4r_step()` is present.
#' 
#' @param x A reference to the x-list object prepared by the `agent`. This
#'   version of the x-list is the same as that generated via
#'   `get_agent_x_list(<agent>, i = <step>)` except this version is internally
#'   generated and hence only available in an internal evaluation context.
#' @param message The message to use for the log entry. When not provided, a
#'   default glue string is used for the messaging. This is dynamic since the
#'   internal `glue::glue()` call occurs in the same environment as `x`, the
#'   x-list that's constrained to the validation step. The default message, used
#'   when `message = NULL` is the glue string `"Step {x$i} exceeded the {level}
#'   failure threshold (f_failed = {x$f_failed}) ['{x$type}']"`. As can be seen,
#'   a custom message can be crafted that uses other elements of the x-list with
#'   the `{x$<component>}` construction.
#' @param append_to The file to which log entries at the warn level are
#'   appended. This can alternatively be one or more **log4r** appenders.
#' 
#' @family Logging
#' @section Function ID:
#' 5-1
#' 
#' @export

# print.action_levels------------------------------------------------------
#' Imprime el objeto `action_levels`
#'
#' This function will allow the `action_levels` to be nicely printed.
#' 
#' @param x An object of class `action_levels`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.ptblank_agent------------------------------------------------------
#' Imprime el objeto `ptblank_agent`
#'
#' This function will allow the agent object to print a useful HTML-based
#' report.
#' 
#' @param x An object of class `ptblank_agent`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.ptblank_informant--------------------------------------------------
#' Imprime el objeto  `ptblank_informant`
#'
#' This function will allow the informant object to print a useful HTML-based
#' report.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.ptblank_multiagent-------------------------------------------------
#' Imprime el objeto `ptblank_multiagent`
#'
#' This function will allow the multiagent object to print a useful HTML-based
#' report.
#' 
#' @param x An object of class `ptblank_multiagent`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.ptblank_multiagent_report.long-------------------------------------
#' Imprime el objeto `ptblank_multiagent_report.long`
#'
#' This function will print the `ptblank_multiagent_report.long` object, which
#' is an HTML-based report.
#' 
#' @param x An object of class `ptblank_multiagent_report.long`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.ptblank_tbl_scan---------------------------------------------------
#' Imprime el objeto `ptblank_tbl_scan`
#'
#' This function will print the `ptblank_tbl_scan` object, which is an
#' HTML-based report.
#'
#' @param x An object of class `ptblank_tbl_scan`.
#' @param ... Any additional parameters.
#' @param view The value for `print()`s `browse` argument.
#'
#' @keywords internal
#'
#' @export

# print.read_fn------------------------------------------------------------
#' Imprime el objeto `read_fn`
#'
#' This function will allow the `read_fn` to be nicely printed.
#' 
#' @param x An object of class `read_fn`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.tbl_store----------------------------------------------------------
#' Imprime el objeto `tbl_store`
#'
#' This function will allow the `tbl_store` to be nicely printed.
#' 
#' @param x An object of class `tbl_store`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.x_list_i-----------------------------------------------------------
#' Imprima una x-list de un solo paso en la consola
#'
#' This function will print an x-list object, for a single step, to the console.
#' 
#' @param x An x-list object of class `x_list_i`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# print.x_list_n-----------------------------------------------------------
#' Imprima una x-list que incluya todos los pasos de validación en la consola
#'
#' This function will print a x-list object, with all validation steps included,
#' to the console.
#' 
#' @param x An x-list object of class `x_list_n`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export

# read_disk_multiagent-----------------------------------------------------
#' Leer objetos **pointblank** *agent* almacenados en el disco como *multiagent*
#'
#' @description 
#' An *agent* or *informant* can be written to disk with the [x_write_disk()]
#' function. While useful for later retrieving the stored agent with
#' [x_read_disk()] it's also possible to read a series of on-disk agents with
#' the `read_disk_multiagent()` function, which creates a `ptblank_multiagent`
#' object. A *multiagent* object can also be generated via the
#' [create_multiagent()] function but is less convenient to use if one is just
#' using agents that have been previous written to disk.
#'
#' @param filenames The names of files (holding *agent* objects) that were
#'   previously written by [x_write_disk()].
#' @param pattern A regex pattern for accessing saved-to-disk *agent* files
#'   located in a directory (specified in the `path` argument).
#' @param path A path to a collection of files. This is either optional in the
#'   case that files are specified in `filenames` (the `path` combined with all
#'   `filenames`), or, required when providing a `pattern` for file names.
#'   
#' @return Un objeto `ptblank_multiagent`.
#' 
#' @family The multiagent
#' @section Function ID:
#' 10-2
#'
#' @export

# remove_read_fn-----------------------------------------------------------
#' Eliminar una fórmula de preparación de tablas asociada con un *agent* o
#' *informant*
#' 
#' @description 
#' Removing an *agent* or an *informant*'s association to a table-pre formula
#' can be done with `remove_read_fn()`. This may be good idea in an interactive
#' session when needing to rely on the direct association of a 'fixed' data
#' table (settable in [create_agent()] and [create_informant()]'s `tbl` argument
#' or with [set_tbl()]) instead of using a table-prep formula that might produce
#' different a different table than expected. The table-prep formula can always
#' be set again with [set_read_fn()].
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#'   
#' @examples 
#' # Set proportional failure thresholds
#' # to the `warn`, `stop`, and `notify`
#' # states using `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Create an agent that directly ingests
#' # the `small_table` object and also has
#' # a table-prep formula (when both are
#' # present the latter always obtains the
#' # table); apply the actions, add some
#' # validation steps and then interrogate
#' # the data that was read in
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # In a situation where `small_table`
#' # changes frequently and it's desirable
#' # to have a snapshot of the table, we
#' # can remove the table-prep formula so
#' # that the ingested `small_table` will
#' # be used
#' agent_2 <-
#'   agent_1 %>%
#'   remove_read_fn() %>%
#'   interrogate()
#'   
#' @family Object Ops
#' @section Function ID:
#' 9-7
#'   
#' @export

# remove_steps-------------------------------------------------------------
#' Eliminar uno o más de los pasos de validación de un objeto *agent*
#'
#' @description
#' Validation steps can be removed from an *agent* object through use of the
#' `remove_steps()` function. This is useful, for instance, when getting an
#' agent from disk (via the [x_read_disk()] function) and omitting one or more
#' steps from the *agent*'s validation plan. Please note that when removing
#' validation steps all stored data extracts will be removed from the *agent*.
#'
#' @param agent Un objeto de agente de clase `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition. If `NULL` (the default) then step removal
#'   won't occur by index.
#'   
#' @return Un objeto `ptblank_agent`.
#' 
#' @examples 
#' # Create an agent that has the
#' # `small_table` object as the
#' # target table, add a few
#' # validation steps, and then use
#' # `interrogate()`
#' agent_1 <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>%
#'   col_exists(vars(date)) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]"
#'   ) %>%
#'   interrogate()
#'   
#' # The second validation step has
#' # been determined to be unneeded and
#' # is to be removed; this can be done
#' # by used `remove_steps()` with the
#' # agent object
#' agent_2 <-
#'   agent_1 %>%
#'   remove_steps(i = 2) %>%
#'   interrogate()
#'
#' @return Un objeto `ptblank_agent`.
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-8
#' 
#' @seealso Instead of removal, the [deactivate_steps()] function will simply
#'   change the `active` status of one or more validation steps to `FALSE` (and
#'   [activate_steps()] will do the opposite).
#'
#' @export

# remove_tbl---------------------------------------------------------------
#' Eliminar una tabla de datos asociada con un *agent* o *informant*
#' 
#' @description 
#' Removing an *agent* or *informant*'s association to a data table can be done
#' with the `remove_tbl()` function. This can be useful to ensure that the table
#' data isn't unintentionally written to disk. It is usually best to avoid
#' directly associating a table to an *agent* or *informant* through the `tbl`
#' argument, instead opting for setting a table-prep formula (via
#' [create_agent()] and [create_informant()]'s `read_fn` argument, or, with
#' [set_read_fn()]). If necessary, the association to a table can be set again
#' with [set_tbl()].
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#' 
#' @examples
#' # Set proportional failure thresholds
#' # to the `warn`, `stop`, and `notify`
#' # states using `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Create an agent that has
#' # `small_table` set as the target
#' # table via `tbl`; apply the actions,
#' # add some validation steps and then
#' # interrogate the data
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # In this case where `small_table`
#' # changes (and the aim is to have
#' # validations run periodically) it is
#' # better to obtain the table from the
#' # source with a table-prep formula;
#' # while doing this, the direct
#' # association to `small_table` can be
#' # removed with `remove_tbl()` so it's
#' # no longer part of the agent object
#' agent_2 <-
#'   agent_1 %>%
#'   remove_tbl() %>%
#'   set_read_fn(read_fn = ~ small_table) %>%
#'   interrogate()
#'   
#' @family Object Ops
#' @section Function ID:
#' 9-5
#'   
#' @export

# rows_count_match---------------------------------------------------------
#' ¿El recuento de filas coincide con el de una tabla diferente?
#'
#' @description
#' La función de validación `row_count_match()`, la función de expectativa
#' `expect_row_count_match()` y la función de prueba `test_row_count_match()`
#' comprueban si el recuento de filas en la tabla de destino coincide con el de
#' una tabla de comparación. La función de validación se puede usar directamente
#' en una tabla de datos o con un objeto *agent* (técnicamente, un objeto
#' `ptblank_agent`) mientras que las funciones de expectativa y prueba solo se
#' pueden usar con una tabla de datos. Los tipos de tablas de datos que se
#' pueden utilizar incluyen marcos de datos, tibbles, tablas de base de datos
#' (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Como paso de validación o como
#' expectativa, hay una única unidad de prueba que depende de si los recuentos
#' de filas para las dos tablas son iguales (después de que se hayan aplicado
#' las `preconditions`).
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que esta validación en particular requiera alguna
#' operación en la tabla de destino antes de que se lleve a cabo la comparación
#' del recuento de filas. Especialmente para un informe basado en *agent*, esto
#' puede ser ventajoso ya que podemos desarrollar un gran plan de validación con
#' una sola tabla de destino y realice ajustes menores en ella, según sea
#' necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere usar
#' el código **dplyr** ya que las declaraciones se pueden traducir a SQL si es
#' necesario (es decir, si la tabla de destino reside en una base de datos). El
#' código se proporciona más fácilmente como una fórmula **R** unilateral
#' (utilizando un `~` inicial). En la representación de la fórmula, el `.` sirve
#' como la tabla de datos de entrada que se va a transformar. Alternativamente,
#' se podría proporcionar una función.
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que se produce mejor con la función [action_levels()]. Lee esa
#' función documentación para la verdad sobre cómo crear reacciones por encima
#' del umbral niveles de falla en la validación. La esencia básica es que
#' querrás al menos un nivel de umbral único (especificado como la fracción de
#' unidades de prueba falló, o un valor absoluto), a menudo usando el argumento
#' `warn_at`. Utilizando `action_levels(warn_at = 1)` o `action_levels(stop_at =
#' 1)` son buenas opciones dependiendo de la situación (el primero produce una
#' advertencia, el otro `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `row_count_match()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `row_count_match()`
#' como paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   row_count_match(
#'     tbl_compare = ~ file_tbl(
#'       file = from_github(
#'         file = "all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       ),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `row_count_match()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - row_count_match:
#'     tbl_compare: ~ file_tbl(
#'       file = from_github(
#'         file = "all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       )
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `row_count_match()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos. Los argumentos con valores
#' por defecto no se escribirán en YAML cuando se utilice [yaml_write()] (aunque
#' es aceptable incluirlos con su valor por defecto al generar el YAML por otros
#' medios). También es posible previsualizar la transformación de un agente a
#' YAML sin necesidad de escribirlo en el disco, utilizando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param tbl_compare Una tabla para comparar con la tabla de destino en
#'   términos de valores de recuento de filas. Puede ser un objeto de tabla como
#'   un marco de datos, un tibble, un objeto `tbl_dbi` o un objeto `tbl_spark`.
#'   Como alternativa, se puede utilizar una fórmula de preparación de tabla (`~
#'   <código de lectura de tabla>`) o una función (`function() <código de
#'   lectura de tabla>`) para leer perezosamente la tabla en el momento de la
#'   interrogación.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Create a simple table with three
#' # columns and four rows of values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5),
#'     b = c(7, 1, 0, 0),
#'     c = c(1, 1, 1, 3)
#'   )
#'
#' # Create a second table which is
#' # quite different but has the
#' # same number of rows as `tbl`
#' tbl_2 <-
#'   dplyr::tibble(
#'     e = c("a", NA, "a", "c"),
#'     f = c(2.6, 1.2, 0, NA)
#'   )
#' 
#' # Validate that the count of rows
#' # in the target table (`tbl`) matches
#' # that of the comparison table
#' # (`tbl_2`)
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   row_count_match(tbl_compare = tbl_2) %>%
#'   interrogate()
#' 
#' # Determine si esta validación
#' # pasó usando `all_passed()`
#' all_passed(agent)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-31
#' 
#' @name row_count_match

# rows_complete------------------------------------------------------------
#' ¿Están completos los datos de las filas?
#'
#' @description
#' La función de validación `rows_complete()`, la función de expectativa
#' `expect_rows_complete()` y la función de prueba `test_rows_complete()`
#' comprueban si las filas contienen algún valor `NA`/`NULL` (opcionalmente
#' restringido a una selección de `columns`). La función de validación se puede
#' usar directamente en una tabla de datos o con un objeto *agent*
#' (técnicamente, un objeto `ptblank_agent`) mientras que las funciones de
#' expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos de
#' tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' Podemos especificar los nombres de las columnas restrictivas entre comillas,
#' en `var()`, y con las siguientes funciones auxiliares **tidyselect**:
#' `starts_with()`, `ends_with()`, `contains()`, `matches()` y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Usar
#' `action_levels(warn_at = 0.25)` o `action_levels(stop_at = 0.25)` son buenas
#' opciones dependiendo de la situación (la primera produce una advertencia
#' cuando una cuarta parte de la prueba total las unidades fallan, las otras
#' `stop()` en el mismo nivel de umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `rows_complete()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `rows_complete()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   rows_complete(
#'     columns = vars(a, b),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `rows_complete()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - rows_complete:
#'     columns: vars(a, b)
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `rows_complete()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos. Un valor para `columns` sólo
#' es sólo es necesario si se buscan valores únicos en un subconjunto de
#' columnas. Los argumentos con valores por defecto no se escribirán en YAML
#' cuando se utilice [yaml_write()] (aunque es aceptable incluirlos con su valor
#' por defecto al generar el YAML por otros medios). También es posible
#' previsualizar la transformación de un agente a YAML sin necesidad de
#' escribirlo en el disco, utilizando la función [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Cree una tabla sencilla con tres
#' # columnas de valores numéricos
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 8, 3),
#'     c = c(1, 1, 1, 3, 3, 3)
#'   )
#' 
#' # Validar que al considerar sólo
#' # los datos de las columnas `a` y
#' # `b`, sólo hay filas completas (es
#' # decir, todas las filas no tienen
#' # valores `NA`)
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   rows_complete(vars(a, b)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación
#' # pasó usando `all_passed()`
#' all_passed(agent)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-21
#' 
#' @name rows_complete

# rows_distinct------------------------------------------------------------
#' ¿Son distintos los datos de las filas?
#'
#' @description
#' La función de validación `rows_distinct()`, la función de expectativa
#' `expect_rows_distinct()` y la función de prueba `test_rows_distinct()`
#' comprueban si los valores de las filas (opcionalmente restringidos a una
#' selección de `columns` especificadas) son, cuando se toman como una unidad
#' completa, distintos de todas las demás unidades de la tabla. La función de
#' validación se puede usar directamente en una tabla de datos o con un objeto
#' *agent* (técnicamente, un objeto `ptblank_agent`) mientras que las funciones
#' de expectativa y prueba solo se pueden usar con una tabla de datos. Los tipos
#' de tablas de datos que se pueden utilizar incluyen marcos de datos, tibbles,
#' tablas de base de datos (`tbl_dbi`) y Spark DataFrames (`tbl_spark`). Cada
#' paso de validación o expectativa operará sobre el número de unidades de
#' prueba que es igual al número de filas en la tabla (después de que se hayan
#' aplicado las `preconditions`).
#'
#' Podemos especificar los nombres de las columnas restrictivas entre comillas,
#' en `var()`, y con las siguientes funciones auxiliares **tidyselect**:
#' `starts_with()`, `ends_with()`, `contains()`, `matches()` y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Usar
#' `action_levels(warn_at = 0.25)` o `action_levels(stop_at = 0.25)` son buenas
#' opciones dependiendo de la situación (la primera produce una advertencia
#' cuando una cuarta parte de la prueba total las unidades fallan, las otras
#' `stop()` en el mismo nivel de umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `rows_distinct()` se representa en YAML
#' (bajo la clave de nivel superior `steps` como un miembro de la lista), la
#' sintaxis sigue de cerca la firma de la función de validación. A continuación
#' se muestra un ejemplo de cómo una llamada compleja de `rows_distinct()` como
#' paso de validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   rows_distinct(
#'     columns = vars(a, b),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `rows_distinct()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - rows_distinct:
#'     columns: vars(a, b)
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `rows_distinct()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos. Un valor para `columns` sólo
#' es sólo es necesario si se buscan valores únicos en un subconjunto de
#' columnas. Los argumentos con valores por defecto no se escribirán en YAML
#' cuando se utilice [yaml_write()] (aunque es aceptable incluirlos con su valor
#' por defecto al generar el YAML por otros medios). También es posible
#' previsualizar la transformación de un agente a YAML sin necesidad de
#' escribirlo en el disco, utilizando la función [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Create a simple table with three
#' # columns of numerical values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 8, 3),
#'     c = c(1, 1, 1, 3, 3, 3)
#'   )
#' 
#' # Validate that when considering only
#' # data in columns `a` and `b`, there
#' # are no duplicate rows (i.e., all
#' # rows are distinct)
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   rows_distinct(vars(a, b)) %>%
#'   interrogate()
#' 
#' # Determine si esta validación
#' # pasó usando `all_passed()`
#' all_passed(agent)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-20
#' 
#' @name rows_distinct

# scan_data----------------------------------------------------------------
#' Escanee minuciosamente una tabla para comprenderla mejor
#'
#' @description
#' Generar un informe HTML que recorra los datos de la tabla de entrada. Antes
#' de llamar a un *agente* para validar los datos, es una buena idea entender
#' los datos con cierto nivel de precisión. Haga que este sea el paso inicial de
#' un flujo de trabajo bien equilibrado de  *flujo de trabajo de información
#' sobre la calidad de los datos. La salida del informe contiene varias
#' secciones para hacer todo más digerible, y estas son:
#' 
#' \describe{
#' \item{Overview}{Dimensiones de la tabla, recuento de filas duplicadas, tipos
#' de columnas y información de reproducibilidad}
#' \item{Variables}{Un resumen para cada variable de la tabla y más estadísticas
#' y resúmenes en función del tipo de variable}
#' \item{Interactions}{Un gráfico matricial que muestra las interacciones entre
#' las variables}
#' \item{Correlations}{Un conjunto de gráficos de matrices de correlación para
#' variables numéricas variables numéricas}
#' \item{Missing Values}{Una figura de resumen que muestra el grado de ausencia
#' en todas las variables}
#' \item{Sample}{Una tabla que proporciona las filas de cabeza y cola del
#' conjunto de datos}
#' }
#' 
#' El informe HTML de salida aparecerá en el visor de RStudio y también puede
#' integrarse en la salida HTML de R Markdown. Si necesita el HTML de salida
#' como una cadena, es posible obtenerlo utilizando `as.character()` (por
#' ejemplo, `scan_data(tbl = mtcars) %>% as.character()`). La cadena HTML
#' resultante es un documento HTML completo donde **Bootstrap** y **jQuery**
#' están incrustados en su interior.
#' 
#' @param tbl La tabla de entrada. Puede ser un marco de datos, un tibble, un
#'   objeto `tbl_dbi` o un objeto `tbl_spark`.
#' @param sections Las secciones a incluir en el informe finalizado de `Escaneo
#'   de Tablas`. Aquí se requiere una cadena con caracteres clave que
#'   representen los nombres de las secciones. La cadena por defecto es
#'   `"OVICMS"` donde cada letra representa las siguientes secciones en su orden
#'   por defecto: `"O"`: `"overview"`; `"V"`: `"variables"`; `"I"`:
#'   `"interactions"`; `"C"`: `"correlations"`; `"M"`: `"missing"`; y `"S"`:
#'   `"sample"`. Esta cadena puede estar compuesta por menos caracteres y el
#'   orden puede cambiarse para adaptarse al diseño deseado del informe. En el
#'   caso de los objetos `tbl_dbi` y `tbl_spark` suministrados a `tbl`, las
#'   secciones de `"interactions"` y `"correlations"` están excluidas.
#' @param navbar ¿Debe haber una barra de navegación anclada en la parte
#'   superior de la página del informe? Por defecto es `TRUE`.
#' @param width Una anchura fija opcional (en píxeles) para el informe HTML. Por
#'   defecto no se aplica ninguna anchura fija.
#' @param lang El idioma que se utilizará para el texto de la etiqueta en el
#'   informe. De forma predeterminada, `NULL` creará texto en inglés (`"en"`).
#'   Otras opciones incluyen francés (`"fr"`), alemán (`"de"`), italiano
#'   (`"it"`), español (`"es"`), portugués (`"pt"`), turco (`"tr"`), chino
#'   (`"zh"`), ruso (`"ru"`), polaco (`"pl"`), danés (`"da"`), sueco (`"sv"` ) y
#'   holandés (`"nl"`).
#' @param locale Un identificador opcional de la configuración regional que se
#'   utilizará para formatear los valores en el informe de acuerdo con las
#'   reglas de la configuración regional. Los ejemplos incluyen `"en_US"` para
#'   el inglés (Estados Unidos) y `"fr_FR"` para el francés (Francia); más
#'   sencillamente, puede ser un identificador de idioma sin designación de
#'   país, como `"es"` para español (España, igual que `"es_ES"`).
#'   
#' @examples
#' if (interactive()) {
#' 
#' # Obtener un documento HTML que describa
#' # los datos de la tabla `dplyr::storms`
#' tbl_scan <- scan_data(tbl = dplyr::storms)
#' 
#' }
#' 
#' @section Figures:
#' \if{html}{\figure{man_scan_data_1.png}{options: width=100\%}}
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-1
#' 
#' @export

# serially-----------------------------------------------------------------
#' Ejecutar varias pruebas y una validación final en serie
#'
#' @description 
#' La función de validación `serially()` permite ejecutar una serie de pruebas
#' en secuencia antes de culminar con un paso de validación final o simplemente
#' salir de la serie. Esta construcción permite realizar pruebas previas que
#' pueden tener sentido antes de un paso de validación. Por ejemplo, puede haber
#' situaciones en las que es vital comprobar el tipo de columna antes de
#' realizar una validación en la misma columna (ya que tener un tipo incorrecto
#' puede dar lugar a un error de evaluación para la validación posterior). Otro
#' flujo de trabajo en serie podría implicar tener un conjunto de comprobaciones
#' en un orden prescrito y, si todas pasan, entonces el objetivo de estas
#' pruebas se ha alcanzado (por ejemplo, comprobar si una tabla coincide con
#' otra a través de una serie de pruebas cada vez más específicas).
#' 
#' Una serie como se especifica dentro de `serially()` se compone con un listado
#' de llamadas, y nos basaríamos en las funciones de prueba (**T**) para
#' describir las pruebas y opcionalmente proporcionar una llamada final con una
#' función de validación (**V**). Se aplican las siguientes restricciones:
#' 
#' - debe haber al menos una función de prueba en la serie (**T** -> **V** es
#' buena, **V** es *no*)
#' - sólo puede haber una llamada a la función de validación, **V**; es opcional
#' pero, si se incluye, debe colocarse al final (**T** -> **T** -> **V** es
#' buena, estas secuencias son malas: (1) **T** -> **V** -> **T**, (2) **T** ->
#' **T** -> **V** -> **V**)
#' - una llamada a la función de validación (**V**), si se incluye, no debe
#' producir por sí misma múltiples pasos de validación (esto puede ocurrir
#' cuando se proporcionan múltiples `columns` o cualquier `segments`)
#' 
#' Este es un ejemplo de cómo organizar las expresiones:
#' 
#' ```
#' ~ test_col_exists(., columns = vars(count)),
#' ~ test_col_is_numeric(., columns = vars(count)),
#' ~ col_vals_gt(., columns = vars(count), value = 2)
#' ```
#' 
#' Esta serie se concentra en la columna llamada `count` y primero comprueba si
#' la columna existe, luego comprueba si esa columna es numérica, y luego
#' finalmente valida si todos los valores de la columna son mayores que `2`.
#' 
#' Tenga en cuenta que en la lista de llamadas anterior, el `.` representa la
#' tabla de destino y siempre es necesario aquí. También es importante que todas
#' las funciones `test_*()` tienen un argumento `threshold` que se establece en
#' `1` por defecto. Si necesita aumentar el valor del umbral, puede cambiarlo a
#' un valor entero diferente (como umbral absoluto de unidades de prueba que
#' fallan) o a un valor decimal entre `0` y `1` (que sirve como umbral
#' fraccionario de unidades de prueba que fallan). unidades de prueba que
#' fallan).
#'
#' @section Nombres de columnas:
#' Si se proporcionan varios nombres de columna en cualquiera de los pasos de
#' validación suministrados, el resultado será una expansión de los pasos de
#' subvalidación a ese número de nombres de columna. Aparte de los nombres de
#' columnas entre comillas y en `vars()`, las funciones de ayuda **tidyselect**
#' están disponibles para especificar columnas. Son: `starts_with()`,
#' `ends_with()`, `contains()`, `matches()` y `everything()`.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()`s en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `serially()` se representa en YAML (bajo
#' la clave de nivel superior `steps` como un miembro de la lista), la sintaxis
#' sigue de cerca la firma de la función de validación. A continuación se
#' muestra un ejemplo de cómo una llamada compleja de `serially()` como paso de
#' validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   serially(
#'     ~ col_vals_lt(., vars(a), 8),
#'     ~ col_vals_gt(., vars(c), vars(a)),
#'     ~ col_vals_not_null(., vars(b)),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
#'     label = "El paso `serially()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - serially:
#'     fns:
#'     - ~col_vals_lt(., vars(a), 8)
#'     - ~col_vals_gt(., vars(c), vars(a))
#'     - ~col_vals_not_null(., vars(b))
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `serially()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo son necesarias
#' las expresiones para los pasos de validación. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param ... Una colección de fórmulas unilaterales que consisten en llamadas a
#'   funciones `test_*()` (por ejemplo, [test_col_vals_between()], etc.)
#'   dispuestas en la secuencia del orden de interrogación previsto.
#'   Típicamente, las validaciones hasta la final tendrían algún valor de
#'   `threshold` establecido (por defecto es `1`) para el cortocircuito dentro
#'   de la serie. Una llamada a la función de validación final (por ejemplo,
#'   [col_vals_increasing()], etc.) puede insertarse opcionalmente al final de
#'   la serie, sirviendo como un paso de validación que sólo se somete a
#'   interrogación si las pruebas anteriores pasan adecuadamente. Un ejemplo de
#'   esto es `~ test_column_exists(., vars(a)), ~ col_vals_not_null(.,
#'   vars(a))`).
#' @param .list Permite el uso de una lista como alternativa de entrada a `...`.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'
#' @examples
#' # Para todos los ejemplos aquí,
#' # usaremos una tabla simple con tres
#' # columnas numéricas (`a`, `b` y `c`);
#' # esta es una tabla muy básica pero
#' # será más útil a la hora de explicar
#' # las cosas más adelante
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 2, 6),
#'     b = c(6, 4, 9),
#'     c = c(1, 2, 3)
#'   )
#'   
#' tbl
#'   
#' # A: Usando un `agent` con funciones de
#' #    validación y luego `interrogate()`
#' 
#' # La función `serially()` puede
#' # configurarse para realizar una serie
#' # de pruebas y luego realizar una
#' # validación (sólo si se superan todas
#' # las pruebas); en este caso, vamos a
#' # (1) comprobar si las columnas `a` y `b`
#' # son numéricas, (2) comprobar que ambas
#' # no tienen ningún valor `NA`, y
#' # (3) realizar una validación final que
#' # compruebe si los valores de `b` son
#' # mayores que los valores de `a`.
#' agent_1 <-
#'   create_agent(tbl = tbl) %>%
#'   serially(
#'     ~ test_col_is_numeric(., vars(a, b)),
#'     ~ test_col_vals_not_null(., vars(a, b)),
#'     ~ col_vals_gt(., vars(b), vars(a))
#'     ) %>%
#'   interrogate()
#'   
#' # Determine si esta validación no tuvo
#' # unidades de prueba fallidas (hay 4
#' # pruebas y una validación final)
#' all_passed(agent_1)
#' 
#' # Llamar a `agent` en la consola imprime
#' # el informe del agente; pero podemos
#' # obtener un objeto `gt_tbl` directamente
#' # con `get_agent_report(agent_1)`
#' 
#' # ¿Qué ocurre? Las cuatro pruebas han
#' # sido aprobadas y, por tanto, se ha
#' # producido la validación final;
#' # ¡tampoco ha habido unidades de prueba
#' # que hayan fallado!
#' 
#' # La validación final es opcional;
#' # aquí hay un agente diferente en el
#' # que sólo se realizan las pruebas en
#' # serie se realizan
#' agent_2 <-
#'   create_agent(tbl = tbl) %>%
#'   serially(
#'     ~ test_col_is_numeric(., vars(a, b)),
#'     ~ test_col_vals_not_null(., vars(a, b))
#'   ) %>%
#'   interrogate()
#'   
#' # Todo es bueno aquí también:
#' all_passed(agent_2)
#' 
#' # B: Usando la función de validación
#' #    directamente en los datos
#' #    (sin `agent`)
#' 
#' # Esta forma de utilizar las funciones
#' # de validación actúa como un filtro de
#' # datos: los datos se pasan a través,
#' # pero deben `stop()` si hay una sola
#' # unidad de prueba que falla; el
#' # comportamiento de los efectos
#' # secundarios se puede personalizar con
#' # la opción `actions`
#' tbl %>%
#'   serially(
#'     ~ test_col_is_numeric(., vars(a, b)),
#'     ~ test_col_vals_not_null(., vars(a, b)),
#'     ~ col_vals_gt(., vars(b), vars(a))
#'   )
#'
#' # C: Usando la función de expectativa
#' 
#' # Con el formulario `expect_*()`,
#' # necesitamos ser más exactos y
#' # proporcionar una columna a la vez;
#' # esto se usa principalmente en
#' # pruebas `testthat`
#' expect_serially(
#'   tbl,
#'   ~ test_col_is_numeric(., vars(a, b)),
#'   ~ test_col_vals_not_null(., vars(a, b)),
#'   ~ col_vals_gt(., vars(b), vars(a))
#' )
#' 
#' # D: Usando la función de prueba
#' 
#' # Con la forma `test_*()`, deberíamos
#' # obtener un único valor lógico devuelto
#' tbl %>%
#'   test_serially(
#'     ~ test_col_is_numeric(., vars(a, b)),
#'     ~ test_col_vals_not_null(., vars(a, b)),
#'     ~ col_vals_gt(., vars(b), vars(a))
#'   )
#'
#' @family validation functions
#' @section Function ID:
#' 2-32
#' 
#' @name serially

# set_read_fn--------------------------------------------------------------
#' Establezca una fórmula de preparación de mesa para un *agent* o *informant*
#'
#' @description
#' Una fórmula de preparación de tablas puede asociarse a un *agent* o
#' *informant* con `set_read_fn()`. En caso de que tanto un `tbl` *como un
#' `read_fn` estén asociados al objeto *agent* o *informant*, el `read_fn`
#' tendrá prioridad. Podemos especificar un valor valor para `read_fn` con una
#' expresión de fórmula RHS (por ejemplo, `~ { <código de lectura de tabla> }`).
#' La fórmula de lectura de la tabla puede eliminarse con [remove_read_fn()] o
#' reemplazada con `set_read_fn()`.
#' 
#' @param x Un objeto *agent* de la clase `ptblank_agent`, o, un objeto
#'   *informant* de clase `ptblank_informant`.
#' @param read_fn Una expresión de fórmula R (por ejemplo, `~ { <código de
#'   lectura de la tabla> }`) que se utiliza para preparar una tabla.
#'
#' @examples
#' # Establecer umbrales de fallo
#' # proporcionales a los estados
#' # `warn`, `stop` y `notify`
#' # utilizando `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Crear un objeto agente que
#' # lea en `small_table` con una
#' # fórmula de preparación de tabla;
#' # aplicar las acciones, añadir
#' # algunos pasos de validación y
#' # luego interrogar los datos
#' agent_1 <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # Cambie la fórmula de preparación
#' # de la tabla para utilizar una
#' # versión mutada de `small_table`
#' # (que elimine las filas duplicadas);
#' # a continuación, interrogue la
#' # tabla de destino de nuevo
#' agent_2 <-
#'   agent_1 %>%
#'   set_read_fn(
#'     read_fn = ~ small_table %>% dplyr::distinct()
#'   ) %>%
#'   interrogate()
#'
#' @family Object Ops
#' @section Function ID:
#' 9-6
#'
#' @export

# set_tbl------------------------------------------------------------------
#' Establecer una tabla de datos para un *agent* o *informant*
#' 
#' @description 
#' Setting a data table to an *agent* or *informant* with `set_tbl()` replaces
#' any associated table (a data frame, a tibble, objects of class `tbl_dbi` or
#' `tbl_spark`). If a data table is associated with an *agent* or *informant*
#' through the `tbl` argument *and* the same object has a table-prep formula
#' (settable in [create_agent()] and [create_informant()]'s `read_fn` argument
#' or with [set_read_fn()]), the table-prep formula will take precedence. If
#' this is undesirable, it be removed with the [remove_read_fn()] function. The
#' association to a table can be removed with with [remove_tbl()].
#'
#' @param x Un objeto *agent* de la clase `ptblank_agent`, o, un objeto
#'   *informant* de clase `ptblank_informant`.
#' @param tbl The input table for the `agent`. This can be a data frame, a
#'   tibble, a `tbl_dbi` object, or a `tbl_spark` object. Any table already
#'   associated with the *agent* or *informant* will be overwritten.
#' 
#' @examples
#' # Establecer umbrales de fallo
#' # proporcionales a los estados
#' # `warn`, `stop` y `notify`
#' # utilizando `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Crear un objeto agente que
#' # lea en `small_table` con una
#' # fórmula de preparación de tabla;
#' # aplicar las acciones, añadir
#' # algunos pasos de validación y
#' # luego interrogar los datos
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # Replace the agent's association to
#' # `small_table` with a mutated version
#' # of it (one that removes duplicate rows);
#' # then, interrogate the new target table
#' agent_2 <-
#'   agent_1 %>%
#'   set_tbl(
#'     tbl = small_table %>% dplyr::distinct()
#'   ) %>%
#'   interrogate()
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-4
#' 
#' @export

# small_table--------------------------------------------------------------
#' Una pequeña tabla que es útil para realizar pruebas
#'
#' Esta es una pequeña tabla con algunos tipos diferentes de columnas.
#' Probablemente sea útil cuando se prueban las funciones desde **pointblank**.
#' Las filas 9 y 10 son duplicados exactos. La columna `c` contiene dos valores
#' `NA`.
#'
#' @format Un tibble con 13 filas y 8 variables:
#' \describe{
#' \item{date_time}{Una columna de fecha y hora (de la clase `POSIXct`) con
#' fechas que corresponden exactamente a las de la columna `date`. Los valores
#' de tiempo son algo aleatorios, pero todos los valores de 'segundos' son
#' `00`.}
#' \item{date}{Una columna `Date` con fechas desde `2016-01-04` a `2016-01-30`.}
#' \item{a}{Una columna `integer` con valores comprendidos entre `1` y `8`.}
#' \item{b}{Una columna de `character` con valores que se adhieren a un patrón
#' común.}
#' \item{c}{Una columna `integer` con valores comprendidos entre `2` y `9`.
#' Contiene dos valores `NA`.}
#' \item{d}{Una columna numérica con valores que van desde `108` a `10000`.}
#' \item{e}{Una columna `logical`.}
#' \item{f}{Una columna `character` con valores `"low"`, `"mid"` y `"high"`}
#' }
#'
#' @examples
#' # Aquí hay un vistazo a los datos
#' # disponibles en `small_table`
#' dplyr::glimpse(small_table)
#'
#' @family Datasets
#' @section Function ID:
#' 14-1
#'

# small_table_sqlite-------------------------------------------------------
#' Una versión SQLite del conjunto de datos `small_table`
#' 
#' La función `small_table_sqlite()` crea una versión SQLite, `tbl_dbi` de la
#' tabla `small_table`. Un requisito es la disponibilidad de los paquetes
#' **DBI** y **RSQLite**. Estos paquetes se pueden instalar con
#' `install.packages("DBI")` y `install.packages("RSQLite")`.
#' 
#' @examples
#' # Utilice `small_table_sqlite()`
#' # para crear una versión SQLite
#' # de la tabla de la tabla
#' # `small_table`
#' # small_table_sqlite <- small_table_sqlite()
#' 
#' @family Datasets
#' @section Function ID:
#' 14-2
#' 
#' @export

# snip_highest-------------------------------------------------------------
#' Un `fn` para `info_snippet()`: obtiene el valor más alto de una columna
#' 
#' La función `snip_highest()` puede utilizarse como una función
#' [info_snippet()] (es decir, proporcionada a `fn`) para obtener el mayor valor
#' numérico, temporal o valor alfabético de una columna de la tabla de destino.
#' 
#' @param column El nombre de la columna que contiene los valores de destino.
#'   
#' @return Una fórmula necesaria para el argumento `fn` de [info_snippet()].
#' 
#' @examples 
#' # Generate an informant object, add
#' # a snippet with `info_snippet()`
#' # and `snip_highest()` (giving us a
#' # method to get the highest value in
#' # column `a`); define a location for
#' # the snippet result in `{ }` and
#' # then `incorporate()` the snippet
#' # into the info text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>% 
#'   info_columns(
#'     columns = "a",
#'     `Highest Value` = "Highest value is {highest_a}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "highest_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `informant` object
#' # to see the information report
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-9
#' 
#' @export

# snip_list----------------------------------------------------------------
#' Un `fn` para `info_snippet()`: obtener una lista de categorías de columnas
#' 
#' The `snip_list()` function can be used as an [info_snippet()] function (i.e.,
#' provided to `fn`) to get a catalog list from a table column. You can limit
#' the of items in that list with the `limit` value.
#' 
#' @param column El nombre de la columna que contiene los valores de destino.
#' @param limit A limit of items put into the generated list. The returned text
#'   will state the remaining number of items beyond the `limit`. By default,
#'   the limit is `5`.
#' @param sorting A keyword used to designate the type of sorting to use for the
#'   list. The three options are `"inorder"` (the default), `"infreq"`, and
#'   `"inseq"`. With `"inorder"`, distinct items are listed in the order in
#'   which they firsts appear. Using `"infreq"` orders the items by the
#'   decreasing frequency of each item. The `"inseq"` option applies an
#'   alphanumeric sorting to the distinct list items.
#' @param reverse An option to reverse the ordering of list items. By default,
#'   this is `FALSE` but using `TRUE` will reverse the items before applying the
#'   `limit`.
#' @param sep The separator to use between list items. By default, this is a
#'   comma.
#' @param and_or The type of conjunction to use between the final and
#'   penultimate list items (should the item length be below the `limit` value).
#'   If `NULL` (the default) is used, then the 'and' conjunction will be used.
#'   Alternatively, the following keywords can be used: `"and"`, `"or"`, or
#'   an empty string (for no conjunction at all).
#' @param oxford Whether to use an Oxford comma under certain conditions. By
#'   default, this is `TRUE`.
#' @param as_code Should each list item appear in a 'code font' (i.e., as
#'   monospaced text)? By default this is `TRUE`. Using `FALSE` keeps all list
#'   items in the same font as the rest of the information report.
#' @param quot_str An option for whether list items should be set in double
#'   quotes. If `NULL` (the default), the quotation marks are mainly associated
#'   with list items derived from `character` or `factor` values; numbers,
#'   dates, and logical values won't have quotation marks. We can explicitly use
#'   quotations (or not) with either `TRUE` or `FALSE` here.
#' @param lang The language to use for any joining words (from the `and_or`
#'   option) or additional words in the generated list string. By default,
#'   `NULL` will use whichever `lang` setting is available in the parent
#'   *informant* object (this is settable in the [create_informant()] `lang`
#'   argument). If specified here as an override, the language options are
#'   English (`"en"`), French (`"fr"`), German (`"de"`), Italian (`"it"`),
#'   Spanish (`"es"`), Portuguese (`"pt"`), Turkish (`"tr"`), Chinese (`"zh"`),
#'   Russian (`"ru"`), Polish (`"pl"`), Danish (`"da"`), Swedish (`"sv"`), and
#'   Dutch (`"nl"`).
#'   
#' @return Una fórmula necesaria para el argumento `fn` de [info_snippet()].
#' 
#' @examples 
#' # Generate an informant object, add
#' # a snippet with `info_snippet()`
#' # and `snip_list()` (giving us a
#' # method to get a distinct list of
#' # column values for column `f`);
#' # define a location for the snippet
#' # result in `{ }` and then
#' # `incorporate()` the snippet into
#' # the info text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>% 
#'   info_columns(
#'     columns = "f",
#'     `Items` = "This column contains {values_f}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "values_f",
#'     fn = snip_list(column = "f")
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `informant` object
#' # to see the information report
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-6
#' 
#' @export

# snip_lowest--------------------------------------------------------------
#' Un `fn` para `info_snippet()`: obtener el valor más bajo de una columna
#' 
#' The `snip_lowest()` function can be used as an [info_snippet()] function
#' (i.e., provided to `fn`) to get the lowest numerical, time value, or
#' alphabetical value from a column in the target table.
#' 
#' @param column El nombre de la columna que contiene los valores de destino.
#'   
#' @return Una fórmula necesaria para el argumento `fn` de [info_snippet()].
#' 
#' @examples 
#' # Generate an informant object, add
#' # a snippet with `info_snippet()`
#' # and `snip_lowest()` (giving us a
#' # method to get the lowest value in
#' # column `a`); define a location for
#' # the snippet result in `{ }` and
#' # then `incorporate()` the snippet
#' # into the info text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>% 
#'   info_columns(
#'     columns = "a",
#'     `Lowest Value` = "Lowest value is {lowest_a}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "lowest_a",
#'     fn = snip_lowest(column = "a")
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `informant` object
#' # to see the information report
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-8
#' 
#' @export

# snip_stats---------------------------------------------------------------
#' Un `fn` para `info_snippet()`: obtener un resumen estadístico en línea
#'
#' @description
#' The `snip_stats()` function can be used as an [info_snippet()] function
#' (i.e., provided to `fn`) to produce a five- or seven-number statistical
#' summary. This inline summary works well within a paragraph of text and can
#' help in describing the distribution of numerical values in a column.
#'
#' For a given column, three different types of inline statistical summaries can
#' be provided:
#' 
#' 1. a five-number summary (`"5num"`): minimum, Q1, median, Q3, maximum
#' 2. a seven-number summary (`"7num"`): P2, P9, Q1, median, Q3, P91, P98
#' 3. Bowley's seven-figure summary (`"bowley"`): minimum, P10, Q1, median, Q3,
#' P90, maximum
#'
#' @param column El nombre de la columna que contiene los valores de destino.
#' @param type The type of summary. By default, the `"5num"` keyword is used to
#'   generate a five-number summary. Two other options provide seven-number
#'   summaries: `"7num"` and `"bowley"`.
#'   
#' @return Una fórmula necesaria para el argumento `fn` de [info_snippet()].
#' 
#' @examples 
#' # Generate an informant object, add
#' # a snippet with `info_snippet()`
#' # and `snip_stats()` (giving us a
#' # method to get some summary stats for
#' # column `a`); define a location for
#' # the snippet result in `{ }` and
#' # then `incorporate()` the snippet
#' # into the info text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "Un ejemplo."
#'   ) %>% 
#'   info_columns(
#'     columns = "a",
#'     `Stats` = "Stats (fivenum): {stats_a}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "stats_a",
#'     fn = snip_stats(column = "a")
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `informant` object
#' # to see the information report
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-7
#' 
#' @export

# specially----------------------------------------------------------------
#' Realizar una validación especializada con una función definida por el usuario
#'
#' @description 
#' La función de validación `specially()` permite una validación personalizada
#' con una función que *usted* proporciona. La principal condición para la
#' función proporcionada es que debe devolver un vector lógico o una tabla donde
#' la columna final sea lógica. La función operará sobre el objeto tabla o, como
#' puede hacer lo que quiera, también podría operar sobre otros tipos de
#' objetos. Para ello, puede transformar la tabla de entrada en `preconditions`
#' o inyectar allí un objeto totalmente diferente. Durante la interrogación, no
#' habrá ninguna comprobación para asegurar que los datos son un objeto de
#' tabla.
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que una validación en particular requiera una
#' columna calculada, algún filtrado de filas o la adición de columnas a través
#' de una combinación, etc. Especialmente para un informe basado en *agent*,
#' esto puede ser ventajoso ya que podemos desarrollar un gran plan de
#' validación con una sola tabla de destino y realice ajustes menores en ella,
#' según sea necesario, a lo largo del camino. Dentro de `specially()`, debido a
#' que esta función es especial, no habrá comprobación interna de si la salida
#' basada en `preconditions` es una tabla.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere
#' usar el código **dplyr** ya que las declaraciones se pueden traducir a SQL
#' si es necesario (es decir, si la tabla de destino reside en una base de
#' datos). El código se proporciona más fácilmente como una fórmula **R**
#' unilateral (utilizando un `~` inicial). En la representación de la fórmula,
#' el `.` sirve como la tabla de datos de entrada que se va a transformar (por
#' ejemplo, ` ~ . %>% dplyr::mutate(col_b = col_a + 10) `). Alternativamente, se
#' podría proporcionar una función (por ejemplo, 
#' `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que es mejor producido por la función [action_levels()]. Lea la
#' documentación de esa función para obtener información sobre cómo crear
#' reacciones a niveles de falla por encima del umbral en la validación. La
#' esencia básica es que querrá al menos un nivel de umbral único (especificado
#' como la fracción de unidades de prueba fallidas o un valor absoluto), a
#' menudo utilizando el argumento `warn_at`. Esto es especialmente cierto cuando
#' `x` es un objeto de tabla porque, de lo contrario, no sucede nada. Para las
#' funciones de tipo `col_vals_*()`, usar `action_levels(warn_at = 0.25)` o
#' `action_levels(stop_at = 0.25)` son buenas opciones dependiendo de la
#' situación (la primera produce una advertencia cuando una cuarta parte de la
#' prueba total las unidades fallan, las otras `stop()`s en el mismo nivel de
#' umbral).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `specially()` se representa en YAML (bajo
#' la clave de nivel superior `steps` como un miembro de la lista), la sintaxis
#' sigue de cerca la firma de la función de validación. A continuación se
#' muestra un ejemplo de cómo una llamada compleja de `specially()` como paso de
#' validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   specially(
#'     fn = function(x) { ... },
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
#'     label = "El paso `specially()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - specially:
#'     fn: function(x) { ... }
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `specially()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos, ya que solo son necesarias
#' las expresiones para los pasos de validación. Los argumentos con valores
#' predeterminados no se escribirán en YAML cuando se use [yaml_write()] (aunque
#' es aceptable incluirlos con sus valores predeterminados al generar el YAML
#' por otros medios). También es posible obtener una vista previa de la
#' transformación de un agente a YAML sin escribir en el disco usando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param fn Una función que realiza la validación especializada de los datos.
#'   Debe devolver un vector lógico o una tabla donde la última columna es una
#'   columna lógica.
#' 
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'
#' @family validation functions
#' @section Function ID:
#' 2-33
#' 
#' @name specially

# specifications-----------------------------------------------------------
#' Una tabla que contiene datos pertenecientes a varias especificaciones
#'
#' La tabla `specifications` es útil para probar las funciones
#' [col_vals_within_spec()], [test_col_vals_within_spec()] y
#' [expect_col_vals_within_spec()]. Para cada columna, que contiene valores de
#' caracteres para diferentes especificaciones, las filas 1-5 contienen valores
#' válidos, la 6ª fila es un valor NA, y los dos últimos valores (filas 7 y 8)
#' son inválidos. Las diferentes palabras clave de especificación (`spec`) se
#' aplican a cada una de las columnas cuando al validar con cualquiera de las
#' funciones mencionadas.
#'
#' @format A tibble with 8 rows and 12 variables:
#' \describe{
#' \item{isbn_numbers}{ISBN-13 numbers; can be validated with the `"isbn"`
#' specification.}
#' \item{vin_numbers}{VIN numbers (identifiers for motor vehicles); can be
#' validated with the `"vin"` specification.}
#' \item{zip_codes}{Postal codes for the U.S.; can be validated with the
#' `"postal[USA]"` specification or its `"zip"` alias.}
#' \item{credit_card_numbers}{Credit card numbers; can be validated with the
#' `"credit_card"` specification or the `"cc"` alias.}
#' \item{iban_austria}{IBAN numbers for Austrian accounts; can be validated with
#' the `"iban[AUT]"` specification.}
#' \item{swift_numbers}{Swift-BIC numbers; can be validated with the `"swift"`
#' specification.}
#' \item{phone_numbers}{Phone numbers; can be validated with the `"phone"`
#' specification.}
#' \item{email_addresses}{Email addresses; can be validated with the `"email"`
#' specification.}
#' \item{urls}{URLs; can be validated with the  `"url"` specification.}
#' \item{ipv4_addresses}{IPv4 addresses; can be validated with the `"ipv4"`
#' specification}
#' \item{ipv6_addresses}{IPv6 addresses; can be validated with the `"ipv6"`
#' specification}
#' \item{mac_addresses}{MAC addresses; can be validated with the `"mac"`
#' specification}
#' }
#'
#' @examples
#' # He aquí un vistazo a los datos
#' # disponibles en `specifications`
#' dplyr::glimpse(specifications)
#'
#' @family Datasets
#' @section Function ID:
#' 14-3
#'

# stock_msg_body-----------------------------------------------------------
#' Proporcione componentes simples del cuerpo del mensaje de correo electrónico:
#' "body"
#' 
#' La función `stock_msg_body()` simplemente proporciona un texto de stock para
#' un mensaje de correo electrónico enviado a través de [email_blast()] u
#' obtenido como un objeto independiente a través de [email_create()].
#'
#' @return Texto adecuado para el argumento `msg_body` de [email_blast()] y
#'   [email_create()].
#' 
#' @family Emailing
#' @section Function ID:
#' 4-3
#' 
#' @export

# stock_msg_footer---------------------------------------------------------
#' Proporcione componentes simples del cuerpo del mensaje de correo electrónico:
#' "footer"
#' 
#' La función `stock_msg_footer()` simplemente proporciona un texto de stock
#' para un mensaje de correo electrónico enviado a través de [email_blast()] u
#' obtenido como un objeto independiente a través de [email_create()].
#'
#' @return Texto adecuado para el argumento `msg_footer` de [email_blast()] y
#'   [email_create()].
#' 
#' @family Emailing
#' @section Function ID:
#' 4-4
#' 
#' @export

# stop_if_not--------------------------------------------------------------
#' La próxima generación de funciones de tipo `stopifnot()`: `stop_if_not()`
#'
#' This is `stopifnot()` but with a twist: it works well as a standalone,
#' replacement for `stopifnot()` but is also customized for use in validation
#' checks in R Markdown documents where **pointblank** is loaded. Using
#' `stop_if_not()` in a code chunk where the `validate = TRUE` option is set
#' will yield the correct reporting of successes and failures whereas
#' `stopifnot()` *does not*.
#' 
#' @param ... R expressions that should each evaluate to (a logical vector of
#' all) `TRUE`.
#' 
#' @return `NULL` if all statements in `...` are `TRUE`.
#' 
#' @examples 
#' # This checks whether the number of
#' # rows in `small_table` is greater
#' # than `10`
#' stop_if_not(nrow(small_table) > 10)
#' 
#' # This will stop for sure: there
#' # isn't a `time` column in `small_table`
#' # (but there are the `date_time` and
#' # `date` columns)
#' # stop_if_not("time" %in% colnames(small_table))
#' 
#' # You're not bound to using tabular
#' # data here, any statements that
#' # evaluate to logical vectors will work
#' stop_if_not(1 < 20:25 - 18)
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-5
#' 
#' @export

# tbl_get------------------------------------------------------------------
#' Obtenga una mesa materializada a través de una tienda de mesa
#' 
#' @description 
#' The `tbl_get()` function gives us the means to materialize a table that has
#' an entry in a table store (i.e., has a table-prep formula with a unique
#' name). The table store that is used for this can be in the form of a
#' `tbl_store` object (created with the [tbl_store()] function) or an on-disk
#' YAML representation of a table store (created by using [yaml_write()] with a
#' `tbl_store` object).
#'
#' Should you want a table-prep formula from a table store to use as a value for
#' `read_fn` (in [create_agent()], [create_informant()], or [set_read_fn()]),
#' then have a look at the [tbl_source()] function.
#'
#' @param tbl The table to retrieve from a table `store`. This table could be
#'   identified by its name (e.g., `tbl = "large_table"`) or by supplying a
#'   reference using a subset (with `$`) of the `tbl_store` object (e.g., `tbl =
#'   store$large_table`). If using the latter method then nothing needs to be
#'   supplied to `store`.
#' @param store Either a table store object created by the [tbl_store()]
#'   function or a path to a table store YAML file created by [yaml_write()].
#' 
#' @return A table object.
#' 
#' @examples 
#' if (interactive()) {
#' 
#' # Define a `tbl_store` object by adding
#' # table-prep formulas in `tbl_store()`
#' tbls <- 
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     ~ db_tbl(
#'       table = "rna",
#'       dbname = "pfmegrnargs",
#'       dbtype = "postgres",
#'       host = "hh-pgsql-public.ebi.ac.uk",
#'       port = 5432,
#'       user = I("reader"),
#'       password = I("NWDMCE5xdipIjRrp")
#'     ),
#'     all_revenue ~ db_tbl(
#'       table = file_tbl(
#'         file = from_github(
#'           file = "all_revenue_large.rds",
#'           repo = "rich-iannone/intendo",
#'           subdir = "data-large"
#'         )
#'       ),
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     sml_table ~ pointblank::small_table
#'   )
#' 
#' # Once this object is available, you can
#' # check that the table of interest is
#' # produced to your specification
#' tbl_get(
#'   tbl = "small_table_duck",
#'   store = tbls
#' )
#' 
#' # An alternative method for getting the
#' # same table materialized is by using `$`
#' # to get the formula of choice from `tbls`
#' tbls$small_table_duck %>% tbl_get()
#' 
#' }
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-10
#' 
#' @export

# tbl_match----------------------------------------------------------------
#' ¿La tabla de destino coincide con una tabla de comparación?
#'
#' @description
#' La función de validación `tbl_match()`, la función de expectativa
#' `expect_tbl_match()` y la función de prueba `test_tbl_match()` comprueban si
#' la composición de la tabla de destino coincide con la de una tabla de
#' comparación. La función de validación se puede usar directamente en una tabla
#' de datos o con un objeto *agent* (técnicamente, un objeto `ptblank_agent`)
#' mientras que las funciones de expectativa y prueba solo se pueden usar con
#' una tabla de datos. Los tipos de tablas de datos que se pueden utilizar
#' incluyen marcos de datos, tibbles, tablas de base de datos (`tbl_dbi`) y
#' Spark DataFrames (`tbl_spark`). Como paso de validación o como expectativa,
#' hay una sola unidad de prueba que depende de si las dos tablas son las son
#' iguales (después de haber aplicado cualquier `preconditions`).
#' 
#' @section Preconditions:
#' Proporcionar expresiones como `preconditions` significa que **pointblank**
#' preprocesará la tabla de destino durante la interrogación como paso
#' preparatorio. Puede suceder que esta validación en particular requiera alguna
#' operación en la tabla de destino antes de que se lleve a cabo la comparación.
#' El uso de `preconditions` puede ser útil en ocasiones, ya que podemos
#' desarrollar un gran plan de validación con una sola tabla de destino y
#' realizarle pequeños ajustes, según sea necesario, a lo largo del camino.
#'
#' La mutación de la tabla está totalmente aislada en el alcance de los pasos de
#' validación en los que se utilizan las `preconditions`. Aquí se sugiere usar
#' el código **dplyr** ya que las declaraciones se pueden traducir a SQL si es
#' necesario (es decir, si la tabla de destino reside en una base de datos). El
#' código se proporciona más fácilmente como una fórmula **R** unilateral
#' (utilizando un `~` inicial). En la representación de la fórmula, el `.` sirve
#' como la tabla de datos de entrada que se va a transformar. Como alternativa,
#' se puede suministrar una función.
#' 
#' @section Segmentos:
#' Al usar el argumento `segments`, es posible definir una validación particular
#' con segmentos (o porciones de fila) de la tabla de destino. Una expresión
#' opcional o un conjunto de expresiones que sirven para segmentar la tabla de
#' destino por valores de columna. Cada expresión se puede dar de una de dos
#' maneras: (1) como nombres de columna, o (2) como una fórmula de dos lados
#' donde el LHS contiene un nombre de columna y el RHS contiene los valores de
#' columna para segmentar.
#' 
#' Como ejemplo del primer tipo de expresión que se puede utilizar,
#' `vars(a_column)` segmentará la tabla de destino en la forma en que estén
#' presentes muchos valores únicos en la columna llamada `a_column`. Esto es
#' excelente si cada valor único en una columna en particular (como diferentes
#' ubicaciones o diferentes fechas) requiere su propia validación repetida.
#' 
#' Con una fórmula, podemos ser más selectivos con los valores de columna que se
#' deben usar para la segmentación. El uso de `a_column ~ c("group_1",
#' "group_2")` intentará obtener dos segmentos donde uno es una porción de datos
#' donde el valor `"group_1"` existe en la columna llamada `"a_column"`, y el
#' otro es un segmento donde existe `"group_2"` en la misma columna. Cada grupo
#' de filas resuelto a partir de la fórmula dará como resultado un paso de
#' validación independiente.
#'
#' La segmentación siempre ocurrirá después de que se apliquen las
#' `preconditions` (es decir, declaraciones que mutan la tabla de destino), si
#' las hay. Con este tipo de combinación, es posible generar etiquetas
#' para la segmentación usando una expresión para `preconditions` y hacer
#' referencia a esas etiquetas en `segments` sin tener que generar una versión
#' separada de la tabla de destino.
#' 
#' @section Actions:
#' A menudo, querremos especificar `actions` para la validación. Este argumento,
#' presente en cada función de validación, toma un objeto de lista especialmente
#' diseñado que se produce mejor con la función [action_levels()]. Lee esa
#' función documentación para la verdad sobre cómo crear reacciones por encima
#' del umbral niveles de falla en la validación. La esencia básica es que
#' querrás al menos un nivel de umbral único (especificado como la fracción de
#' unidades de prueba falló, o un valor absoluto), a menudo usando el argumento
#' `warn_at`. Utilizando `action_levels(warn_at = 1)` o `action_levels(stop_at =
#' 1)` son buenas opciones dependiendo de la situación (el primero produce una
#' advertencia, el otro `stop()`).
#' 
#' @section Briefs:
#' ¿Quiere describir este paso de validación con algún detalle? Tenga en cuenta
#' que esto sólo es útil si `x` es un objeto *agent*. Si ese es el caso, use un
#' texto `brief` que se ajuste a la situación. No se preocupe si no quiere
#' hacerlo. Un *autobrief* se activa cuando `brief = NULL` y el texto luego se
#' generará automáticamente.
#' 
#' @section YAML:
#' Se puede escribir un agente **pointblank** en YAML con [yaml_write()] y el
#' YAML resultante se puede usar para regenerar un agente (con
#' [yaml_read_agent()]) o interrogar la tabla de destino (a través de
#' [yaml_agent_interrogate()]). Cuando `tbl_match()` se representa en YAML (bajo
#' la clave de nivel superior `steps` como un miembro de la lista), la sintaxis
#' sigue de cerca la firma de la función de validación. A continuación se
#' muestra un ejemplo de cómo una llamada compleja de `tbl_match()` como paso de
#' validación se expresa en código R y en la representación YAML
#' correspondiente.
#' 
#' ```
#' # Código R
#' agent %>% 
#'   tbl_match(
#'     tbl_compare = ~ file_tbl(
#'       file = from_github(
#'         file = "all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       ),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "El paso `tbl_match()`.",
#'     active = FALSE
#'   )
#' 
#' # Representación YAML
#' steps:
#' - tbl_match:
#'     tbl_compare: ~ file_tbl(
#'       file = from_github(
#'         file = "all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       )
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: El paso `tbl_match()`.
#'     active: false
#' ```
#' 
#' En la práctica, ambos serán a menudo más cortos. Los argumentos con valores
#' por defecto no se escribirán en YAML cuando se utilice [yaml_write()] (aunque
#' es aceptable incluirlos con su valor por defecto al generar el YAML por otros
#' medios). También es posible previsualizar la transformación de un agente a
#' YAML sin necesidad de escribirlo en el disco, utilizando la función
#' [yaml_agent_string()].
#'
#' @inheritParams col_vals_gt
#' @param tbl_compare Una tabla para comparar con la tabla de destino. Esto
#'   puede ser un objeto de tabla, una fórmula de preparación de tabla. Puede
#'   ser un objeto de tabla como un marco de datos, un tibble, un objeto
#'   `tbl_dbi` o un objeto `tbl_spark`. Como alternativa, se puede utilizar una
#'   fórmula de preparación de tabla (`~ <código de lectura de tabla>`) o una
#'   función (`function() <código de lectura de tabla>`) para leer perezosamente
#'   la tabla en el momento de la interrogación.
#'   
#' @return Para la función de validación, el valor de retorno es un objeto
#'   `ptblank_agent` o un objeto de tabla (dependiendo de si se pasó un objeto
#'   *agent* o una tabla a `x`). La función de expectativa devuelve
#'   invisiblemente su entrada pero, en el contexto de los datos de prueba, la
#'   función se llama principalmente por sus posibles efectos secundarios (por
#'   ejemplo, falla de señalización). La función de prueba devuelve un valor
#'   lógico.
#'   
#' @examples
#' # Cree una tabla simple con tres
#' # columnas y cuatro filas de valores
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5),
#'     b = c(7, 1, 0, 0),
#'     c = c(1, 1, 1, 3)
#'   )
#'
#' # Crear una segunda tabla que sea
#' # igual a `tbl`.
#' tbl_2 <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5),
#'     b = c(7, 1, 0, 0),
#'     c = c(1, 1, 1, 3)
#'   )
#' 
#' # Validar que la tabla de destino
#' # (`tbl`) y la tabla de comparación
#' # (`tbl_2`) son equivalentes en
#' # términos de contenido
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   tbl_match(tbl_compare = tbl_2) %>%
#'   interrogate()
#' 
#' # Determine si esta validación
#' # pasó usando `all_passed()`
#' all_passed(agent)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-32
#' 
#' @name tbl_match

# tbl_source---------------------------------------------------------------
#' Obtenga una fórmula de preparación de mesa en una tienda de mesa
#' 
#' @description
#' La función `tbl_source()` proporciona un medio conveniente para acceder a una
#' fórmula de preparación de tablas desde un objeto `tbl_store` o un archivo
#' YAML de almacenamiento de tablas (que puede ser creado con la función
#' [yaml_write()]). Una llamada a `tbl_source()` es más útil como entrada al
#' argumento `read_fn` de [create_agent()], [create_informant()], o
#' [set_read_fn()].
#'
#' Si necesita obtener la tabla propiamente dicha (que se genera a través de la
#' fórmula fórmula de preparación de la tabla), se debe utilizar la función
#' [tbl_get()] para ello.
#' 
#' @param tbl El nombre de la tabla asociado a una fórmula de preparación de la
#'   tabla. Forma parte de la tabla `store`. Esta tabla puede identificarse por
#'   su nombre (por ejemplo, `tbl = "large_table"`) o proporcionando una
#'   referencia utilizando un subconjunto (con `$`) del objeto `tbl_store` (por
#'   ejemplo, `tbl = store$large_table`). Si se utiliza este último método, no
#'   es necesario suministrar nada a `store`.
#' @param store O bien un objeto de almacén de tablas creado por la función
#'   [tbl_store()] o una ruta a un archivo YAML del almacén de tablas creado por
#'   [yaml_write()].
#' 
#' @return Una fórmula de preparación de la tabla.
#' 
#' @examples 
#' if (interactive()) {
#' 
#' # Vamos a crear un objeto `tbl_store`
#' # dándole dos fórmulas de preparación
#' # de la tabla a `tbl_store()`
#' tbls <- 
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     sml_table ~ pointblank::small_table
#'   )
#' 
#' # Podemos pasar una fórmula de
#' # preparación de la tabla a
#' # `create_agent()` e interrogar la
#' # tabla poco después
#' agent <- 
#'   create_agent(
#'     read_fn = ~ tbl_source("sml_table", tbls),
#'     label = "An example that uses a table store.",
#'     actions = action_levels(warn_at = 0.10)
#'   ) %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   interrogate()
#'
#' # Tanto el objeto `tbl_store` como el
#' # objeto `agent` pueden ser transformados
#' # a YAML con la función `yaml_write()`.
#' 
#' # Esto escribe el archivo `tbl_store.yml`
#' # por defecto (pero un nombre diferente
#' # podría usarse)
#' yaml_write(tbls)
#' 
#' # Modifiquemos el `read_fn` del agente
#' # para que apunte a la representación
#' # YAML del `tbl_store`.
#' agent <-
#'   agent %>% 
#'   set_read_fn(
#'     ~ tbl_source(
#'         tbl = "sml_table",
#'         store = "tbl_store.yml"
#'       )
#'   )
#' 
#' # Entonces podemos escribir el agente
#' # en un archivo YAML (escribe en
#' # `agent-sml_table.yml` por defecto)
#' yaml_write(agent)
#' 
#' # Ahora que ambos están en este
#' # formato en el disco se puede hacer
#' # una interrogación accediendo a el
#' # agente YAML
#' agent <-
#'   yaml_agent_interrogate(
#'     filename = "agent-sml_table.yml"
#'   )
#' 
#' }
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-9
#' 
#' @export

# tbl_store----------------------------------------------------------------
#' Definir un almacén de tablas con fórmulas de preparación de tablas: un "table
#' store"
#' 
#' @description 
#' Puede ser útil configurar todas las fuentes de datos que necesites y sólo
#' extraerlas cuando sea necesario. Esta configuración por adelantado con
#' `tbl_store()` nos permite definir los métodos para obtener datos tabulares de
#' fuentes mixtas (por ejemplo, tablas de bases de datos, tablas generadas a
#' partir de archivos planos, etc.) y proporcionar nombres para estos
#' procedimientos de preparación de datos. Entonces tenemos una forma
#' conveniente de acceder a las tablas materializadas con [tbl_get()], o, a las
#' fórmulas de preparación de tablas con [tbl_source()]. Las fórmulas table-prep
#' pueden ser tan simples como obtener una tabla de una ubicación, o, puede
#' involucrar tanta mutación como sea necesaria (imagine obtener varias
#' variaciones mutadas de la misma tabla fuente, generar una tabla desde
#' múltiples fuentes, o pre-filtrar una tabla de la base de datos de acuerdo al
#' tiempo del sistema). Otro aspecto agradable de organizar las fórmulas de
#' preparación de tablas en un solo objeto es suministrarlo al argumento
#' `read_fn` de [create_agent()] o [create_informant()] mediante la notación `$`
#' (por ejemplo, `create_agent(read_fn = <tbl_store>$<nombre>)`) o con
#' [tbl_source()] (por ejemplo, `create_agent(read_fn = ~ tbl_source("<nombre>",
#' <tbl_store>))`).
#' 
#' @section YAML:
#' Un **pointblank** `tbl_store` se puede escribir en YAML con [yaml_write()] y
#' el YAML resultante se puede utilizar de varias maneras. El escenario ideal es
#' tener agentes e informantes pointblank también en forma YAML. De esta manera
#' el agente y el informante pueden referirse a la tabla de almacenamiento YAML
#' (a través de [tbl_source()]), y, el procesamiento de ambos agentes e
#' informantes puede realizarse con [yaml_agent_interrogate()] y
#' [yaml_informant_incorporate()]. Con el siguiente código de R, se genera un
#' almacén de tablas con dos fórmulas de preparación de tablas y se escribe en
#' YAML (si no se da un nombre de archivo, el YAML se escribe en
#' `"tbl_store.yml"`).
#' 
#' ```
#' # Código R para generar el archivo "tbl_store.yml"
#' tbl_store(
#'   tbl_duckdb ~ db_tbl(small_table, dbname = ":memory:", dbtype = "duckdb"),
#'   sml_table_high ~ small_table %>% dplyr::filter(f == "high")
#' ) %>%
#'   yaml_write()
#' 
#' # Representación YAML ("tbl_store.yml")
#' tbls:
#'   tbl_duckdb: ~ db_tbl(small_table, dbname = ":memory:", dbtype = "duckdb")
#'   sml_table_high: ~ small_table %>% dplyr::filter(f == "high")
#' ```
#' 
#' Esto es útil cuando se quiere obtener tiradas frescas de datos preparados de
#' una fuente materializada en una sesión de R (con la función [tbl_get()]. Por
#' ejemplo, la tabla `sml_table_high` puede obtenerse utilizando
#' `tbl_get("sml_table_high", "tbl_store.yml")`. Para conseguir que un agente
#' compruebe estos datos preparados periódicamente, será útil el siguiente
#' ejemplo con [tbl_source()]:
#' 
#' ```
#' # Generar un objeto agente que compruebe
#' # `sml_table_high`, escribirlo en YAML
#' create_agent(
#'   read_fn = ~ tbl_source("sml_table_high", "tbl_store.yml"),
#'   label = "An example that uses a table store.",
#'   actions = action_levels(warn_at = 0.10)
#' ) %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   write_yaml()
#'   
#' # Representación YAML ("agent-sml_table_high.yml")
#' read_fn: ~ tbl_source("sml_table_high", "tbl_store.yml")
#' tbl_name: sml_table_high
#' label: An example that uses a table store.
#' actions:
#'   warn_fraction: 0.1
#' locale: en
#' steps:
#'   - col_exists:
#'     columns: vars(date, date_time)
#' ```
#' 
#' Ahora, siempre que la tabla `sml_table_high` necesite ser validada, se puede
#' hacer con [yaml_agent_interrogate()] (por ejemplo,
#' `yaml_agent_interrogate("agent-sml_table_high.yml")`).
#' 
#' @param ... Expresiones que contienen fórmulas de preparación de tablas y
#'   nombres de tablas para la recuperación de datos. Deben utilizarse fórmulas
#'   de dos lados (por ejemplo, `<LHS> ~ <RHS>`), donde el lado izquierdo es un
#'   nombre dado y el derecho es la parte que se utiliza para obtener la tabla.
#' @param .list Permite el uso de una lista como alternativa de entrada a `...`.
#' 
#' @return Un objeto `tbl_store` que contiene fórmulas de preparación de tablas.
#' 
#' @examples 
#' if (interactive()) {
#' 
#' # Definir un objeto `tbl_store` añadiendo
#' # fórmulas de preparación de tablas
#' # dentro de la `tbl_store()` llamada
#' tbls <- 
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     ~ db_tbl(
#'       table = "rna",
#'       dbname = "pfmegrnargs",
#'       dbtype = "postgres",
#'       host = "hh-pgsql-public.ebi.ac.uk",
#'       port = 5432,
#'       user = I("reader"),
#'       password = I("NWDMCE5xdipIjRrp")
#'     ),
#'     all_revenue ~ db_tbl(
#'       table = file_tbl(
#'         file = from_github(
#'           file = "all_revenue_large.rds",
#'           repo = "rich-iannone/intendo",
#'           subdir = "data-large"
#'         )
#'       ),
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     sml_table ~ pointblank::small_table
#'   )
#' 
#' # Una vez que este objeto está disponible,
#' # se puede comprobar que la tabla de
#' # interés se produce a su especificación
#' # con la función `tbl_get()`.
#' tbl_get(
#'   tbl = "small_table_duck",
#'   store = tbls
#' )
#' 
#' # Otra forma más sencilla de obtener la
#' # misma tabla materializada es utilizando
#' # `$` para obtener la entrada de
#' # elección para `tbl_get()`.
#' tbls$small_table_duck %>% tbl_get()
#' 
#' # La creación de un agente es fácil
#' # cuando todas las fórmulas de
#' # preparación de tablas están
#' # encapsuladas en un objeto `tbl_store`;
#' # utilice la notación `$` para pasar el
#' # procedimiento apropiado para leer una
#' # tabla al argumento `read_fn`
#' agent_1 <-
#'   create_agent(
#'     read_fn = tbls$small_table_duck
#'   )
#'   
#' # Existen otras formas de utilizar el
#' # almacén de tablas para asignar una
#' # tabla de destino a un agente, como
#' # el uso de la función `tbl_source()`
#' agent_2 <-
#'   create_agent(
#'     read_fn = ~ tbl_source(
#'       tbl = "small_table_duck",
#'       store = tbls
#'       )
#'   )
#' 
#' # El almacén de tablas puede ser
#' # trasladado a YAML con `yaml_write`
#' # y la llamada `tbl_source()` podría
#' # entonces referirse a ese almacén
#' # de tablas en disco; hagamos esa
#' # conversión a YAML
#' yaml_write(tbls)
#' 
#' # Lo anterior escribe el archivo
#' # `tbl_store.yml` (al no
#' # proporcionar un `filename` se
#' # elige este nombre de archivo por
#' # defecto); a continuación, modifica
#' # el `tbl_source()` para que `store`
#' # haga referencia al archivo YAML 
#' agent_3 <-
#'   create_agent(
#'     read_fn = ~ tbl_source(
#'       tbl = "small_table_duck",
#'       store = "tbl_store.yml"
#'     )
#'   )
#' 
#' }
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-8
#' 
#' @export

# tt_string_info-----------------------------------------------------------
#' Table Transformer: obtenga una tabla de resumen para columnas de cadena
#' 
#' @description
#' With any table object, you can produce a summary table that is scoped to
#' string-based columns. The output summary table will have a leading column
#' called `".param."` with labels for each of the three rows, each corresponding
#' to the following pieces of information pertaining to string length:
#'
#' 1. Mean String Length (`"length_mean"`)
#' 2. Minimum String Length (`"length_min"`)
#' 3. Maximum String Length (`"length_max"`)
#'
#' Only string data from the input table will generate columns in the output
#' table. Column names from the input will be used in the output, preserving
#' order as well.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return Un objeto `tibble`.
#' 
#' @examples 
#' # Get string information for the
#' # string-based columns in the
#' # `game_revenue` dataset
#' tt_string_info(game_revenue)
#' 
#' # Ensure that `player_id` and
#' # `session_id` values always have
#' # the same number of characters
#' # throughout the table
#' tt_string_info(game_revenue) %>%
#'   col_vals_equal(
#'     columns = vars(player_id),
#'     value = 15
#'   ) %>%
#'   col_vals_equal(
#'     columns = vars(session_id),
#'     value = 24
#'   )
#' 
#' # Check that the maximum string
#' # length in column `f` of the
#' # `small_table` dataset is no
#' # greater than `4`
#' tt_string_info(small_table) %>%
#'   test_col_vals_lte(
#'     columns = vars(f),
#'     value = 4
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-2
#' 
#' @export

# tt_summary_stats---------------------------------------------------------
#' Table Transformer: obtener una tabla de estadísticas de resumen para columnas
#' numéricas
#' 
#' @description
#' With any table object, you can produce a summary table that is scoped to the
#' numeric column values. The output summary table will have a leading column
#' called `".param."` with labels for each of the nine rows, each corresponding
#' to the following summary statistics:
#' 
#' 1. Minimum (`"min"`)
#' 2. 5th Percentile (`"p05"`)
#' 3. 1st Quartile (`"q_1"`)
#' 4. Median (`"med"`)
#' 5. 3rd Quartile (`"q_3"`)
#' 6. 95th Percentile (`"p95"`)
#' 7. Maximum (`"max"`)
#' 8. Interquartile Range (`"iqr"`)
#' 9. Range (`"range"`)
#' 
#' Only numerical data from the input table will generate columns in the output
#' table. Column names from the input will be used in the output, preserving
#' order as well.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return Un objeto `tibble`.
#' 
#' @examples 
#' # Get summary statistics for the
#' # `game_revenue` dataset that is
#' # included in the package
#' tt_summary_stats(game_revenue)
#' 
#' # Ensure that the maximum revenue
#' # for individual purchases in the
#' # `game_revenue` table is less than
#' # $150
#' tt_summary_stats(game_revenue) %>%
#'   col_vals_lt(
#'     columns = vars(item_revenue),
#'     value = 150,
#'     segments = .param. ~ "max"
#'   )
#' 
#' # For in-app purchases in the
#' # `game_revenue` table, check that
#' # median revenue is somewhere
#' # between $8 and $12
#' game_revenue %>% 
#'   dplyr::filter(item_type == "iap") %>%
#'   tt_summary_stats() %>%
#'   col_vals_between(
#'     columns = vars(item_revenue),
#'     left = 8, right = 12,
#'     segments = .param. ~ "med"
#'   )
#'
#' # While performing validations of the
#' # `game_revenue` table with an agent
#' # we can include the same revenue
#' # check by using `tt_summary_stats()`
#' # in the `preconditions` argument (this
#' # will transform the target table for
#' # the validation step); we also need
#' # to get just a segment of that table
#' # (the row with the median values)
#' agent <- 
#'   create_agent(
#'     read_fn = ~ game_revenue,
#'     tbl_name = "game_revenue",
#'     label = "Un ejemplo.",
#'     actions = action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'       notify_at = 0.35
#'     )
#'   ) %>%
#'   rows_complete() %>%
#'   rows_distinct() %>%
#'   col_vals_between(
#'     columns = vars(item_revenue),
#'     left = 8, right = 12,
#'     preconditions = ~ . %>%
#'       dplyr::filter(item_type == "iap") %>%
#'       tt_summary_stats(),
#'     segments = .param. ~ "med"
#'   ) %>%
#'   interrogate()
#' 
#' # This should all pass but let's check:
#' all_passed(agent)
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-1
#' 
#' @export

# tt_tbl_colnames----------------------------------------------------------
#' Table Transformer: obtener los nombres de las columnas de una tabla
#' 
#' @description
#' With any table object, you can produce a summary table that contains table's
#' column names. The output summary table will have two columns and as many rows
#' as there are columns in the input table. The first column is the `".param."`
#' column, which is an integer-based column containing the indices of the
#' columns from the input table. The second column, `"value"`, contains the
#' column names from the input table.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return Un objeto `tibble`.
#' 
#' @examples
#' # Get the column names of the
#' # `game_revenue` dataset that's
#' # included in the package
#' tt_tbl_colnames(game_revenue)
#' 
#' # This output table is useful when
#' # you want to validate the
#' # column names of the table; here,
#' # we check that `game_revenue` has
#' # certain column names present
#' tt_tbl_colnames(game_revenue) %>%
#'   test_col_vals_make_subset(
#'     columns = vars(value),
#'     set = c("acquisition", "country")
#'   )
#' 
#' # We can check to see whether the
#' # column names in the `specifications`
#' # table are all less than 15
#' # characters in length
#' specifications %>%
#'   tt_tbl_colnames() %>%
#'   tt_string_info() %>%
#'   test_col_vals_lt(
#'     columns = vars(value),
#'     value = 15
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-4
#' 
#' @export

# tt_tbl_dims--------------------------------------------------------------
#' Table Transformer: obtener las dimensiones de una mesa
#' 
#' @description
#' With any table object, you can produce a summary table that contains nothing
#' more than the table's dimensions: the number of rows and the number of
#' columns. The output summary table will have two columns and two rows. The
#' first is the `".param."` column with the labels `"rows"` and `"columns"`; the
#' second column, `"value"`, contains the row and column counts.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return Un objeto `tibble`.
#' 
#' @examples
#' # Get the dimensions of the
#' # `game_revenue` dataset that's
#' # included in the package
#' tt_tbl_dims(game_revenue)
#' 
#' # This output table is useful when
#' # you want to validate the
#' # dimensions of the table; here,
#' # we check that `game_revenue` has
#' # at least 1500 rows
#' tt_tbl_dims(game_revenue) %>%
#'   dplyr::filter(.param. == "rows") %>%
#'   test_col_vals_gt(
#'     columns = vars(value),
#'     value = 1500
#'   )
#' 
#' # We can check `small_table` for
#' # an exact number of columns (`8`)
#' tt_tbl_dims(small_table) %>%
#'   dplyr::filter(.param. == "columns") %>%
#'   test_col_vals_equal(
#'     columns = vars(value),
#'     value = 8
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-3
#' 
#' @export

# tt_time_shift------------------------------------------------------------
#' Table Transformer: cambiar los tiempos de una mesa
#' 
#' @description
#' With any table object containing date or date-time columns, these values can
#' be precisely shifted with `tt_time_shift()` and specification of the time
#' shift. We can either provide a string with the time shift components and the
#' shift direction (like `"-4y 10d"`) or a `difftime` object (which can be
#' created via **lubridate** expressions or by using the [base::difftime()]
#' function).
#' 
#' @details 
#' The `time_shift` specification cannot have a higher time granularity than the
#' least granular time column in the input table. Put in simpler terms, if there
#' are any date-based based columns (or just a single date-based column) then
#' the time shifting can only be in terms of years, months, and days. Using a
#' `time_shift` specification of `"20d 6H"` in the presence of any dates will
#' result in a truncation to `"20d"`. Similarly, a `difftime` object will be
#' altered in the same circumstances, however, the object will resolved to an
#' exact number of days through rounding.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' @param time_shift Either a character-based representation that specifies the
#'   time difference by which all time values in time-based columns will be
#'   shifted, or, a `difftime` object. The character string is constructed in
#'   the format `"0y 0m 0d 0H 0M 0S"` and individual time components can be
#'   omitted (i.e., `"1y 5d"` is a valid specification of shifting time values
#'   ahead one year and five days). Adding a `"-"` at the beginning of the
#'   string (e.g., `"-2y"`) will shift time values back.
#' 
#' @return A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
#'   depending on what was provided as `tbl`.
#' 
#' @examples
#' # With the `game_revenue` dataset,
#' # which has entries in the first
#' # 21 days of 2015, move all of the
#' # date and date-time values to the
#' # beginning of 2021
#' tt_time_shift(
#'   tbl = game_revenue,
#'   time_shift = "6y"
#' )
#' 
#' # Keeping only the `date_time` and
#' # `a`-`f` columns of `small_table`,
#' # shift the times back 2 days and
#' # 12 hours
#' small_table %>%
#'   dplyr::select(-date) %>%
#'   tt_time_shift("-2d 12H")
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-5
#' 
#' @export

# tt_time_slice------------------------------------------------------------
#' Table Transformer: cortar una tabla con un punto de corte en una columna de
#' tiempo
#' 
#' @description
#' With any table object containing date, date-time columns, or a mixture
#' thereof, any one of those columns can be used to effectively slice the data
#' table in two with a `slice_point`: and you get to choose which of those
#' slices you want to keep. The slice point can be defined in several ways. One
#' method involves using a decimal value between `0` and `1`, which defines the
#' slice point as the time instant somewhere between the earliest time value (at
#' `0`) and the latest time value (at `1`). Another way of defining the slice
#' point is by supplying a time value, and the following input types are
#' accepted: (1) an ISO 8601 formatted time string (as a date or a date-time),
#' (2) a `POSIXct` time, or (3) a `Date` object.
#' 
#' @details 
#' There is the option to `arrange` the table by the date or date-time values in
#' the `time_column`. This ordering is always done in an ascending manner. Any
#' `NA`/`NULL` values in the `time_column` will result in the corresponding rows
#' can being removed (no matter which slice is retained).
#'  
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' @param time_column The time-based column that will be used as a basis for the
#'   slicing. If no time column is provided then the first one found will be
#'   used.
#' @param slice_point The location on the `time_column` where the slicing will
#'   occur. This can either be a decimal value from `0` to `1`, an ISO 8601
#'   formatted time string (as a date or a date-time), a `POSIXct` time, or a
#'   `Date` object.
#' @param keep Which slice should be kept? The `"left"` side (the default)
#'   contains data rows that are earlier than the `slice_point` and the
#'   `"right"` side will have rows that are later.
#' @param arrange Should the slice be arranged by the `time_column`? This may be
#'   useful if the input `tbl` isn't ordered by the `time_column`. By default,
#'   this is `FALSE` and the original ordering is retained.
#' 
#' @return A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
#'   depending on what was provided as `tbl`.
#' 
#' @examples
#' # With the `game_revenue` dataset,
#' # which has entries in the first
#' # 21 days of 2015, elect to get all
#' # of the records where the `time`
#' # values are strictly for the first
#' # 15 days of 2015
#' tt_time_slice(
#'   tbl = game_revenue,
#'   time_column = "time",
#'   slice_point = "2015-01-16"
#' )
#' 
#' # Omit the first 25% of records
#' # from `small_table` on the basis
#' # of a timeline that begins at 
#' # `2016-01-04 11:00:00` and
#' # ends at `2016-01-30 11:23:00`
#' small_table %>%
#'   tt_time_slice(
#'     slice_point = 0.25,
#'     keep = "right"
#'   )
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-6
#' 
#' @export

# validate_rmd-------------------------------------------------------------
#' Modificar las opciones de prueba de validación **pointblank** dentro de los
#' documentos de R Markdown
#' 
#' @description 
#' Using **pointblank** in an R Markdown workflow is enabled by default once the
#' **pointblank** library is loaded. The framework allows for validation testing
#' within specialized validation code chunks where the `validate = TRUE` option
#' is set. Using **pointblank** validation functions on data in these
#' marked code chunks will flag overall failure if the stop threshold is
#' exceeded anywhere. All errors are reported in the validation code chunk after
#' rendering the document to HTML, where green or red status buttons indicate
#' whether all validations succeeded or failures occurred. Clicking any such
#' button reveals the otherwise hidden validation statements and their error
#' messages (if any). While the framework for such testing is set up by default,
#' the `validate_rmd()` function offers an opportunity to set UI and logging
#' options.
#'
#' @param summary If `TRUE` (the default), then there will be a leading summary
#'   of all validations in the rendered R Markdown document. With `FALSE`, this
#'   element is not shown.
#' @param log_to_file An option to log errors to a text file. By default, no
#'   logging is done but `TRUE` will write log entries to
#'   `"validation_errors.log"` in the working directory. To both enable logging
#'   and to specify a file name, include a path to a log file of the desired
#'   name.
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-4
#' 
#' @export

# write_testthat_file------------------------------------------------------
#' Transforma un agente **pointblank** en un **testthat** archivo de prueba
#' 
#' @description
#' Con un objeto **pointblank** *agent*, podemos escribir un archivo de prueba
#' **testthat** y optar por colocarlo en `testthat/tests` si está disponible en
#' la ruta del proyecto (también podemos especificar una ruta alternativa). Esto
#' funciona transformando los pasos de validación en una serie de llamadas a
#' `expect_*()` dentro de declaraciones individuales [testthat::test_that()].
#' 
#' Un requisito indispensable para utilizar `write_testthat_file()` en un agente
#' es la presencia de una `read_fn`, que es una función que se invoca para
#' obtener la tabla de destino. La sentencia `read_fn` se colocará en la parte
#' superior del archivo de prueba **testthat** para que la tabla de destino esté
#' disponible para cada una de las sentencias [testthat::test_that()] que le
#' siguen. Si un objeto *agent* no tiene una `read_fn` puede añadirse a través
#' de [set_read_fn()].
#' 
#' Los umbrales se obtendrán a partir de los aplicados para el estado `stop`.
#' Esto puede configurarse para un objeto *agente** en blanco pasando un objeto
#' `action_levels` al argumento `actions` de [create_agent()] o el mismo
#' argumento de cualquier función de validación incluida. Si los umbrales `stop`
#' no están disponibles, se utilizará un valor de umbral de `1` para cada
#' sentencia `expect_*()` generada en el archivo de prueba **testthat**
#' resultante.
#' 
#' No es necesario que el objeto **agent** se someta primero a una interrogación
#' con [interrogate()]. Sin embargo, puede ser útil como prueba de
#' funcionamiento realizar interactivamente una interrogación en los datos de
#' destino antes de generar el archivo de prueba **testthat**.
#' 
#' @details 
#' Las pruebas de los pasos de validación inactivos se omitirán con un mensaje
#' claro que indique que el motivo de la omisión se debe a que la prueba no está
#' activa. Cualquier paso de validación inactivo puede ser forzado a un estado
#' activo utilizando la función [activate_steps()] en un objeto *agent* (lo
#' contrario es posible con la función [deactivate_steps()]).
#' 
#' El paquete **testthat** viene con una serie de funciones `skip_on_*()` que
#' convenientemente hacen que el archivo de prueba se salte por completo si se
#' cumplen ciertas condiciones. Podemos establecer rápidamente cualquier número
#' de ellas en la parte superior del archivo de prueba **testthat**
#' suministrando palabras clave como un vector a la opción `skips` de
#' `write_testthat_file()`. Por ejemplo, si establecemos
#' `skips = c("cran", "windows)` añadiremos el código `skip_on_cran()` y
#' `skip_on_os("windows")`, lo que significa que el archivo de prueba generado
#' no se ejecutará en un sistema CRAN o si el sistema operativo es Windows.
#' 
#' Este es un ejemplo de la salida del archivo de prueba **testthat**:
#' 
#' `test-small_table.R`
#' ```
#' # Generated by pointblank
#' 
#' tbl <- small_table
#' 
#' test_that("column `date_time` exists", {
#'   
#'   expect_col_exists(
#'     tbl,
#'     columns = vars(date_time),
#'     threshold = 1
#'   ) 
#' })
#' 
#' test_that("values in `c` should be <= `5`", {
#'   
#'   expect_col_vals_lte(
#'     tbl,
#'     columns = vars(c),
#'     value = 5,
#'     threshold = 0.25
#'   ) 
#' })
#' 
#' ```
#' 
#' Esto fue generado por el siguiente conjunto de declaraciones:
#' 
#' ```
#' library(pointblank)
#' 
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     actions = action_levels(stop_at = 0.25)
#'   ) %>%
#'   col_exists(vars(date_time)) %>%
#'   col_vals_lte(vars(c), value = 5)
#'   
#' write_testthat_file(
#'   agent = agent,
#'   name = "small_table",
#'   path = "."
#' )
#' ```
#' 
#' @param agent Un objeto de agente de clase `ptblank_agent`.
#' @param name Un nombre opcional para el archivo de prueba **testhat**. Debe
#'   ser un nombre sin extensión y sin el texto inicial `"test-"`. Si no se
#'   suministra nada, el nombre se derivará del `tbl_name` en el objeto agente.
#'   Si no está presente, se utilizará un nombre genérico.
#' @param path Aquí se puede especificar una ruta si no se debe intentar colocar
#'   el archivo en `testthat/tests`.
#' @param overwrite ¿Debe sobrescribirse un archivo **testthat** del mismo
#'   nombre? Por defecto, esto es `FALSE`.
#' @param skips Se trata de un vector opcional de palabras clave de omisión de
#'   pruebas modelado a partir de las funciones **testthat** `skip_on_*()`. Las
#'   siguientes palabras clave pueden utilizarse para incluir declaraciones
#'   `skip_on_*()`: `"cran"` ([testthat::skip_on_cran()]), `"travis"`
#'   ([testthat::skip_on_travis()]), `"appveyor"`
#'   ([testthat::skip_on_appveyor()]), `"ci"` ([testthat::skip_on_ci()]),
#'   `"covr"` ([testthat::skip_on_covr()]), `"bioc"`
#'   ([testthat::skip_on_bioc()]). Existen palabras clave para omitir pruebas en
#'   determinados sistemas operativos y todas ellas insertarán una llamada
#'   específica [testthat::skip_on_os()]. Estas son `"windows"`
#'   (`skip_on_os("windows")`), `"mac"` (`skip_on_os("mac")`), `"linux"`
#'   (`skip_on_os("linux")`) y `"solaris"` (`skip_on_os("solaris")`). Estas
#'   declaraciones se colocarán en la parte superior del archivo de prueba
#'   **testthat** generado.
#' @param quiet ¿La función no debe informar cuando se escribe el archivo? En
#'   por defecto es `FALSE`.
#'   
#' @return Devuelve invisiblemente `TRUE` si el archivo **testthat** ha sido
#'   escrito.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # Creating an `action_levels` object is a
#' # common workflow step when creating a
#' # pointblank agent; we designate failure
#' # thresholds to the `warn`, `stop`, and
#' # `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # A pointblank `agent` object is now
#' # created and the `al` object is provided
#' # to the agent; the static thresholds
#' # provided by `al` make reports a bit
#' # more useful after interrogation
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     label = "Un ejemplo.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5) %>%
#'   interrogate()
#' 
#' # This agent and all of the checks can
#' # be transformed into a testthat file
#' # with `write_testthat_file()`; the `stop`
#' # thresholds will be ported over
#' write_testthat_file(
#'   agent = agent,
#'   name = "small_table",
#'   path = "."
#' )
#' 
#' # The above code will generate a file with
#' # the name `test-small_table.R`; the path
#' # was specified with `"."` but, by default,
#' # the function will place the file in the
#' # `tests/testthat` folder if it's available
#' 
#' # An agent on disk as a YAML file can be
#' # made into a testthat file; the
#' # 'agent-small_table.yml' file is
#' # available in the package through
#' # `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#'
#' # Writing the testthat file into the
#' # working directory is much the same
#' # as before but we're reading the
#' # agent from disk this time
#' write_testthat_file(
#'   agent = yaml_read_agent(yml_file),
#'   name = "from_agent_yaml",
#'   path = "."
#' )
#' 
#' }
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-5
#' 
#' @export

# x_read_disk--------------------------------------------------------------
#' Leer un *agent*, *informant*, *multiagent* o escaneo de tabla desde el disco
#' 
#' @description 
#' An *agent*, *informant*, *multiagent*, or table scan that has been written to
#' disk (with [x_write_disk()]) can be read back into memory with the
#' `x_read_disk()` function. For an *agent* or an *informant* object that has
#' been generated in this way, it may not have a data table associated with it
#' (depending on whether the `keep_tbl` option was `TRUE` or `FALSE` when
#' writing to disk) but it should still be able to produce reporting (by
#' printing the *agent* or *informant* to the console or using
#' [get_agent_report()]/[get_informant_report()]). An *agent* will return an
#' x-list with [get_agent_x_list()] and yield any available data extracts with
#' [get_data_extracts()]. Furthermore, all of an *agent*'s validation steps will
#' still be present (along with results from the last interrogation).
#' 
#' @details
#' Should a written-to-disk *agent* or *informant* possess a table-prep formula
#' (can be set any time with [set_read_fn()]) or a specific table (settable with
#' [set_tbl()]) we could use the [interrogate()] or [incorporate()] function
#' again. For a *data quality reporting* workflow, it is useful to
#' [interrogate()] target tables that evolve over time. While the same
#' validation steps will be used, more can be added before calling
#' [interrogate()]. For an *information management* workflow with an *informant*
#' object, using [incorporate()] will update aspects of the reporting such as
#' table dimensions, and info snippets/text will be regenerated.
#' 
#' @param filename The name of a file that was previously written by
#'   [x_write_disk()].
#' @param path An optional path to the file (combined with `filename`).
#' @param quiet Should the function *not* inform when the file is read? By
#'   default this is `FALSE`.
#' 
#' @return Either a `ptblank_agent`, `ptblank_informant`, or a
#'   `ptblank_tbl_scan` object.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # A: Reading an agent from disk 
#' 
#' # The process of developing an agent
#' # and writing it to disk with the
#' # `x_write_disk()` function is explained
#' # in that function's documentation;
#' # but suppose we have such a written file
#' # that's named "agent-small_table.rds",
#' # we could read that to a new agent
#' # object with `x_read_disk()`
#' agent <-
#'   x_read_disk("agent-small_table.rds")
#' 
#' # B: Reading an informant from disk
#' 
#' # If there is an informant written
#' # to disk via `x_write_disk()` and it's
#' # named "informant-small_table.rds",
#' # we could read that to a new informant
#' # object with `x_read_disk()`
#' informant <-
#'   x_read_disk("informant-small_table.rds")
#' 
#' # C: Reading a multiagent from disk 
#' 
#' # The process of creating a multiagent
#' # and writing it to disk with the
#' # `x_write_disk()` function is shown
#' # in that function's documentation;
#' # but should we have such a written file
#' # called "multiagent-small_table.rds",
#' # we could read that to a new multiagent
#' # object with `x_read_disk()`
#' agent <-
#'   x_read_disk("multiagent-small_table.rds")
#' 
#' # D: Reading a table scan from disk
#' 
#' # If there is a table scan written
#' # to disk via `x_write_disk()` and it's
#' # named "tbl_scan-storms.rds", we could
#' # read it back into R with `x_read_disk()`
#' tbl_scan <-
#'   x_read_disk("tbl_scan-storms.rds")
#' 
#' }
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-2
#' 
#' @export

# x_write_disk-------------------------------------------------------------
#' Escriba un *agent*, *informant*, *multiagent* o escaneo de tabla al disco
#' 
#' @description 
#' Writing an *agent*, *informant*, *multiagent*, or even a table scan to disk
#' with `x_write_disk()` can be useful for keeping data validation intel or
#' table information close at hand for later retrieval (with [x_read_disk()]).
#' By default, any data table that the *agent* or *informant* may have held
#' before being committed to disk will be expunged (not applicable to any table
#' scan since they never hold a table object). This behavior can be changed by
#' setting `keep_tbl` to `TRUE` but this only works in the case where the table
#' is not of the `tbl_dbi` or the `tbl_spark` class.
#'
#' @details
#' It is recommended to set up a table-prep formula so that the *agent* and
#' *informant* can access refreshed data after being read from disk through
#' [x_read_disk()]. This can be done initially with the `read_fn` argument of
#' [create_agent()]/[create_informant()] or, later, with [set_read_fn()].
#' Alternatively, we can reintroduce the *agent* or *informant* to a data table
#' with the [set_tbl()] function.
#' 
#' @param x An *agent* object of class `ptblank_agent`, an *informant* of class
#'   `ptblank_informant`, or an table scan of class `ptblank_tbl_scan`.
#' @param filename The filename to create on disk for the `agent`, `informant`,
#'   or table scan.
#' @param path An optional path to which the file should be saved (this is
#'   automatically combined with `filename`).
#' @param keep_tbl An option to keep a data table that is associated with the
#'   *agent* or *informant* (which is the case when the *agent*, for example, is
#'   created using `create_agent(tbl = <data table, ...)`). The default is
#'   `FALSE` where the data table is removed before writing to disk. For
#'   database tables of the class `tbl_dbi` and for Spark DataFrames
#'   (`tbl_spark`) the table is always removed (even if `keep_tbl` is set to
#'   `TRUE`).
#' @param keep_extracts An option to keep any collected extract data for failing
#'   rows. Only applies to *agent* objects. By default, this is `FALSE` (i.e.,
#'   extract data is removed).
#' @param quiet Should the function *not* inform when the file is written? By
#'   default this is `FALSE`.
#'   
#' @return Invisibly returns `TRUE` if the file has been written.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # A: Writing an `agent` to disk 
#' 
#' # Let's go through the process of (1)
#' # developing an agent with a validation
#' # plan (to be used for the data quality
#' # analysis of the `small_table` dataset),
#' # (2) interrogating the agent with the
#' # `interrogate()` function, and (3) writing
#' # the agent and all its intel to a file
#' 
#' # Creating an `action_levels` object is a
#' # common workflow step when creating a
#' # pointblank agent; we designate failure
#' # thresholds to the `warn`, `stop`, and
#' # `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Now create a pointblank `agent` object
#' # and give it the `al` object (which
#' # serves as a default for all validation
#' # steps which can be overridden); the
#' # data will be referenced in a `read_fn`
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`",
#'     actions = al
#'   )
#' 
#' # Then, as with any `agent` object, we
#' # can add steps to the validation plan by
#' # using as many validation functions as we
#' # want; then, we `interrogate()`
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5) %>%
#'   interrogate()
#'
#' # The `agent` can be written to a file with
#' # the `x_write_disk()` function
#' x_write_disk(
#'   agent,
#'   filename = "agent-small_table.rds"
#' )
#' 
#' # We can read the file back as an agent
#' # with the `x_read_disk()` function and
#' # we'll get all of the intel along with the
#' # restored agent
#' 
#' # If you're consistently writing agent
#' # reports when periodically checking data,
#' # we could make use of the `affix_date()`
#' # or `affix_datetime()` depending on the
#' # granularity you need; here's an example
#' # that writes the file with the format:
#' # 'agent-small_table-YYYY-mm-dd_HH-MM-SS.rds'
#' x_write_disk(
#'   agent,
#'   filename = affix_datetime(
#'     "agent-small_table.rds"
#'   )
#' )
#' 
#' # B: Writing an `informant` to disk
#' 
#' # Let's go through the process of (1)
#' # creating an informant object that
#' # minimally describes the `small_table`
#' # dataset, (2) ensuring that data is
#' # captured from the target table using
#' # the `incorporate()` function, and (3)
#' # writing the informant to a file
#' 
#' # Create a pointblank `informant`
#' # object with `create_informant()`
#' # and the `small_table` dataset; use
#' # `incorporate()` so that info snippets
#' # are integrated into the text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "high_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "low_a",
#'     fn = snip_lowest(column = "a")
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "From {low_a} to {high_a}."
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`."
#'   ) %>%
#'   incorporate()
#'
#' # The `informant` can be written to a
#' # file with `x_write_disk()`; let's do
#' # this with `affix_date()` so that the
#' # filename has a datestamp
#' x_write_disk(
#'   informant,
#'   filename = affix_date(
#'     "informant-small_table.rds"
#'   )
#' )
#' 
#' # We can read the file back into a
#' # new informant object (in the same
#' # state as when it was saved) by using
#' # `x_read_disk()`
#' 
#' # C: Writing a multiagent to disk
#' 
#' # Let's create one more pointblank
#' # agent object, provide it with some
#' # validation steps, and `interrogate()`
#' agent_b <-
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     vars(b), vars(g), na_pass = TRUE,
#'     label = "b > g"
#'   ) %>%
#'   col_is_character(
#'     vars(b, f),
#'     label = "Verifying character-type columns" 
#'   ) %>%
#'   interrogate()
#' 
#' # Now we can combine the earlier `agent`
#' # object with the newer `agent_b` to 
#' # create a `multiagent`
#' multiagent <-
#'   create_multiagent(agent, agent_b)
#'   
#' # The `multiagent` can be written to
#' # a file with the `x_write_disk()` function
#' x_write_disk(
#'   multiagent,
#'   filename = "multiagent-small_table.rds"
#' )
#' 
#' # We can read the file back as a multiagent
#' # with the `x_read_disk()` function and
#' # we'll get all of the constituent agents
#' # and their associated intel back as well
#' 
#' # D: Writing a table scan to disk
#' 
#' # We can get an report that describes all
#' # of the data in the `storms` dataset
#' tbl_scan <- scan_data(tbl = dplyr::storms)
#' 
#' # The table scan object can be written
#' # to a file with `x_write_disk()`
#' x_write_disk(
#'   tbl_scan,
#'   filename = "tbl_scan-storms.rds"
#' )
#' 
#' }
#'   
#' @family Object Ops
#' @section Function ID:
#' 9-1
#' 
#' @export

# yaml_agent_interrogate---------------------------------------------------
#' Obtenga un *agent* de **pointblank** YAML e `interrogate()`
#'
#' @description 
#' The `yaml_agent_interrogate()` function operates much like the
#' [yaml_read_agent()] function (reading a **pointblank** YAML file and
#' generating an *agent* with a validation plan in place). The key difference is
#' that this function takes things a step further and interrogates the target
#' table (defined by table-prep formula that is required in the YAML file). The
#' additional auto-invocation of [interrogate()] uses the default options of
#' that function. As with [yaml_read_agent()] the agent is returned except, this
#' time, it has intel from the interrogation.
#' 
#' @param filename The name of the YAML file that contains fields related to an
#'   *agent*.
#' @param path An optional path to the YAML file (combined with `filename`).
#' 
#' @return Un objeto `ptblank_agent`.
#'
#' @examples
#' if (interactive()) {
#' 
#' # Let's go through the process of
#' # developing an agent with a validation
#' # plan (to be used for the data quality
#' # analysis of the `small_table` dataset),
#' # and then offloading that validation
#' # plan to a pointblank YAML file; this
#' # will later be read in as a new agent and
#' # the target data will be interrogated
#' # (one step) with `yaml_agent_interrogate()`
#' 
#' # Creating an `action_levels` object is a
#' # common workflow step when creating a
#' # pointblank agent; we designate failure
#' # thresholds to the `warn`, `stop`, and
#' # `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Now create a pointblank `agent` object
#' # and give it the `al` object (which
#' # serves as a default for all validation
#' # steps which can be overridden); the
#' # data will be referenced in a `read_fn`
#' # (a requirement for writing to YAML)
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`.",
#'     actions = al
#'   )
#' 
#' # Then, as with any `agent` object, we
#' # can add steps to the validation plan by
#' # using as many validation functions as we
#' # want
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5)
#'
#' # The agent can be written to a pointblank
#' # YAML file with `yaml_write()`
#' yaml_write(
#'   agent = agent,
#'   filename = "agent-small_table.yml"
#' )
#' 
#' # The 'agent-small_table.yml' file is
#' # available in the package through `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' 
#' # We can view the YAML file in the console
#' # with the `yaml_agent_string()` function
#' yaml_agent_string(filename = yml_file)
#' 
#' # The YAML can also be printed in the console
#' # by supplying the agent as the input
#' yaml_agent_string(agent = agent)
#' 
#' # We can interrogate the data (which
#' # is accessible through the `read_fn`)
#' # through direct use of the YAML file
#' # with `yaml_agent_interrogate()`
#' agent <- 
#'   yaml_agent_interrogate(filename = yml_file)
#' 
#' class(agent)
#'
#' # If it's desired to only create a new
#' # agent with the validation plan in place
#' # (stopping short of interrogating the data),
#' # then the `yaml_read_agent()` function
#' # will be useful
#' agent <- 
#'   yaml_read_agent(filename = yml_file)
#' class(agent)
#' 
#' }
#'
#' @family pointblank YAML
#' @section Function ID:
#' 11-4
#'
#' @export

# yaml_agent_show_exprs----------------------------------------------------
#' Mostrar expresiones de validación usando **pointblank** YAML
#'
#' @description 
#' The `yaml_agent_show_exprs()` function follows the specifications of a
#' **pointblank** YAML file to generate and show the **pointblank** expressions
#' for generating the described validation plan. The expressions are shown in
#' the console, providing an opportunity to copy the statements and extend as
#' needed. A **pointblank** YAML file can itself be generated by using the
#' [yaml_write()] function with a pre-existing *agent*, or, it can be carefully
#' written by hand.
#'
#' @param filename The name of the YAML file that contains fields related to an
#'   *agent*.
#' @param path An optional path to the YAML file (combined with `filename`).
#' 
#' @examples
#' if (interactive()) {
#' 
#' # Let's create a validation plan for the
#' # data quality analysis of the `small_table`
#' # dataset; we need an agent and its
#' # table-prep formula enables retrieval
#' # of the target table
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`.",
#'     actions = action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'       notify_at = 0.35
#'     )
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5)
#'
#' # The agent can be written to a pointblank
#' # YAML file with `yaml_write()`
#' yaml_write(
#'   agent = agent,
#'   filename = "agent-small_table.yml"
#' )
#' 
#' # The 'agent-small_table.yml' file is
#' # available in the package through
#' # `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' 
#' # At a later time, the YAML file can
#' # be read into a new agent with the
#' # `yaml_read_agent()` function
#' agent <- 
#'   yaml_read_agent(filename = yml_file)
#' 
#' class(agent)
#' 
#' # To get a sense of which expressions are
#' # being used to generate the new agent, we
#' # can use `yaml_agent_show_exprs()`
#' yaml_agent_show_exprs(filename = yml_file)
#' 
#' }
#'   
#' @family pointblank YAML
#' @section Function ID:
#' 11-6
#'
#' @export

# yaml_agent_string--------------------------------------------------------
#' Mostrar **pointblank** YAML usando un agente o un archivo YAML
#' 
#' @description 
#' With **pointblank** YAML, we can serialize an agent's validation plan (with
#' [yaml_write()]), read it back later with a new agent (with
#' [yaml_read_agent()]), or perform an interrogation on the target data table
#' directly with the YAML file (with [yaml_agent_interrogate()]). The
#' `yaml_agent_string()` function allows us to inspect the YAML generated by
#' [yaml_write()] in the console, giving us a look at the YAML without needing
#' to open the file directly. Alternatively, we can provide an *agent* to the
#' `yaml_agent_string()` and view the YAML representation of the validation plan
#' without needing to write the YAML to disk beforehand.
#'
#' @param agent An *agent* object of class `ptblank_agent`. If an object is
#'   provided here, then `filename` must not be provided.
#' @param filename The name of the YAML file that contains fields related to an
#'   *agent*. If a file name is provided here, then *agent* object must not be
#'   provided in `agent`.
#' @param path An optional path to the YAML file (combined with `filename`).
#' @param expanded Should the written validation expressions for an *agent* be
#'   expanded such that **tidyselect** and [vars()] expressions for columns are
#'   evaluated, yielding a validation function per column? By default, this is
#'   `FALSE` so expressions as written will be retained in the YAML
#'   representation.
#'   
#' @examples 
#' if (interactive()) {
#' 
#' # Let's create a validation plan for the
#' # data quality analysis of the `small_table`
#' # dataset; we need an agent and its
#' # table-prep formula enables retrieval
#' # of the target table
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`.",
#'     actions = action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'       notify_at = 0.35
#'     )
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5)
#'
#' # We can view the YAML file in the console
#' # with the `yaml_agent_string()` function,
#' # providing the `agent` object as the input
#' yaml_agent_string(agent = agent)
#'
#' # The agent can be written to a pointblank
#' # YAML file with `yaml_write()`
#' yaml_write(
#'   agent = agent,
#'   filename = "agent-small_table.yml"
#' )
#' 
#' # There's a similar file in the package
#' # ('agent-small_table.yml') and it's
#' # accessible with `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' 
#' # The `yaml_agent_string()` function can
#' # be used with the YAML file as well,
#' # use the `filename` argument instead
#' yaml_agent_string(filename = yml_file)
#' 
#' # At some later time, the YAML file can
#' # be read as a new agent with the
#' # `yaml_read_agent()` function
#' agent <- yaml_read_agent(filename = yml_file)
#' class(agent)
#' 
#' }
#'   
#' @family pointblank YAML
#' @section Function ID:
#' 11-5
#' 
#' @export

# yaml_exec----------------------------------------------------------------
#' Ejecutar todas las tareas YAML de agentes e informantes
#' 
#' @description
#' The `yaml_exec()` function takes all relevant **pointblank** YAML files in a
#' directory and executes them. Execution involves interrogation of agents for
#' YAML agents and incorporation of informants for YAML informants. Under the
#' hood, this uses [yaml_agent_interrogate()] and [yaml_informant_incorporate()]
#' and then [x_write_disk()] to save the processed objects to an output
#' directory. These written artifacts can be read in at any later time with the
#' [x_read_disk()] function or the [read_disk_multiagent()] function. This is
#' useful when data in the target tables are changing and the periodic testing
#' of such tables is part of a data quality monitoring plan.
#' 
#' The output RDS files are named according to the object type processed, the
#' target table, and the date-time of processing. For convenience and
#' modularity, this setup is ideal when a table store YAML file (typically named
#' `"tbl_store.yml"` and produced via the [tbl_store()] and [yaml_write()]
#' workflow) is available in the directory, and when table-prep formulas are
#' accessed by name through [tbl_source()].
#' 
#' A typical directory of files set up for execution in this way might have the
#' following contents:
#' 
#' - a `"tbl_store.yml"` file for holding table-prep formulas (created with
#' [tbl_store()] and written to YAML with [yaml_write()])
#' - one or more YAML *agent* files to validate tables (ideally using
#' [tbl_source()])
#' - one or more YAML *informant* files to provide refreshed metadata on tables
#' (again, using [tbl_source()] to reference table preparations is ideal)
#' - an output folder (default is `"output"`) to save serialized versions of
#' processed agents and informants
#' 
#' Minimal example files of the aforementioned types can be found in the
#' **pointblank** package through the following `system.file()` calls:
#' 
#' - `system.file("yaml", "agent-small_table.yml", package = "pointblank")`
#' - `system.file("yaml", "informant-small_table.yml", package = "pointblank")`
#' - `system.file("yaml", "tbl_store.yml", package = "pointblank")`
#' 
#' The directory itself can be accessed using `system.file("yaml", package =
#' "pointblank")`.
#' 
#' @param path The path that contains the YAML files for agents and informants.
#' @param files A vector of YAML files to use in the execution workflow. By
#'   default, `yaml_exec()` will attempt to process every valid YAML file in
#'   `path` but supplying a vector here limits the scope to the specified files.
#' @param write_to_disk Should the execution workflow include a step that writes
#'   output files to disk? This internally calls [x_write_disk()] to write RDS
#'   files and uses the base filename of the agent/informant YAML file as part
#'   of the output filename, appending the date-time to the basename.
#' @param output_path The output path for any generated output files. By
#'   default, this will be a subdirectory of the provided `path` called
#'   `"output"`.
#' @param keep_tbl,keep_extracts For agents, the table may be kept if it is a
#'   data frame object (databases tables will never be pulled for storage) and
#'   *extracts*, collections of table rows that failed a validation step, may
#'   also be stored. By default, both of these options are set to `FALSE`.
#' 
#' @return Invisibly returns a named vector of file paths for the input files
#'   that were processed; file output paths (for wherever writing occurred) are
#'   given as the names.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # The 'yaml' directory that is
#' # accessible in the package through
#' # `system.file()` contains the files
#' # 1. `agent-small_table.yml`
#' # 2. `informant-small_table.yml`
#' # 3. `tbl_store.yml`
#' 
#' # There are references in YAML files
#' # 1 & 2 to the table store YAML file,
#' # so, they all work together cohesively
#' 
#' # Let's process the agent and the
#' # informant YAML files with `yaml_exec()`;
#' # and we'll specify the working directory
#' # as the place where the output RDS files
#' # are written
#' 
#' output_dir <- getwd()
#' 
#' yaml_exec(
#'   path = system.file(
#'     "yaml", package = "pointblank"
#'   ),
#'   output = output_dir
#' )
#' 
#' # This generates two RDS files in the
#' # working directory: one for the agent
#' # and the other for the informant; each
#' # of them are automatically time-stamped
#' # so that periodic execution can be
#' # safely carried out without risk of
#' # overwriting 
#' 
#' }
#' 
#' @family pointblank YAML
#' @section Function ID:
#' 11-8
#' 
#' @export

# yaml_informant_incorporate-----------------------------------------------
#' Obtener un *informant* de **pointblank** YAML e `incorporate()`
#'
#' @description 
#' The `yaml_informant_incorporate()` function operates much like the
#' [yaml_read_informant()] function (reading a **pointblank** YAML file and
#' generating an *informant* with all information in place). The key difference
#' is that this function takes things a step further and incorporates aspects
#' from the the target table (defined by table-prep formula that is required in
#' the YAML file). The additional auto-invocation of [incorporate()] uses the
#' default options of that function. As with [yaml_read_informant()] the
#' informant is returned except, this time, it has been updated with the latest
#' information from the target table.
#'
#' @param filename The name of the YAML file that contains fields related to an
#'   *informant*.
#' @param path An optional path to the YAML file (combined with `filename`).
#' 
#' @return Un objeto `ptblank_informant`.
#'
#' @examples
#' if (interactive()) {
#' 
#' # Let's go through the process of
#' # developing an informant with information
#' # about the `small_table` dataset and then
#' # move all that to a pointblank YAML
#' # file; this will later be read in as a
#' # new informant and the target data will
#' # be incorporated into the info text
#' # (in one step) with
#' # `yaml_informant_incorporate()`
#' 
#' # Now create a pointblank `informant`
#' # object; the data will be referenced
#' # in a `read_fn` (a requirement for
#' # writing to YAML)
#' informant <- 
#'   create_informant(
#'     read_fn = ~small_table,
#'     label = "A simple example with the `small_table`."
#'   )
#' 
#' # Then, as with any `informant` object, we
#' # can add information by using as many
#' # `info_*()` functions as we want
#' informant <-
#'   informant %>%
#'   info_columns(
#'    columns = vars(a),
#'    info = "In the range of 1 to 10. (SIMPLE)"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. (CALC)"
#'   ) %>%
#'   info_section(
#'     section_name = "rows",
#'     row_count = "There are {row_count} rows available."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "row_count",
#'     fn = ~ . %>% nrow()
#'   ) %>%
#'   incorporate()
#'
#' # The informant can be written to a pointblank
#' # YAML file with `yaml_write()`
#' yaml_write(
#'   informant = informant,
#'   filename = "informant-small_table.yml"
#' )
#' 
#' # The 'informant-small_table.yml' file
#' # is available in the package through
#' # `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "informant-small_table.yml",
#'     package = "pointblank"
#'   )
#' 
#' # We can incorporate the data (which
#' # is accessible through the `read_fn`)
#' # into the info text through direct
#' # use of the YAML file with
#' # `yaml_informant_incorporate()`
#' informant <- 
#'   yaml_informant_incorporate(filename = yml_file)
#' 
#' class(informant)
#'
#' # If it's desired to only create a new
#' # informant with the information in place
#' # (stopping short of processing), then the
#' # `yaml_read_informant()` function will
#' # be useful
#' informant <- 
#'   yaml_read_informant(filename = yml_file)
#' 
#' class(informant)
#' 
#' }
#'
#' @family pointblank YAML
#' @section Function ID:
#' 11-7
#'
#' @export

# yaml_read_agent----------------------------------------------------------
#' Leer un archivo YAML **pointblank** para crear un objeto *agent*
#'
#' @description 
#' With `yaml_read_agent()` we can read a **pointblank** YAML file that
#' describes a validation plan to be carried out by an *agent* (typically
#' generated by the [yaml_write()] function. What's returned is a new *agent*
#' with that validation plan, ready to interrogate the target table at will
#' (using the table-prep formula that is set with the `read_fn` argument). The
#' agent can be given more validation steps if needed before using
#' [interrogate()] or taking part in any other agent ops (e.g., writing to disk
#' with outputs intact via [x_write_disk()] or again to **pointblank** YAML with
#' [yaml_write()]).
#'
#' To get a picture of how `yaml_read_agent()` is interpreting the validation
#' plan specified in the **pointblank** YAML, we can use the
#' [yaml_agent_show_exprs()] function. That function shows us (in the console)
#' the **pointblank** expressions for generating the described validation plan.
#'   
#' @param filename The name of the YAML file that contains fields related to an
#'   *agent*.
#' @param path An optional path to the YAML file (combined with `filename`).
#'   
#' @return Un objeto `ptblank_agent`.
#'   
#' @examples
#' if (interactive()) {
#' 
#' # Let's go through the process of
#' # developing an agent with a validation
#' # plan (to be used for the data quality
#' # analysis of the `small_table` dataset),
#' # and then offloading that validation
#' # plan to a pointblank YAML file; this
#' # will be read in with `yaml_read_agent()`
#' 
#' # Creating an `action_levels` object is a
#' # common workflow step when creating a
#' # pointblank agent; we designate failure
#' # thresholds to the `warn`, `stop`, and
#' # `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Now create a pointblank `agent` object
#' # and give it the `al` object (which
#' # serves as a default for all validation
#' # steps which can be overridden); the
#' # data will be referenced in a `read_fn`
#' # (a requirement for writing to YAML)
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`.",
#'     actions = al
#'   )
#' 
#' # Then, as with any `agent` object, we
#' # can add steps to the validation plan by
#' # using as many validation functions as we
#' # want
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5)
#'
#' # The agent can be written to a pointblank
#' # YAML file with `yaml_write()`
#' yaml_write(
#'   agent = agent,
#'   filename = "agent-small_table.yml"
#' )
#' 
#' # The 'agent-small_table.yml' file is
#' # available in the package through
#' # `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' 
#' # We can view the YAML file in the console
#' # with the `yaml_agent_string()` function
#' yaml_agent_string(filename = yml_file)
#' 
#' # The YAML can also be printed in the console
#' # by supplying the agent as the input
#' yaml_agent_string(agent = agent)
#' 
#' # At some later time, the YAML file can
#' # be read as a new agent with the
#' # `yaml_read_agent()` function
#' agent <- yaml_read_agent(filename = yml_file)
#' 
#' class(agent)
#' 
#' # We can interrogate the data (which
#' # is accessible through the `read_fn`)
#' # with `interrogate()` and get an
#' # agent with intel, or, we can
#' # interrogate directly from the YAML
#' # file with `yaml_agent_interrogate()`
#' agent <- 
#'   yaml_agent_interrogate(
#'     filename = yml_file
#'   )
#' 
#' class(agent)
#' 
#' }
#' 
#' @family pointblank YAML
#' @section Function ID:
#' 11-2
#' 
#' @export

# yaml_read_informant------------------------------------------------------
#' Leer un archivo YAML **pointblank** para crear un objeto *informant*
#'
#' @description 
#' With `yaml_read_informant()` we can read a **pointblank** YAML file that
#' describes table information (typically generated by the [yaml_write()]
#' function. What's returned is a new *informant* object with the information
#' intact. The *informant* object can be given more information through use of
#' the `info_*()` functions.
#'   
#' @param filename The name of the YAML file that contains fields related to an
#'   *informant*.
#' @param path An optional path to the YAML file (combined with `filename`).
#' 
#' @return A `ptblank_informant` object.
#' 
#' @examples 
#' if (interactive()) {
#' 
#' # Create a pointblank `informant`
#' # object with `create_informant()`
#' # and the `small_table` dataset
#' informant <- create_informant(small_table)
#' 
#' # An `informant` object can be written
#' # to a YAML file with the `yaml_write()`
#' # function
#' # yaml_write(
#' #   informant = informant,
#' #   filename = "informant-small_table.yml"
#' # )
#' 
#' # The `informant-small_table.yml` file
#' # looks like this when written
#' 
#' #> info_label: '[2020-09-06|13:37:38]'
#' #> table:
#' #>   name: small_table
#' #> _columns: 8
#' #> _rows: 13
#' #> _type: tbl_df
#' #> columns:
#' #>   date_time:
#' #>     _type: POSIXct, POSIXt
#' #>   date:
#' #>     _type: Date
#' #>   a:
#' #>     _type: integer
#' #>   b:
#' #>     _type: character
#' #>   c:
#' #>     _type: numeric
#' #>   d:
#' #>     _type: numeric
#' #>   e:
#' #>     _type: logical
#' #>   f:
#' #>     _type: character
#' 
#' # We can add keys and values to
#' # add more pertinent information; with
#' # some direct editing of the file we get:
#' 
#' #> info_label: '[2020-09-06|13:37:38]'
#' #> table:
#' #>   name: small_table
#' #>   _columns: 8
#' #>   _rows: 13
#' #>   _type: tbl_df
#' #> columns:
#' #>   date_time:
#' #>     _type: POSIXct, POSIXt
#' #>     info: Date-time values.
#' #>   date:
#' #>     _type: Date
#' #>     info: Date values (the date part of `date_time`).
#' #>   a:
#' #>     _type: integer
#' #>     info: Small integer values (no missing values).
#' #>   b:
#' #>     _type: character
#' #>     info: Strings with a common pattern.
#' #>   c:
#' #>     _type: numeric
#' #>     info: Small numeric values (contains missing values).
#' #>   d:
#' #>     _type: numeric
#' #>     info: Large numeric values (much greater than `c`).
#' #>   e:
#' #>     _type: logical
#' #>     info: TRUE and FALSE values.
#' #>   f:
#' #>     _type: character
#' #>     info: Strings of the set `"low"`, `"mid"`, and `"high"`.
#' 
#' # We could also have done the same
#' # with the `informant` object by use of
#' # the `info_columns()` function
#' 
#' # The 'informant-small_table.yml' file
#' # is available in the package through
#' # `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "informant-small_table.yml",
#'     package = "pointblank"
#'   )
#' 
#' # We can read this YAML file back
#' # as an `informant` object by using
#' # `yaml_read_informant()`
#' informant <- 
#'   yaml_read_informant(filename = yml_file)
#' 
#' class(informant)
#' 
#' }
#' 
#' @family pointblank YAML
#' @section Function ID:
#' 11-3
#' 
#' @export

# yaml_write---------------------------------------------------------------
#' Escribir objetos **pointblank** en archivos YAML
#' 
#' @description
#' With `yaml_write()` we can take different **pointblank** objects (these are
#' the `ptblank_agent`, `ptblank_informant`, and `tbl_store`) and write them to
#' YAML. With an *agent*, for example, `yaml_write()` will write that everything
#' that is needed to specify an *agent* and it's validation plan to a YAML file.
#' With YAML, we can modify the YAML markup if so desired, or, use as is to
#' create a new agent with the [yaml_read_agent()] function. That *agent* will
#' have a validation plan and is ready to [interrogate()] the data. We can go a
#' step further and perform an interrogation directly from the YAML file with
#' the [yaml_agent_interrogate()] function. That returns an agent with intel
#' (having already interrogated the target data table). An *informant* object
#' can also be written to YAML with `yaml_write()`.
#'
#' One requirement for writing an *agent* or an *informant* to YAML is that we
#' need to have a table-prep formula (`read_fn`) specified (it's an R formula
#' that is used to read the target table when [interrogate()] or [incorporate()]
#' is called). This option can be set when using
#' [create_agent()]/[create_informant()] or with [set_read_fn()] (useful with an
#' existing agent or informant object).
#' 
#' @param ... Any mix of **pointblank** objects such as the *agent*
#'   (`ptblank_agent`), the *informant* (`ptblank_informant`), or the table
#'   store (`tbl_store`). The agent and informant can be combined into a single
#'   YAML file (so as both objects have the same value for `read_fn`). A table
#'   store cannot be combined with either an agent or an informant so it must
#'   undergo conversion alone.
#' @param .list Allows for the use of a list as an input alternative to `...`.
#' @param filename The name of the YAML file to create on disk. It is
#'   recommended that either the `.yaml` or `.yml` extension be used for this
#'   file. If not provided then default names will be used (`"tbl_store.yml"`)
#'   for a table store and the other objects will get default naming to the
#'   effect of `"<object>-<tbl_name>.yml"`.
#' @param path An optional path to which the YAML file should be saved (combined
#'   with `filename`).
#' @param expanded Should the written validation expressions for an *agent* be
#'   expanded such that **tidyselect** and [vars()] expressions for columns are
#'   evaluated, yielding a validation function per column? By default, this is
#'   `FALSE` so expressions as written will be retained in the YAML
#'   representation.
#' @param quiet Should the function *not* inform when the file is written? By
#'   default this is `FALSE`.
#'   
#' @return Invisibly returns `TRUE` if the YAML file has been written. 
#'   
#' @examples
#' if (interactive()) {
#' 
#' # Let's go through the process of
#' # developing an agent with a validation
#' # plan (to be used for the data quality
#' # analysis of the `small_table` dataset),
#' # and then offloading that validation
#' # plan to a pointblank YAML file
#' 
#' # Creating an `action_levels` object is a
#' # common workflow step when creating a
#' # pointblank agent; we designate failure
#' # thresholds to the `warn`, `stop`, and
#' # `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Now create a pointblank `agent` object
#' # and give it the `al` object (which
#' # serves as a default for all validation
#' # steps which can be overridden); the
#' # data will be referenced in a `read_fn`
#' # (a requirement for writing to YAML)
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`.",
#'     actions = al
#'   )
#' 
#' # Then, as with any `agent` object, we
#' # can add steps to the validation plan by
#' # using as many validation functions as we
#' # want
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5)
#'
#' # The agent can be written to a pointblank
#' # YAML file with `yaml_write()`
#' yaml_write(
#'   agent,
#'   filename = "agent-small_table.yml"
#' )
#' 
#' # The 'agent-small_table.yml' file is
#' # available in the package through
#' # `system.file()`
#' yml_file <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' 
#' # We can view the YAML file in the console
#' # with the `yaml_agent_string()` function
#' yaml_agent_string(filename = yml_file)
#' 
#' # The YAML can also be printed in the console
#' # by supplying the agent as the input
#' yaml_agent_string(agent = agent)
#' 
#' # At some later time, the YAML file can
#' # be read as a new agent with the
#' # `yaml_read_agent()` function
#' agent <- 
#'   yaml_read_agent(filename = yml_file)
#' 
#' class(agent)
#' 
#' # We can interrogate the data (which
#' # is accessible through the `read_fn`)
#' # with `interrogate()` and get an
#' # agent with intel, or, we can
#' # interrogate directly from the YAML
#' # file with `yaml_agent_interrogate()`
#' agent <- 
#'   yaml_agent_interrogate(filename = yml_file)
#' 
#' class(agent)
#' 
#' }
#' 
#' @family pointblank YAML
#' @section Function ID:
#' 11-1
#' 
#' @export
