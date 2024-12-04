sys_path_agent <- system.file("yaml/agent-small_table.yml", package = "pointblank")
sys_path_informant <- system.file("yaml/informant-small_table.yml", package = "pointblank")
sys_path_tbl_store <- system.file("yaml/tbl_store.yml", package = "pointblank")

path_yml_files <- "./yaml_files"
work_path <- "./generated_r_files"

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}

if (fs::dir_exists(path = path_yml_files)) {
  fs::dir_delete(path = path_yml_files)
}

fs::dir_create(path = work_path)
fs::dir_create(path = path_yml_files)

# Copy the YAML files over to the `path_yml_files` directory
fs::file_copy(
  path = c(sys_path_agent, sys_path_informant, sys_path_tbl_store),
  new_path = fs::path_norm(fs::path(getwd(), path_yml_files))
)

path_agent <- fs::path_norm(fs::path(getwd(), path_yml_files, "agent-small_table.yml"))
path_informant <- fs::path_norm(fs::path(getwd(), path_yml_files, "informant-small_table.yml"))
path_tbl_store <- fs::path_norm(fs::path(getwd(), path_yml_files, "tbl_store.yml"))

test_that("The `yaml_exec()` function effectively processes .yml files", {

  # Read YAML files from the specified path, write output to a path
  # relative to the working directory
  yaml_exec(path = path_yml_files, output_path = work_path)

  # Get the agent path and expect that the file represents an agent
  written_agent <-
    fs::path_abs(fs::dir_ls(path = work_path, regexp = "agent.*?rds$"))
  expect_true(is_ptblank_agent(x_read_disk(filename = written_agent)))

  # Get the informant path and expect that the file represents an informant
  written_informant <-
    fs::path_abs(fs::dir_ls(path = work_path, regexp = "informant.*?rds$"))
  expect_true(is_ptblank_informant(x_read_disk(filename = written_informant)))

  # Delete the written agent and informant files
  fs::file_delete(written_agent)
  fs::file_delete(written_informant)

  # Read YAML files from the specified path, write output
  # to an absolute path
  work_path_abs <- fs::path_abs(work_path)
  yaml_exec(path = path_yml_files, output_path = work_path_abs)

  # Get the agent path and expect that the file represents an agent
  written_agent <-
    fs::path_abs(fs::dir_ls(path = work_path, regexp = "agent.*?rds$"))
  expect_true(is_ptblank_agent(x_read_disk(filename = written_agent)))

  # Get the informant path and expect that the file represents an informant
  written_informant <-
    fs::path_abs(fs::dir_ls(path = work_path, regexp = "informant.*?rds$"))
  expect_true(is_ptblank_informant(x_read_disk(filename = written_informant)))

  # Delete the written agent and informant files
  fs::file_delete(written_agent)
  fs::file_delete(written_informant)

  # Read YAML files from the specified path, write output files to the
  # 'output' subdir of the `path` (`yaml/output`)
  yaml_exec(path = path_yml_files)

  # Get the agent path and expect that the file represents an agent
  written_agent <-
    fs::path_abs(fs::dir_ls(path = file.path(path_yml_files, "output"), regexp = "agent.*?rds$"))
  expect_true(is_ptblank_agent(x_read_disk(filename = written_agent)))

  # Get the informant path and expect that the file represents an informant
  written_informant <-
    fs::path_abs(fs::dir_ls(path = file.path(path_yml_files, "output"), regexp = "informant.*?rds$"))
  expect_true(is_ptblank_informant(x_read_disk(filename = written_informant)))

  # Delete the written agent and informant files
  fs::file_delete(written_agent)
  fs::file_delete(written_informant)

  # Delete the 'output' subdirectory
  if (fs::dir_exists(path = file.path(path_yml_files, "output"))) {
    fs::dir_delete(path = file.path(path_yml_files, "output"))
  }

  # Copy the YAML files over to the working directory
  fs::file_copy(
    path = c(path_agent, path_informant, path_tbl_store),
    new_path = getwd(),
    overwrite = TRUE
  )

  # Read YAML files from the working directory, write output
  # files to the 'output' subdir
  yaml_exec()

  # Get the agent path and expect that the file represents an agent
  written_agent <-
    fs::path_abs(fs::dir_ls(path = file.path(getwd(), "output"), regexp = "agent.*?rds$"))
  expect_true(is_ptblank_agent(x_read_disk(filename = written_agent)))

  # Get the informant path and expect that the file represents an informant
  written_informant <-
    fs::path_abs(fs::dir_ls(path = file.path(getwd(), "output"), regexp = "informant.*?rds$"))
  expect_true(is_ptblank_informant(x_read_disk(filename = written_informant)))

  # Delete the 'output' subdirectory
  if (fs::dir_exists(path = "./output")) {
    fs::dir_delete(path = "./output")
  }

  # Delete the YAML files in the working directory
  fs::file_delete(fs::dir_ls(path = getwd(), regexp = "(agent|informant|tbl_store).*?yml$"))

  # Read just one of the YAML files (the agent) from the specified path,
  # write output to a path relative to the working directory
  yaml_exec(
    path = path_yml_files,
    files = "agent-small_table.yml",
    output_path = work_path
  )

  # Get that the agent has been written (and the informant was not)
  expect_gt(length(fs::path_abs(fs::dir_ls(path = work_path, regexp = "agent.*?rds$"))), 0)
  expect_equal(length(fs::path_abs(fs::dir_ls(path = work_path, regexp = "informant.*?rds$"))), 0)

  # Get the agent path and expect that the file represents an agent
  written_agent <- fs::path_abs(fs::dir_ls(path = work_path, regexp = "agent.*?rds$"))
  expect_true(is_ptblank_agent(x_read_disk(filename = written_agent)))

  # Delete the written agent file
  fs::file_delete(written_agent)

  # Read YAML files from the specified path but don't write output
  # at all (this can theoretically still perform useful side effects
  # when executing, it's just that the .rds artifacts won't be saved)
  yaml_exec(
    path = path_yml_files,
    output_path = work_path,
    write_to_disk = FALSE
  )

  # Expect that neither the agent nor the informant were saved
  expect_length(fs::path_abs(fs::dir_ls(path = work_path, regexp = "agent.*?rds$")), 0)
  expect_length(fs::path_abs(fs::dir_ls(path = work_path, regexp = "informant.*?rds$")), 0)

  # Read just one of the YAML files (the agent) from the specified path,
  # write output to a path relative to the working directory; we will
  # inspect the saved agent for data retention (table and extracts) and
  # expect none since the defaults for `yaml_exec()` prevent their storage
  yaml_exec(
    path = path_yml_files,
    files = "agent-small_table.yml",
    output_path = work_path
  )

  # Get the agent path and expect that the file represents an agent
  written_agent <- fs::path_abs(fs::dir_ls(path = work_path, regexp = "agent.*?rds$"))
  expect_true(is_ptblank_agent(x_read_disk(filename = written_agent)))

  # Read in the saved agent as `agent_1`
  agent_1 <- x_read_disk(filename = written_agent)

  # Expect no table stored in the `agent_1` object and expect an
  # empty list for `extracts` (there normally might be extracts since
  # not all units passed, let's check for that as well) and no
  # table to be present (would be in `agent_1$tbl`)
  expect_false(all_passed(agent_1))
  expect_length(agent_1$extracts, 0)
  expect_null(agent_1$tbl)

  # Delete the written agent file
  fs::file_delete(written_agent)

  # Read just one of the YAML files (the agent) from the specified path,
  # write output to a path relative to the working directory, this time
  # enabling the storage of data extracts and the input table; we will
  # inspect the saved agent for the table and extracts
  yaml_exec(
    path = path_yml_files,
    files = "agent-small_table.yml",
    output_path = work_path,
    keep_tbl = TRUE,
    keep_extracts = TRUE
  )

  # Get the agent path and expect that the file represents an agent
  written_agent <- fs::path_abs(fs::dir_ls(path = work_path, regexp = "agent.*?rds$"))
  expect_true(is_ptblank_agent(x_read_disk(filename = written_agent)))

  # Read in the saved agent as `agent_2`
  agent_2 <- x_read_disk(filename = written_agent)

  # Expect no table stored in the `agent_2` object and expect a
  # non-empty list for `extracts` and also expect the `small_table`
  # in `agent_2$tbl`
  expect_false(all_passed(agent_2))
  expect_gt(length(agent_2$extracts), 0)
  expect_s3_class(agent_2$tbl, "tbl_df")
  expect_equal(agent_2$tbl, small_table)

  # Delete the written agent file
  fs::file_delete(written_agent)

  # Expect an error if the `path` provided doesn't exist
  expect_error(yaml_exec(path = "does/not/exist", output_path = work_path))
})

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}

if (fs::dir_exists(path = path_yml_files)) {
  fs::dir_delete(path = path_yml_files)
}
