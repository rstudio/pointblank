# Setup
library(pointblank)
agent <- create_agent(data.frame(x = 1)) |> 
  col_vals_equal(x, 1) |> 
  interrogate()
show_size <- function(x) {
  size <- if (is.character(x) && file.exists(x)) file.size(x) else object.size(x)
  scales::label_bytes()(as.integer(size))
}

# Assign something large to env
largeobj <- replicate(100, mtcars[sample(nrow(mtcars), 1e4, replace = TRUE),])
show_size(largeobj)

# Serialize
f <- tempfile(fileext = ".rds")
saveRDS(agent, f)

# Should be equivalent
stopifnot(identical(agent, readRDS(f)))

# File size check
show_size(agent)
show_size(f)

# Should be uninfluenced by size of objects in env
stopifnot(file.size(f) < as.integer(object.size(largeobj)))

# Cleanup
file.remove(f)
