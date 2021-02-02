# mega cool nested df because we fancy
lib_spec_dummy <- tidyr::tibble(
   lib_id = c("A", "B", "C"),
   lib = c(
      list(data.frame(code = "blah", decode = "boop")),
      list(c("a", "b", "c")),
      list(c("external_id"))
      )
)


# or we make three babies instead?
lib_spec_1 <- tibble(
   lib_id = "A",
   lib = data.frame(code = "blah", decode = "boop")
)

lib_spec_2 <- tibble(
   lib_id = "B",
   lib = list(c("a", "b", "c"))
)


lib_spec_3 <- tibble(
   lib = "C",
   list(c("external_id"))
)

# is this how we'd compare their sizes?
# what other pros and cons are there?
object.size(lib_spec_dummy)
object.size(lib_spec_1) + object.size(lib_spec_2) + object.size(lib_spec_3)
