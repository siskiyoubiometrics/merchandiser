linear_dib <- function(dbh, ht, h, aux) {
  as.double(dbh * (1 - h / ht))
}

register_test_model <- function(id, dib = linear_dib, dob = NULL,
                                height_at_dib = NULL, volume = NULL,
                                inputs = list(
                                  required = character(), optional = character(),
                                  pairs = list()
                                ), species = integer(), bark_ratio = 0.9) {
  model <- new_taper_model(
    id = id,
    family = "test",
    dib = dib,
    dob = dob,
    height_at_dib = height_at_dib,
    volume = volume,
    inputs = inputs,
    species = species,
    bark_ratio = bark_ratio,
    source = "test"
  )
  register_taper_model(model)
  model
}

capture_warnings <- function(code) {
  messages <- character()
  value <- withCallingHandlers(
    code,
    warning = function(warning) {
      messages <<- c(messages, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = messages)
}
