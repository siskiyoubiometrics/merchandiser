## These tables are illustrative, not a sourced specification.
example_products_pnw <- products(
  product(product = 'export',  ## product name, unitless
          spcd = c(202, 263),  ## species codes, unitless
          min_dbh = 12,  ## inches
          min_length = 32,  ## feet
          max_length = 40,  ## feet
          trim = 1,  ## feet
          min_sed = 12,  ## inches
          inside_bark = TRUE,  ## logical, unitless
          volume_unit = 'scribner',  ## board feet
          split_scale = FALSE),  ## logical, unitless
  product(product = 'domestic_saw',  ## product name, unitless
          spcd = c(202, 263),  ## species codes, unitless
          min_dbh = 10,  ## inches
          min_length = 16,  ## feet
          max_length = 40,  ## feet
          trim = 1,  ## feet
          min_sed = 6,  ## inches
          inside_bark = TRUE,  ## logical, unitless
          volume_unit = 'scribner',  ## board feet
          split_scale = FALSE),  ## logical, unitless
  product(product = 'pulp',  ## product name, unitless
          spcd = c(202, 263),  ## species codes, unitless
          min_dbh = 5,  ## inches
          min_length = 8,  ## feet
          max_length = 40,  ## feet
          trim = 0.5,  ## feet
          min_sed = 3,  ## inches
          inside_bark = TRUE,  ## logical, unitless
          volume_unit = 'green_ton')  ## green short tons
)

example_products_south <- products(
  product(product = 'sawtimber',  ## product name, unitless
          spcd = 131,  ## species code, unitless
          min_dbh = 12,  ## inches
          min_length = 16,  ## feet
          max_length = 40,  ## feet
          trim = 0.5,  ## feet
          min_sed = 8,  ## inches
          inside_bark = FALSE,  ## logical, unitless
          max_logs = 1,  ## logs per segment
          volume_unit = 'green_ton'),  ## green short tons
  product(product = 'chip_n_saw',  ## product name, unitless
          spcd = 131,  ## species code, unitless
          min_dbh = 8,  ## inches
          max_dbh = 12,  ## inches
          min_length = 16,  ## feet
          max_length = 40,  ## feet
          trim = 0.5,  ## feet
          min_sed = 6,  ## inches
          inside_bark = FALSE,  ## logical, unitless
          max_logs = 1,  ## logs per segment
          volume_unit = 'green_ton'),  ## green short tons
  product(product = 'pulpwood',  ## product name, unitless
          spcd = 131,  ## species code, unitless
          min_dbh = 5,  ## inches
          max_dbh = 8,  ## inches
          min_length = 8,  ## feet
          max_length = 40,  ## feet
          trim = 0.5,  ## feet
          min_sed = 3,  ## inches
          inside_bark = FALSE,  ## logical, unitless
          max_logs = 1,  ## logs per segment
          volume_unit = 'green_ton')  ## green short tons
)
