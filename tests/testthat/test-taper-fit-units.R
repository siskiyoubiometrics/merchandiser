test_that("taper diagnostics use inches and feet without changing the fitted equation", {
  ## Literals were captured with dput() from the unedited fit_taper() example.
  fit <- fit_taper(
    tree_id = example_stem_measurements$tree_id,
    dbh = example_stem_measurements$dbh,
    ht = example_stem_measurements$ht,
    h = example_stem_measurements$h,
    dib = example_stem_measurements$dib,
    spcd = example_stem_measurements$spcd,
    form = "max_burkhart"
  )
  coefficients <- c(
    b1 = -3.0495954190660313,
    b2 = 1.5383392687548754,
    b3 = -1.4426112735177701,
    b4 = 25.709639182454588,
    a1 = 0.65716321869683592,
    a2 = 0.08603773305330456
  )
  prediction <- c(
    11.155465108180342,
    10.445260412228329,
    9.9340104304901296,
    9.4010423484615124,
    8.8424299314557526,
    8.252967514688283,
    7.6255042033267104,
    6.9497547987007611,
    6.2099762495385553,
    5.3798191538618934,
    4.43843513644895,
    3.4811024477061761,
    2.5233667428645994,
    1.5644880514635204,
    0.59900224036057481,
    13.014709292877066,
    12.186137147599718,
    11.589678835571817,
    10.967882739871769,
    10.316168253365042,
    9.6284621004696636,
    8.8964215705478296,
    8.1080472651508924,
    7.244972291128315,
    6.2764556795055428,
    5.1781743258571096,
    4.0612861889905387,
    2.9439278666753737,
    1.8252360600407769,
    0.69883594708733732,
    14.87395347757379,
    13.927013882971105,
    13.245347240653505,
    12.534723131282016,
    11.789906575274333,
    11.003956686251044,
    10.167338937768948,
    9.2663397316010148,
    8.2799683327180738,
    7.1730922051491914,
    5.9179135152652673,
    4.6414699302749014,
    3.3644889904861328,
    2.0859840686180204,
    0.79866965381409982,
    16.733197662270513,
    15.667890618342494,
    14.901015645735193,
    14.101563522692272,
    13.263644897183623,
    12.379451272032425,
    11.438256304990066,
    10.424632198051141,
    9.3149643743078325,
    8.0697287307928409,
    6.657652704673426,
    5.2216536715592676,
    3.7850501142968986,
    2.3467320771952802,
    0.89850336054086233,
    18.592441846967237,
    17.408767353713877,
    16.55668405081688,
    15.66840391410252,
    14.737383219092917,
    13.754945857813803,
    12.709173672211183,
    11.582924664501267,
    10.349960415897591,
    8.9663652564364877,
    7.3973918940815926,
    5.8018374128436303,
    4.2056112381076654,
    2.6074800857725382,
    0.99833706726759142,
    22.310930216360685,
    20.890520824456658,
    19.868020860980259,
    18.802084696923025,
    17.684859862911505,
    16.505935029376566,
    15.251008406653421,
    13.899509597401522,
    12.419952499077111,
    10.759638307723787,
    8.8768702728979001,
    6.9622048954123521,
    5.0467334857291988,
    3.1289761029270409,
    1.1980044807211496
  )
  expect_equal(
    fit$coefficients,
    coefficients,
    tolerance = 1e-8
  )
  expect_equal(
    fit$fit_statistics$overall$rmse * 2.54,
    0.69728051900131516,
    tolerance = 1e-8
  )
  expect_equal(
    fit$fit_statistics$overall$bias * 2.54,
    0.0090356461706518742,
    tolerance = 1e-8
  )
  expect_equal(
    predict(fit),
    prediction,
    tolerance = 1e-8
  )
  diagnostics <- fit$residual_diagnostics
  expect_equal(
    diagnostics$dbh,
    example_stem_measurements$dbh
  )
  expect_equal(
    diagnostics$ht,
    example_stem_measurements$ht
  )
  expect_equal(
    diagnostics$h,
    example_stem_measurements$h
  )
  expect_equal(
    diagnostics$observed_dib,
    example_stem_measurements$dib
  )
  expect_equal(
    diagnostics$fitted_dib,
    prediction
  )
  expect_equal(
    diagnostics$population_fitted_dib,
    prediction
  )
  expect_equal(
    diagnostics$residual,
    example_stem_measurements$dib - prediction,
    tolerance = 1e-8
  )
  expect_equal(
    fit$data_metric$dbh,
    example_stem_measurements$dbh * 2.54
  )
  expect_equal(
    fit$data_metric$dib,
    example_stem_measurements$dib * 2.54
  )
  expect_equal(
    fit$data_metric$ht,
    example_stem_measurements$ht * 0.3048
  )
  expect_equal(
    fit$data_metric$h,
    example_stem_measurements$h * 0.3048
  )
  by_height <- fit$fit_statistics$by_relative_height
  expect_equal(
    by_height$rmse * 2.54,
    c(
      0.917959138782878,
      1.09455247729603,
      0.88664342366139,
      0.668993051211509,
      0.356337152749256,
      0.291784629767596,
      0.489400810992188,
      0.686543843161271,
      0.695981469129053,
      0.358788002453095
    ),
    tolerance = 1e-8
  )
  expect_equal(
    by_height$bias * 2.54,
    c(
      0.0979381951002048,
      0.0405127688754619,
      0.00578295962956214,
      0.0933458624373615,
      0.112581194485528,
      -0.169860483223053,
      -0.0731310084230514,
      0.0569031917275039,
      -0.0459512827257262,
      -0.0157970140707276
    ),
    tolerance = 1e-8
  )
  expect_output(
    print(fit),
    " inches"
  )
  expect_output(
    print(summary(fit)),
    "Overall fit statistics in inches:"
  )
})
