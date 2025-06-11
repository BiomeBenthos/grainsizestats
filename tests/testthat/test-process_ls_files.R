test_that("process_laser_file parses diam and volume correctly", {
  # Chemin vers le fichier de test
  test_file <- testthat::test_path("testdata", "laser.ls")

  # Résultat de la fonction principale
  result <- process_laser_file(test_file)

  # Test : structure de base
  expect_s3_class(result, "data.frame")
  expect_named(result, c("diameter (µm)", "volume_diff (%)"))
  expect_equal(nrow(result), 6)

  # Vérifie quelques valeurs précises
  expect_equal(result[1, "diameter (µm)"], 0.04)
  expect_equal(round(sum(result[["volume_diff (%)"]]), 6), 100)
})
