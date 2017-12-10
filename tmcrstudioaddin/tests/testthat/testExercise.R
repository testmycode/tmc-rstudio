library('testthat')

testResourcesDir <- file.path(getwd(), "resources")
noProjectsDir <- file.path(testResourcesDir, "tmcr-projects-folders", "no-projects")
containsProjects <- file.path(testResourcesDir, "tmcr-projects-folders", "contains-projects")
printingProjects <- file.path(testResourcesDir, "tmcr-projects-folders", "printing-projects")

test_that("findExercisesFromPath returns only '' for noProjectsDir", {
  foundProjects <- findExercisesFromPath(noProjectsDir)
  expect_equal(length(foundProjects), 1)
  expect_equal(foundProjects, "")
})

test_that("findExercisesFromPath find right projects for containsProjects", {
  cat(containsProjects, "\n")
  foundProjects <- findExercisesFromPath(containsProjects)

  expect_equal(length(foundProjects), 5)
  expect_true(file.path(containsProjects, "simple_all_tests_pass") %in% foundProjects)
  expect_true(file.path(containsProjects, "rtmc-r", "viikko1", "01_laskuoperaatioita") %in% foundProjects)
  expect_true(file.path(containsProjects, "rtmc-r", "viikko1", "02_vektorin_kasittelya") %in% foundProjects)
  expect_true(file.path(containsProjects, "rtmc-r", "viikko1", "03_matriisioperaatioita") %in% foundProjects)
})

test_that("downloadedExercises returns only empty for noProjectsDir", {
  with_mock(get_projects_folder = function(x) return(noProjectsDir), {
    result <- downloadedExercisesPaths()
    expect_equal(length(result), 1)
    expect_equal(names(result)[[1]], "")
    expect_equal(result[[1]], "")
  })
})

test_that("downloadedExercises returns correct names and paths for containsProjects", {
  with_mock(get_projects_folder = function(x) return(containsProjects), {
    result <- downloadedExercisesPaths()
    resultNames <- names(result)

    expect_equal(length(result), 5)
    expect_true(file.path(containsProjects, "simple_all_tests_pass") %in% result)
    expect_true("simple_all_tests_pass" %in% resultNames)
    expect_true(file.path(containsProjects, "rtmc-r", "viikko1", "01_laskuoperaatioita") %in% result)
    expect_true("viikko1-01_laskuoperaatioita" %in% resultNames)
    expect_true(file.path(containsProjects, "rtmc-r", "viikko1", "02_vektorin_kasittelya") %in% result)
    expect_true("viikko1-02_vektorin_kasittelya" %in% resultNames)
    expect_true(file.path(containsProjects, "rtmc-r", "viikko1", "03_matriisioperaatioita") %in% result)
    expect_true("viikko1-03_matriisioperaatioita" %in% resultNames)
  })
})

test_that("exercisePathFromWd returns empty when wd is not a project", {
  with_mock(getwd = function(x) return(noProjectsDir),
            get_projects_folder = function(x) return(noProjectsDir), {
    result <- exercisePathFromWd()

    expect_equal(length(result), 1)
    expect_equal(result[[1]], " ")
    expect_equal(names(result)[[1]], " ")
  })
})

test_that("exercisePathFromWd returns right output for a project wd", {
  with_mock(getwd = function(x) return(file.path(containsProjects, "simple_all_tests_pass")),
            get_projects_folder = function(x) return(containsProjects), {
    result <- exercisePathFromWd()

    expect_equal(length(result), 1)
    expect_equal(result[[1]], file.path(containsProjects, "simple_all_tests_pass"))
    expect_equal(names(result)[[1]], "simple_all_tests_pass")
  })
})
