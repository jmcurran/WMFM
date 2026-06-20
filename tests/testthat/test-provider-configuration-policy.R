test_that("deployed app detection can be controlled explicitly", {
  withr::local_envvar(WMFM_DEPLOYED_APP = "1")
  expect_true(isWmfmDeployedApp())

  withr::local_envvar(WMFM_DEPLOYED_APP = "0")
  expect_false(isWmfmDeployedApp())
})

test_that("deployed apps block ordinary provider and credential editing", {
  withr::local_envvar(
    WMFM_DEPLOYED_APP = "1",
    WMFM_ALLOW_USER_PROVIDER_CONFIG = NA,
    WMFM_ALLOW_USER_CREDENTIAL_ENTRY = NA
  )

  expect_false(isWmfmProviderConfigurationEditable())
  expect_false(isWmfmCredentialEntryAllowed())
})

test_that("local desktop sessions allow provider configuration by default", {
  withr::local_envvar(
    WMFM_DEPLOYED_APP = "0",
    WMFM_ALLOW_USER_PROVIDER_CONFIG = NA,
    WMFM_ALLOW_USER_CREDENTIAL_ENTRY = NA
  )

  expect_true(isWmfmProviderConfigurationEditable())
  expect_true(isWmfmCredentialEntryAllowed())
})

test_that("provider policy text distinguishes deployment and desktop use", {
  withr::local_envvar(WMFM_DEPLOYED_APP = "1")
  deployedText = paste(buildWmfmProviderSetupPolicyText(), collapse = " ")
  expect_match(deployedText, "administrator-managed", fixed = TRUE)
  expect_match(deployedText, "cannot enter API keys", fixed = TRUE)

  withr::local_envvar(WMFM_DEPLOYED_APP = "0")
  desktopText = paste(buildWmfmProviderSetupPolicyText(), collapse = " ")
  expect_match(desktopText, "local desktop", fixed = TRUE)
  expect_match(desktopText, "separate dialog", fixed = TRUE)
})
