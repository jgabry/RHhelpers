context("Tests for renaming variables for imputations")

test_that("Renaming works for including variables", {
  expect_error(include_waves("imp_variable",waves = 0, year = 17),"Waves start at 1")
  expect_error(include_waves("imp_variable",waves = 0, year = c(1,2)),"only impute 1 year at a time")
  expect_equal(length(include_waves(c('a','b','c'), waves = 1:3)),9)
  expect_error(imp_varlist("imp_variable",type = "num", bounds = NA,waves = 0, year = 17),"first wave as 1")
})

test_that("Checking and adjusting for flags", {
  expect_error(flag_names(vars=NULL),"should not be empty")
  expect_output(flag_names(vars="variable",waves=1:2,year=17),"Not using year or wave")
  expect_setequal(flag_names(vars=c("y17_q1variable","y17_q2variable"),waves=1:2,year=17:18),c("y17_q1pimp.variable","y17_q2pimp.variable"))
  expect_setequal(flag_names(vars=c("y17_q1variable","y18_q2variable"),waves=1:2,year=17:18),c("y17_q1pimp.variable","y18_q2pimp.variable"))
})

test_df1 <- data.frame(y17_q1var1=c(1,2,1,2,1,2),
                      y17_q1pimp.var1 = c(1,1,1,0,0,0),
                      y17_q2var2=c(3,4,3,4,3,4),
                      y17_q2pimp.var2 = c(0,1,1,1,0,1),
                      q12var3 = c(NA,5,5,2,1,2),
                      q12pimp.var3 = c(0,0,0,0,1,0)
)
prepared_test_df1 <- prepare_imp_NAs(test_df1, c("y17_q1var1","y17_q2var2","q12var3"), flag = "pimp.", waves = 1:12)

test_that("Recoding flagged imputations to NAs", {
  expect_equal(sum(is.na(prepared_test_df1$y17_q1var1)),3)
  expect_equal(sum(is.na(prepared_test_df1$y17_q2var2)),4)
  expect_equal(sum(is.na(prepared_test_df1$q12var3)),2)
  expect_setequal(is.na(prepared_test_df1$y17_q2var2),test_df1$y17_q2pimp.var2==1)
  expect_setequal(is.na(prepared_test_df1$y17_q1var1),test_df1$y17_q1pimp.var1==1)
})

test_df2 <- data.frame(q1var1=c(1,2,1,2,1,2),
                       q1pimp.var1 = c(1,1,1,0,0,0),
                       q2var2=c(3,4,3,4,3,4),
                       q2pimp.var2 = c(0,1,1,1,0,1),
                       q12var3 = c(NA,5,5,2,1,2),
                       q12pimp.var3 = c(0,0,0,0,1,0)
)
prepared_test_df2 <-   prepare_imp_NAs(test_df2, c("q1var1","q2var2","q12var3"), flag = "pimp.", waves = 1:12)


test_that("Backwards compatibility for longitudinal naming convention stands", {
  expect_equal(sum(is.na(prepared_test_df2$q12var3)),2)
  expect_setequal(is.na(prepared_test_df2$q2var2),test_df2$q2pimp.var2==1)
  expect_setequal(is.na(prepared_test_df2$q1var1),test_df2$q1pimp.var1==1)
})

test_that("Naive imputation function returns a warning if var is not present", {
  expect_error(naive_imputation_step(test_df1, contvars = "q4var1", catvars="q12var3", breaks = c(1:3)),"Missing variables in contvars")
  expect_error(naive_imputation_step(test_df1, contvars = "q12var3", catvars="q4var1", breaks = c(1:3)),"Missing variables in catvars")
})
