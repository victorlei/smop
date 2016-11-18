FCN_FILE_DIRS += statistics/tests

statistics_tests_FCN_FILES = \
  statistics/tests/anova.m \
  statistics/tests/bartlett_test.m \
  statistics/tests/chisquare_test_homogeneity.m \
  statistics/tests/chisquare_test_independence.m \
  statistics/tests/cor_test.m \
  statistics/tests/f_test_regression.m \
  statistics/tests/hotelling_test.m \
  statistics/tests/hotelling_test_2.m \
  statistics/tests/kolmogorov_smirnov_test.m \
  statistics/tests/kolmogorov_smirnov_test_2.m \
  statistics/tests/kruskal_wallis_test.m \
  statistics/tests/manova.m \
  statistics/tests/mcnemar_test.m \
  statistics/tests/prop_test_2.m \
  statistics/tests/run_test.m \
  statistics/tests/sign_test.m \
  statistics/tests/t_test.m \
  statistics/tests/t_test_2.m \
  statistics/tests/t_test_regression.m \
  statistics/tests/u_test.m \
  statistics/tests/var_test.m \
  statistics/tests/welch_test.m \
  statistics/tests/wilcoxon_test.m \
  statistics/tests/z_test.m \
  statistics/tests/z_test_2.m

FCN_FILES += $(statistics_tests_FCN_FILES)

PKG_ADD_FILES += statistics/tests/PKG_ADD

DIRSTAMP_FILES += statistics/tests/$(octave_dirstamp)
