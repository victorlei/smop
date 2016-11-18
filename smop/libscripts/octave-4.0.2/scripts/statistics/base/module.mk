FCN_FILE_DIRS += statistics/base

statistics_base_FCN_FILES = \
  statistics/base/center.m \
  statistics/base/cloglog.m \
  statistics/base/corr.m \
  statistics/base/cov.m \
  statistics/base/gls.m \
  statistics/base/histc.m \
  statistics/base/iqr.m \
  statistics/base/kendall.m \
  statistics/base/kurtosis.m \
  statistics/base/logit.m \
  statistics/base/lscov.m \
  statistics/base/mahalanobis.m \
  statistics/base/mean.m \
  statistics/base/meansq.m \
  statistics/base/median.m \
  statistics/base/mode.m \
  statistics/base/moment.m \
  statistics/base/ols.m \
  statistics/base/ppplot.m \
  statistics/base/prctile.m \
  statistics/base/probit.m \
  statistics/base/qqplot.m \
  statistics/base/quantile.m \
  statistics/base/range.m \
  statistics/base/ranks.m \
  statistics/base/run_count.m \
  statistics/base/runlength.m \
  statistics/base/skewness.m \
  statistics/base/spearman.m \
  statistics/base/statistics.m \
  statistics/base/std.m \
  statistics/base/table.m \
  statistics/base/var.m \
  statistics/base/zscore.m

FCN_FILES += $(statistics_base_FCN_FILES)

PKG_ADD_FILES += statistics/base/PKG_ADD

DIRSTAMP_FILES += statistics/base/$(octave_dirstamp)
