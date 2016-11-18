FCN_FILE_DIRS += statistics/models

statistics_models_PRIVATE_FCN_FILES = \
  statistics/models/private/logistic_regression_derivatives.m \
  statistics/models/private/logistic_regression_likelihood.m

statistics_models_FCN_FILES = \
  statistics/models/logistic_regression.m \
  $(statistics_models_PRIVATE_FCN_FILES)

FCN_FILES += $(statistics_models_FCN_FILES)

PKG_ADD_FILES += statistics/models/PKG_ADD

DIRSTAMP_FILES += statistics/models/$(octave_dirstamp)
