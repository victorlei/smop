FCN_FILE_DIRS += statistics/distributions

statistics_distributions_FCN_FILES = \
  statistics/distributions/betacdf.m \
  statistics/distributions/betainv.m \
  statistics/distributions/betapdf.m \
  statistics/distributions/betarnd.m \
  statistics/distributions/binocdf.m \
  statistics/distributions/binoinv.m \
  statistics/distributions/binopdf.m \
  statistics/distributions/binornd.m \
  statistics/distributions/cauchy_cdf.m \
  statistics/distributions/cauchy_inv.m \
  statistics/distributions/cauchy_pdf.m \
  statistics/distributions/cauchy_rnd.m \
  statistics/distributions/chi2cdf.m \
  statistics/distributions/chi2inv.m \
  statistics/distributions/chi2pdf.m \
  statistics/distributions/chi2rnd.m \
  statistics/distributions/discrete_cdf.m \
  statistics/distributions/discrete_inv.m \
  statistics/distributions/discrete_pdf.m \
  statistics/distributions/discrete_rnd.m \
  statistics/distributions/empirical_cdf.m \
  statistics/distributions/empirical_inv.m \
  statistics/distributions/empirical_pdf.m \
  statistics/distributions/empirical_rnd.m \
  statistics/distributions/expcdf.m \
  statistics/distributions/expinv.m \
  statistics/distributions/exppdf.m \
  statistics/distributions/exprnd.m \
  statistics/distributions/fcdf.m \
  statistics/distributions/finv.m \
  statistics/distributions/fpdf.m \
  statistics/distributions/frnd.m \
  statistics/distributions/gamcdf.m \
  statistics/distributions/gaminv.m \
  statistics/distributions/gampdf.m \
  statistics/distributions/gamrnd.m \
  statistics/distributions/geocdf.m \
  statistics/distributions/geoinv.m \
  statistics/distributions/geopdf.m \
  statistics/distributions/geornd.m \
  statistics/distributions/hygecdf.m \
  statistics/distributions/hygeinv.m \
  statistics/distributions/hygepdf.m \
  statistics/distributions/hygernd.m \
  statistics/distributions/kolmogorov_smirnov_cdf.m \
  statistics/distributions/laplace_cdf.m \
  statistics/distributions/laplace_inv.m \
  statistics/distributions/laplace_pdf.m \
  statistics/distributions/laplace_rnd.m \
  statistics/distributions/logistic_cdf.m \
  statistics/distributions/logistic_inv.m \
  statistics/distributions/logistic_pdf.m \
  statistics/distributions/logistic_rnd.m \
  statistics/distributions/logncdf.m \
  statistics/distributions/logninv.m \
  statistics/distributions/lognpdf.m \
  statistics/distributions/lognrnd.m \
  statistics/distributions/nbincdf.m \
  statistics/distributions/nbininv.m \
  statistics/distributions/nbinpdf.m \
  statistics/distributions/nbinrnd.m \
  statistics/distributions/normcdf.m \
  statistics/distributions/norminv.m \
  statistics/distributions/normpdf.m \
  statistics/distributions/normrnd.m \
  statistics/distributions/poisscdf.m \
  statistics/distributions/poissinv.m \
  statistics/distributions/poisspdf.m \
  statistics/distributions/poissrnd.m \
  statistics/distributions/stdnormal_cdf.m \
  statistics/distributions/stdnormal_inv.m \
  statistics/distributions/stdnormal_pdf.m \
  statistics/distributions/stdnormal_rnd.m \
  statistics/distributions/tcdf.m \
  statistics/distributions/tinv.m \
  statistics/distributions/tpdf.m \
  statistics/distributions/trnd.m \
  statistics/distributions/unidrnd.m \
  statistics/distributions/unidcdf.m \
  statistics/distributions/unidinv.m \
  statistics/distributions/unidpdf.m \
  statistics/distributions/unifrnd.m \
  statistics/distributions/unifcdf.m \
  statistics/distributions/unifinv.m \
  statistics/distributions/unifpdf.m \
  statistics/distributions/wblcdf.m \
  statistics/distributions/wblinv.m \
  statistics/distributions/wblpdf.m \
  statistics/distributions/wblrnd.m \
  statistics/distributions/wienrnd.m

FCN_FILES += $(statistics_distributions_FCN_FILES)

PKG_ADD_FILES += statistics/distributions/PKG_ADD

DIRSTAMP_FILES += statistics/distributions/$(octave_dirstamp)
