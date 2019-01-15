require(testthat)
require(attribrisk)


## Add example to test using the baseline and the strata statements together.

data(chapter.dat)
baseline.dat <- chapter.dat 
set.seed(10)
baseline.dat$hbp[sample(x=1:nrow(baseline.dat), size=1000)] <- 'HBP-0'

set.seed(1012)
x <- attribrisk(cases ~ strata(match.id) + expos(hbp), data = chapter.dat)
x2 <- attribrisk(cases ~ strata(match.id) + expos(hbp), data = chapter.dat, baseline=baseline.dat)

expect_equal(x2$attribrisk,0.1316692, 
             label=paste("Attribrisk estimate not within tolerance for following call: ", paste(deparse(x$call), collapse="")), tolerance=1.0e-6, scale=1)


expect_equal(object=sqrt(x2$var), 0.0150093, 
             label=paste("Attribrisk estimate for sterr not in tolerance for following call:", paste(deparse(x$call), collapse="")), tolerance=1.0e-6, scale=1)
