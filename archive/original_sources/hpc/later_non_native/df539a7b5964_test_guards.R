# Original source: test_guards.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/test_guards.R
# Original source SHA-256: df539a7b59641dd9f1a1598f565c2f877f9e95f204b8a3f74dffaa62db185a41
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
source('sensitivity_guards.R')
f=y~x+I(x^2)+GDP2020_2.5min+I(GDP2020_2.5min^2)+x:ID+z
q=prune_formula(f,c('GDP2020_2.5min','ID'))
stopifnot(identical(attr(terms(q),'term.labels'),c('x','I(x^2)','z')),identical(environment(q),environment(f)))
stopifnot(identical(attr(terms(prune_formula(y~0+x+ID,'ID')),'term.labels'),'x'),attr(terms(prune_formula(y~0+x+ID,'ID')),'intercept')==0)
stopifnot(identical(predictor_subset(c('z','ID','x','GDP2020_2.5min'),c('GDP2020_2.5min'),'CLEAN_ENV'),c('z','x')))
stopifnot(identical(predictor_subset(c('z','ID','x','GDP2020_2.5min'),c('GDP2020_2.5min'),'CLEAN_FULL'),c('z','x','GDP2020_2.5min')))
stopifnot(inherits(try(predictor_subset(c('x','ID'),character(),'ENV'),silent=TRUE),'try-error'))
cat('GUARD_TESTS_PASS\n')
