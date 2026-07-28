test_that("consICA(seed=) is reproducible, including in parallel", {

    require("fastICA", include.only = c('fastICA'))
    data("samples_data")

    ## single core: same seed -> identical result
    a1 <- consICA(samples_data, ncomp = 5, ntry = 3, ncores = 1,
                  seed = 1, show.every = 0)
    a2 <- consICA(samples_data, ncomp = 5, ntry = 3, ncores = 1,
                  seed = 1, show.every = 0)
    expect_equal(a1$S, a2$S)
    expect_equal(a1$M, a2$M)

    ## parallel (2 cores): same seed -> identical result
    p1 <- consICA(samples_data, ncomp = 5, ntry = 4, ncores = 2,
                  seed = 1, show.every = 0)
    p2 <- consICA(samples_data, ncomp = 5, ntry = 4, ncores = 2,
                  seed = 1, show.every = 0)
    expect_equal(p1$S, p2$S)
    expect_equal(p1$M, p2$M)

    ## different seed -> different result
    p3 <- consICA(samples_data, ncomp = 5, ntry = 4, ncores = 2,
                  seed = 99, show.every = 0)
    expect_false(isTRUE(all.equal(p1$S, p3$S)))
})
