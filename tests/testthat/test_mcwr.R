library(testthat)
library(mcwr)
    # not sure whether I should do this
    # building under RStudio does not require it, but I need them if I run
    #   the script from R CMD batch in order to test in all versions

######## TESTS TO SKIP ############

skiplist <- c(# ".mcwr_statelocals",
              # ".mcwr_rewardslocals",
              # "mcwr_check",
              # "mcwr_switch",
              # "mcwr_exit",
              # "mcwr_genvars",
              # "mcwr_expectancies",
              'jnk') # never comment out this line


######## CERT GLOBALS ########

certdir <- 'c:/dan/r/source/mcwr/tests/testthat/'
load(paste0(certdir, 'mcwr_certdata.Rdata')) # cert data sets: mcwr_{1|...|8}, mcwr_rwnote_repl_trans

######## CERT PROGRAM DEFS ########

.mcwrcert_genvars <- function(vlist, data = data.frame(age=c(1:5))) {
    for (v in vlist) {
        if (length(vlist)>1 | trimws(vlist[1])!='')  # allows call .mcwrcert_genvars() w/o args
            data[2:nrow(data),v] <- 0.2
    }
    return(data)
}

.mcwrcert_defvll <- function(vll) {

    if (!any(grepl(vll, c("rwnote_sh", "rwnote_sh_u", "rwnote"))))
        stop(".mcwrcert_defvll: vll name not found.")

    if (vll=="rwnote_sh") {
        MCWR        <- c(1:5)
        names(MCWR) <- c('w', 'u', 'o', 'r', 'd')
    }
    if (vll=="rwnote_sh_u") {
        MCWR        <- c(1:5)
        names(MCWR) <- c('W', 'U', 'O', 'R', 'D')
    }
    if (vll=="rwnote") {
        MCWR        <- c(1:5)
        names(MCWR) <- c('working', 'unemployed', 'outofLF', 'retired', 'dead')
    }

    return(MCWR)
}

.mcwrcert_mreldif <- function(X, Y) {
    return(max(abs(X-Y) / (abs(Y)+1)))
}

################################



context("Mata functions")

test_that(".mcwr_statelocals", {

    skip_if(".mcwr_statelocals" %in% skiplist)

    # errors
    #      chars in p?? varnames
    varlists <- c("p12 p1k", "pk1 p12", "p12 p1_", "p1_ p12", "p1K pK1 p12", "p12 pKK")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_statelocals(df), ".*as numeric digits.")
    }

    #      state 0 not allowed
    varlists <- c("p01", "p01 p11 p12 p13 p21 p22 p23", "p10 p11 p12 p20 p21 p22 p23", "p00 p23")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_statelocals(df), ".*0 is not allowed.")
    }

    #      highest target state not higher than highest from-state
    varlists <- c("p11", "p11 p12 p21 p22", "p11 p12 p21 p22 p23 p43", "p11 p33")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_statelocals(df), ".*not bigger than largest from-state.*")
    }

    # works
    #      ji default
    varlists <- c("p12 p13 p14 p22 p33", "p33 abc p12 d p22 p15def p14 p13", "_13 p33 p22 a12 p14 p13 p12")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_statelocals(df)
        expect_equal(rv$p_exi, c("p12", "p13", "p14", "p22", "p33"))
        expect_equal(rv$p_ful, c("p11", "p12", "p13", "p14", "p21", "p22", "p23", "p24", "p31", "p32", "p33", "p34"))
        expect_equal(rv$p_new, c("p11", "p21", "p23", "p24", "p31", "p32", "p34"))

        expect_equal(rv$s_trn, c(12, 13, 14, 22, 33))
        expect_equal(rv$s_frm, c(1, 2, 3))
        expect_equal(rv$s_trg, c(2, 3, 4))
        expect_equal(rv$s_abs, c(4))
        expect_equal(rv$s_omt, integer(0))
    }

    #      ij option
    varlists <- c("p21 p31 p41 p22 p33", "p33 abc p21 d p22 p51def p41 p31", "_31 p33 p22 a21 p41 p31 p21")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_statelocals(df), ".*")
        rv <- .mcwr_statelocals(df, ij=TRUE)
        expect_equal(rv$p_exi, c("p21", "p31", "p41", "p22", "p33"))
        expect_equal(rv$p_ful, c("p11", "p21", "p31", "p41", "p12", "p22", "p32", "p42", "p13", "p23", "p33", "p43"))
        expect_equal(rv$p_new, c("p11", "p12", "p32", "p42", "p13", "p23", "p43"))

        expect_equal(rv$s_trn, c(21, 31, 41, 22, 33))
        expect_equal(rv$s_frm, c(1, 2, 3))
        expect_equal(rv$s_trg, c(2, 3, 4))
        expect_equal(rv$s_abs, c(4))
        expect_equal(rv$s_omt, c(integer(0)))
    }

    #      ji default
    varlists <- c("p11 p12 p14 p15 p21 p22 p24 p25 p41", "p41 p21 p22 p24 p25 p11 p12 p14 p15")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_statelocals(df)
        expect_equal(rv$p_exi, c("p11", "p12", "p14", "p15", "p21", "p22", "p24", "p25", "p41"))
        expect_equal(rv$p_ful, c("p11", "p12", "p14", "p15", "p21", "p22", "p24", "p25", "p41", "p42", "p44", "p45"))
        expect_equal(rv$p_new, c("p42", "p44", "p45"))

        expect_equal(rv$s_frm, c(1, 2, 4))
        expect_equal(rv$s_trg, c(1, 2, 4, 5))
        expect_equal(rv$s_abs, c(5))
        expect_equal(rv$s_omt, c(3))
    }

     #      ij option
     varlists <- c("p11 p21 p41 p51 p12 p22 p42 p52 p14", "p14 p12 p22 p42 p52 p11 p21 p41 p51")
     for (vl in varlists) {
         vl2 <- strsplit(vl, ' ')
         df <- .mcwrcert_genvars(vl2)
         expect_error(.mcwr_statelocals(df), ".*")
         rv <- .mcwr_statelocals(df, TRUE)
         expect_equal(rv$p_exi, c("p11", "p21", "p41", "p51", "p12", "p22", "p42", "p52", "p14"))
         expect_equal(rv$p_ful, c("p11", "p21", "p41", "p51", "p12", "p22", "p42", "p52", "p14", "p24", "p44", "p54"))
         expect_equal(rv$p_new, c("p24", "p44", "p54"))

         expect_equal(rv$s_frm, c(1, 2, 4))
         expect_equal(rv$s_trg, c(1, 2, 4, 5))
         expect_equal(rv$s_abs, c(5))
         expect_equal(rv$s_omt, c(3))
     }

    #      ji default
    varlists <- c("p11 p12 p29")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_statelocals(df)
        expect_equal(rv$p_exi, c("p11", "p12", "p29"))
        expect_equal(rv$p_ful, c("p11", "p12", "p19", "p21", "p22", "p29"))
        expect_equal(rv$p_new, c("p19", "p21", "p22"))

        expect_equal(rv$s_frm, c(1, 2))
        expect_equal(rv$s_trg, c(1, 2, 9))
        expect_equal(rv$s_abs, c(9))
        expect_equal(rv$s_omt, c(3, 4, 5, 6, 7, 8))
    }

    #      ij option
    varlists <- c("p11 p21 p92")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_statelocals(df), ".*")
        rv <- .mcwr_statelocals(df, TRUE)
        expect_equal(rv$p_exi, c("p11", "p21", "p92"))
        expect_equal(rv$p_ful, c("p11", "p21", "p91", "p12", "p22", "p92"))
        expect_equal(rv$p_new, c("p91", "p12", "p22"))

        expect_equal(rv$s_frm, c(1, 2))
        expect_equal(rv$s_trg, c(1, 2, 9))
        expect_equal(rv$s_abs, c(9))
        expect_equal(rv$s_omt, c(3, 4, 5, 6, 7, 8))
    }

})

test_that(".mcwr_rewardslocals", { # also .mcwr_revvars()

    skip_if(".mcwr_rewardslocals" %in% skiplist)

    # errors
    #      chars in p?? varnames
    varlists <- c("r1_12 r2_1k", "r1_k1 r1_12", "r1_12 r1_1_", "r1_1_ r1_12", "r1_1K r1_K1 r1_12", "r1_12 r1_KK", "rK_11")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_rewardslocals(df, c(1,2,3)), ".*as numeric digits.")
    }

    #      state 0 not allowed
    varlists <- c("r1_01", "r1_01 r1_11 r1_12 r1_13 r1_21 r1_22 r1_23", "r1_10 r1_11 r1_12 r1_20 r1_21 r1_22 r1_23", "r1_00 r1_23", "r0_23")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_rewardslocals(df, c(1,2,3)), ".*0 is not allowed.")
    }

    #      rewards states not in p?? variables
    varlists <- c("r1_12", "r2_12", "r3_33 r4_34", "r2_23")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_rewardslocals(df, c(1,3)), ".*inconsistent with the set of.*")
    }

    #      rewards states not in rewards transitions
    varlists <- c("r3_12", "r2_11", "r1_11 r1_12 r1_23")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        expect_error(.mcwr_rewardslocals(df, c(1,2,3)), ".*reward states other than from/target states.")
    }

    # works
    #      ji default
    varlists <- c(" ")  # no r?_?? variables at all
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_rewardslocals(df, c(1,2))
        expect_equal(rv$r_exi, character(0))
        expect_equal(rv$r_ful, c("r1_11", "r1_12"))
        expect_equal(rv$r_new, c("r1_11", "r1_12"))

        expect_equal(rv$r_trn, integer(0))
        expect_equal(rv$r_rcv, integer(0))
        expect_equal(rv$r_nrc, c(1))
    }

    #      ij option
    varlists <- c(" ")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_rewardslocals(df, c(1,2), ij=TRUE)
        expect_equal(rv$r_exi, character(0))
        expect_equal(rv$r_ful, c("r1_11", "r1_21"))
        expect_equal(rv$r_new, c("r1_11", "r1_21"))

        expect_equal(rv$r_trn, integer(0))
        expect_equal(rv$r_rcv, integer(0))
        expect_equal(rv$r_nrc, c(1))
    }

    #      ji default
    varlists <- c("r1_11 r1_12", "r1_12 r1_11", "r1_13def r1_11 r1_12")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_rewardslocals(df, c(1,2))
        expect_equal(rv$r_exi, c("r1_11", "r1_12"))
        expect_equal(rv$r_ful, c("r1_11", "r1_12"))
        expect_equal(rv$r_new, character(0))  # TODO: make all character(0) and integer(0) so that length(x) on output lists gives intuitive result

        expect_equal(rv$r_trn, c(11, 12))
        expect_equal(rv$r_rcv, c(1))
        expect_equal(rv$r_nrc, integer(0))
    }

    #      ij option
    varlists <- c("r1_11 r1_21", "r1_21 r1_11", "r1_31def r1_11 r1_21")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_rewardslocals(df, c(1,2), TRUE)
        expect_equal(rv$r_exi, c("r1_11", "r1_21"))
        expect_equal(rv$r_ful, c("r1_11", "r1_21"))
        expect_equal(rv$r_new, character(0))

        expect_equal(rv$r_trn, c(11, 21))
        expect_equal(rv$r_rcv, c(1))
        expect_equal(rv$r_nrc, integer(0))
    }

    #      ji default
    varlists <- c("r1_11 r1_12 r4_45", "r4_45 r1_12 r1_11")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_rewardslocals(df, c(1,2,4,5))
        expect_equal(rv$r_exi, c("r1_11", "r1_12", "r4_45"))
        expect_equal(rv$r_ful, c("r1_11", "r1_12", "r1_14", "r1_15", "r1_21", "r1_41", "r2_12", "r2_21", "r2_22", "r2_24", "r2_25", "r2_42", "r4_14", "r4_24", "r4_41", "r4_42", "r4_44", "r4_45"))
        expect_equal(rv$r_new, c("r1_14", "r1_15", "r1_21", "r1_41", "r2_12", "r2_21", "r2_22", "r2_24", "r2_25", "r2_42", "r4_14", "r4_24", "r4_41", "r4_42", "r4_44"))

        expect_equal(rv$r_trn, c(11, 12, 45))
        expect_equal(rv$r_rcv, c(1, 4))
        expect_equal(rv$r_nrc, c(2))
    }

    #      ij option
    varlists <- c("r1_11 r1_21 r4_54", "r4_54 r1_21 r1_11")
    for (vl in varlists) {
        vl2 <- strsplit(vl, ' ')
        df <- .mcwrcert_genvars(vl2)
        rv <- .mcwr_rewardslocals(df, c(1,2,4,5), TRUE)
        expect_equal(rv$r_exi, c("r1_11", "r1_21", "r4_54"))
        expect_equal(rv$r_ful, c("r1_11", "r1_21", "r1_41", "r1_51", "r1_12", "r1_14", "r2_21", "r2_12", "r2_22", "r2_42", "r2_52", "r2_24", "r4_41", "r4_42", "r4_14", "r4_24", "r4_44", "r4_54"))
        expect_equal(rv$r_new, c("r1_41", "r1_51", "r1_12", "r1_14", "r2_21", "r2_12", "r2_22", "r2_42", "r2_52", "r2_24", "r4_41", "r4_42", "r4_14", "r4_24", "r4_44"))

        expect_equal(rv$r_trn, c(11, 21, 54))
        expect_equal(rv$r_rcv, c(1, 4))
        expect_equal(rv$r_nrc, c(2))
    }

    # _mcwr_revvars()
    revvars <- .mcwr_revvars("")
    expect_equal(revvars, "")

    revvars <- .mcwr_revvars("p12")
    expect_equal(revvars, "p21")

    revvars <- .mcwr_revvars("r5_12")
    expect_equal(revvars, "r5_21")

    revvars <- .mcwr_revvars("p11 p12 p54 r1_29 r9_29 r2_99 r7_43 abc")
    expect_equal(revvars, c("p11", "p21", "p45", "r1_92", "r9_92", "r2_99", "r7_34", "acb"))

    revvars <- .mcwr_revvars(c("p11 p12 p54 r1_29 r9_29 r2_99 r7_43 abc"))
    expect_equal(revvars, c("p11", "p21", "p45", "r1_92", "r9_92", "r2_99", "r7_34", "acb"))

    revvars <- .mcwr_revvars(c("p11", "p12", "p54", "r1_29", "r9_29", "r2_99", "r7_43", "abc"))
    expect_equal(revvars, c("p11", "p21", "p45", "r1_92", "r9_92", "r2_99", "r7_34", "acb"))

})

test_that("mcwr_check", { #  [TODO: checks on r-vars]

    skip_if("mcwr_check" %in% skiplist)

    expect_type(mcwr_check(mcwr_3), 'list')

    chkdf <- mcwr_3[-(1:nrow(mcwr_3)),]
    expect_error(mcwr_check(chkdf), ".*Not enough observations.")

    # p errors
    chkdf <- mcwr_3
    chkdf[,'p24'] <- NULL
    expect_error(mcwr_check(chkdf), ".*do not sum close enough to 1.")

    chkdf <- mcwr_3
    chkdf[2:(nrow(chkdf)-1),'p11'] <- 0.5
    expect_error(mcwr_check(chkdf), ".*do not sum close enough to 1.")

    chkdf <- mcwr_3
    chkdf[2:(nrow(chkdf)-1),'p11'] <- 1.1
    expect_error(mcwr_check(chkdf), ".*outside of.*")

    chkdf <- mcwr_3
    chkdf[1:(nrow(chkdf)-1),'p11'] <- 0.2
    expect_error(mcwr_check(chkdf), ".*First row may only hold missing values.")

    # age errors
    chkdf <- mcwr_3
    chkdf[1,'age'] <- 57.5 # age not sorted
    expect_error(mcwr_check(chkdf), ".*Data frame must be sorted by age.")

    chkdf <- mcwr_3
    chkdf[1,'age'] <- NA
    expect_error(mcwr_check(chkdf), ".*Column 'age' has missing values.")

    chkdf <- mcwr_3
    chkdf[1,'age'] <- -3
    expect_type(mcwr_check(mcwr_3), 'list') # this works: negative age values are allowed!

    chkdf <- mcwr_3
    chkdf[2, 'p11'] <- NA
    expect_error(mcwr_check(chkdf), ".*Transition probabilites have missing values.")

    chkdf <- mcwr_3
    chkdf[1:3, 'age'] <- 0
    expect_error(mcwr_check(chkdf), ".*Values of column 'age' must be all distinct.")

# pres - restore until here
#
# // ij option
# mcwr switch
# rcof "`noi' mcwr check , ij"==0
#
# preserve
# replace p11 = 0.5 in 2/l
# rcof "`noi' mcwr check , ij"==459
# restore

    # exit() option
    chkdf <- mcwr_1
    expect_error(mcwr_check(chkdf), ".*Problem with absorption row value.*")
    expect_type(mcwr_check(chkdf, skipexit=TRUE), 'list')
# _cassert , str(`r(hasexit)') to(0)

    chkdf <- mcwr_3
    expect_type(mcwr_check(chkdf), 'list')
# _cassert , str(`r(hasexit)') to(1)
    expect_type(mcwr_check(chkdf, skipexit=TRUE), 'list')

    # // return values, value label MCWR
    # * use mcwr/mcwr_1 , clear
    # * mcwr check
    # * ret li

})

test_that("mcwr_switch", {

    skip_if("mcwr_switch" %in% skiplist)

    jivarlist <- c('p11', 'p12', 'p14', 'p22')
    ijvarlist <- c('p11', 'p21', 'p41', 'p22')

    chk <- .mcwrcert_genvars(jivarlist)
    chk[2:nrow(chk),'p14'] <- 0.6 # make p sum to one
    chk[2:nrow(chk),'p22'] <- 1

    expect_type(swi <- mcwr_switch(chk), 'list')
    expect_equal(names(swi), c('age', ijvarlist))
    # expec_equal(rv$oldorder   , 'ji')
    # expec_equal(rv$neworder   , 'ij')
    # expec_equal(rv$oldvarnames, jivarlist)
    # expec_equal(rv$newvarnames, ijvarlist)

    expect_type(swi <- mcwr_switch(swi), 'list')
    expect_equal(names(swi), c('age', jivarlist))
    # expect_equal(oldorder   , 'ij')
    # expect_equal(neworder   , 'ji')
    # expect_equal(oldvarnames, ijvarlist)
    # expect_equal(newvarnames, jivarlist)

    jiadd <- c('jnk', 'r1_12', 'r1_14', 'r2_12', 'r2_22')
    ijadd <- c('jnk', 'r1_21', 'r1_41', 'r2_21', 'r2_22')
    chk <- .mcwrcert_genvars(jiadd, data=chk)

    expect_type(swi <- mcwr_switch(chk), 'list')
    expect_equal(names(swi), c('age', ijvarlist, ijadd))
    # expect_equal(oldorder   , 'ji')
    # expect_equal(neworder   , 'ij')
    # expect_equal(oldvarnames, c(jivarlist, jiadd))
    # expect_equal(newvarnames, c(ijvarlist, ijadd))

    expect_type(swi <- mcwr_switch(swi), 'list')
    expect_equal(names(swi), c('age', jivarlist, jiadd))
    # expect_equal(oldorder   , 'ij|)
    # expect_equal(neworder   , 'ji|)
    # expect_equal(oldvarnames, c(ijvarlist, ijadd))
    # expect_equal(newvarnames, c(jivarlist, jiadd))

})

test_that("mcwr_exit", {

    skip_if("mcwr_exit" %in% skiplist)

    # errors
    backup <- mcwr_1
    expect_error(jnk <- mcwr_exit(mcwr_1, age="abc"         , replace=TRUE), ".*numeric scalar.")
    expect_error(jnk <- mcwr_exit(mcwr_1, age=110           , replace=TRUE), ".*smaller than the maximum age in the data frame.")  # max age is 111
    expect_error(jnk <- mcwr_exit(mcwr_1, age=NA            , replace=TRUE), ".*numeric scalar.")
    expect_error(jnk <- mcwr_exit(mcwr_1, age=as.numeric(NA), replace=TRUE), ".*numeric scalar.")
    expect_identical(mcwr_1, backup)

    # correcting last row
    expect_error(jnk <- mcwr_exit(mcwr_1, age=111), ".*already exists.*")
    expect_s3_class(jnk <- mcwr_exit(mcwr_1, age=111, replace=TRUE), "data.frame")
    expect_equivalent(jnk, mcwr_3) # does not compare attributes, which fails b/c of attrib from Stata data sets

    expect_error(jnk <- mcwr_exit(mcwr_2, age=111, reward=1), ".*already exists.*")
    expect_s3_class(jnk <- mcwr_exit(mcwr_2, age=111, reward=5, replace=TRUE), "data.frame") # exit reward not updated
    expect_equivalent(jnk, mcwr_4)

    expect_s3_class(jnk <- mcwr_exit(mcwr_2, age=111, reward=5, replace=TRUE, update=TRUE), "data.frame")
    expect_equal(jnk[61:62,'r4_45'], c(1,5))
    jnk[,'r4_45']  <- NULL
    jnk2           <- mcwr_4
    jnk2[,'r4_45'] <- NULL
    expect_equivalent(jnk, jnk2)

    # generating last row
    expect_s3_class(jnk <- mcwr_exit(mcwr_4[-62,], age=111, reward=1), "data.frame")
    expect_equivalent(jnk, mcwr_4)

    expect_s3_class(jnk <- mcwr_exit(mcwr_4[-62,], age=111, reward=0), "data.frame")
    expect_equivalent(jnk[, -15], mcwr_4[,-15]) # exclude r4_45
    expect_equal(jnk[60:62,15], c(1,1,0))
})

test_that("mcwr_genvars", {

    skip_if("mcwr_genvars" %in% skiplist)

    # errors
    #      no r-vars
    expect_error(jnk <- mcwr_genvars(mcwr_3), "Specify arg.*") # timing incorrect
    expect_error(jnk <- mcwr_genvars(mcwr_3, timing='wrong'), ".*incorrectly specified.")
    expect_error(jnk <- mcwr_genvars(mcwr_3, timing='wrong', add=TRUE), ".*incorrectly specified.")
    expect_error(jnk <- mcwr_genvars(mcwr_3, replace=TRUE), "Specify arg.*")

    #      some r-vars
    expect_error(jnk <- mcwr_genvars(mcwr_4, timing='eop') , "Exactly one of.*")                                # add/replace missing
    expect_error(jnk <- mcwr_genvars(mcwr_4, add=TRUE)     , "Specify arg.*")                  # timing missing
    expect_error(jnk <- mcwr_genvars(mcwr_4, timing='wrong', add=TRUE)    , ".*not or incorrectly specified")   # timing wrong

    # works
    #      no r-vars / full set of r-vars
    expect_s3_class(jnk <- mcwr_genvars(mcwr_3, timing='eop', add=TRUE)  , "data.frame")
    expect_error(jnk2 <- mcwr_genvars(jnk, timing='eop'))
    expect_s3_class(jnk2 <- mcwr_genvars(jnk, timing='eop', add=TRUE)    , "data.frame")
    expect_s3_class(jnk2 <- mcwr_genvars(jnk, timing='eop', replace=TRUE), "data.frame")

    #      some r-vars
    expect_equal(mcwr_4[2:61,'r1_12'], rep.int(1,60))
    expect_s3_class(jnk <- mcwr_genvars(mcwr_4, timing='eop', add=TRUE)    , "data.frame")
    expect_equal(jnk[2:61,'r1_12'], rep.int(1,60))
    expect_s3_class(jnk <- mcwr_genvars(mcwr_4, timing='mid', replace=TRUE), "data.frame")
    expect_equal(jnk[2:61,'r1_12'], rep.int(0.5,60))
    expect_s3_class(jnk <- mcwr_genvars(mcwr_4, timing='bop', replace=TRUE), "data.frame")
    expect_equal(jnk[2:61,'r1_12'], rep.int(0,60))

    # nor, nop, order options, r-values
    expect_s3_class(jnk <- mcwr_genvars(mcwr_4, timing='eop', add=TRUE, nop=TRUE)    , "data.frame")
    expect_true(!any(c('p41', 'p42') %in% names(jnk)))
    vars_existed <- c('r1_11', 'r1_12', 'r2_22', 'r4_45')
    expect_true(all(vars_existed %in% names(jnk)))
    vars_filled  <- c('r1_14', 'r1_15', 'r1_21', 'r1_41', 'r2_12', 'r2_21', 'r2_24', 'r2_25', 'r2_42', 'r4_14', 'r4_24', 'r4_41', 'r4_42', 'r4_44')
    expect_true(all(vars_filled %in% names(jnk)))

    expect_s3_class(jnk <- mcwr_genvars(mcwr_4, nor=TRUE)    , "data.frame")
    expect_equal(ncol(jnk), 17)
    vars_existed <- c('p11', 'p12', 'p14', 'p15', 'p21', 'p22', 'p24', 'p25', 'p44', 'p45')
    expect_true(all(vars_existed %in% names(jnk)))
    vars_filled  <- c('p41', 'p42')
    expect_true(all(vars_filled %in% names(jnk)))

    chk <- mcwr_4[,ncol(mcwr_4):1]
    expect_s3_class(jnk <- mcwr_genvars(chk, nor=TRUE, nop=TRUE, order=TRUE)    , "data.frame")
        # variables will still be ordered with -order- even if -nop nor-
    expect_equal(ncol(jnk), 15)
    expect_equal(names(jnk), c('age', 'p11', 'p12', 'p14', 'p15', 'p21', 'p22', 'p24', 'p25', 'p44', 'p45', 'r1_11', 'r1_12', 'r2_22', 'r4_45'))

    jnk2 <- mcwr_4[,c('p11', 'p12')]
    names(jnk2) <- c('abc', 'def')  # vars in data frame that do not belong to model
    chk <- cbind(mcwr_4[,1:2], jnk2, mcwr_4[3:15])
    expect_s3_class(jnk <- mcwr_genvars(chk, nor=TRUE, nop=TRUE, order=TRUE)    , "data.frame")
    expect_equal(ncol(jnk), 17)
    expect_equal(names(jnk), c('age', 'p11', 'p12', 'p14', 'p15', 'p21', 'p22', 'p24', 'p25', 'p44', 'p45', 'r1_11', 'r1_12', 'r2_22', 'r4_45', 'abc', 'def'))

    # replicate bop/mid/eop using fractrg
    chk <- mcwr_6
    chk[, grep('^r._..$', names(chk))] <- NULL
    expect_s3_class(jnk <- mcwr_genvars(chk, timing='mid', add=TRUE, nop=TRUE)    , "data.frame")
    expect_equivalent(jnk, mcwr_6)

    chk <- mcwr_6
    chk[, grep('^r._..$', names(chk))] <- NULL
    expect_s3_class(jnk <- mcwr_genvars(chk, timing=0.5, nop=TRUE)    , "data.frame")
    expect_equivalent(jnk, mcwr_6)

    expect_s3_class(jnk  <- mcwr_genvars(mcwr_3, timing='bop', nop=TRUE)    , "data.frame")
    expect_s3_class(jnk2 <- mcwr_genvars(mcwr_3, timing=0    , nop=TRUE)    , "data.frame")
    expect_equivalent(jnk, jnk2)

    expect_s3_class(jnk  <- mcwr_genvars(mcwr_3, timing='eop', nop=TRUE)    , "data.frame")
    expect_s3_class(jnk2 <- mcwr_genvars(mcwr_3, timing=1    , nop=TRUE)    , "data.frame")
    expect_equivalent(jnk, jnk2)
})

test_that("mcwr_expectancies", {

    skip_if("mcwr_expectancies" %in% skiplist)

    # mcwr expectancies
    #      errors
    #          initprop() option
    expect_s3_class(jnk <- mcwr_genvars(mcwr_4, timing=1, add=TRUE), "data.frame")

    init <- c(0.2, 0.2, 0.6)
    names(init) <- c(1, 2, 4)
    init_wrong <- init
    init_wrong[1] <- 0.5
    init_wrong2 <- init
    names(init_wrong2) <- c(1, 3, 4)

    expect_error(outmats <- mcwr_expectancies(jnk, timing='mid', add=TRUE, initprop=init_wrong     ), ".*sum to 1.")
    expect_error(outmats <- mcwr_expectancies(jnk, timing='mid', add=TRUE, initprop=init_wrong[1:2]), ".*has an incorrect number of states.")
    expect_error(outmats <- mcwr_expectancies(jnk, timing='mid', add=TRUE, initprop=init_wrong2    ), ".*must be identical.*")

    #          vllmap option
    vllmap <- c(1, 2, 4)
    names(vllmap) <- c('W', 'U', 'R')

    vllmap_wrong <- vllmap[1:2]
    expect_error(outmats <- mcwr_expectancies(jnk, timing='mid', add=TRUE, vllmap=vllmap_wrong), ".*does not cover all states.")
    vllmap_wrong <- c(1, 2, 4)
    expect_error(outmats <- mcwr_expectancies(jnk, timing='mid', add=TRUE, vllmap=vllmap_wrong), ".*numeric vector with element names.")

    # #      works
    expect_type(outmats <- mcwr_expectancies(jnk, timing='mid', add=TRUE, initprop=init), 'list')

    #          compare to rwnote result
    #          init props and results numbers taken from rwnote, eur men, LE 1
    init <- c(0.9016, 0.065, 0.0334)
    names(init) <- c(1,2,4)
    expect_s3_class(jnk <- mcwr_genvars(mcwr_3, timing='mid'), "data.frame")
    expect_type(outmats <- mcwr_expectancies(jnk, initprop=init), 'list')
    cmp <- rbind( c( 9.9783 ,  1.5704 ,  0      ,  9.0982 ) ,
                  c( 1.4072 ,  9.8967 ,  0      ,  1.9122 ) ,
                  c(15.4338 , 15.3522 , 26.8192 , 15.8088 ) ,
                  c(26.8192 , 26.8192 , 26.8192 , 26.8192 ) )
    expect_true(.mcwrcert_mreldif(outmats$e, cmp)<0.0002)
        # same replication precision as Stata; 2020-12-03: visual comparison to Stata shows no difference

    expect_s3_class(jnk <- mcwr_genvars(jnk, timing='eop', replace=TRUE), "data.frame")
    expect_type(outmats <- mcwr_expectancies(jnk, initprop=init), 'list')
    cmp <- rbind( c(10.4783 ,  1.5704 ,  0      ,  9.549  ) ,
                  c( 1.4072 , 10.3967 ,  0      ,  1.9447 ) ,
                  c(15.4338 , 15.3522 , 27.3192 , 15.8255 ) ,
                  c(27.3192 , 27.3192 , 27.3192 , 27.3192 ) )
    expect_true(.mcwrcert_mreldif(outmats$e, cmp)<0.0002)

    # return values / mcwr matlist / matbrowse
    expect_type(outmats <- mcwr_expectancies(jnk, initprop=init), 'list')
    expect_equal(length(outmats), 6)
    expect_equal(names(outmats), c('e','P','F','R1','R2','R4'))
    expect_equal(rownames(outmats$e), c('e50-1', 'e50-2', 'e50-4', 'e50-total'))
    expect_equal(colnames(outmats$e), c('init-1', 'init-2', 'init-4', 'weighted'))

    MCWR <- .mcwrcert_defvll("rwnote_sh_u")
    expect_type(outmats <- mcwr_expectancies(jnk, initprop=init, vllmap=MCWR), 'list')
    expect_equal(rownames(outmats$e), c('e50-W', 'e50-U', 'e50-R', 'e50-total'))
    expect_equal(colnames(outmats$e), c('init-W', 'init-U', 'init-R', 'weighted'))
    expect_equal(rownames(outmats$R4)[c(1,183,184)], c('W:50','R:110', 'D'))
    expect_equal(colnames(outmats$F)[ c(1,182,183)], c('W:50','R:109', 'R:110'))
    expect_equal(colnames(outmats$P)[ c(1,183,184)], c('W:50','R:110', 'D'))

})



