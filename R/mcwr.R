
p_exi <- p_ful <- p_new <- r_exi <- r_ful <- r_new <- s_abs <- s_frm <- sdesc <- vll_colstripe_e <- vll_rowstripe_e <- vll_stripe_F <- NULL
    # assign value to vars that are processed with list2env()
    # to avoid "no visible binding" error messages of R CMD CHECK

############################# EXPOSED FUNCTIONS ###############################

#' Check mcwr data frame
#'
#' Check that mcwr data frame is set up correctly
#'
#' Run mcwr check to check whether your data frame is a valid mcwr data frame or whether you have to modify
#' something. It is also run internally by the other mcwr functions, so you are always safe not to be using
#' incorrectly set up data.
#'
#' A second use of the functions is to gather comprehensive information about existing and missing model
#' variables, and more.
#'
#' @param data an mcwr data.frame
#' @param ij a boolean TRUE/FALSE. If TRUE, states that data set is in ij-format.
#' @param skipexit a boolean TRUE/FALSE. If TRUE, does not check the last row of the data frame.
#' @param noexit a boolean TRUE/FALSE. If TRUE, states that the exit row is missing from the data frame.
#'
#' @return a list containing model information. Its named elements are:
#' \tabular{ll}{
#' p_exi         \tab  character vector: p-variables in the data set                                          \cr
#' p_ful         \tab  character vector: full set of p-variables implied by states                            \cr
#' p_new         \tab  character vector: p-variables that are implied by states but not in the data set       \cr
#' s_trn         \tab  numeric vector: list of transitions occuring in data set                               \cr
#' s_frm         \tab  numeric vector: list of from-states                                                    \cr
#' s_trg         \tab  numeric vector: list of target states                                                  \cr
#' s_abs         \tab  numeric vector: absorbing state                                                        \cr
#' s_omt         \tab  numeric vector: states omitted from the model                                          \cr
#' r_exi         \tab  character vector: r-variables in the data set                                          \cr
#' r_ful         \tab  character vector: full set of p-variables implied by states                            \cr
#' r_new         \tab  character vector: r-variables that are implied by states but not in the data set       \cr
#' r_trn         \tab  character vector: list of transitions covered by existing r-variables                  \cr
#' s_rcv         \tab  numeric vector: states receiving rewards                                               \cr
#' s_nrc         \tab  numeric vector: states not receiving rewards                                           \cr
#' numages       \tab  number of age classes in the model                                                     \cr
#' agelist       \tab  numeric vector: age classes of the model                                               \cr
#' ageintervals  \tab  numeric vector: list of lengths of age intervals                                       \cr
#' hasexit       \tab  TRUE/FALSE: whether data has an exit row
#' }
#'
#' @family mcwr
#' @export

mcwr_check <- function(data, ij = FALSE, skipexit=FALSE, noexit=FALSE) {

    if (!is.data.frame(data))
        stop("Input arg 'data' must be a data.frame.")

    minobs <- 3
    if (noexit) minobs <- 2
    N <- nrow(data)
    if (N<minobs)
        stop("Not enough observations.")

    vars <- names(data)
    if (!('age' %in% vars))
        stop("Column 'age' not found.")

    age <- data[,'age']

    if (any(is.na(age)))
        stop("Column 'age' has missing values.")

    if (length(unique(age))!=length(age))
        stop("Values of column 'age' must be all distinct.")

    if (!all(age==sort(age)))
        stop("Data frame must be sorted by age.")

    if (!is.logical(skipexit) || !is.logical(noexit))
        stop("Args 'skipexit' or 'noexit' must be a logical.")

    if (skipexit && noexit)
        stop("Args 'skipexit' and 'noexit' are mutually exclusive.")

    absN <- N    # row to check for absorption values
    trN  <- N-1  # last transition obs to be checked

    if (skipexit) {
        absN <- NA  # do not check absorption row
    }
    if (noexit) {
        absN <- NA  # do not check absorption row
        trN  <- N
    }

    # 'p??' variables
    statevecs <- .mcwr_statelocals(data, ij)
    list2env(statevecs, envir=as.environment(-1))
        # assign state vectors to current environment

    chk <- data[,p_exi, drop=FALSE]
    if (!all(sapply(chk, is.numeric)))
        stop("Some columns are not numeric.")

    if (!all(is.na(chk[1,])))
        stop("First row may only hold missing values.")

    if (any(is.na(chk[2:trN,])))
        stop("Transition probabilites have missing values.")

    if (!all( chk[2:trN,]>=0 & chk[2:trN,]<=1))
        stop("Some variables are outside of [0 1].")

    for (s in s_frm) {
        pat <- paste0('^p', s, '[1-9]$')
        if (ij)
            pat <- paste0('^p[1-9]', s, '$')
        allfrom <- grep(pat, p_exi, value=TRUE)
        ones <- rowSums(chk[2:trN,allfrom,drop=FALSE])
        reldif <- abs(ones-1) / (2)  # reldif() from Stata
        if (max(reldif)>1e-6)
            stop(paste("Probabilities for from-state", s, "do not sum close enough to 1."))
    }

    # check absorption row
    if (!is.na(absN)) {
        pat_toabs <- paste0('^p.'  , s_abs,  '$')
        pat_totra <- paste0('^p.[^', s_abs, ']$')
        if (ij) {
            pat_toabs <- paste0('^p'  , s_abs,  '.$')
            pat_totra <- paste0('^p[^', s_abs, '].$')
        }

        vars_toabs <- grep(pat_toabs, p_exi, value=TRUE)
        vars_totra <- grep(pat_totra, p_exi, value=TRUE)

        if ( (!all(chk[absN,vars_toabs]==1)) || (!all(is.na(chk[absN,vars_totra]))))
            stop("Problem with absorption row value(s).")
    }

    # r?_?? variables
    rewardsvecs <- .mcwr_rewardslocals(data, c(s_frm, s_abs), ij)
    list2env(rewardsvecs, envir=as.environment(-1))

    if (length(r_exi)>0) {
        chk <- data[,r_exi, drop=FALSE]

        if (!all(is.na(chk[1,])))
            stop("First row may only hold missing values.")

        if (any(is.na(chk[2:trN,])))
            stop("Rewards have missing values.")

        # check absorption row
        if (!is.na(absN)) {
            pat_toabs <- paste0('^r._.'  , s_abs,  '$')
            pat_totra <- paste0('^r._.[^', s_abs, ']$')
            if (ij) {
                pat_toabs <- paste0('^r._'  , s_abs,  '.$')
                pat_totra <- paste0('^r._[^', s_abs, '].$')
            }

            vars_toabs <- grep(pat_toabs, r_exi, value=TRUE)
            vars_totra <- grep(pat_totra, r_exi, value=TRUE)

            if ( (any(is.na(chk[absN,vars_toabs]))) || (!all(is.na(chk[absN,vars_totra]))))
                stop("Problem with absorption row value(s).")
        }
    }

    # returning model information
    agelist <- data[,'age']
    numages <- trN

    n <- data[2:nrow(data),'age'] - data[1:(nrow(data)-1),'age']
    nlast <- 0
    if (is.na(absN))
        nlast <- is.numeric(NA)
    n <- c(n, nlast)
    data[,'n'] <- n

    ageintervals <- data[1:trN,'n']
    hasexit <- !is.na(absN)

    retlist <- c(statevecs, rewardsvecs, list(numages, agelist, ageintervals, hasexit))
    names(retlist) <- c(names(statevecs), names(rewardsvecs), 'numages', 'agelist', 'ageintervals', 'hasexit')

    return(retlist)
}

#' Switch from-state and target state
#'
#' Change from-state and target state index convention of variable names
#'
#' The appendix notation follows the ij-notation, where the first index refers to the target state and the
#' second index to the from-state. This has the advantage of conforming with the conventions of matrix algebra.
#' p- and r-variables of mcwr data frames, however, generally follow the ji-convention.  The advantage of this is
#' that sorting the variables alphabetically results in a sensible and intuitive ordering. Therefore, your data
#' frame variables are required to follow the ji-convention.  The convenience function \code{mcwr_switch()} allows you to
#' switch between the two conventions.  If you have a consistent data frame in ij-format, running this function
#' will rename variables according to the ji-convention.  Your data must be in ji-format before you can run any
#' other of the mcwr functions.  As a brief example, we load data in
#' ji-format, then switch to ij-format and back:
#'
#' @param data an mcwr data.frame
#' @param ji boolean TRUE/FALSE. If TRUE, the data frame will always be in ji-format
#' after function conclusion.  That is, if the data are in ij-format
#' they will be converted to ji-format, and will be left untouched otherwise.
#' @param verbose boolan TRUE/FALSE. If TRUE, the function
#' will display verbose error messages. This is useful if the function tells you that it can neither
#' find a consistent ji data frame nor a consistent ij data frame. The error messages under option \code{verbose}
#' may give you a clue about the source of the error.
#'
#' @return A data.frame, suitable for further processing by \code{mcwr_*()} functions.
#'
#' @examples
#' data <- mcwr_exampledata(1)
#' head(data)
#' data <- mcwr_switch(data)
#' head(data)
#' data <- mcwr_switch(data)
#' head(data)
#'
#' @family mcwr
#' @export

mcwr_switch <- function(data, ji=as.logical(NA), verbose=FALSE) {

    if (!is.data.frame(data))
        stop("Input arg 'data' must be a data.frame.")

    if (!is.logical(ji) | length(ji)!=1)
        stop("Input arg 'ji' must be a logical scalar.")

    if (is.na(ji)) {
        keepji <- FALSE
    } else {
        keepji <- ji
    }

    ji <- TRUE  # symbol ji has different meaning from Stata: refers to the presumed current format, not the request to preserve ji format
    ij <- FALSE
    oldorder <- 'ji'

    silent <- !verbose

    if (verbose)
        print("Checking data frame for consistency with ji-format...")
    err <- try({
        jnk <- mcwr_check(data, skipexit=TRUE) # fails if probs do not sum to 1
    }, silent)

    if (class(err)=="try-error") {
        if (verbose) {
            print("    ...failed.")
            print("Checking data in memory for consistency with ij-format...")
        }
        err <- try({
            jnk <- mcwr_check(data, ij=TRUE, skipexit=TRUE) # fails if probs do not sum to 1
        }, silent)

        if (class(err)=="try-error") {
            if (verbose)
                print("    ...failed.\n")

            errmsg <- paste("Either data are neither in ji-format nor in ij-format,\n",
                            "or another data requirement is not fulfilled.")

            if (!verbose)
                errmsg <- paste(errmsg, "\nRe-run with 'verbose' option for more information.")

            stop(errmsg)
        }

        ij <- TRUE
        oldorder <- 'ij'
    }

    if (verbose)
        print("    ...success.")

    neworder <- 'ij'
    if (oldorder=="ij")
        neworder <- 'ji'

    statevecs   <- .mcwr_statelocals(data, ij)
    list2env(statevecs, envir=as.environment(-1))
    rewardsvecs <- .mcwr_rewardslocals(data, c(s_frm, s_abs), ij)
    list2env(rewardsvecs, envir=as.environment(-1))

    if (keepji==TRUE & oldorder=="ji") {
        # newvarnames <- c(p_exi, r_exi)
        # oldvarnames <- c(p_exi, r_exi)
        # neworder    <- oldorder
        # oldorder    <- oldorder
    } else {

        if (verbose)
            print(paste0("Switching format to ", neworder, "-format."))

        revvars <- .mcwr_revvars(paste(c(p_exi, r_exi), collapse=' '))

        vars    <- names(data)
        varsidx <- sapply(c(p_exi, r_exi), function(x) grep(x, vars))
        for (i in 1:length(revvars)) {
            names(data)[varsidx[i]] <- revvars[i]
        }

        # newvarnames <- revvars
        # oldvarnames <- c(p_exi, r_exi)
        # neworder    <- neworder
        # oldorder    <- oldorder
    }

    return(data)

    # retlist <- list(oldorder, neworder, oldvarnames, newvarnames)
    # names(retlist) <- c('oldorder', 'neworder', 'oldvarnames', 'newvarnames')
    # return(retlist)

}

#' Examine last (exit) row of data set
#'
#' \code{mcwr_exit()} allows easy editing and consistency checks of the last (exit) row of an mcwr data frame.
#'
#' \pkg{mcwr} requires that all data points that do not enter matrix calculations be set to missing in order to avoid
#' incorrectly set up data. This rule makes the last (exit) row of the mcwr data frame somewhat tedious to manage.
#' The convenience function \code{mcwr_exit()} makes it easier to create or edit the last (exit) row of the data set.
#'
#' @param data an mcwr data.frame
#' @param age a real number \emph{ageval}.
#' It specifies the age of the exit row in the data frame. It may or may not exist.
#' It may not be smaller than the largest age in the data frame.\cr\cr
#' If \emph{ageval} corresponds to the largest age in the data frame, option \code{replace} must be specified. The
#' values of the corresponding row are replaced.  Exit transition values for p-variables are set to 1.
#' Exit transition values for r-variables are left as-is if they are non-missing and option \code{update} is
#' not used. Otherwise they are set to \emph{rwval}.  Values of all other transitions are set to missing.\cr\cr
#' If ageval is larger than the largest age in the data frame, a new row will be inserted.  Exit
#' transitions are set to 1 for p-variables and to rwval for r-variables.  Values of all other
#' transitions are set to missing.
#' @param replace boolean TRUE/FALSE. See option \code{age}.
#' @param update boolean TRUE/FALSE. See option \code{age}.
#' @param rewards a real number \emph{rwval}.
#' It determines the rewards value for exit transitions.
#'
#' @return A data.frame, suitable for further processing by \code{mcwr_*()} functions.
#'
#' @family mcwr
#' @export

mcwr_exit <- function(data, age, replace=FALSE, update=FALSE, rewards = 0) {

    if (!is.data.frame(data))
        stop("Input arg 'data' must be a data.frame.")

    if (!(is.numeric(age) & length(age)==1) | is.na(age))
        stop("Arg 'age' must be a numeric scalar.")

    if (!(is.logical(replace) && is.logical(update)))
        stop("Args 'replace' and 'update' must be logicals.")

    if (!(is.numeric(rewards) & length(rewards)==1) | is.na(rewards))
        stop("Arg 'rewards' must be a numeric scalar.")

    maxage <- max(data[,'age'])
    if (age<maxage)
        stop("Arg 'age' may not be smaller than the maximum age in the data frame.")

    ismaxage <- (age==maxage)

    if (ismaxage) {
        if (!replace)
            stop(paste0("Age ", age, " already exists.\n",
                        "Use arg 'replace' to allow modification of any incorrect values."))

        modelvecs <- mcwr_check(data, skipexit=TRUE)
    }
    else {  # age>agemax
        modelvecs <- mcwr_check(data, noexit=TRUE)

        data[nrow(data)+1,] <- as.numeric(NA)
        data[nrow(data),'age'] <- age
    }
    p_exi <- modelvecs$p_exi
    r_exi <- modelvecs$r_exi
    s_abs <- modelvecs$s_abs

    N <- nrow(data)

    # set p-vars
    pat_toabs  <- paste0('^p.'  , s_abs,  '$')
    pat_totra  <- paste0('^p.[^', s_abs, ']$')
    vars_toabs <- grep(pat_toabs, p_exi, value=TRUE)
    vars_totra <- grep(pat_totra, p_exi, value=TRUE)
    data[N,vars_toabs] <- 1
    data[N,vars_totra] <- as.numeric(NA)

    # set r-vars
    pat_totra  <- paste0('^r._.[^', s_abs, ']$')
    vars_totra <- grep(pat_totra, r_exi, value=TRUE)
    data[N,vars_totra] <- as.numeric(NA)

    pat_toabs  <- paste0('^r._.'  , s_abs,  '$')
    vars_toabs <- grep(pat_toabs, r_exi, value=TRUE)
    if (ismaxage) {
        if (update) {
            data[N,vars_toabs] <- rewards
        } else {
            vars_miss <- names( data[N, is.na(data[N,] )] ) # colnames with missings in last obs
            vars_repl <- vars_miss %in% vars_toabs
            data[N,vars_repl] <- rewards
        }
    } else {
        data[N,vars_toabs] <- rewards
    }

    return(data)
}

#' Generate mcwr variables
#'
#' Generate (additional) transition probability or rewards variables
#'
#' The main purpose of this function is to generate rewards variables (r-variables).  It examines existing p-
#' and r-variables, determines the implied full set of states, and generates any missing variables that are
#' missing from the data frame.  It interacts flexibly with existing r-variables:  You can leave them unchanged or
#' have them replaced.
#'
#' An effective way to create r-variables may be to generate a full set of r-variables using \code{mcwr_genvars()} and
#' then edit them where necessary.  This is illustrated under in the examples section of \link{mcwr}.
#'
#' @param data an mcwr data.frame
#' @param timing a character or numeric scalar.
#' It specifies how rewards are distributed to from and target states. It is
#' required if option \code{nor} is not used.  timespec can be one of \sQuote{bop}, \sQuote{mid}, and \sQuote{eop}, which stands for
#' \sQuote{beginning-of-period}, \sQuote{mid-period}, and \sQuote{end-of period}, respectively.  Alternatively, it can also
#' be a number in the interval [0 1] that specifies the fraction of the interval that goes to the
#' from-state.  Values of 0, 0.5, and 1 correspond to \sQuote{beginning-of-period}, \sQuote{mid-period}, and
#' \sQuote{end-of-period}, respectively.
#' @param add a boolean TRUE/FALSE. If TRUE, existing r-variables are left unchanged.
#' @param replace add a boolean TRUE/FALSE. If TRUE, existing r-variables are replaced.
#' @param nop a boolean TRUE/FALSE. If TRUE, not generate any p-variables.
#' By default, all missing p-variables are generated. Since existing
#' p-variables must satisfy the sums-to-unity condition, only p-variables that are (by implication)
#' all-zero can be missing. \code{mcwr_expectancies()} will run whether such redundant variables exist or not.
#' @param nor a boolean TRUE/FALSE. If TRUE, does not generate any r-variables.
#' @param order a boolean TRUE/FALSE. If TRUE, orders variables alphabetically according to the column list 'age p* r*'.
#'
#' @return A data.frame, suitable for further processing by \code{mcwr_*()} functions.
#'
#' @family mcwr
#' @export

mcwr_genvars <- function(data, timing='', add=FALSE, replace=FALSE, nop=FALSE, nor=FALSE, order=FALSE) {

    if (!is.data.frame(data))
        stop("Input arg 'data' must be a data.frame.")

    if (!(is.logical(add) & is.logical(replace) & is.logical(nop) & is.logical(nor) & is.logical(order)))
        stop("Some arg that needs to be a logical is not.")

    modelvecs <- mcwr_check(data)
    list2env(modelvecs, envir=as.environment(-1))

    N <- nrow(data)

    if (!nop) {
        data[2:(N-1), p_new] <- 0
        p_abs <- grep(paste0('p_.', s_abs), p_new, value=TRUE)
        data[N, p_abs] <- 1
    }

    if (!nor) {

        if (timing=="")
            stop("Specify arg 'timing()'.")

        if (length(r_exi)>0) {
            if ((add & replace) | (!add & !replace))
                stop("Exactly one of args 'add' or 'replace' must be TRUE.")
        }

        if (is.character(timing)) {
            if (sum(grepl(timing, c("bop", "mid", "eop")))!=1 | timing=='')
                stop("Arg 'timing' not or incorrectly specified.")
            if (timing=="bop") fractrg <- 0
            if (timing=="mid") fractrg <- 0.5
            if (timing=="eop") fractrg <- 1
        } else if (is.numeric(timing)) {
            fractrg <- timing
            if (length(timing)!=1 | any(is.na(timing)))
                stop("If numeric, arg 'timing' must be a (non-missing) scalar.")

            if (fractrg<0 | fractrg>1)
                stop("Number in arg 'timespec()' must be in the range [0 1].")

        } else {
            stop("Arg 'timing' incorrectly specified.")
        }

        newvars <- r_new
        if (replace) {
            newvars <- r_ful
            vars_drop <- grep('^r._..$', names(data), value=TRUE)
            if (length(vars_drop)>0)
                data[,vars_drop] <- NULL
        }

        data[,'nlagged'] <- c(as.numeric(NA), data[2:N,'age'] - data[1:(N-1),'age'])

        data[,newvars] <- as.numeric(NA)
        data[2:(N-1),newvars] <- 0
        for (v in newvars) {
            rwd <- substr(v, 2, 2)
            frm <- substr(v, 4, 4)
            trg <- substr(v, 5, 5)

            # remember that the rewards assignment statemente below critically hinge
            #   on the convention that only rewards for from/target states are allowed / present / used
            # remember that in all cases, rewards are distriuted for the previous period only ([_n-1] / nlagged)

            # fractrg : trg-state reward; fractrg=0 => bop;  fractrg=0.5 => mid; fractrg=1 => eop
            if (rwd==frm)   data[2:(N-1),v] <-                      fractrg  * data[2:(N-1),'nlagged']
            if (rwd==trg)   data[2:(N-1),v] <- data[2:(N-1),v] + (1-fractrg) * data[2:(N-1),'nlagged']
            if (trg==s_abs) data[N      ,v] <- fractrg * data[N,'nlagged']
        }

        data[,'nlagged'] <- NULL
    }

    if (order) {
        vars_mcwr <- c('age', p_ful, r_ful)
        vars_frst <- intersect(vars_mcwr, names(data))
        vars_last <- setdiff(names(data), vars_mcwr)
        data <- data[,c(vars_frst, vars_last)]
    }

    return(data)
}


#' Calculate expectancies
#'
#' Calculate state and overall expectancies. This is the function that does the actual calculations.
#'
#' @param data an mcwr data.frame
#' @param initprop a numeric vector. It supplies information about the initial state fractions at baseline age.
#' Its elements must be in the interval [0 1] and sum to 1.
#' It must have element names corresponding to all from-states in the model, in ascending order.
#' For example, If your model contains from-states 1, 2, and 7,
#' initspec must be a 3-element numeric vector, specifying the initial proportion of each state in turn.
#' Its names attribute must consist of the sequential from-states ('1 2 7' in the above example).
#' @param vllmap a numeric vector.
#' Its elements must have the from-states of the model as a subset.
#' If elements of the vector have names, they are used in labelling output.
#' @param ... options \code{timing}, \code{add}, and \code{replace}. See \code{\link{mcwr_genvars}}.\cr\cr
#' In most cases, you do not have to create a full set of r-variables using function \code{mcwr_genvars()}.
#' r-variables that correspond to timings that can be accommodated by the \code{timing} option can be created
#' automatically, behind the scenes.  You do so by specifying the \code{timing} option in function
#' expectancies instead in the function genvars.  Any missing r-variables (and p-variables, for that
#' matter) will then be created behind the scenes before calculations are done.  They will get deleted
#' before the function concludes.\cr\cr
#' r-variables with more complicated timings have to be created explicitly before running function
#' \code{mcwr_expectancies()}.
#'
#' @return a list of matrices that have been involved in the calculations.
#' Matrix names are e, P, F, R#.
#' Matrix e contains the overall results.
#'
#' @family mcwr
#' @export

mcwr_expectancies <- function(data, initprop = NULL, vllmap = NULL, ...) {

    origvars <- names(data)

    statevecs <- .mcwr_statelocals(data)
    list2env(statevecs, envir=as.environment(-1))

    vllvecs <- .mwcr_vlldefs(data, c(s_frm, s_abs), vllmap) # defines sdesc, for which sdesc['s'] always works (s is a state)
    list2env(vllvecs, envir=as.environment(-1))

    if (!is.null(initprop)) {
        if (!is.numeric(initprop) || !is.vector(initprop))
            stop("Arg 'initprop' must be a (non-missing) numeric vector.")

        initnames <- names(initprop)
        if (is.null(initnames))
            stop("Arg 'initprop' must have element names.")

        if (length(initprop)!=length(s_frm))
            stop("Arg 'initprop' has an incorrect number of states.")

        if (!all(initnames==s_frm))
            stop("Element names of arg 'initprop' must be identical to ascending numeric encodings of from-states.")

        reldif <- abs(sum(initprop)-1) / (2)  # reldif() from Stata
        if (reldif>1e-3)
            stop("Elements in option 'initprop()' do not sum to 1.")
    }

    if (length(list(...))>0)
        data <- mcwr_genvars(data, order=TRUE, ...)

    mcwr_check(data)

    statevecs <- .mcwr_statelocals(data)
    list2env(statevecs, envir=as.environment(-1))
    rewardsvecs <- .mcwr_rewardslocals(data, c(s_frm, s_abs))
    list2env(rewardsvecs, envir=as.environment(-1))

    if (length(c(p_new, r_new))>0)
        stop("Transition probability or rewards variables missing.")
    lexplist <- .mcwr_lexp(data, c(s_frm, s_abs), initprop)

    P_last <- sdesc[as.character(s_abs)]
    names(P_last) <- NULL # sdesc has element names, which would go into matrix dimnames attributes
    rownames(lexplist$e) <-   vll_rowstripe_e
    colnames(lexplist$e) <-   vll_colstripe_e
    rownames(lexplist$F) <-   vll_stripe_F
    colnames(lexplist$F) <-   vll_stripe_F
    rownames(lexplist$P) <- c(vll_stripe_F, P_last)
    colnames(lexplist$P) <- c(vll_stripe_F, P_last)
    for (i in 1:(length(lexplist)-3)) {
        r <- s_frm[i]
        rownames(lexplist[[paste0('R', r)]]) <- c(vll_stripe_F, P_last)
        colnames(lexplist[[paste0('R', r)]]) <- c(vll_stripe_F, P_last)
    }

    return(lexplist)
}

#' Load mcwr example data
#'
#' This function is used in examples sections and loads mcwr example data.
#' It only exists for documentation purposes.
#'
#' @param snipnum a integer [1-9]. Determines the data frame returned.
#'
#' @return A data.frame, suitable for further processing by \code{mcwr_*()} functions.
#'
#' @export

mcwr_exampledata <- function(snipnum) {

    if (!is.numeric(snipnum) | !length(snipnum)==1 | is.na(snipnum) | !(snipnum %in% c(1:9)))
        stop("Arg 'snipnum' must be an integer between 1 and 9.")

    if (snipnum==1) return(mcwr_ret_3s_1y)
    if (snipnum==2) return(mcwr_ltb_1s_1y)
    if (snipnum==3) return(mcwr_ltb_1s_5y)

    if (snipnum==4) {
        data <- mcwr_exampledata(1)
        data[,'p11'] <- data[,'p11'] + data[,'p12']
        data[,c('p12', 'p21', 'p22', 'p24', 'p25')] <- NULL
    }
    if (snipnum==5) {
        data <- mcwr_exampledata(4)
        data <- mcwr_genvars(data, timing='mid', nop=TRUE)
    }
    if (snipnum %in% c(6,7)) {
        data <- mcwr_exampledata(snipnum - 4)

        data <- rbind(data[1,], data)
        data[1,c('p11','p12','ax')] <- as.numeric(NA)

        data[,'age'] <- c(data[2:nrow(data),'age'], data[nrow(data),'age'] + 1)

        data[nrow(data),'p11'] <- as.numeric(NA)
    }
    if (snipnum %in% c(8,9)) {
        data <- mcwr_exampledata(snipnum - 2)
        data <- mcwr_genvars(data, timing='mid')
    }

    return(data)

}

############################# INTERNAL FUNCTIONS ###############################

.mcwr_statelocals <- function(data, ij = FALSE) {

    if (!(ij %in% c(TRUE, FALSE)))
        stop("arg 'ij' incorrectly specified")

    # TODO: check input arg data types

    vnames <- names(data)

    pnames <-           grep('^p[^0-9].$', vnames, ignore.case=FALSE, value=TRUE)
    pnames <- c(pnames, grep('^p.[^0-9]$', vnames, ignore.case=FALSE, value=TRUE))
    if (length(pnames)>0)
        stop("3-char 'p??' columns must have '??' as numeric digits.")

    pnames <- grep('^p[0-9][0-9]$', vnames, ignore.case=TRUE, value=TRUE)
    if (length(pnames)==0)
        stop("no 'p??' variables in data frame")

    if (any(substr(pnames, 2, 2)=="0") || any(substr(pnames, 3, 3)=="0"))
        stop("State encoding in 'p??' column names must be 1-9. 0 is not allowed.")

    s_trn <- as.numeric(substr(pnames, 2, 3))

    if (!ij) {  # default ji variables : sort pji in order j-i
        s_trn <- sort(s_trn)
        s_frm <- unique(floor(s_trn/10))
        s_trg <- unique(s_trn %% 10)
    } else {  # ij variables : sort pij in order j-i
        s_trn <- s_trn[order(s_trn %% 10, floor(s_trn/10))]
        s_trg <- sort(unique(floor(s_trn/10)))  # not sorted if p-vars are missing
        s_frm <- sort(unique(s_trn %% 10))
    }

    s_abs <- max(s_trg)
    s_jnk <- max(s_frm)
    if (s_jnk>=s_abs)
        stop(paste0("Largest target state (", s_abs,
                   ") not bigger than largest from-state (", s_jnk, ")."))

    s_omt <- setdiff(seq(1,s_abs), c(s_frm, s_trg))
        # setdiff(A,B) returns elems of A not in B
        # numeric(0) or character(0), if emtpy

    # require that all target states (except for the absorbing state) must also exist as a from-state
    if (length(setdiff(s_trg[1:(length(s_trg)-1)], s_frm))>0)
        stop("Transitions are incomplete. Some from-states are not in the data set.")
        # it does not make sense to specify a target state and have no out-transitions from that state.
        # Hence, I require that all from-states exist in the data set, and that their probs sum to one.

    # lists of existing, full, and new trans vars,
    p_exi <- paste0("p", s_trn)

    m_jnk <- expand.grid(c(s_frm, s_abs), s_frm)[,c(2,1)]

    if (ij)
        m_jnk <- m_jnk[,c(2,1)]
    p_ful <- paste0('p', m_jnk[,1], m_jnk[,2])

    p_new <- setdiff(p_ful, p_exi)

    statevecs <- list(p_exi, p_ful, p_new, s_trn, s_frm, s_trg, s_abs, s_omt)
    names(statevecs) <- c('p_exi', 'p_ful', 'p_new', 's_trn', 's_frm', 's_trg', 's_abs', 's_omt')
        # p_exi : p-variables, existing
        # p_ful : p-variables, full model-implied list
        # p_new : p-variables that are model-implied but not existing

        # s_trn : transitions   that occur in existing p-variables
        # s_frm : from-states   that occur in existing p-variables
        # s_trg : target-states that occur in existing p-variables
        # s_abs : absorbing state (scalar)
        # s_omt : does not occur in the data set, will be omitted from analysis

        # all vectors are sorted
        # sorted means w.r.t to ji/ij convention, with the from-states index being slower than the to-states index
        # empty vectors are character(0) or integer(0)

    return(statevecs)
}

.mcwr_rewardslocals <- function(data, s_all, ij = FALSE) {

    if (!(ij %in% c(TRUE, FALSE)))
        stop("arg 'ij' incorrectly specified")

    # TODO: check input arg data types (not really necessary b/c function is internal)
    #   data  : data frame
    #   s_all : numeric vector (NOT a string scalar!)
    #   ij    : TRUE/FALSE

    s_frm <- s_all[1:length(s_all)-1]
    s_abs <- s_all[length(s_all)]

    vnames <- names(data)

    rnames <-           grep('^r[^0-9]_..$', vnames, ignore.case=FALSE, value=TRUE)
    rnames <- c(rnames, grep('^r._[^0-9].$', vnames, ignore.case=FALSE, value=TRUE))
    rnames <- c(rnames, grep('^r._.[^0-9]$', vnames, ignore.case=FALSE, value=TRUE))
    if (length(rnames)>0)
        stop("5-char 'r?_??' columns must have all '?' as numeric digits.")

    rnames <- grep('^r[0-9]_[0-9][0-9]$', vnames, ignore.case=FALSE, value=TRUE)
        # character(0) if no match

    if (length(rnames)>0) {

        m_jnk <- cbind(as.numeric(substr(rnames, 2, 2)), as.numeric(substr(rnames, 4, 4)), as.numeric(substr(rnames, 5, 5)))
        if (any(m_jnk==0))
            stop("State encoding in 'r?_??' column names must be 1-9. 0 is not allowed.")

        r_trn <- unique(as.numeric(substr(rnames, 4, 5)))

        if (!ij) {  # default ji variables : sort pji in order j-i
            r_trn <- sort(r_trn)
        } else {  # ij variables : sort pij in order j-i
            r_trn <- r_trn[order(r_trn %% 10, floor(r_trn/10))]
        }

        # lists of existing, full, and new rewards vars,
        r_all = unique(as.vector(m_jnk))  # all states mentioned in rewards vars must be in s_all
        if (!all(r_all %in% s_all))
            stop("Some 'r?_??' columns are inconsistent with the set of 'p??' columns." )

        if (!ij) {  # default ji variables : sort rr_ji in order r-j-i
            r_exi = sort(rnames)
        } else {       # ij variables         : sort rr_ij in order r-j-i
            m_jnk = m_jnk[order(m_jnk[,1], m_jnk[,3], m_jnk[,2]),]
            r_exi = paste0("r", as.character(m_jnk[,1]), "_", as.character(m_jnk[,2]), as.character(m_jnk[,3]))
        }

        # check that rewards only go to from or target states
        if ( any(rowSums(cbind(m_jnk[,1, drop=FALSE], m_jnk[,1, drop=FALSE])==m_jnk[,2:3, drop=FALSE])==0) )
            stop("Some 'r?_??' variables reward states other than from/target states." )
    }
    else {
        r_exi <- character(0)
        r_trn <- integer(0)
    }


    m_jnk <- expand.grid(s_all, s_frm, s_frm)[,c(3,2,1)]
    if (ij)
        m_jnk = m_jnk[,c(1,3,2)]
    m_jnk = m_jnk[(rowSums(cbind(m_jnk[,1, drop=FALSE], m_jnk[,1, drop=FALSE])==m_jnk[,2:3, drop=FALSE])>=1 ),]
    r_ful = paste0("r", as.character(m_jnk[,1]), "_", as.character(m_jnk[,2]), as.character(m_jnk[,3]))

    if (length(rnames)==0) {
        r_new <- r_ful
        r_rcv <- integer(0)
        r_nrc <- s_frm
    } else {

        r_new <- setdiff(r_ful, r_exi)

        # rewards (non-)receiving states
        r_rcv = sort(unique(as.numeric(substr(rnames, 2, 2))))
        r_nrc <- setdiff(s_frm, r_rcv)
    }

    rewardslists <- list(r_exi, r_ful, r_new, r_trn, r_rcv, r_nrc)
    names(rewardslists) <- c('r_exi', 'r_ful', 'r_new', 'r_trn', 'r_rcv', 'r_nrc')
        # r_exi : r-variables, existing
        # r_ful : r-variables, full model-implied
        # r_new : r-variables that are model-implied but not existing

        # r_trn : transitions that occur in existing r-variables
        # r_rcv : rewards     receiving states
        # r_nrc : rewards non-receiving states

        # see comments for return values of .mcwr_statelocals()
        # additional remark for sort order: rewards index is moving the slowest

    return(rewardslists)
}

.mcwr_revvars <- function(vlist) {

    if (!(is.character(vlist)))
        stop("Argument 'vlist' must be a character vector.")

    vlist <- trimws(vlist)
    if (length(vlist)==1) {
        if (nchar(vlist)==0)
            return(vlist) # returns either '' or character(0)
        vars <- strsplit(vlist, ' ')[[1]]
            # strsplit() returns a list
            # the above is not robust to repeated sep tokens, as in 'a   b'
    } else {
        vars <- vlist
    }
    vlen <- nchar(vars)

    revvars <- paste0(substr(vars, 1, vlen-2), substr(vars, vlen, vlen), substr(vars, vlen-1, vlen-1))
        # works for pure lists of p-/r-vars and for mixtures thereof

    return(revvars)
}

.mcwr_lexp <- function(data, s_all, initprop = NULL) {
# calculates lexp, P, F, R`r'
# s_all : numeric vector (NOT a string scalar!)

    data <- as.matrix(data)

    s_frm <- s_all[1:(length(s_all)-1)]
    s_abs <- s_all[length(s_all)]
    tau <- length(s_frm)
    om  <- nrow(data) - 1  # omega as defined in paper

    # j is from state, always!
    # good to remember this rule, especially b/c of complications of ji/ij conventions in -mcwr- and paper
    # example variables in code are always in the default ji-format

    P <- matrix(0, nrow=tau*om+1, ncol=tau*om+1)
    jidx <- 1
    for (j in 1:tau) {
        vars <- paste0("p", (rep.int(s_frm[j]*10, tau) + s_frm))
        pdata <- data[2:om, vars, drop=FALSE]
        iidx <- 1
        for (i in 1:tau) {
            P[iidx:(iidx+om-1),jidx:(jidx+om-1)] <- .ds_gdiag(pdata[,i], -1)
            iidx <- iidx + om
        }
        jidx <- jidx + om
    }
    vars <- paste0("p", (10*s_frm + s_abs))
    pdata <- data[2:(om+1),vars]
    P[iidx,1:jidx] <- cbind(matrix(pdata,nrow=1) , 1)

    F <- solve(diag(1,tau*om,tau*om) - P[1:(tau*om),1:(tau*om)])  # TODO: check for invertibility first!!

    Rlist <- list()
    for (r in 1:tau) {
        # algo makes use of the fact that rewards only occur for from- or target states
        #   other submatrices are always zero => no nested i/j loop

        X <- matrix(0, nrow=tau*om+1, ncol=tau*om+1)
        vars  <- paste0("r", s_frm[r], "_", (rep.int(s_frm*10, tau) + s_frm[r]))  # e.g. r2_12, r2_22, r2_42
        rdata <- data[2:om,vars, drop=FALSE]
        iidx  <- om * (r-1) + 1
        jidx  <- 1
        for (j in 1:tau) {
            X[iidx:(iidx+om-1),jidx:(jidx+om-1)] <- .ds_gdiag(rdata[,j], -1)
            jidx <- jidx + om
        }

        vars  <- paste0("r", s_frm[r], "_", (rep.int(s_frm[r]*10, tau) + s_frm))  # e.g. r2_21, r2_22, r2_24
        rdata <- data[2:om,vars, drop=FALSE]
        jidx  <- om * (r-1) + 1
        iidx  <- 1
        for (i in 1:tau) {
            X[iidx:(iidx+om-1),jidx:(jidx+om-1)] <- .ds_gdiag(rdata[,i], -1)
            iidx <- iidx + om
        }

        vars  <- paste0("r", s_frm[r], "_", s_frm[r]*10 + s_abs) # e.g. r2_25
        rdata <- data[2:(om+1),vars, drop=FALSE]
        e     <- matrix(0, 1, tau)  # careful e <-> lexp; later, e is the returned lexp matrix
        e[r]  <- 1

        X[tau*om+1,1:(tau*om+1)] <- cbind(e %x% t(rdata) , 0)
        Rlist[[r]] <- X
    }

    lexp <- matrix(as.numeric(NA), 0, tau)
    for (r in 1:tau) {
        lexp <- rbind(lexp , (t(F) %*% (t(colSums( P*Rlist[[r]] )))[1:(tau*om)])[seq(1, 1+(tau-1)*om, om)] )
    }

    if (tau>1) {
        lexp <- rbind(lexp , colSums(lexp))
        lexp <- cbind(lexp , matrix(as.numeric(NA), nrow(lexp), 1))

        if (!is.null(initprop)) {
            initprop <- as.matrix(initprop)
            dim(initprop) <- c(1, length(initprop))
            lexp[,tau+1] <- rowSums(lexp[,1:tau] * (initprop %x% matrix(1, nrow(lexp),1)) )
        }
    }

    e <- lexp
    retlist <- c(list(e, P, F), Rlist)
    names(retlist) <- c('e','P', 'F', paste0('R', s_frm))

    return(retlist)
}

.mwcr_vlldefs <- function(data, s_all, vllmap = NULL) {
# does all symbol defs related to vll MCWR
# s_all  : numeric vector (not a string scalar!)
#          must have elem names, all states must be present, can be unsorted
# vllmap : numeric vector, names are state descriptors

    om  <- nrow(data) - 1
    tau <- length(s_all) - 1

    if (!is.null(vllmap)) {
        if (!is.numeric(vllmap) || !is.vector(vllmap) || is.null(names(vllmap)))
            stop("Arg 'vllmap' must be a numeric vector with element names.")

        if (!all(s_all %in% vllmap))
            stop("Arg 'vllmap' does not cover all states.")

        jnk <- sort(vllmap[vllmap %in% s_all])
        sdesc        <- names(jnk)
        names(sdesc) <- jnk
    }
    else {
        sdesc <- as.character(s_all)
    }
    names(sdesc) <- as.character(s_all)
        # sdesc:
        # - elems are characters: either useful descriptions or states converted to chars
        # - vector names are always state converted to chars
        #   so it always works to index the vec as in sdesc['4']
        # - sole current purpose (2020-12-03) is to label output matrices

    agedesc <- data[1:om,'age']
    jnk <- expand.grid(agedesc, sdesc[1:tau])[c(2,1)]

    vll_stripe_F <- paste(jnk[,1], jnk[,2], sep=':')
    if (tau==1) {
        vll_rowstripe_e <- paste0('e', agedesc[1], '-', sdesc[1:tau])
        vll_colstripe_e <- paste0('init-', sdesc[1:tau])
    } else {
        vll_rowstripe_e <- paste0('e', agedesc[1], '-', c(sdesc[1:tau], 'total'))
        vll_colstripe_e <- c(paste0('init-', sdesc[1:tau]), 'weighted')
    }

    retlist <- list(vll_stripe_F, vll_rowstripe_e, vll_colstripe_e, sdesc)
    names(retlist) <- c('vll_stripe_F', 'vll_rowstripe_e', 'vll_colstripe_e', 'sdesc')

    return(retlist)
}

############################### R-SPECIFIC #####################################

.ds_gdiag <- function(vecin, pos) {
# creates a square matrix with the elems of vecin on subdiagonal pos and
#   zeroes elsewhere;
# negative (positive) values of pos refer to subdiagonals below (above)
#   the main diagonal

    n <- length(vecin)

    if (pos==0) {
        return(diag(vecin))
    } else if (pos<0) {
        return( rbind(matrix(0, nrow=-pos, ncol=n-pos) , cbind(diag(vecin) , matrix(0, nrow=n   , ncol=-pos))) )
    } else if (pos>0) {
        return( cbind(matrix(0, nrow=n+pos, ncol=pos)  , rbind(diag(vecin) , matrix(0, nrow=pos , ncol=n   ))) )
    }
}


