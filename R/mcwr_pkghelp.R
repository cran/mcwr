#' mcwr: Markov chain with rewards calculations
#'
#' mcwr implements Markov chain with rewards calculations.
#' It accompanies the article:\cr\cr
#'   Schneider / Myrskyla / van Raalte (2021):\cr
#'     Flexible Transition Timing in Discrete-Time Multistate Life Tables\cr
#'     Using Markov Chains with Rewards. MPIDR Working Paper WP-2021-002.\cr
#'     Available \href{https://www.demogr.mpg.de/en/publications_databases_6118/publications_1904/mpidr_working_papers/flexible_transition_timing_in_discrete_time_multistate_life_tables_using_markov_chains_with_rewards_6932/}{here}.\cr\cr
#' In the \code{mcwr} help entries, this
#' article will be referred to as "the paper" and its appendix as "the appendix".
#' 
#' @section Overview:
#' The main function, which does the actual calculations, is \code{\link{mcwr_expectancies}}.
#' The other functions either check the data for consistency or
#' carry out data management tasks.
#' 
#' Running mcwr demands a very specific data frame (called mcwr data frame, laid out in section \sQuote{Data
#' setup} below).  Several data management functions aide in creating such a specific data frame.
#' 
#' The main data management function is \code{\link{mcwr_genvars}}, which generates p- and r- variables that are missing
#' from the data frame.  These variables specify transition probabilities and rewards.  Other data management
#' functions check the data frame for consistency (\code{\link{mcwr_check}}), transform variable names (\code{\link{mcwr_switch}}), or check or
#' create an exit row (\code{\link{mcwr_exit}}).
#' 
#' @section Abbreviations and definitions used in this help entry:
#'
#' \describe{
#' \item{MCWR}{Markov chain with rewards}
#' \item{p-variables}{Variables whose name is of the format 'p##'.  They contain transition
#' probability data.  The numbers encode from and target state.  The default for
#' this number pair is the ji-format (see below).  For example, variable 'p12'
#' has transition probabilities from-state 1 to state 2.}
#' \item{r-variables}{Variables whose name is of the format 'r#_##'.  They contain rewards data.  The
#' first number encodes the state that receives the reward.  The second and third
#' numbers encode from and target state.  The default for this number pair is the
#' ji-format (see below).}
#' \item{ji/ij-format}{Variable (names) are said to be in ji-format if the first number of the variable
#' name specifies the from-state and the second number specifies the target
#' state.  The index j always denotes the from-state.  Consequently, variable
#' (names) in ij-format state the target state first and then the from-state.}
#' \item{from/target/initial states}{From-state and target state are the two states that are connected by a state
#' transition. The initial state is the state at baseline age.}
#' \item{mcwr data frame}{A data frame that fulfills all requirements for mcwr to run on it without error.
#' The structure of the data frame is set forth in section Data setup.}
#' }
#' 
#' @section Data setup:
#' 
#' \pkg{mcwr} requires the transition data to be in a very specific format, called 'mcwr data frame' in this help entry.
#' To give you a quick idea of what is required, execute:
#' 
#' \preformatted{
#' data <- mcwr_exampledata(1)
#' head(data)
#' tail(data)
#' }
#' 
#' In general, the following rules and conventions apply:
#' 
#' \itemize{
#' \item Transition probabilities are specified in p-variables.  The default convention for specifying from and
#' target states is the ji-format:  The first number encodes the from-state, the second number the target
#' state.
#' \item Rewards are specified in r-variables.  The numbers occuring in the variable names specify the rewarded
#' state, the from-state, and the target state, respectively.
#' \item A maximum number of 9 states (including the absorbing state) is allowed.  States must be encoded using
#' numbers 1-9. 0 is not allowed.
#' \item Only a single absorbing state is allowed. It is encoded by the highest number occuring for all states. In
#' the example, 5 is the aborbing state.
#' \item States can be non-contiguous. In the example, states 2 and 3 are missing from the model. The states of
#' the model are 1, 4, 5.
#' \item Transition probabilities must sum to 1. For example, columns p11, p14, p15 sum to 1.
#' \item As long as the sums-to-unity condition is satisfied, not all p-variables must be present.  In the
#' example, variable p41 is missing, and all-zero by implication.
#' \item Age (or, more general, time) is specified in a variable called 'age'.
#' \item Irregular age intervals are allowed. The Examples section illustrates this with a life table on a
#' 'demographic' 5-year age grid (childhood age intervals are shorter).
#' \item The first row of the data frame specifies the baseline age. All p- and r-variables must have missing values
#' in the first row.
#' \item The last row of the data frame corresponds to the exit age. At this age, all subjects are required to die
#' (enter the absorbing state). In the example, variables p15 and p45 are set to 1 at age 111. All other
#' transitions at the exit age must be set to missing.
#' \item In general, whenever values of a mcwr data frame are certain to not enter matrix calculations, they must be
#' set to missing in the data frame. Conversely, data points that are certain to enter matrix calculations
#' must never be missing.
#' \item The first transition takes place in row 2 (age 51 in the example).  It is important to keep in mind that
#' age specifies a point in time, not an interval.  It is the point in time when the subject turns 51.
#' \item At this point, rewards can be distributed for the previous age (interval).  Standard Markov chain
#' calculations would distribute occupancy times end-of-period (the transition takes place at exact age 51).
#' By contrast, in the example we distribute time rewards and assume mid-period transitions, i.e. we assume
#' that state transitions take place at ages 50.5, 51.5, etc. The reward for state 1 of the 1->4 transition
#' at age 51 is specified to be 0.5. This covers the period [50 50.5).  The reward for state 4 of the 1->4
#' transition at age 51 is also specified to be 0.5. This covers the period [50.5 51). From the same logic,
#' staying in the same state carries a reward of 1. It is made up of rewards of 0.5 for each one of the
#' periods [50 50.5) and [50.5 51), respectively.
#' \item Rewards can only flow to from or target states.  For example, a variable 'r3_12' in the data frame will be
#' flagged as an error.
#' \item Rewards can only flow to transient states.  In the example, where the absorbing state is 5, a variable
#' 'r5_45' will generate an error.
#' \item Rewards can take on any numeric value, including negative ones.
#' \item You may have additional variables (variables other than p- and r-variables) in the data frame.  They will
#' be ignored.
#' }
#' 
#' As a minor remark on the example data frame, it is derived from the SHARE application in the paper. In that
#' application, state 1 is left with certainty when turning 71, with no return possible.  The values shown in
#' the data frame for p14 and p15 for ages 71 and higher are therefore immaterial, given that this state is never
#' occupied at these ages.
#' 
#' @section Limitations:
#' 
#' At the risk of redundancy, here are the limitations that \pkg{mcwr} places on the model setup:
#' 
#' \itemize{
#' \item The maximum number of states is 9 (including the absorbing state).
#' \item Only a single absorbing state is allowed.
#' \item Rewards can only flow to from or target states.
#' }
#' 
#' @section Object design:
#' We implemented \pkg{mcwr} using plain data frames. The input argument to all 
#' package functions is a data frame, as opposed to a specialized S3 object.
#' We did so on purpose, since an S3 object here provides only moderate gains
#' and yet complicates things in some respects.
#' This design decision may change in the future,
#' so the current \code{mcwr_*()} function names
#' may change (e.g., \code{mcwr_check()} will turn into \code{check()}).
#' However, the \code{mcwr_*()} functions are guaranteed to continue to work 
#' in the future in order to ensure backward compatibility.
#' 
#' @section Life tables and open age intervals:
#' 
#' The last age group of life tables is frequently an open interval.  What does this mean for mcwr data frames?
#' In the present context, the important thing to recognize is that mcwr expectancies must have access to the
#' proper ax value (Chiangs a).  The best way to achieve this is to set exit age (certain transition into
#' death) to some number greater than the previous age.  It does not really matter which age you choose as long
#' as you assign the ax value to the correct rewards variable.  The Examples section below illustrates this.
#' 
#' @examples
#' 
#' ## The first example uses a subset of the data from the retirement
#' ## example in the paper.  The example data frame
#' ## only contains transition variables.  We verify that the data frame
#' ## is suitable for mcwr and gather some model information.
#' 
#' ex_dat <- mcwr_exampledata(1)
#' ex_modelinfo <- mcwr_check(ex_dat)
#' names(ex_modelinfo)
#' ex_modelinfo$s_frm
#' ex_modelinfo$s_abs
#' ex_modelinfo$numages
#' 
#' ## modelinfo$s_frm tells us that the from-states are 1 2 4,
#' ## and modelinfo$s_abs indicates 5 to be the absorbing state.
#' ## There are 61 age classes in the model, which we can see
#' ## from modelinfo$numages.  Given initial proportions, we
#' ## can immediately calculate expectancies with different timing assumptions:
#' 
#' ex_init <- c(0.95, 0.04, 0.01)
#' names(ex_init) <- c(1,2,4)
#' mcwr_expectancies(ex_dat, timing='mid', initprop=ex_init)[[1]]
#' mcwr_expectancies(ex_dat, timing='eop', initprop=ex_init)[[1]]
#' 
#' ## Since we have a regular age grid of 1, the corresponding
#' ## magnitudes in the 'end-of-period' specification
#' ## are higher by 0.5. The output also tells us that total
#' ## life expectancy does not depend on the initial
#' ## state and that some occupation times are zero.
#' ## This comes from the particular data restrictions and
#' ## assumptions of the application in the paper:
#' ## Mortality does not differ across states, and transitions
#' ## from state 4 (retirement) to states 1 (working) or
#' ## 2 (unemployed) never occur (by assumption). See the
#' ## paper for more details.
#' ##
#' ## Since we are talking about the meaning of the state encodings:
#' ## Let's label the numeric values with meaningful descriptions.
#' ## This can be done via the vllmap option:
#' 
#' ex_MCWR <- c(1,2,3,4,5)
#' names(ex_MCWR) <- c('work', 'unem', 'oolf', 'retr', 'dead')
#' mcwr_expectancies(ex_dat, timing='eop', initprop=ex_init, vllmap=ex_MCWR)[[1]]
#' 
#' ## Let's now assume that retirement occurs on average
#' ## 3 months into the retirement year. The easiest way to
#' ## specify this is to generate all rewards variables first
#' ## as mid period and then edit them as needed:
#' 
#' ex_dat <- mcwr_genvars(ex_dat, timing='mid', order=TRUE)
#' names(ex_dat)
#' ex_dat[2:(nrow(ex_dat)-1),'r1_14'] <- 0.25
#' ex_dat[2:(nrow(ex_dat)-1),'r4_14'] <- 0.75
#' ex_dat[2:(nrow(ex_dat)-1),'r2_24'] <- 0.25
#' ex_dat[2:(nrow(ex_dat)-1),'r4_24'] <- 0.75
#' 
#' ## We do not have to worry about transitions out
#' ## of retirement since their probability is zero.
#' ## 
#' ## At this point it is useful to look at the data. In particular,
#' ## take note of how the first and last few rows are structured.
#' 
#' head(ex_dat)
#' tail(ex_dat)
#' 
#' ## We recalculate:
#' 
#' mcwr_expectancies(ex_dat, initprop=ex_init, vllmap=ex_MCWR)[[1]]
#' 
#' ## As an alternative to the above data replacement statements,
#' ## we could have solely used function mcwr_genvars() in
#' ## conjunction with the timing option of mcwr_expectancies().
#' ## This has the advantage of not having to worry
#' ## about getting the baseline and exit age rows right
#' ## (the row-indexing part of the statements).
#' 
#' ex_dat[,grep('r._..', names(ex_dat))] <- NULL
#' ex_dat <- mcwr_genvars(ex_dat, timing=0.25, order=TRUE)
#' ex_getn <- function(pat) grep(pat, names(ex_dat), value=TRUE)
#' ex_keepvars <- c('age', ex_getn('p..'), ex_getn('r._14'), ex_getn('r._24'))
#' ex_dat <- ex_dat[,ex_keepvars]
#' mcwr_expectancies(ex_dat, initprop=ex_init, vllmap=ex_MCWR, timing='mid', add=TRUE)[[1]]
#' 
#' ## After the creation of the 'timing(0.25)' rewards variables,
#' ## we could have generated the remaining rewards variables explicitly:
#' 
#' ex_dat <- mcwr_genvars(ex_dat, timing='mid', add=TRUE, order=TRUE)
#' mcwr_expectancies(ex_dat, initprop=ex_init, vllmap=ex_MCWR)[[1]]
#' 
#' ## Next, we illustrate the equivalence to standard life table
#' ## calculations. We first look at a regularly spaced 1-year life table.
#' 
#' ex_dat <- mcwr_exampledata(2)
#' names(ex_dat)
#' 
#' ## Checking whether we have a proper mcwr data frame reveals problems:
#' 
#' junk <- try(ex_modelinfo <- mcwr_check(ex_dat)) # generates error
#' 
#' ## We look at the beginning and ending rows of the data frame:
#' 
#' head(ex_dat)
#' tail(ex_dat)
#' 
#' ## We see that a number of mcwr data requirements, in particular
#' ## those concerning the first and last row of the data
#' ## frame, are not met. The data in proper mcwr format look like this:
#' 
#' ex_dat <- mcwr_exampledata(6)
#' head(ex_dat)
#' tail(ex_dat)
#' 
#' ## The correct life table value for e0 is 80.16.
#' ## Mid-period mcwr calculations yield
#' 
#' mcwr_expectancies(ex_dat, timing='mid')[[1]]
#' 
#' ## This ignores the values in the ax variable, which are different
#' ## from 0.5 for the first and last age. We take them
#' ## into account be generating an r-variable with corresponding values.
#' 
#' ex_dat[,'r1_12'] <- ex_dat[,'ax']
#' mcwr_expectancies(ex_dat, timing='mid', add=TRUE)[[1]]
#' 
#' ## The result is a little more accurate than the previous calculation.
#' ## 
#' ## Let's look at the corresponding 5-year life table.
#' 
#' ex_dat <- mcwr_exampledata(3)
#' junk <- try(modelinfo <- mcwr_check(ex_dat)) # generates error
#' head(ex_dat)
#' tail(ex_dat)
#' 
#' ## The data set must again be first transformed to a valid mcwr data set.
#' ## It then looks like:
#' 
#' ex_dat <- mcwr_exampledata(7)
#' head(ex_dat)
#' tail(ex_dat)
#' 
#' ## We again perform calculations with and without taking ax into account:
#' 
#' mcwr_expectancies(ex_dat, timing='mid')[[1]]
#' 
#' ex_dat[,'r1_12'] <- ex_dat[,'ax']
#' ex_dat <- mcwr_genvars(ex_dat, timing='mid', add=TRUE, order=TRUE)
#' ex_dat <- mcwr_expectancies(ex_dat)[[1]]
#' 
#' ex_dat
#' 
#' ## We explicitly created all rewards variables using mcwr_genvars()
#' ## instead of using them implicitly in mcwr_expectancies().
#' ## We did this to illustrate how mcwr uses rewards to
#' ## handle irregularly spaced age intervals.
#'
#' @docType package
#' @name mcwr
NULL
#> NULL