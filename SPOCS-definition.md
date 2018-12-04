# SPOCS Definition

This document contains an abbreviated discussion of the properties
enumerated by each letter in the SPOCS acronym.

## Single-point

This library and algorithm is primarily for computing point-wise
statistics and reducing the dimensionality of large data sets through
ensemble and temporal or spatial descriptive statistics. Such point
wise statistics include the mean, maximum, minimum, centered
statistical moments, such as those used to compute the variance,
skewness, curtosis, etc. and co-moments between two or more
variables. This library is not for computing two-point correlations,
spatial or temporal spectograms, etc. although it may still be of
utility as a component in some of those contexts.

## Parallel

The algorithm has the ability to partition a set of elements to visit
into a number of subsets. Each subset may be processed simultaneously
or asynchronously. Then, to compute the statistics of interest on the
union of the subsets (i.e., the orginal set) a simple reduction
operation is computed using the fixed size state from each subset or
partition. When caried out symbolically, this reduction operation
would be exact for reconstructing the various means, centered moments
and co-moments. This property is anticipated to be usefull in large
parellel simulations of chaotic or stochastic phenomina such as fuid
turbulence, so that basic analysis can be carried out _in situ_ as the
simulation is running. It may also be of utility in other areas where
large quantities of data must be processed in a timely fashion and
portions of the data set may be spread across multiple servers.

## Online

The algorithm is an online (i.e., streaming) algorithm. It carries
constant state that does not depend on the number of elements visited,
and only requires one pass through the data set. This is particularly
usefull when the data set is very large and must be read from disk or
some "slow" storage, or if the data is coming from some physical
instrument and being processed on a device that requires real-time
performance with limited resources. In addition, the updating
algorithm has numerical properties that prevent the introduction of
numerical error from, for example, catastrophic cancellation, that
more naive one-pass algorithms suffer from. In fact, in most cases it
performs as well or better than if the statistics are computed in two
passes. (The first pass computes the means, the second the statistical
moments from the zero-centered fluctuations about these means.)

## Converging

For ergodic data sets, it is mathematically proveable that the
statistics will converge as the number of data points added tends to
infinity. Of course this does not make you imune to sampling problems,
etc. An additional benefit of this property and the online/streaming
property is that when a single new data point is added, or a chunk of
new data points are added, the "update" the produce can be monitored
in real time. The rate at which this update or residual approaches
zero can quantify the rate of statistical convergence, and its final
value can be used to estimate or bound the error in the obtained
statistical moments.

## Statistics

This point is self explanitory. Basically, the library is designed
primarily for obtaining means and arbitrarily high order statistical
moments and comements. These may be used as building blocks in other
capacities, but these other capacities are beyond the scope of this
software package.
