wbd= read.csv("data_pivoted_CO_47+2.csv", stringsAsFactors = TRUE)

lminter = lm(Gini.index ~ Individuals.using.the.Internet....of.population. +   
               Access.to.electricity....of.population.+
               Access.to.electricity..urban....of.urban.population.+
               Access.to.electricity..rural....of.rural.population.+
               Communications..computer..etc.....of.service.imports..BoP.+
               Compulsory.education..duration..years.+
               Foreign.direct.investment..net.inflows....of.GDP.+
               Employers..total....of.total.employment...modeled.ILO.estimate.+
               Fixed.broadband.subscriptions+
               Labor.force..female....of.total.labor.force.+
               International.tourism..expenditures..current.US..+
               Computer..communications.and.other.services....of.commercial.service.exports.+
               Poverty.gap.at..2.15.a.day..2017.PPP.....+
               Secure.Internet.servers..per.1.million.people.+
               Mobile.cellular.subscriptions..per.100.people.+
               Labor.force.participation.rate.for.ages.15.24..total......national.estimate.+
               School.enrollment..tertiary....gross.,
              data =wbd)
summary(lminter)
