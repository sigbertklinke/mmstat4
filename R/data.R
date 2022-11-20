#' German lotto numbers
#' Lotto numbers 6 out of 49 including bonus and super number starting from 9 Oct 1955.
#'
#' @format A data frame with 4181 rows and 10 variables:
#'
#' 1.  __Datum__  date of drawing
#' 2. __Wochentag__   week day of drawing (in german)
#' 3. __Gewinnzahlen__   first drawn number
#' 4.  __V4__   second drawn number
#' 5.  __V5__   third drawn number
#' 6.  __V6__   fourth drawn number
#' 7.  __V7__   fifth drawn number
#' 8. __V8__   sixth drawn number
#' 9. __Zusatzzahl__   bonus number, always \code{NA}
#' 10.  __Superzahl__   super number, always \code{NA}
#'
"lottozahlen"

#' haushalte_deutschland
#'
#' Development of household shares by household size in Germany.
#'
#' @format A data frame with 54 rows and 7 variables:
#'
#' 1.  __Jahr__ year
#' 2. __Gesamt__ total number of households
#' 3. __Einpersonen__  percentage of one person households
#' 4. __Zweipersonen__  percentage of two person households
#' 5. __Dreipersonen__  percentage of three person households
#' 6. __Vierpersonen__  percentage of four person households
#' 7. __Fuenf und mehr Personen__  percentage of five or more person households
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/) 12211-9022: Privathaushalte: Deutschland, Jahre (bis 2019), Haushaltsgröße
"haushalte_deutschland"

#' haushalte_berlin
#'
#' Development of the number of households by household size in Berlin.
#'
#' @format A data frame with 54 rows and 7 variables:
#'
#' 1.  __Jahr__  year
#' 2.  __Einpersonenhaushalte__  number of one person housholds (in 1000)
#' 3.  __Zweipersonenhaushalte__  number of two person housholds (in 1000)
#' 4.  __Dreipersonenhaushalte__  number of three person housholds (in 1000)
#' 5.  __Vierpersonenhaushalte__ number of four person housholds (in 1000)
#' 6.  __5 und mehr__ number of five or more person housholds (in 1000)
#' 7.  __Privathaushalte__  total number of households (in 1000)
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/) 12211-9034: Privathaushalte: Bundesländer, Jahre (bis 2019), Haushaltsgröße
"haushalte_berlin"

#' destatis12411_0006
#'
#' Number of males and females by age.
#'
#' @format A data frame with 86 rows and 3 variables:
#'
#' 1.  __V1__  age (in years)
#' 2.  __M__  total number of males in age group
#' 3.  __F__  total number of females in age group
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/)
"destatis12411_0006"

#' destatis13211_0002
#'
#' Number of males and females by age.
#'
#' @format A data frame with 86 rows and 3 variables:
#'
#' 1. __Gebiet__  territory
#' 2. __Jahr__  year
#' 3. __Monat__  month
#' 4. __Arbeitslose__ unemployed
#' 4. __Arbeitslosenquote aller zivilen Erwerbspersonen__ unemployment rate based on the total civilian workforce
#' 5. __Arbeitslosenquote d. Abhaengigen ziv. Erwerbspers.__ unemployment rate based on dependant civilian working people
#' 6. __Gemeldete Stellen__ registered vacancies
#' 7. __Kurzarbeiter__ short-time working
#' 8. __Kurzarbeitende Betriebe__ companies that implemented short-time work
#'
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/)
"destatis13211_0002"


#' allbus2012
#'
#' An exercept of three variables of the ALLBUS 2012 data set.
#'
#' @format A data frame with 3480 rows and 3 variables:
#'
#' 1. __v220__ age of the respondent (years)
#' 2. __v593__ body height of the respondent (cm)
#' 3. __v595__ weight of the respondent (kg)
#'
#' @references GESIS - Leibniz-Institut für Sozialwissenschaften (2013). Allgemeine Bevölkerungsumfrage der Sozialwissenschaften ALLBUS 2012. GESIS Datenarchiv, Köln. ZA4614 Datenfile Version 1.1.1, https://doi.org/10.4232/1.11753.
"allbus2012"

#' allbus2018
#'
#' An exercept of some variables of the ALLBUS 2018 data set.
#'
#' @format A data frame with 3477 rows and 20 variables:
#'
#' 1. __eastwest__ SURVEY AREA (RESIDENCE AREA): WEST - EAST
#' 1. __ep03__ ECONOMIC SITUATION OF RESPONDENT TODAY
#' 1. __ep06__ ECONOMIC SITUATION OF RESPONDENT IN 1 YEAR
#' 1. __di05__ HOUSEHOLD NET INCOME: OPEN QUESTION
#' 1. __di06__ HOUSEHOLD NET INCOME: LIST QUESTION
#' 1. __hhinc__ HOUSEHOLD NET INCOME (OPEN+LISTS INPUT), CATEGORIES
#' 1. __land__ FEDERAL STATE WHERE RESPONDENT RESIDES
#' 1. __pt01__ TRUST: HEALTH CARE
#' 1. __pt02__ TRUST: FEDERAL CONSTITUTIONAL COURT
#' 1. __pt03__ TRUST: FEDERAL PARLIAMENT
#' 1. __pt04__ TRUST: CITY, MUNICIPAL ADMINISTRATION
#' 1. __pt08__ TRUST: JUSTICE
#' 1. __pt09__ TRUST: TELEVISION
#' 1. __pt10__ TRUST: NEWSPAPER
#' 1. __pt11__ TRUST: HIGHER EDUCATION, UNIVERSITIES
#' 1. __pt12__ TRUST: FEDERAL GOVERNMENT
#' 1. __pt14__ TRUST: POLICE
#' 1. __pt15__ TRUST: POLITICAL PARTIES
#' 1. __pt19__ TRUST: COMMISSION OF THE EU
#' 1. __pt20__ TRUST: EUROPEAN PARLIAMENT
#'
#' @references GESIS - Leibniz-Institut für Sozialwissenschaften (2019). German General Social Survey - ALLBUS 2018. GESIS Datenarchiv, Köln. ZA5272 Datenfile Version 1.0.0, https://doi.org/10.4232/1.13325.
"allbus2018"

#' anscombe
#'
#' Four artificial data sets which have the same traditional statistical properties
#' (mean, variance, correlation, regression line, etc.), yet are quite different.
#'
#' @format A data frame with 3477 rows and 20 variables:
#'
#' 1. __ID__ number of data set
#' 2. __X__ x
#' 3. __Y__ y
#'
#' @source [datasets::anscombe]
#' @references Tufte, Edward R. (1989). The Visual Display of Quantitative Information, 13–14. Graphics Press.
"anscombe"

#' bank2
#'
#' The data set contains six measurements made on 100 genuine and 100 counterfeit old-Swiss 1000-franc bank notes.
#'
#' @format A data frame with 200 rows and 6 variables:
#'
#' 1. __WIDTH__ width of bill (mm)
#' 2. __LEFT__ width of left edge (mm)
#' 3. __RIGHT__ width of right edge (mm)
#' 4. __UPPER__ upper margin width (mm)
#' 5. __LOWER__ lower margin width (mm)
#' 6. __DIAGONAL__ length of diagonal (mm)
#'
#' @source Flury, B. and Riedwyl, H. (1988). Multivariate Statistics: A practical approach. London: Chapman & Hall, Tables 1.1 and 1.2, pp. 5-8.
"bank2"

#' benzinverbrauch
#'
#' The data set with grouped data of fuel consumption of cars.
#'
#' @format A data frame with 200 rows and 6 variables:
#'
#' 1. __Klasse__ fuel consumption (liter/100 km)
#' 2. __h(x)__ absolute frequency
#' 3. __f(x)__ relative frequency
"benzinverbrauch"

#' bostonc
#'
#' Housing data for 506 census tracts of Boston from the 1970 census. It contains the corrected version with additional spatial information.
#'
#' @format A data frame with 506 rows and 20 variables:
#'
#' 1. __TOWN__ name of town
#' 2. __TOWN#__ number of town
#' 3. __TRACT__ census tract
#' 4. __LON__	longitude of census tract
#' 5. __LAT__ latitude of census tract
#' 6. __MEDV__ median value of owner-occupied homes in USD 1000's
#' 7. __CMEDV__ corrected median value of owner-occupied homes in USD 1000's
#' 8. __CRIM__	per capita crime rate by town
#' 9. __ZN__ proportion of residential land zoned for lots over 25,000 sq.ft
#' 10. __INDUS__ proportion of non-retail business acres per town
#' 11. __CHAS__	Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#' 12. __NOX__ nitric oxides concentration (parts per 10 million)
#' 13. __RM__	average number of rooms per dwelling
#' 14. __AGE__ proportion of owner-occupied units built prior to 1940
#' 15. __DIS__ weighted distances to five Boston employment centres
#' 16. __RAD__ index of accessibility to radial highways
#' 17. __TAX__ full-value property-tax rate per USD 10,000
#' 18. __PTRATIO__ pupil-teacher ratio by town
#' 19. __B__ 1000(B - 0.63)^2 where B is the proportion of blacks by town
#' 20. __LSTAT__ percentage of lower status of the population
#'
#' @source [From Statlib: boston_corrected](http://lib.stat.cmu.edu/datasets/boston_corrected.txt). Submitted by Kelley Pace (kpace@unix1.sncc.lsu.edu).
#'
#' @seealso [mlbench::BostonHousing], [spData::boston]
#'
#' @references
#'
#' * Harrison, David, and Daniel L. Rubinfeld, Hedonic Housing Prices and the Demand for Clean Air, Journal of Environmental Economics and Management, Volume 5, (1978), 81-102.
#' * Gilley, O.W., and R. Kelley Pace, On the Harrison and Rubinfeld Data, Journal of Environmental Economics and Management, 31 (1996), 403-405. Provided corrections and examined censoring.
#' * Pace, R. Kelley, and O.W. Gilley, Using the Spatial Configuration of the Data to Improve Estimation, Journal of the Real Estate Finance and Economics 14 (1997), 333-340. Added georeferencing and spatial estimation.
"bostonc"

#' bostonh
#'
#' Housing data for 506 census tracts of Boston from the 1970 census. It contains the original data by Harrison and Rubinfeld (1979),
#'
#' @format A data frame with 506 rows and 20 variables:
#'
#' 1. __CRIM__	per capita crime rate by town
#' 2. __ZN__ proportion of residential land zoned for lots over 25,000 sq.ft
#' 3. __INDUS__ proportion of non-retail business acres per town
#' 4. __CHAS__	Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#' 5. __NOX__ nitric oxides concentration (parts per 10 million)
#' 6. __RM__	average number of rooms per dwelling
#' 7. __AGE__ proportion of owner-occupied units built prior to 1940
#' 8. __DIS__ weighted distances to five Boston employment centres
#' 9. __RAD__ index of accessibility to radial highways
#' 10. __TAX__ full-value property-tax rate per USD 10,000
#' 11. __PTRATIO__ pupil-teacher ratio by town
#' 12. __B__ 1000(B - 0.63)^2 where B is the proportion of blacks by town
#' 13. __LSTAT__ percentage of lower status of the population
#' 14. __MEDV__ median value of owner-occupied homes in USD 1000's
#'
#' @source [From Statlib: boston_corrected](http://lib.stat.cmu.edu/datasets/boston_corrected.txt). Submitted by Kelley Pace (kpace@unix1.sncc.lsu.edu).
#'
#' @seealso [MASS::Boston], [mlbench::BostonHousing]
#'
#' @references Harrison, David, and Daniel L. Rubinfeld, Hedonic Housing Prices and the Demand for Clean Air, Journal of Environmental Economics and Management, Volume 5, (1978), 81-102.
"bostonh"

#' concrete
#'
#' Data about concrete mixtures and resulting properties (Slump, Flow and Strength).
#' Concrete is the most important material in civil engineering. The
#' concrete compressive strength is a highly nonlinear function of age and
# 'ingredients. These ingredients include cement, blast furnace slag, fly ash,
#' water, superplasticizer, coarse aggregate, and fine aggregate.
#'
#' * Water content: ACI 211.1 (Standard practice for selecting proportions for normal, heavyweight, and mass concrete) assumes that, for a given maximum size of coarse aggregate, the slump or consistency of concrete is a direct function of the water content; i.e., within limits it is independent of other factors such as aggregate grading and cement content.
#' * Superplasticizer:  When the water content of a concrete mixture is held constant, the addition of a superplasticizer will increase the consistency.
#' * Pozzolanic admixtures: Pozzolanic admixtures tend to improve the cohesiveness of concrete. Fly ash, when used as a partial replacement for fine aggregate, generally increases the consistency at a given water content.
#' * Cement content: Concretes containing a very high proportion of cement show excellent cohesiveness, but tend to be sticky. At a given water content, a considerable lowering of the cement content tends to produce harsh mixtures, with poor cohesiveness.
#' * Aggregate characteristics: Very fine sands require more water for a given consistency; alternatively, they will produce harsh and unworkable mixtures at a water content that might have been adequate with coarser sands. Also, the particle size of coarse aggregate influences the water requirement for a given consistency.
#'
#' @format A data frame with 103 rows and 10 variables:
#'
#' 1. __Cement__ Cement (in kg/m$^3$ concrete)
#' 2. __Slag__ Slag (in kg/m$^3$ concrete)
#' 3. __Flyash__ Fly ash (in kg/m$^3$ concrete)
#' 4. __Water__ Water (in kg/m$^3$ concrete)\\
#' 5. __SP__ Superplasticizer (in kg/m$^3$ concrete)
#' 6. __CoarseAggr__ Coarse sand (in kg/m$^3$ concrete)
#' 7. __FineAggr__ Fine sand (in kg/m$^3$ concrete)
#' 8. __Slump__ Slump (in cm)
#' 9. __Flow__ Flow (in cm)
#' 10. __Strength__ 28day compressive strength (in Megapascal)
#'
#' @source [UCI Machine Learning Repository - Concrete Compressive Strength Data Set](https://archive.ics.uci.edu/ml/datasets/concrete+compressive+strength)
#' @references I-Cheng Yeh (1998), Modeling of strength of high performance concrete using artificial neural networks, Cement and Concrete Research, Vol. 28, No. 12, pp. 1797-1808
"concrete"

#' country
#' The data set contains data about 122 countries in 1992.
#'
#' @format A data frame with 122 rows and 22 variables:
#'
#' 1. __COUNTRY__ Country name
#' 2. __POP92__ Population, 1992, in millions
#' 3. __URBAN__ Percent urban, 1992
#' 4. __GDP__ GDP per capita
#' 5. __LIFEEXPM__ Male life expectancy 1992
#' 6. __LIFEEXPF__ Female life expectancy 1992
#' 7. __BIRTHRAT__ Births per 1000 population, 1992
#' 8. __DEATHRAT__ Deaths per 1000 people, 1992
#' 9. __INFMR__ Infant mortality rate 1992 (per 1000 live births)
#' 10. __FERTRATE__ Fertility rate per woman, 1990
#' 11. __REGION__ Region of the world (1=Eastern Africa, 2=Middle Africa, 3=Northern Africa, 4=Southern Africa, 5=Western Africa, 6=Caribbean, 7=Central America, 8=South America, 9=North America, 10=Eastern Asia, 11=Southeast Asia, 12=Southern Asia, 13=Western Asia, 14=Eastern Europe, 15=Northern Europe, 16=Southern Europe, 17=Western Europe, 18=Oceania, 19=USSR)
#' 12. __DEVELOP__ Status as Developing Country (0=Developed country, 1=Developing country)
#' 13. __RADIO__ Radios per 100 people
#' 14. __PHONE__ Phones per 100 people
#' 15. __HOSPBED__ Hospital beds per 10,000 people
#' 16. __DOCS__ Doctors per 10,000 people
#' 17. __LNDOCS__ Natural log of doctors per 10000
#' 18. __LNRADIO__  Natural log of radios per 100 people
#' 19. __LNPHONE__ Natural log of phones per 100 people
#' 20. __LNGDP__ Natural log of
#' 21. __SEQUENCE__ Arbitrary sequence number
#' 22. __LNBEDS__ Natural log hospital beds/10,000
#'
#' @source [Norušis, Marija J. IBM SPSS Statistics 19: Guide to Data Analysis. Internat. ed. Upper Saddle River: Prentice Hall, 2011.](http://www.norusis.com/book_DA_v19.php)
"country"

#' cp75_85
#'
#' The Current Population Survey (CPS) is used to supplement census information between census years. These data consist of a random sample of 534 persons from the CPS 1985 and 550 persons from the CPS 1978, with information on wages and other characteristics of the workers, including sex, number of years of education, years of work experience, occupational status, region of residence and union membership. We wish to determine (i) whether wages are related to these characteristics and (ii) whether there is a gender gap in wages.
#'
#' @format A data frame with 1084 rows and 11 variables:
#'
#' 1. __educ__ Number of years of education
#' 2. __south__ Indicator variable for Southern Region (1=Person lives in South, 0=Person lives elsewhere)
#' 3. __nonwhite__ Race (0=White, 1=Other)
#' 4. __female__ Indicator variable for sex (1=Female, 0=Male)
#' 5. __married__ Marital Status (0=Unmarried,  1=Married)
#' 6. __exper__ Number of years of work experience
#' 7. __expersq__ Number of years of work experience squared
#' 8. __union__ Indicator variable for union membership (1=Union member, 0=Not union member)
#' 9. __lwage__ Logarithm of wage (dollars per hour)
#' 10. __age__ Age (years)
#' 11. __year__ Survey year (78=1978, 85=1985)
#'
#' @source [Statlib: Determinants of Wages from the 1985 Current Population Survey](http://lib.stat.cmu.edu/datasets/CPS_85_Wages), 1978 Current Population Survey (???)
#' @references Berndt, ER. (1991) The Practice of Econometrics, NY: Addison-Wesley.
"cps78_85"

#' credit
#'
#' The granting of personal loans is made dependent on the creditworthiness of the customers,
#' i.e. on the willingness and ability of the customers to duly pay the interest and redemption
#' instalments due. A potential borrower can be classified either as a problem-free borrower or
#' as a problematic borrower who should be checked more closely or who should not be granted a loan.
#' Each borrower is characterised by a series of features that characterise his personal, economic
#' and legal situation. On the basis of these characteristics, an attempt is made to make a statistically
#' sound decision on granting or rejecting the loan.
#' The data are a stratified learning sample conducted by a major bank in southern Germany.
#' With a population of 1000 former borrowers, mainly ordinal and nominal survey characteristics were recorded
#' to describe their living conditions. 30 percent of these former customers were not able to repay the
#' loan as agreed.
#'
#' @format A data frame with 1000 rows and 8 variables:
#'
#' 1. __Problems__ Credit repaid
#' 2. __BankAccount__ Current account with the bank (no/bad running/good running)
#' 3. __Duration__ Duration in months
#' 4. __PreviousPayment__ Previous payment record (bad customer/good customer)
#' 5. __CreditUse__ Intended purpose (private/professional)
#' 6. __DM__ Loan amount in DM
#' 7. __Sex__ Gender (female/male)
#' 8. __CivilState__ Marital status (married/not married)
#'
#' @source Exercept from [Kreditscoring zur Klassifikation von Kreditnehmern. 2010. Open Data LMU.](https://data.ub.uni-muenchen.de/23/)
#' @references
#' * Fahrmeir, L. / Tutz, G. (1994): Multivariate Statistical Modelling Based on Generalized Linear Models. Springer, New York.
#' * Fahrmeir, L. / Hamerle, A. / Tutz, G. (1984, 1. Aufl.): Multivariate statistische Verfahren. de Gruyter, Berlin.
#' * Fahrmeir, L. / Hamerle, A. / Tutz, G. (1996, 2. Aufl.): Multivariate statistische Verfahren. de Gruyter, Berlin.
#' * Fahrmeir, L. / Hamerle, A. (1981): Kategoriale Regression in der betrieblichen Planung. Zeitschrift für Operations Research, 44/B, 63-78.
#' * Feilmeier, M. / Fergel, I. / Segerer, G. (1981): Lineare Diskriminanz- und Clusteranalyseverfahren bei Kreditscoring-Systemen. Zeitschrift für Operations Research, 25/B, 25-28.
"credit"

#' sas_body
#'
#' The data set consists of data about children taken from the SAS online documentation V8:
#'
#' @format A data frame with 237 rows and 4 variables:
#'
#' 1. __Sex__ Gender
#' 2. __Age__ Age in month
#' 3. __Height__ Height in inches
#' 4. __Weight__ Weight in pounds
#'
#' @source SAS online documentation V8
"sas_body"

#' salary
#'
#' The data set contains information about 474 employees hired by the Harris Trust and Savings Bank between 1969 and 1971. The bank was involved into a Equal Employment Opportunity litigation were the data was used.
#'
#' @format A data frame with 474 rows and 11 variables:
#'
#' 1. __id__ Ssquence number
#' 2. __salbeg__  beginning salary (US$)
#' 3. __sex__ sex of employee (males, females)
#' 4. __time__ job seniority (years)
#' 5. __age__ age of employee (years)
#' 6. __salnow__ current salary (US$)
#' 7. __edlevel__ educational level (years)
#' 8. __work__  work experience (years)
#' 9. __jobcat__  employment category (clerical/office trainee/security officer/college trainee/exempt employee/mba trainee/technical)
#' 10. __minority__ minority classification (white/nonwhite)
#' 11. __sexrace__ sex & race classification (white males/minority males/white females/minority females)
#' @references
#'   * Roberts, H.V. (1979) _An analysis of employee compensation._ Report 7946, Center for Mathematical Studies in Business and Economics, University of Chicago.
#'   * Roberts, H.V. (1980) _Statistical bases in the measurement of employment discrimination._ In: Comparable Worth: Issues and Alternatives by E.R. Livernash (ed.) Washington, D.C.: Equal Employment Advisory Council.
"salary"

#' kriegsschiffe
#'
#' The dataset describes various characteristics of artillery ships in service at the time of the Second World War.
#'
#' #' @format A data frame with 114 rows and 12 variables:
#'
#' 1. __KlasseLand__ Ship name and country
#' 2. __Art__ Ship type (1=battleship, 3=heavy cruiser, 4=light cruiser, 5=destroyer)
#' 3. __Stapellauf__ Ship launch (year)
#' 4. __Verdraengung__ Ship displacement (in 1000 tons)
#' 5. __Laenge__ Ship length (in m)
#' 6. __Breite__ Ship beam (in m)
#' 7. __Tiefgang__ Ship draught (in m)
#' 8. __Panzerung__ Ship armour
#' 9. __Maschine__ Ship engine (in 1000 PS)
#' 10. __Geschwindigkeit__ Ship speed (in knots)
#' 11. __Aktionsradius__ Ship action radius
#' 12. __Besatzung__ Ship crew
#' @source [User Philipendula from the german wikipedia](https://de.wikibooks.org/wiki/Benutzer:Philipendula)
"kriegsschiffe"

#' gluehlampen
#'
#' Articial grouped data set about burning time of light bulbs in hours.
#'
#' @format A data frame with 4 rows and 6 variables:
#'
#' 1. __k_u__ lower class limit
#' 2. __k_o__ upper class limit
#' 3. __h_hat__ absolute frequency
#' 4. __f_hat__ relative frequency
#' 5. __H_hat__ cumulative absolute frequency
#' 6. __F_hat__ cumulative relative frequency
"gluehlampen"

#' olympic
#'
#' This data set gives the performances of 33 men's decathlon at the Olympic Games (1988).
#'
#' @format A data frame with 33 rows and 12 variables:
#'
#' 1. __Nr__ place in the competition
#' 2. __@100__ 100 m run
#' 3. __long__ long jump
#' 4. __poid__ shotput
#' 5. __haut__ high jump
#' 6. __@400__ 400 m run
#' 7. __@110__ 110-meter hurdles
#' 8. __disq__ discus throw
#' 9. __perc__ pole vault
#' 10. __jave__ javelin
#' 11. __@1500__ 1500 m run
#' 12. __score__ final points scores
#'
#' @seealso [ade4::olympic]
#' @source
#'
#' * Example 357 in: Hand, D.J., Daly, F., Lunn, A.D., McConway, K.J. and Ostrowski, E. (1994) A handbook of small data sets, Chapman & Hall, London. 458 p.
#' * Lunn, A. D. and McNeil, D.R. (1991) Computer-Interactive Data Analysis, Wiley, New York
"olympic"

#' pechstein
#'
#' Claudia Pechstein is a German speed skater which was banned because of blood doping.
#' The ban was based on irregular levels of reticulocytes in her blood. The data set is a
#' non-random excerpt of 95 blood controls since 2000.
#'
#' @format A data frame with 29 rows and 3 variables:
#'
#' 1. __Datum__ Date of control
#' 2. __Tag__ Date of control as days from 1st Jan 2000
#' 3. __Retikulozyten__ Reticulocytes levels
#'
#' @source Bild.de, 17.07.2009, behind paywall: https://www.bild.de/sport/mehr-sport/claudia-pechstein/was-verraten-ihre-blutwerte-im-doping-skandal-8932510.bild.html
"pechstein"

#' pechstein_isu
#'
#' Claudia Pechstein is a German speed skater which was banned because of blood doping.
#' The ban was based on irregular levels of reticulocytes in her blood. The data set
#' with the full values from the 95 blood controls since 2000. The threshold value
#' the ISU used was 2.4.
#'
#' @format A data frame with 29 rows and 3 variables:
#'
#' 1. __no__ Number of control from 1 to 95
#' 2. __date__ Date of control as `Date`
#' 3. __place__ Place of control, `(Out of Competition)` means a control during training
#' 3. __reti__ Reticulocytes levels in percent
#'
#' @source [Blog of Jens Weinreich, 08.07.2009](https://www.jensweinreich.de/2009/07/08/die-blutkontrollen-von-claudia-pechstein/)
"pechstein_isu"

#' titanic3
#'
#' The sinking of the RMS Titanic is one of the most infamous shipwrecks in history. On April 15, 1912, during her maiden voyage, the Titanic sank after colliding with an iceberg, killing 1502 out of 2224 passengers and crew.
#' The data set describes the survival status of individual passengers on the Titanic. The data set does not contain information for the crew, but it does contain actual and estimated ages for almost 80% of the passengers.
#'
#' @format A data frame with 1309 rows and 14 variables:
#' 1. __pclass__ a factor with levels 1st, 2nd, and 3rd
#' 2. __survived__ survival with 0 = No and  1 = Yes
#' 3. __name__ name of passenger
#' 4. __sex__ a factor with levels female and male
#' 5. __age__ age in years
#' 6. __sibsp__ number of Siblings/Spouses Aboard
#' 7. __parch__ number of Parents/Children Aboard
#' 8. __ticket__ ticket number
#' 9. __fare__ passenger fare
#' 10. __cabin__ cabin
#' 11. __embarked__ a factor with levels Cherbourg, Queenstown, and Southampton
#' 12. __boat__ lifeboat number
#' 13. __body__ body identification number
#' 14. __home.dest__ home/destination
#'
#' @seealso [PASWR2::TITANIC3]
#' @source http://biostat.mc.vanderbilt.edu/twiki/pub/Main/DataSets/titanic.html
#' @references
#' * Harrell, F. E. 2001. Regression Modeling Strategies with Applications to Linear Models, Logistic Regression, and Survival Analysis. Springer.
#' * Ugarte, M. D., Militino, A. F., and Arnholt, A. T. 2015. Probability and Statistics with R, Second Edition. Chapman & Hall / CRC.
"titanic3"

#' titanic_full
#'
#' This data set provides information on the fate of passengers on the fatal maiden voyage of the ocean liner ‘Titanic’, according to economic status (class), sex, age and survival.
#'
#' @format A data frame with 2201 rows and 4 variables:
#'
#' 1. __Class__ 1=1st, 2=2nd, 3=3rd, 4=Crew
#' 2. __Sex__ 1=Male, 2=Female
#' 3. __Age__ 1=Child, 2=Adult
#' 4. __Survived__ 1=No, 2=Yes
#'
#' @seealso [datasets::Titanic]
#' @source Dawson, Robert J. MacG. (1995), The ‘Unusual Episode’ Data Revisited. Journal of Statistics Education, 3. doi: [10.1080/10691898.1995.11910499](https://www.tandfonline.com/doi/full/10.1080/10691898.1995.11910499).
"titanic_full"

#' marathon
#'
#' Data about the marathon at 07.10.2001 in Chicago.
#'
#' @format A data frame with 28764 rows and 7 variables:
#'
#' 1. __AGE__ age
#' 2. __SEX__ Sex (M=male, F=female)
#' 3. __TIME__ time in difftime
#' 4. __HOURS__ time in hours
#' 5. __AGECAT8__ age categories (1=15-24, 2=25-39, 3=40-44, 4=45-49, 5=50-54, 6=55-59, 7=60-64, 8=65+)
#' 6. __AGECAT6__ age categories (1=15-44, 2=45-49, 3=50-54, 4=55-59, 5=60-64, 6=65+)
#' 7. __NAME__ name or blank
#'
#' @source [Norušis, Marija J. IBM SPSS Statistics 19: Guide to Data Analysis. Internat. ed. Upper Saddle River: Prentice Hall, 2011.](http://www.norusis.com/book_DA_v19.php)
"marathon"

#' fatalities_statlib
#'
#' Fatalities per 100 000 inhabitants in New Mexico and the US about forty years.
#'
#' @format A data frame with 40 rows and 3 variables:
#'
#' 1. __YR__ year numbered from 1 to 40
#' 2. __NM__ fatalities per 100 000 inhabitants in New Mexico
#' 3. __US__ fatalities per 100 000 inhabitants in the US
#'
#' @source StatLib?
"fatalities_statlib"

#' staedtemietenr
#'
#' Housing offers in almost forty German cities.
#'
#' @format A data frame with 16777 rows and 7 variables:
#'
#' 1. __Thema__ Group
#' 2. __Stadt__ City
#' 3. __Stadtbezirk__ Muncipality
#' 4. __Zimmer__ Number of rooms
#' 5. __Etage__ Floor
#' 6. __Miete__ Basic rent in EUR
#' 7. __Flaeche__ Living space in m2
#'
"staedtemietenr"

#' thousand_songs
#'
#' Selection of the 1000 songs everyone must hear performed by The Guardian in the year 2009.
#'
#' @format A data frame with 994 rows and 5 variables:
#'
#' 1. __THEME__ Song theme (Heartbreak/Life and death/Love/Party songs/People and places/Politics and protest/Sex)
#' 2. __TITLE__ Song title
#' 3. __ARTIST__ Song artist
#' 4. __YEAR__ Year of publishing
#' 5. __SPOTIFY_URL__ Spotify URL or blank
#'
#' @source [Kaggle: 1000 songs everyone must hear - The Guardian's songs list](https://www.kaggle.com/iampedroalz/1000songs)
"thousand_songs"



# europa - nicht benutzt
# cardata - nicht benutzt
# gss
# heliumfootball - nicht benutzt
# manners - nur Aufgaben
# mhne - nicht benutzt
# mikrozensus2002 - nicht benutzt
# six_of_49  - nicht benutzt
# titanic_statisticalmodeling
# wages - nur Aufgaben
# wartezeiten - nicht benutzt
