sum
ren pctstck y
mlogit y choice age educ wealth89 prftshr,  baseoutcome(0) 
table y educ
predict p1 p2 p3, p
mlogit, rrr
margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) atmeans 

margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) at((p25) *)

margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) at(age = 69 choice = 0)
clear all 

use "D:\Users\DNS\Econometrics_Rosetta\asya_work\data\tradrole.dta", clear

sum 
hist tradrole
oprobit tradrole i.cath i.fpro meduc i.urb
