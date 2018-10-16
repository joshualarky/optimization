####### set working directory as same path as where the 4 tables are stored
setwd('/Users/joshualarky')

### The two packages that need to be installed:

# install.packages('data.table')
# install.packages('lpSolve')

library(data.table)
library(lpSolve)

### From baseball savant, data is gathered for groundballs vs lefty pitchers and vs righty pitchers for both 
### Christian Yelich and Edwin Encarnacion.

### Comment out the two Edwin Encarnacion dataframes if you want to optimize defensive positioning against yelich grounders
### Comment out the two Christian Yelich dataframes if you want to optimize defensive positioning against Encarnacion's grounders
### If you select all and run this script right now, Yelich's grounders will be optimized for, since Encarnacion would be commented out.

vsL <- read.csv('yelich_l.csv')
vsR <- read.csv('yelich_r.csv')

#vsL <- read.csv('edwin_l.csv')
#vsR <- read.csv('edwin_r.csv')


#### if you select the whole script and run it at once, this "oldtime" variable is set up to let you know how long the script takes
#### to run all the way through (less than 0.5 seconds on my local Mac computer)

oldtime <- Sys.time()

## get a few relevant columns that could be used to form a dataset for many different projects
colnames(vsL)
vsL <- vsL[,c(1,2,3,6,7,19,23,24,25,26,32,33,34,38,39,53,54,55)]
vsR <- vsR[,c(1,2,3,6,7,19,23,24,25,26,32,33,34,38,39,53,54,55)]
head(vsL)

#### Thanks to https://www.fangraphs.com/tht/research-notebook-new-format-for-statcast-data-export-at-baseball-savant/ 
#### for the spray angle equation.  There's no spray angle from baseball savant, so this equation infers spray angle based on hc_x and hc_y
vsL$spray_angle <- round(atan((vsL$hc_x-125.42)/(198.27-vsL$hc_y))*180/pi*.75,2)

vsL <- data.table(vsL)
vsL[,mean(spray_angle)]
vsL[,.(avg_spray_angle=mean(spray_angle)),by=strikes]

vsR$spray_angle <- round(atan((vsR$hc_x-125.42)/(198.27-vsR$hc_y))*180/pi*.75,2)

vsR <- data.table(vsR)
vsR[,mean(spray_angle)]
vsR[,.(avg_spray_angle=mean(spray_angle)),by=strikes]

vsL[,.(avg_spray_angle=mean(spray_angle)),by=pitch_type]
vsR[,.(avg_spray_angle=mean(spray_angle)),by=pitch_type]

vsL$isL <- 1
head(vsL)

vsR$isL <- 0

new_df <- rbind(vsL,vsR)
head(new_df)

df <- new_df

#quantile(df$spray_angle,probs=seq(0,1,.1))

#### Split the infield into 18 zones... -45 to 45 degree spray angle
#### Assume each fielder will stand at least 5 degrees apart from each other
#### optimize the four infielders' positioning to stop the maximum amount of ground balls

df$zone <- ifelse(df$spray_angle<=-40,1,
                  ifelse(df$spray_angle<=-35,2,
                    ifelse(df$spray_angle<=-30,3,
                       ifelse(df$spray_angle<=-25,4,
                          ifelse(df$spray_angle<=-20,5,
                             ifelse(df$spray_angle<=-15,6,
                                ifelse(df$spray_angle<=-10,7,
                                   ifelse(df$spray_angle<=-5,8,
                                       ifelse(df$spray_angle<=0,9,
                                          ifelse(df$spray_angle<=5,10,
                                              ifelse(df$spray_angle<=10,11,
                                                  ifelse(df$spray_angle<=15,12,
                                                     ifelse(df$spray_angle<=20,13,
                                                         ifelse(df$spray_angle<=25,14,
                                                             ifelse(df$spray_angle<=30,15,
                                                                 ifelse(df$spray_angle<=35,16,
                                                                      ifelse(df$spray_angle<=40,17,18)))))))))))))))))

head(df)

spray_vec <- c()

for (i in 1:18){
  spray_vec[i] <- dim(df[zone==i])[1]
}

### getting the % of the time that the batter hits it to each zone
expected_spray <- (round(spray_vec/dim(df)[1],3)*100)



####### Let's assume that the 1B has a range of 2 total zones, 2B/3B has a range of 3 total zones, and SS has a range of 4 total zones
####### for example, if the positioning is 17-18__1B, this would be placing the 1B between zones 17 and 18, so he can cover them both
####### another example... if the positioning is 5-8__SS, this is the shortstop playing between Zones 6 and 7,
####### with the ability to cover zones 5, 6, 7, and 8

# Also, it will be assumed that the 1B has to play close enough to first base that a play can be made

defensive_options <- c('14-15__1B','15-16__1B','16-17__1B','17-18__1B',
                       '4-6__2B','5-7__2B','6-8__2B','7-9__2B','8-10__2B','9-11__2B','10-12__2B','11-13__2B','12-14__2B','13-15__2B',
                       '1-3__3B','2-4__3B','3-5__3B','4-6__3B','5-7__3B','6-8__3B','7-9__3B','8-10__3B','9-11__3B','10-12__3B',
                       '11-13__3B','12-14__3B',
                       '1-4__SS','2-5__SS','3-6__SS','4-7__SS','5-8__SS','6-9__SS','7-10__SS','8-11__SS','9-12__SS',
                       '10-13__SS','11-14__SS','12-15__SS','13-16__SS')


##### calculating all the values for the expected spray that a fielder will be able to reach based on their positioning
### There are 4 possibilities for where first baseman will play, 10 options for 2B, 12 options for 3B, and 13 options for SS

first1 <- expected_spray[14]+expected_spray[15]
first2 <- expected_spray[15]+expected_spray[16]
first3 <- expected_spray[16]+expected_spray[17]
first4 <- expected_spray[17]+expected_spray[18]
second1 <- expected_spray[4]+expected_spray[5]+expected_spray[6]
second2 <- expected_spray[5]+expected_spray[6]+expected_spray[7]
second3 <- expected_spray[6]+expected_spray[7]+expected_spray[8]
second4 <- expected_spray[7]+expected_spray[8]+expected_spray[9]
second5 <- expected_spray[8]+expected_spray[9]+expected_spray[10]
second6 <- expected_spray[9]+expected_spray[10]+expected_spray[11]
second7 <- expected_spray[10]+expected_spray[11]+expected_spray[12]
second8 <- expected_spray[11]+expected_spray[12]+expected_spray[13]
second9 <- expected_spray[12]+expected_spray[13]+expected_spray[14]
second10 <- expected_spray[13]+expected_spray[14]+expected_spray[15]
third1 <- expected_spray[1]+expected_spray[2]+expected_spray[3]
third2 <- expected_spray[2]+expected_spray[3]+expected_spray[4]
third3 <- expected_spray[3]+expected_spray[4]+expected_spray[5]
third4 <- expected_spray[4]+expected_spray[5]+expected_spray[6]
third5 <- expected_spray[5]+expected_spray[6]+expected_spray[7]
third6 <- expected_spray[6]+expected_spray[7]+expected_spray[8]
third7 <- expected_spray[7]+expected_spray[8]+expected_spray[9]
third8 <- expected_spray[8]+expected_spray[9]+expected_spray[10]
third9 <- expected_spray[9]+expected_spray[10]+expected_spray[11]
third10 <- expected_spray[10]+expected_spray[11]+expected_spray[12]
third11<- expected_spray[11]+expected_spray[12]+expected_spray[13]
third12 <- expected_spray[12]+expected_spray[13]+expected_spray[14]
short1 <- expected_spray[1]+expected_spray[2]+expected_spray[3]+expected_spray[4]
short2 <- expected_spray[2]+expected_spray[3]+expected_spray[4]+expected_spray[5]
short3 <- expected_spray[3]+expected_spray[4]+expected_spray[5]+expected_spray[6]
short4 <- expected_spray[4]+expected_spray[5]+expected_spray[6]+expected_spray[7]
short5 <- expected_spray[5]+expected_spray[6]+expected_spray[7]+expected_spray[8]
short6 <- expected_spray[6]+expected_spray[7]+expected_spray[8]+expected_spray[9]
short7 <- expected_spray[7]+expected_spray[8]+expected_spray[9]+expected_spray[10]
short8 <- expected_spray[8]+expected_spray[9]+expected_spray[10]+expected_spray[11]
short9 <- expected_spray[9]+expected_spray[10]+expected_spray[11]+expected_spray[12]
short10 <- expected_spray[10]+expected_spray[11]+expected_spray[12]+expected_spray[13]
short11 <- expected_spray[11]+expected_spray[12]+expected_spray[13]+expected_spray[14]
short12 <- expected_spray[12]+expected_spray[13]+expected_spray[14]+expected_spray[15]
short13 <- expected_spray[13]+expected_spray[14]+expected_spray[15]+expected_spray[16]

#### we now want to take the 39 possible options and create an objective vector that is the amount of the expected spray 
### of the batter that a fielder covers in that position
objective_vector <- c(first1,first2,first3,first4,
                      second1,second2,second3,second4,second5,second6,second7,second8,second9,second10,
                      third1,third2,third3,third4,third5,third6,third7,third8,third9,third10,third11,third12,
                      short1,short2,short3,short4,short5,short6,short7,short8,short9,short10,short11,short12,short13)


#### now we need to create the large matrix that will factor in the constraints that you cannot repeat a player/position combination
mat <- diag(39)

position_rows <- matrix(0,22,39) ### we add 22 rows to our diagonal matrix
## the first 4 rows are for each position (1B/2B/3B/SS) to make sure that only 1 is chosen
## the other 18 rows are for each of the 18 zones, to ensure that no overlap of zones occurs
## for ex. it would not be smart to have the SS and 2B at 8-10__2B and 8-11__SS, since then you have two guys playing on top of each other

mat <- rbind(mat,position_rows)
colnames(mat) <- defensive_options

mat[40,1:4] <- 1 ### first four columns correspond to first base 
mat[41,5:14] <- 1 ### next 10 columns correspond to second base
mat[42,15:26] <- 1 #### next 12 columns correspond to third base
mat[43,27:39] <- 1 ### next 13 columns correspond to shortstop

### now we fill in the bottom 18 rows of the matrix where we make sure none of zones 1-18 is covered more than once
mat[44,c(15,27)] <- 1 # zone 1
mat[45,c(15,16,27,28)] <- 1 # zone2
mat[46,c(15,16,17,27,28,29)] <- 1 # zone3
mat[47,c(5,16,17,18,27,28,29,30)] <- 1 # zone4
mat[48,c(5,6,17,18,19,28,29,30,31)] <- 1 # zone5
mat[49,c(5,6,7,18,19,20,29,30,31,32)] <- 1 # zone6
mat[50,c(6,7,8,19,20,21,30,31,32,33)] <- 1 # zone7
mat[51,c(7,8,9,20,21,22,31,32,33,34)] <- 1 # zone8
mat[52,c(8,9,10,21,22,23,32,33,34,35)] <- 1 # zone9
mat[53,c(9,10,11,22,23,24,33,34,35,36)] <- 1 # zone10
mat[54,c(10,11,12,23,24,25,34,35,36,37)] <- 1 # zone11
mat[55,c(11,12,13,24,25,26,35,36,37,38)] <- 1 # zone12
mat[56,c(12,13,14,25,26,36,37,38,39)] <- 1 # zone13
mat[57,c(1,13,14,26,37,38,39)] <- 1 # zone14
mat[58,c(1,2,14,38,39)] <- 1 # zone15
mat[59,c(2,3,39)] <- 1 # zone16
mat[60,c(3,4)] <- 1 # zone17
mat[61,4] <- 1 # zone18

## now that we have created the "A matrix", it is time to create the b vector for lpSolve
## The b vector is on the right side of the A matrix in the equation Ax=b where A is 'mat' and x is the 39 variables from 'objective_vector'
## and b is what is created below

b_vector <- rep(1,61)

dir <- rep("<=",61)

s=lp("max",objective_vector,mat,dir,b_vector,int.vec=seq(1:39))
s$objval ## % of IF coverage attainable with optimal strategy
s$solution ## shows the 4 positions chosen out of the 39 possibilities that were analyzed by the linear program optimizer

#### now we convert the output from s$solution to something more readable

ones_vector <- s$solution
answer_key <- data.frame(ones_vector,seq(1:39))
answer_key <- answer_key[answer_key$ones_vector!=0,]

paste('Play 1B in the middle of Zones',substr(defensive_options[answer_key$seq.1.39.][1],1,nchar(defensive_options[answer_key$seq.1.39.][1])-4))
paste('Play 2B in the middle of Zones',substr(defensive_options[answer_key$seq.1.39.][2],1,nchar(defensive_options[answer_key$seq.1.39.][2])-4))
paste('Play 3B in the middle of Zones',substr(defensive_options[answer_key$seq.1.39.][3],1,nchar(defensive_options[answer_key$seq.1.39.][3])-4))
paste('Play SS in the middle of Zones',substr(defensive_options[answer_key$seq.1.39.][4],1,nchar(defensive_options[answer_key$seq.1.39.][4])-4))

paste('This binary integer program took',Sys.time()-oldtime,'seconds to run.')

