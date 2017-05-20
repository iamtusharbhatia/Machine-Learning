balance <- 1000
wager <- 100
Svar <- 0
flag <- 0
count<- 0
SumSet <-  c(0,0)
NoGames <- 10

sink("output.txt")
for (round in 1:5) 
{			
	################################################### Even Wager	
	while(TRUE)
	{
		if(balance <= 0){
		break
		}
		if(count == 10){
		break
		}
		num1 <- sample(1:6, 1)	
		num2 <- sample(1:6, 1)
		sum <- num1 +num2
		SumSet[count+1] <- sum
		if (flag == 0)
		{
			if(sum == 7 || sum == 11){
			   balance = balance + wager;
			   count = count +1;
			   next
			}
			else if(sum == 2 || sum == 3 || sum == 12){
			   balance = balance - wager;
			   count = count +1;
			   next
			}
			else {
				flag <- 1
				count = count +1;
				Svar <- sum
				next
			}
		}
		if(flag == 1)
		{			
			if(sum == 7){
			   balance = balance - wager;
			   count = count +1;
			}
			else if(sum == Svar){
			   balance = balance + wager;
			   count = count +1;
			}
		}			
	}
 
   cat("Round ")
   cat(round)
   cat("\n")
   cat("Strategy","                   No of games","     Ending Balance\n")
   cat("Even Wager   				",10,"             ",balance,"\n")
   
  balance <- 1000
	flag <- 0
	wager <- 100
	Svar <- 0
	count <- 0
	
	
	################################################### Martingale System	
	
	while(TRUE) 
	{
		if(balance <= 0)
		{
			NoGames <- count
			break
		}
		if(count == 10)    
		{
			break
		}
		
		sum <- SumSet[count+1]
		if (flag == 0)
		{
			if(sum == 7 || sum == 11){
			   balance = balance + wager;
			   count = count +1;
			   wager <- 100
			   next
			}
			else if(sum == 2 || sum == 3 || sum == 12){
			   balance = balance - wager;
			   count = count +1;
			   wager <- wager*2
			   if(wager > balance)
			   {
					wager <- balance
			   }
			   next
			}
			else {
				flag <- 1
				Svar <- sum
			}
		}
		if(flag == 1)
		{			
			if(sum == 7){
			   balance = balance - wager;
			   count = count +1;
			   wager <- wager*2
			   if(wager > balance)
			   {
					wager <- balance
			   }
			}
			else if(sum == Svar){
			   balance = balance + wager;
			   count = count +1;
			   wager <- 100
			}
		}			
	}
	
	cat("Martingale System   		",NoGames,"             ",balance,"\n")
	
	
	balance <- 1000
	flag <- 0
	wager <- 100
	Svar <- 0
	count <- 0
	NoGames <- 10
	
	
	################################################ Reverse Martingale System
	
	
	while(TRUE) 
	{
		if(balance <= 0)
		{
			NoGames <- count
			break
		}
		if(count == 10)
		{
			break
		}
		sum <- SumSet[count+1]
		if (flag == 0)
		{
			if(sum == 7 || sum == 11){
			   balance = balance + wager;
			   count = count +1;
			   wager <- wager*2
			   if(wager > balance)
			   {
					wager <- balance
			   }
			   next
			}
			else if(sum == 2 || sum == 3 || sum == 12){
			   balance = balance - wager;
			   count = count +1;
			   wager <- 100
			   next
			}
			else {
				flag <- 1
				Svar <- sum
			}
		}
		if(flag == 1)
		{			
			if(sum == 7){
			   balance = balance - wager;
			   count = count +1;
			   wager <- 100
			}
			else if(sum == Svar){
			   balance = balance + wager;
			   count = count +1;
			   wager <- wager*2
			   if(wager > balance)
			   {
					wager <- balance
			   }
			}
		}			
	}
	cat("Reverse Martingale System   ",NoGames,"             ",balance,"\n\n\n")
	
	balance <- 1000
	flag <- 0
	wager <- 100
	Svar <- 0
	count <- 0	
	NoGames <- 10
}
sink()
