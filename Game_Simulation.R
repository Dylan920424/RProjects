print("In this game")
Sys.sleep(1.5)
print("You'll have to roll a die and spin a spinner")
Sys.sleep(1.5)
print("And if the two is identical, you score the point")
Sys.sleep(1.5)
print("We'll play for 5 rounds")
Sys.sleep(1.5)
print("Starting in 3")
Sys.sleep(1)
print("2")
Sys.sleep(1)
print("1")
Sys.sleep(2)

numbered_spinner <- c(1,2,3,4,5,6)
numbered_spinner_prob <- c(0.24,0.30,0.12,0.2,0.08,0.06)
dice_prob <- c(0.12,0.12,0.12,0.12,0.12,0.4)
my_total_score <- 0
player2_score <- 0
rounds <- 0
round_limit <- 5

while(rounds<=round_limit) {
spinner_result <- sample(numbered_spinner,1,prob=numbered_spinner_prob)
print(spinner_result)
dice_result <- sample(dice,1,prob=dice_prob)
print(dice_result)
if(spinner_result == dice_result) {
  my_total_score <- my_total_score + spinner_result
} 
rounds = rounds + 1
}

print(paste0("Your score is ", my_total_score))

Sys.sleep(3)
rounds <- 0

while(rounds<=round_limit) {
spinner_result <- sample(numbered_spinner,1,prob=numbered_spinner_prob)
print(spinner_result)
dice_result <- sample(dice,1,prob=dice_prob)
print(dice_result)
if(spinner_result == dice_result) {
  player2_score <- player2_score + spinner_result
}
rounds = rounds + 1
}

print(paste0("Computer's score is ", player2_score))
Sys.sleep(3)

if(my_total_score>player2_score) {
  print(paste0("You won by ", my_total_score - player2_score, " points"))
} else {
  if(player2_score>my_total_score) {
    print(paste0("The computer won by ", player2_score - my_total_score, " points"))
  } else{
    print(paste0("Tied ", my_total_score ,":", player2_score))
  }
}
