foo <-
function(N) {
        
        game.interactions  <- 10000
        contestant.action <- rep(NA, game.interactions)
        game.result       <- rep('lose', game.interactions)
        
        for(i in 1:game.interactions) {
            
            door <- c(0,0,0)
            door[sample(3, 1)] = 1            # assign nice prize to a door
                                        # door  with '1' has  nice prize
                                        # doors with '0' have bad  prize
            initial.pick <- sample(3, 1)      # initial contestant action
            not.picked   <- c(1:3)[-initial.pick]
            door.opened.by.host <- not.picked[1]
            if(door[initial.pick   ]==1) door.opened.by.host = not.picked[
                sample(2,1)]
            if(door[  not.picked[1]]==1) door.opened.by.host = not.picked[2]
            contestant.action[i] <- sample(c('k', 's'), 1)
            second.pick <- ifelse(contestant.action[i] == 'k', initial.pick, 
                                  not.picked[which(not.picked!=door.opened.by.host)])
            if(door[second.pick]==1) game.result[i] = 'win'
        }
        
        x <- table(contestant.action , game.result)         # examine probability of 
                                        # winning by action 
        prop.table(x)
        
    }
