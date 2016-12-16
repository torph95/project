n.words <- 20  #total number of words in the lexicon of the model
initial.activation <- 0  #activation value at beginning
activation.studied <- 1  #activation value when word is studied
n.words.list <- 15  #list of words actually related to the 
lure.word <- 16   #the position of the lure word
sd <- .05     #standard deviation for computing the reverse weight of an edge
n.trials <- 500    #number of trials
decay.rate <- .2  # decay rate
activation.threshold <- .8  #threshold above which a node fires
recalled.threshold <- .8   #threshold above which a word is considered to be recalled

words <- rep(initial.activation, each= n.words) #activation values

#the 15 words that are related to the lure word are words with index 1-15, the critical lure is
# the word with index 16, the remaining 4 are unrelated to the lure word

#we initialize a matrix of weight with weights for each pair of words
weights <- matrix(rep(0, n.words*n.words), nrow = n.words, ncol = n.words)

# we consider that weights between .4 and .7 represent strong association between two words,
# weights between .2 and .4 represent a mild association between two words, and a weight 
# under .2 to represent a very low association between two words. A value of 0 means that the
# words are not related.
initialize.weights <- function(){
  for (i in 1:n.words.list){
    # weights of words of the list to lure word
    weights[i,lure.word] <<- runif(1, min= .4, max = .7)
    # weights of the lure word to the 15 words of the list
    weights[lure.word, i] <<- rnorm(n = 1, mean = weights[i, lure.word], sd = sd)
    for (n in 1:n.words.list) {
      #weights of the 15 words between each other
      weights[i, n] <<- runif(1, min = .2, max = .4 )
      weights[n, i] <<- rnorm(n = 1, mean = weights[i, n], sd = sd)
    }
  }
  for (i in (lure.word+1):n.words) {
    for (n in 1:n.words){
      # weights btween the unrelated words to all the words
      weights[i, n] <<- runif(1, min=0, max = .7)
      weights[n, i] <<- rnorm(n= 1, mean = weights[i,n], sd= sd)
    }
    for (n in 1:n.words.list) {
      #weights of the unrelated words to the studied words
      weights[i, n] <<- runif(1, min=0, max = .4)
      weights[n, i] <<- rnorm(n= 1, mean = weights[i,n], sd= sd)
    }
    #weights between unrelated words and lure word
    weights[i,lure.word] <<- 0
    weights[lure.word, i] <<- 0
  }
  # check that values are between 0 and 1
  for (n in 1:n.words) {
    for (m in 1:n.words) {
      if (weights[n,m] > 1){
        weights[n,m] <<- 1
      }
      if (weights[n,m] < 0){
        weights[n,m] <<- 0
      }
    }
    #weights of any word to itself is set to 1
    weights[n, n] <<- 1
  }
}
initialize.weights()


#each trial is divided into activation steps. what we reference as step is the presentation of
#a word from the studied list in the model. Once a step is initiated, the activation of the word
# is set to 1, and the node fires. Once it has fired and all activation values have been computed
# for the other nodes, the nodes with an activation above the activation threshold will fire. 
run.trial <- function(){
  for (n in 1:n.words.list){
    words[n] <<- activation.studied
    for (i in 1:n.words){
      if (i != n){
        words[i] <<- words[i] + (words[n] * decay.rate * weights[n,i])
        if(words[i] > 1){
          words[i] <<- 1
        }
        if(words[i] < 0){
          words[i] <<- 0
        }
        #print(words)
        firing.node <- which(words[-n] >= activation.threshold)
        if(is.null(firing.node)){
          for (a in 1:length(firing.node)) {
            for(b in 1:n.words){
              if (firing.node[a] != b){
                words[b] <<- words[b] + (words[firing.node[a]] * decay.rate * weights[firing.node[a],b])
                if(words[b] > 1){
                  words[b] <<- 1
                }
                if(words[b] < 0){
                  words[b] <<- 0
                }
                #print(words)
              }
            }
          }
        }
      }
    }
    words[n] <<- words[n] - decay.rate
    if (words[n] > 1){
      words[n] <<- 1
    }
    if (words[n] < 0){
      words[n] <<- 0
    }
  }
}

run.simulation <- function(trials){
  word.list.counter <- 0
  lure.word.counter <- 0
  unrelated.word.counter <- 0
  for (i in 1:trials){
    words <<- rep(initial.activation, each= n.words)
    initialize.weights()
    run.trial()
    for(n in 1:n.words.list){
      if(words[n] >= recalled.threshold){
        word.list.counter <- word.list.counter + 1
      }
    }
    for(n in (lure.word+1):n.words){
      if(words[n] >= recalled.threshold){
        unrelated.word.counter <- unrelated.word.counter + 1
      }
    }
    if (words[lure.word] >= recalled.threshold){
      lure.word.counter <- lure.word.counter + 1
    }
  }
  proportion.recall.list <- word.list.counter/(trials*n.words.list)
  proportion.recall.lure <- lure.word.counter/trials
  proportion.recall.unrelated <- unrelated.word.counter/(trials*(n.words-lure.word))
  return(list(proportion.recall.list = proportion.recall.list, 
              proportion.recall.lure = proportion.recall.lure, 
              proportion.recall.unrelated = proportion.recall.unrelated))
}
run.simulation(n.trials)
words
