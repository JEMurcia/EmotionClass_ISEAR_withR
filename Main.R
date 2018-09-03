source("PreProcessEmail.R")
library(plotly)
# MAIN PROGRAM


# MAIN PROCEDURE

# path : location of all the uninteresting files to be B-cells
# kt : number of mc's to begenerated
# ksm : initial stimulation count for mc's
# ksb : initial stimulation count for bc's
# ka : affinity threshold
# kl : constant for rate of cloning
# km : constant for rate of mutation
# kc : classification threshold
# return a list of the clasification
test <- function(path,kt,ksm,ksb,ka,kl,km,kc, num_emails = 100){
  write("The model is training ...", stdout())
  defenses <- train(get.emails(num_emails = num_emails, path = paste(path, "spam", sep = "/") ),kt,ksm,ksb,ka,kl,km)
  spam <- list.files(path = paste(path, "spam", sep = "/") )
  ham <- list.files(path = paste(path, "ham", sep = "/") )
  clas_spam <- list(); clas_ham <- list(); write("Evluation starts ...", stdout())
  write("Spam classification ...", stdout())
  for ( i in spam ){
    ag <- process.email( paste(path, "spam", i, sep = "/") )
    clas_spam[length(clas_spam)+1] <- classify(ag,defenses[[1]],defenses[[2]],kc)[1]
    if ( runif(1) >= 0.9 ){ 
      defenses <- update_population(defenses[[1]], defenses[[2]], ka, ksm, ksb, kl, km, 'uninteresting', clas_spam[length(clas_spam)][[1]], ag)
    }
  }
  clas_spam <- unlist(clas_spam)
  write("Ham classification ...", stdout())
  for ( i in ham ){
    ag <- process.email( paste(path, "ham", i, sep = "/") )
    clas_ham[length(clas_ham)+1] <- classify(ag,defenses[[1]],defenses[[2]],kc)[1]
    if ( runif(1) >= 0.8 ){ 
      defenses <- update_population(defenses[[1]], defenses[[2]], ka, ksm, ksb, kl, km, 'interesting', clas_ham[length(clas_ham)][[1]], ag)
    }
  }
  clas_ham <- unlist(clas_ham)
  return( list( clas_ham, clas_spam ) )
}

graph <- function(){
  x <- c(100, 140, 300, 500)
  y_ham <- c(); y_spam <- c(); j <- 1
  for ( i in x ){
    lista_clas <- test("C:/Users/Sebastian/Documents/APAU/enron1/enron1",20,25,125,0.5,7.0,0.7,0.5, i)
    y_ham[j] <- ( sum(lista_clas[[1]] == "interesting")*100) / ( sum(lista_clas[[1]] == "interesting") + sum(lista_clas[[1]] == "uninteresting") )
    y_spam[j] <- ( sum(lista_clas[[2]] == "uninteresting")*100) / ( sum(lista_clas[[2]] == "interesting") + sum(lista_clas[[2]] == "uninteresting") )
    j <- j+1
  }
  plot(x, unlist(y_ham), main = "Porcentaje de efectividad interesantes vs training set")
  plot(x, unlist(y_spam), main = "Porcentaje de efectividad no interesantes vs training set")
  return( list( unlist(y_ham), unlist(y_spam) ) )
}

# path : location of all the uninteresting files to be B-cells
# kt : number of mc's to begenerated
# ksm : initial stimulation count for mc's
# ksb : initial stimulation count for bc's
# ka : affinity threshold
# kl : constant for rate of cloning
# km : constant for rate of mutation
# kc : classification threshold
# no return
aisec.main <- function(path,kt,ksm,ksb,ka,kl,km,kc){
  
  #defenses <- train(emails.files(path),kt,ksm,ksb,ka,kl,km)
  defenses <- train(get.emails(num_emails = 500, path = path),kt,ksm,ksb,ka,kl,km)
  # wait until an e-mail arrives or a user action is intercepted
  opc <- 1
  while(opc == 1 || opc == 2){
    cat('\n\n WELCOME, PLEASE PRESS THE NUMBER OF YOUR CHOICE')
    cat('\n\t 1: CLASSIFY AN EMAIL/ANTIGEN')
    cat('\n\t 2: GIVE A FEEDBACK FOR AN EMAIL/ANTIGEN')
    cat('\n\t Other : Quit')
    opc <- readline()
    if (opc == 1 || opc==2) {
      # convert email into antigen
      path <- readline('Input the email file source: ')
      ag <- process.email(path)
      # ag requires classification
      if (opc == 1) {
        response <- classify(ag,defenses[[1]],defenses[[2]],kc)
        if (response == 'uninteresting') {
          # move ag into user accessible storage 
          cat('\n The e-mail was considering as uninteresting')
          process.cell()
        }else{
          cat('\n The e-mail was considering as interesting')
        }
      }
      # user has given feedback on ag
      if (opc == 2) {
        user.feedback <- readline('\n Your Feedback (uninteresting or interesting) : ')
        defenses <- update_population(defenses[[1]], defenses[[2]], ka, ksm, ksb, kl, km, user.feedback, 'uninteresting', ag)
        cat('\n\t Population Updated')
        if( user.feedback == 'interesting' ){ file.remove(path) }
      }
    }else{
      cat('BYE :(  \n')
      break()
    }
  }
}



# READING TRAINING EMAILS

# filespath : location of all the uninteresting files to be B-cells
# return vector of all the files names
emails.files <- function(filespath){
  
  files <- list.files(path = filespath)
  return(paste(filespath, files, sep = "/"))
  
}



# TRAINING PROCEDURE

# training.emails : vector of all the unisteresting files names
# kt : number of mc's to begenerated
# ksm : initial stimulation count for mc's
# ksb : initial stimulation count for bc's
# ka : affinity threshold
# kl : constant for rate of cloning
# km : constant for rate of mutation
# return the trained naive B-cells & Memory B-Cells by uninteresting emails as a list
train <- function(training.emails,kt,ksm,ksb,ka,kl,km){
  t.cells <- list(); m.cells <- list(); b.cells <- list()
  m.index <- sample(1:length(training.emails),kt)
  count <- 1
  gene.library <<- list(c(),c())
  #for (email in training.emails) {
  for (email in 1:length(training.emails)) {
    # Procces email to a B-cell
    #t.bc <- process.email(email)
    t.bc <- training.emails[[email]]
    if (count %in% m.index) {
      # insert Kt random elements from TE into MC
      m.cells[[length(m.cells)+1]] <- t.bc
    }else{
      t.cells[[length(t.cells)+1]] <- t.bc
    }
    count <- count + 1
    # add subject words and sender words to appropriate library
    gene.library[[1]] <<- c(gene.library[[1]],t.bc[[1]])
    gene.library[[2]] <<- c(gene.library[[2]],t.bc[[2]])
  }
  for (mc in 1:length(m.cells)) {
    # mc's stimulation count <- Ksm
    m.cells[[mc]][[3]] <- ksm
  }
  for (te in 1:length(t.cells)) {
    # te's stimulation count <- Ksb
    t.cells[[te]][[3]] <- ksb
    for (mc in 1:length(m.cells)) {
      # IF(affinity(mc,te) > Ka)
      if (affinity(m.cells[[mc]],t.cells[[te]]) > ka) {
        # clones <- clone_mutate(mc,te)
        clones <- clone_mutate(m.cells[[mc]],t.cells[[te]],ksb,kl,km)
        for (clon in 1:length(clones)) {
          # IF(affinity(clo,'bc'/>'mc) >= affinity(mc,te))
          if ( affinity(clones[[clon]],t.cells[[te]]) >= affinity(m.cells[[mc]],t.cells[[te]])) {
            # BC <- clon with higher affinity than his 'predecessor'te
            b.cells[[length(b.cells)+1]] <- clones[[clon]]
          }
        }
      }
    }
  }
  
  return(list(b.cells,m.cells)) 
  
}




# AFFINITY PROCEDURE

# bc1 & bc2 : B-cells/antigens
# return the affinity between the B-cells/antigens
affinity <- function(bc1, bc2){
  
  # IF(bc1 has a shorter feature vector than bc2) 
  if ((length(bc1[[1]])+length(bc1[[2]])) < (length(bc2[[1]])+length(bc2[[2]]))) {
    bshort <- bc1; blong <- bc2
  }else{
    bshort <- bc2; blong <- bc1
  }
  # rep. words <- the number of words in bshort present in blong 
  rep.words <- 0
  for (subject.w in bshort[[1]]) {
    if (subject.w %in% blong[[1]]) {
      rep.words <- rep.words + 1
    }
  }
  for (sender.w in bshort[[2]]) {
    if (sender.w %in% blong[[2]]) {
      rep.words <- rep.words + 1
    }
  }
  
  # rep.words / the length of bshort's feature vector
  return(rep.words/(length(bshort[[1]])+length(bshort[[2]])))
  
}



# CLONE & MUTATE PROCEDURE

# bc1 & bc2 : B-cells/antigens
# ksb : initial stimulation count for bc's
# kl : constant for rate of cloning
# km : constant for rate of mutation
# return a bc1 B-cell cloned and mutated
clone_mutate <- function(bc1,bc2,ksb,kl,km){
  aff <- affinity(bc1,bc2)
  clones <- list()
  feature.vect.len <- (length(bc1[[1]])+length(bc1[[2]]))
  num_clones <- floor(aff*kl)
  num_mutate <- floor((1-aff)*km*feature.vect.len)
  for (i in 1:num_clones) {
    bcx <- bc1
    for (j in 1:num_mutate) {
      # a random for subject or sender
      sub.or.send <- sample.int(2,1)
      # a random word from the appropriate gene library
      gene.change <- sample(gene.library[[sub.or.send]],1)
      # a random point in bcx's feature vector
      # replace word in bcx's feature vector at location p with w 
      bcx[[sub.or.send]][sample(bcx[[sub.or.send]],1)] <- gene.change
    }
    # bcx's stimulation level <- Ksb
    bcx[[3]] <- ksb
    clones[[length(clones)+1]] <- bcx
  }
  
  return(clones)
  
}




# CLASSIFY PROCEDURE

# BC : naive B-Cells
# MC : memory B-Cells
# kc : classification threshold
# return if the antigen is uninteresting or interesting
classify <- function(ag,BC,MC,kc){
  
  response <- 'interesting'
  for (bc in 1:length(BC)) {
    if (affinity(ag,BC[[bc]]) > kc) {
      # classify ag as uninteresting
      #str(affinity(ag,BC[[bc]]))
      response <- 'uninteresting'
      return(response)
    }
  }
  for (mc in 1:length(MC)) {
    if (affinity(ag,MC[[mc]]) > kc) {
      # classify ag as uninteresting
      #str(affinity(ag,MC[[mc]]))
      response <- 'uninteresting'
      return(response)
    }
  }
  return(response)
  
}

#UPDATE POPULATION PROCEDURE

# BC : naive B-Cells
# MC : memory B-Cells
# ka : affinity threshold
# ksm : initial stimulation count for mc's
# ksb : initial stimulation count for bc's
# kl : constant for rate of cloning
# km : constant for rate of mutation
# feedback: feedback provided by the user about the classification 
# classification: the current classification
# ag: antigen classified
# return the trained naive B-cells & Memory B-Cells by uninteresting emails as a list

update_population <- function(BC, MC, ka, ksm, ksb, kl, km, feedback, classification, ag){
  if ( feedback == classification ){
    max_aff <- -1; bc_best <- -1
    for ( i in 1:length(BC) ){
      aff <- affinity(ag, BC[[i]])
      if ( aff > ka ){
        BC[[i]][[3]] <- BC[[i]][[3]]+1
      }
      if ( max_aff < aff ){
        bc_best <- i; max_aff <- aff
      }
    }
    n <- length(BC); clones <- clone_mutate( BC[[bc_best]], ag, ksb, kl, km ); BC <- append(BC, clones)
    for ( i in 1:length(clones) ){
      aff <- affinity( clones[[i]], ag )
      if ( aff > max_aff ){
        bc_best <- n+i; max_aff <- aff
      }
    }
    mc_best <- -1; max_aff <- -1
    for ( i in 1:length(MC) ){
      aff <- affinity( MC[[i]], ag )
      if ( aff > max_aff ){
        mc_best <- i; max_aff <- aff
      }
    }
    if ( affinity( BC[[bc_best]], ag) > affinity( MC[[mc_best]], ag ) ){
      b_cell_best <- BC[[ bc_best ]]; BC <- BC[ -bc_best ]
      MC[[length(MC)+1]] <- b_cell_best
      for ( i in 1:length(MC) ){
        if ( affinity( MC[[i]], b_cell_best ) > ka ){
          MC[[i]][[3]] <- MC[[i]][[3]]-1
        }
      }
    }
    #Add words from ag's feature vector to gene libraries
  } else {
    rm <- list(); j <- 1; 
    for ( i in 1:length(BC) ){
      if ( affinity( BC[[i]], ag ) > ka ){
        #Remove all words in bc's feature vector from gene libraries
        rm[j] <- -i; j <- j+1
      }
    }
    if ( length(rm) > 0 && length(rm) != length(BC) ){ BC <- BC[unlist(rm)] }
    rm <- list(); j <- 1
    for ( i in 1:length(MC) ){
      if ( affinity( MC[[i]], ag ) > ka ){
        #Remove all words in bc's feature vector from gene libraries
        rm[j] <- -i; j <- j+1
      }
    }
    if ( length(rm) > 0 && length(rm) != length(MC) ){ MC <- MC[unlist(rm)] }
  }
  rm <- list(); j <- 1
  for ( i in 1:length(BC) ){
    BC[[i]][[3]] <- BC[[i]][[3]] - 1
    if ( BC[[i]][[3]] <= 0 ){
      rm[j] <- -i; j <- j+1
    }
  }
  if ( length(rm) > 0  && length(rm) != length(BC)){ BC <- BC[unlist(rm)] }
  rm <- list(); j <- 1
  for ( i in 1:length(MC) ){
    if ( MC[[i]][[3]] <= 0 ){
      rm[j] <- -i; j <- j+1
    }
  }
  if ( length(rm) > 0  && length(rm) != length(MC)){ MC <- MC[unlist(rm)] }
  return(list(BC,MC))
  
}

