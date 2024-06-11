mt_makeTransitionRatesCode <- function(mtModel, indent=4, toClipboard=F) {
  spaces <- paste0(rep(' ', indent),collapse='')
  result <- mtModel$tbTransitions %>% 
    mutate(comma=ifelse(TransExpression!=last(TransExpression),',','')) %>% 
    rowwise() %>% 
    mutate(spacedTransitionCode=paste0(spaces,'  ',TransExpression,comma,'  # ',TransitionComment,collapse=glue::glue('\n'))) %>%
    ungroup() %>% 
    pull(spacedTransitionCode) %>% 
    paste0(collapse='\n') %>% 
    glue(
      '{spaces}tranrate <- array(c(\n',.,'\n{spaces}), dim=c(N, {nrow(mtModel$tbTransitions)}))\n{spaces}tranrate <- c(t(tranrate))'
    )
  if (toClipboard) {
    clipr::write_clip(result)
  }
  result
}

mt_makeTransitionCreationCode <- function(mtModel, modelLetter, indent=8, toClipboard=F) {
  spaces <- paste0(rep(' ', indent),collapse='')
  result <- mtModel$tbTransitions %>% 
    rowwise() %>% 
    mutate(u1=ifelse(str_detect(From, 'Null'),To,From) %>% str_replace('\\[[nxt]+\\]',''),
           u2=ifelse(str_detect(To, 'Null'),From,To) %>% str_replace('\\[[nxt]+\\]',''),
           n1=From %>% str_extract('\\[(.+)\\]') %>% str_extract('[^\\[\\]]+'),
           n2=To %>% str_extract('\\[(.+)\\]') %>% str_extract('[^\\[\\]]+'),
           v1=ifelse(str_detect(From, 'Null'),'0','-1'),
           v2=ifelse(str_detect(To, 'Null'),'0','+1')) %>% 
    mutate(u1=ifelse(str_detect(u1, 'Null'), first(mtModel$tbCompartments$State), u1),
           u2=ifelse(str_detect(u2, 'Null'), first(mtModel$tbCompartments$State), u2))
  if (any(is.na(result$n1)|is.na(result$n2))) warning('One or more compartments did not contain [n] or [nxt]. You should review your transitions list. Replacing them with [n] so the model has a chance to run ;)')
  result <- result %>%
    ungroup() %>% 
    mutate(n1=ifelse(is.na(n1), 'n', n1),
           n2=ifelse(is.na(n2), 'n', n2)) %>% 
    left_join(select(mtModel$tbCompartments, srcCompartmentID=id, srcCompartment=State), by=c(u1='srcCompartment')) %>% 
    left_join(select(mtModel$tbCompartments, dstCompartmentID=id, dstCompartment=State), by=c(u2='dstCompartment')) %>% 
    mutate(transitions=glue::glue(
      "{spaces}transitions_{modelLetter}[traind_{modelLetter}[{id},n],] <- c(varind_{modelLetter}[{srcCompartmentID},{n1}],  {v1}, varind_{modelLetter}[{dstCompartmentID},{n2}], {v2})"
    )) %>% 
    pull(transitions) %>% 
    paste0(collapse='\n')
  if (toClipboard) {
    clipr::write_clip(result)
  }
  result
}

modelFilename <- 'RubellaModel.xlsx'

mt_getTibblesInList <- function(modelFilename) {
  lsModel <- read_excel_allsheets(modelFilename, tibble=T)
  
  tbCompartments <- lsModel$Compartments %>%
    rowid_to_column(var='id')
  
  tbTransitions <- lsModel$Transitions %>%
    mutate(TransitionComment=ifelse(is.na(TransitionComment), '', TransitionComment)) %>% 
    rowid_to_column(var='id')
  
  alivepop <- tbCompartments %>%
    filter(isAlive) %>%
    pull(id)
  
  # varind
  varind<-matrix(seq(nrow(tbCompartments)*N), ncol = N)
  rownames(varind) <- tbCompartments$State
  # traind
  traind<-matrix(seq(nrow(tbTransitions)*N), ncol = N)
  rownames(traind) <- tbTransitions$TransitionName
  
  list(lsModel=lsModel,
       tbCompartments=tbCompartments,
       tbTransitions=tbTransitions,
       tbAges=lsModel$Ages,
       alivepop=alivepop,
       varind=varind,
       traind=traind)
}
