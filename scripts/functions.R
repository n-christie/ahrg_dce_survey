# functions

# create age groups
get_age_group <- function(age) {
  case_when(
    between(age, 55, 64) ~ "55-64",
    between(age, 65, 74) ~ "65-74",
    between(age, 75, 84) ~ "75-84",
    age >= 85 ~ "85+",
    TRUE ~ NA_character_
  )
}



stargazernote <- function(starGazerCmd, outputfile, note){
  # inserts 'note' to the end of stargazer generated latex table
  ssn <- gsub(x=starGazerCmd, pattern='\\end{tabular}',
              replacement=paste('\\end{tabular}', '\n',
                                '\\centerline{\\begin{minipage}{0.95\\textwidth}~\\', '\n',
                                '\\footnotesize{' , note, 
                                '} \\end{minipage}}',  sep=''), fixed=T)
  
  cat(ssn,sep='\n',file=outputfile)
}
