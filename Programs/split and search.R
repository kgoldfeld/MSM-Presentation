if (type == "first" & (family == "binomial" | family == "survival")) {
  tempdat$selvar <- do.call("c", 
              lapply(split(tempdat$exposure, 
                           tempdat$id), 
                function(x) if (!is.na(match(1, x))) 
                return(c(rep(1, match(1, x)), rep(0, length(x) -match(1, x))))
                else return(rep(1, length(x)))))
}


dx <- data.table(id=c(1,1,1,1,2,2,2,2,2), x = c(0,1,1,1,0,0,0,0,0))
j <- lapply(split(dx$x, f=dx$id),
       function(x) if (!is.na(match(1, x))) 
         return(c(rep(1, match(1, x)), rep(0, length(x) - match(1, x))))
         else return(rep(1, length(x))))
