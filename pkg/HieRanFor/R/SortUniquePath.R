

# sort unique.path according to consecutive levels of the hierarcy, starting from L0 and ending with the lowest level
SortUniquePath=function(unique.path,
                        ...)
{ # start function
  #require(doBy)
  fmla             <- as.formula(paste(" ~ " , paste(colnames(unique.path) , collapse = "+")))
  unique.path.sort <- doBy::orderBy(fmla , data=unique.path)
  
} # end function