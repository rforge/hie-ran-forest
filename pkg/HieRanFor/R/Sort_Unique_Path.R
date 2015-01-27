

# sort Unique_Path according to consecutive levels of the hierarcy, starting from L0 and ending with the lowest level
Sort_Unique_Path=function(Unique_Path,
                          ...)
{ # start function
  require(doBy)
  fmla             <- as.formula(paste(" ~ ", paste(colnames(Unique_Path), collapse= "+")))
  Unique_Path_sort <- orderBy(fmla, data=Unique_Path)
  
} # end function