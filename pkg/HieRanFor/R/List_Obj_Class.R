

# return a data frame of all the objects in the global environment and their class. 

List_Obj_Class = function(All_Objects=ls(.GlobalEnv),
                          ...)
{
  Num_Object <- length(All_Objects)
  Obj_Class_Table <- data.frame(Objects_Names    = c(NA),
                                Class_of_Objects = c(NA))
  Obj_Class_Table[1:Num_Object,] <- NA
  
  for (i in 1:Num_Object)
  {
    Obj_Class_Table[i,1] <- All_Objects[i]
    Focal_Class <- class(get(All_Objects[i]))
    Focal_Class <- paste(as.vector(Focal_Class),collapse="_")
    Obj_Class_Table[i,2] <-  Focal_Class
  }
  Obj_Class_Table
}