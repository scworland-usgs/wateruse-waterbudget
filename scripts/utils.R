
sw_weighted_interp <- function(from,value,to,to_group_id,weight_var,sum=TRUE){
  
  # several checks
  if(inherits(weight_var,"VeloxRaster")==FALSE) stop("'weight_var' must be of class VeloxRaster")
  if(inherits(from,"sf")==FALSE) stop("'from' must be of class sf")
  #if(all(st_is(from, "MULTIPOLYGON"))==FALSE) stop("'from' geometry type must be 'MULTIPOLYGON'")
  if(inherits(to,"sf")==FALSE) stop("'to' must be of class sf")
  #if(all(st_is(to, "MULTIPOLYGON"))==FALSE) stop("'from' geometry type must be 'MULTIPOLYGON'")
  
  if(sum==T){# use sum
    # aggregate weighting variable to "from" geometry
    from_weight_var <- weight_var$extract(st_transform(from,weight_var$crs),fun=function(x) sum(x,na.rm=T),df=TRUE)
    
    # add weight var to "from" geometry
    from$from_weight_var <- from_weight_var$out
    
    # find intersection between "from" and "to"
    from_to <- from %>%
      st_intersection(to) %>%
      st_cast('MULTIPOLYGON') %>%
      st_transform(weight_var$crs)
    
    # aggregate weighting variable to "to" geometry
    to_weight_var <- weight_var$extract(from_to,fun=function(x) sum(x,na.rm=T),df=TRUE)
    
    # calculate weight and new value
    result <- from_to %>%
      dplyr::mutate(to_weight_var = to_weight_var$out,
                    p = to_weight_var/from_weight_var,
                    to_value = round(!!sym(value)*p,3)) %>%
      group_by(!!sym(to_group_id)) %>%
      dplyr::summarize(to_value = sum(to_value, na.rm=T))
    
  }else{#use mean
    # aggregate weighting variable to "from" geometry
    from_weight_var <- weight_var$extract(st_transform(from,weight_var$crs),fun=function(x) mean(x,na.rm=T),df=TRUE)
    
    # add weight var to "from" geometry
    from$from_weight_var <- from_weight_var$out
    
    # find intersection between "from" and "to"
    from_to <- from %>%
      st_intersection(to) %>%
      st_cast('MULTIPOLYGON') %>%
      st_transform(weight_var$crs)
    
    # aggregate weighting variable to "to" geometry
    to_weight_var <- weight_var$extract(from_to,fun=function(x) mean(x,na.rm=T),df=TRUE)
    
    # calculate weight and new value
    result <- from_to %>%
      dplyr::mutate(to_weight_var = to_weight_var$out,
                    p = to_weight_var/from_weight_var,
                    to_value = round(!!sym(value)*p,3)) %>%
      group_by(!!sym(to_group_id)) %>%
      dplyr::summarize(to_value = mean(to_value, na.rm=T))
  }
  
  return(result)
}







