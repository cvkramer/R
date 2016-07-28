multiClass = function(data, vector, keys){
    i = data[vector] == keys[1]
    if( length(keys) > 1 ){
        for( key in 2:length(keys) ){
            i = i | data[vector] == keys[key]
        }
    }
    return( as.vector(i) )
}
