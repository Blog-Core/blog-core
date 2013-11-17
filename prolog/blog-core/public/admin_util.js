var util = (function(exports) {
    
    exports.nameToLabel = function(name) {
        
        return name.substring(0, 1).toUpperCase() +
            name.substring(1);
    };
    
    // Sorts documents by the given property.
    
    exports.sortByProperty = function(docs, name, desc) {
        
        docs.sort(function(d1, d2) {
            
            var v1 = d1[name];
            
            var v2 = d2[name];
            
            var ret = 0;
            
            if (v1 < v2) {
                
                ret = -1;
                
            } else if (v1 > v2) {
                
                ret = 1;
            }
            
            return desc ? -1 * ret : ret;
        });        
    };
    
    return exports;
    
})({}); 
