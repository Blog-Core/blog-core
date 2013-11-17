var api = (function(exports) {
    
    function get(url, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('GET', url, true);
        
        xhr.addEventListener('load', function() {
            
            var res = JSON.parse(xhr.responseText);            
            
            cb(null, res.data);
            
        }, false);
        
        xhr.send();        
    }
    
    function del(url, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('DELETE', url, true);
        
        xhr.addEventListener('load', function() {
            
            var res = JSON.parse(xhr.responseText);            
            
            cb(null, res.data);
            
        }, false);
        
        xhr.send();   
    }
    
    function put(url, doc, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('PUT', url, true);
        xhr.setRequestHeader('Content-Type', 'application/json');
        
        xhr.addEventListener('load', function() {
            
            var res = JSON.parse(xhr.responseText);            
            
            cb(null, res.data);
            
        }, false);
        
        xhr.send(JSON.stringify(doc));
    }
    
    function post(url, doc, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('POST', url, true);
        xhr.setRequestHeader('Content-Type', 'application/json');
        
        xhr.addEventListener('load', function() {
            
            var res = JSON.parse(xhr.responseText);            
            
            cb(null, res.data);
            
        }, false);
        
        xhr.send(JSON.stringify(doc));
    }
    
    exports.types = function(cb) {
        
        get('/api/col/types', cb);
    };

    exports.doc = function(id, cb) {
        
        get('/api/doc/' + id, cb);
    };
    
    exports.docType = function(id, cb) {
        
        get('/api/type/doc/' + id, cb);
    };
    
    exports.colType = function(id, cb) {
        
        get('/api/type/col/' + id, cb);
    };
    
    exports.collection = function(name, cb) {
    
        get('/api/col/' + name, cb);
    };
    
    exports.update = function(doc, cb) {
        
        put('/api/doc/' + doc.$id, doc, cb);
    };
    
    exports.remove = function(id, cb) {
        
        del('/api/doc/' + id, cb);
    };
    
    exports.create = function(name, doc, cb) {
        
        post('/api/col/' + name, doc, cb);
    };
    
    return exports;
    
})({});
