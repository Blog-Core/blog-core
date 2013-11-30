var api = (function(exports) {
    
    function handle(res, cb) {
        
        if (res.status === 'error') {
                
            var error = new Error('API call failed.');
            
            error.code = res.code;
            
            cb(error);
            
        } else {
            
            cb(null, res.data);
        }
    }
    
    function get(url, key, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('GET', url, true);
        
        xhr.setRequestHeader('X-Key', key);
        
        xhr.addEventListener('load', function() {
            
            handle(JSON.parse(xhr.responseText), cb);                        
            
        }, false);
        
        xhr.send();        
    }
    
    function del(url, key, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('DELETE', url, true);
        
        xhr.setRequestHeader('X-Key', key);
        
        xhr.addEventListener('load', function() {
            
            handle(JSON.parse(xhr.responseText), cb);
            
        }, false);
        
        xhr.send();   
    }
    
    function put(url, doc, key, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('PUT', url, true);
        
        xhr.setRequestHeader('Content-Type', 'application/json');
        
        xhr.setRequestHeader('X-Key', key);
        
        xhr.addEventListener('load', function() {
            
            handle(JSON.parse(xhr.responseText), cb);
            
        }, false);
        
        xhr.send(JSON.stringify(doc));
    }
    
    function post(url, doc, key, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('POST', url, true);
        
        xhr.setRequestHeader('Content-Type', 'application/json');
        
        xhr.setRequestHeader('X-Key', key);
        
        xhr.addEventListener('load', function() {
            
            handle(JSON.parse(xhr.responseText), cb);
            
        }, false);
        
        xhr.send(JSON.stringify(doc));
    }
    
    exports.types = function(key, cb) {
        
        get('/api/types', key, cb);
    };

    exports.doc = function(id, key, cb) {
        
        get('/api/doc/' + id, key, cb);
    };
    
    exports.docType = function(id, key, cb) {
        
        get('/api/doc/' + id + '/type', key, cb);
    };
    
    exports.colType = function(id, key, cb) {
        
        get('/api/col/' + id + '/type', key, cb);
    };
    
    exports.collection = function(name, key, cb) {
    
        get('/api/col/' + name, key, cb);
    };
    
    exports.update = function(doc, key, cb) {
        
        put('/api/doc/' + doc.$id, doc, key, cb);
    };
    
    exports.remove = function(id, key, cb) {
        
        del('/api/doc/' + id, key, cb);
    };
    
    exports.create = function(name, doc, key, cb) {
        
        post('/api/col/' + name + '/doc', doc, key, cb);
    };
    
    exports.login = function(username, password, cb) {
        
        post('/api/login', { username: username, password: password }, null, cb);
    };
    
    return exports;
    
})({});
