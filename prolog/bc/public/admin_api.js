var api = (function(exports) {

    var reqs = 0;

    function begin() {

        if (reqs === 0 &&
            typeof exports.onbegin === 'function') {

            exports.onbegin();
        }

        reqs += 1;
    }

    function end() {

        reqs -= 1;

        if (reqs === 0 &&
            typeof exports.onend === 'function') {

            exports.onend();
        }
    }
    
    function handle(res, cb) {

        end();
        
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

        begin();
        
        xhr.send();        
    }
    
    function del(url, key, cb) {
        
        var xhr = new XMLHttpRequest();
        
        xhr.open('DELETE', url, true);
        
        xhr.setRequestHeader('X-Key', key);
        
        xhr.addEventListener('load', function() {
            
            handle(JSON.parse(xhr.responseText), cb);
            
        }, false);

        begin();
        
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

        begin();
        
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

        begin();
        
        xhr.send(JSON.stringify(doc));
    }
    
    exports.types = function(key, cb) {
        
        get('/api/types', key, cb);
    };

    exports.doc = function(id, key, cb) {
        
        get('/api/document/' + id, key, cb);
    };
    
    exports.docType = function(id, key, cb) {
        
        get('/api/document/' + id + '/type', key, cb);
    };
    
    exports.colType = function(id, key, cb) {
        
        get('/api/collection/' + id + '/type', key, cb);
    };
    
    exports.collection = function(name, key, cb) {
    
        get('/api/collection/' + name, key, cb);
    };
    
    exports.update = function(doc, key, cb) {
        
        put('/api/document/' + doc.$id, doc, key, cb);
    };
    
    exports.remove = function(id, key, cb) {
        
        del('/api/document/' + id, key, cb);
    };
    
    exports.create = function(name, doc, key, cb) {
        
        post('/api/collection/' + name + '/document', doc, key, cb);
    };
    
    exports.login = function(username, password, cb) {
        
        post('/api/login', { username: username, password: password }, null, cb);
    };
    
    return exports;
    
})({});
