var auth = (function(exports) {
    
    var key;
    
    // Use value from sessionStorage. Might be unset
    // in sessionStorage.
    
    key = sessionStorage.getItem('admin-key');
    
    // Creates authentication form.
    // Executes cb on success.
    
    exports.create = function(cb) {
        
        function submit(e) {
            
            e.preventDefault();
            
            api.login(username.value, password.value, function(err, data) {
                
                if (err) {
                
                    if (err.code === 102) {
                        
                        messages.textContent = 'Wrong username or password';
                    }
                    
                } else {
                    
                    key = data;
                    
                    // Keep also in sessionStorage.
                    
                    sessionStorage.setItem('admin-key', key);
                    
                    if (typeof cb === 'function') {
                        
                        cb();
                    }
                }
            });
        }
        
        function clean() {
            
            messages.textContent = '';
        }
        
        var messages = document.createElement('div');
        
        var username = document.createElement('input');
    
        username.type = 'text';
        username.placeholder = 'username';
        
        username.addEventListener('focus', clean, false);
        
        var password = document.createElement('input');
        
        password.type = 'password';
        password.placeholder = 'password';
        
        password.addEventListener('focus', clean, false);
        
        var login = document.createElement('button');
        
        login.textContent = 'Login';
        
        var form = document.createElement('form');
        
        form.addEventListener('submit', submit, false);
        
        form.appendChild(messages);
        form.appendChild(username);
        form.appendChild(password);
        form.appendChild(login);
        
        var title = document.createElement('h2');
        
        title.textContent = 'Log in';
        
        var content = document.createElement('div');
        
        content.appendChild(title);    
        content.appendChild(form);
        
        return content;
    };
    
    exports.hasKey = function() {
        
        return !!key;
    };
    
    exports.key = function() {
        
        return key;
    };
    
    exports.unsetKey = function() {
        
        key = null;
        sessionStorage.removeItem('admin-key');
    };
    
    return exports;
    
})({});
