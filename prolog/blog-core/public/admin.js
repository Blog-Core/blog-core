function setContent(dom) {
    
    var content = document.getElementById('content');

    content.innerHTML = '';
    content.appendChild(dom);
}

function showAuth() {        
    
    var username = document.createElement('input');
    
    username.type = 'text';
    username.placeholder = 'username';
    
    var password = document.createElement('input');
    
    password.type = 'password';
    password.placeholder = 'password';
    
    var login = document.createElement('button');
    
    login.textContent = 'Login';
    
    var form = document.createElement('form');
    
    form.addEventListener('submit', function(e) {
        
        e.preventDefault();
        
        api.login(username.value, password.value, function(err) {
            
            console.log(err);
            
            // FIXME error handling.
            
            window.location.hash = '';
        });
    });
    
    form.appendChild(username);
    form.appendChild(password);
    form.appendChild(login);
    
    var title = document.createElement('h2');
    
    title.textContent = 'Log in';
    
    var content = document.createElement('div');
    
    content.appendChild(title);
    content.appendChild(form);
    
    setContent(content);
}

function showTypes() {
    
    api.types(function(err, list) {
        
        var types = document.getElementById('types');
        
        list.forEach(function(type) {
                
            var a = document.createElement('a');
            
            a.textContent = util.nameToLabel(type.name);
            a.href = '#!/list/' + type.name;
            
            var li = document.createElement('li');
            
            li.appendChild(a);
            
            types.appendChild(li);
        });
        
        var p = document.createElement('p');
        
        p.textContent = 'Pick a collection.';
        
        setContent(p);
    });        
}

function showCollection(name) {
    
    api.colType(name, function(err, type) {
        
        api.collection(name, function(err, docs) {
            
            // Sort documents when the sort order is specified.
            
            if (typeof type.order === 'object') {
                
                var desc = type.order.direction === 'desc';
                
                util.sortByProperty(docs, type.order.property, desc);
            }
            
            setContent(list.create(docs, type));
        });
    });
}

// Displays the given document.

function showDoc(id) {
    
    api.doc(id, function(err, doc) {
        
        api.docType(id, function(err, type) {
            
            setContent(detail.create(doc, type));
        });
    });
}

// Displays the edit form for the document.

function editDoc(id) {
    
    api.doc(id, function(err, doc) {
        
        api.docType(id, function(err, type) {
            
            setContent(edit.create(doc, type));
        });
    });  
}

// Displays the edit form for a new document.

function newDoc(name) {

    api.colType(name, function(err, type) {
        
        setContent(create.create(type));
    });
}

var routes = [
    {
        exp: /#!\/auth/,
        fun: showAuth
    },
    {
        exp: /#!\/list\/(\w+)/,
        fun: showCollection
    },
    {
        exp: /#!\/new\/(\w+)/,
        fun: newDoc
    },
    {
        exp: /^$/,
        fun: showTypes
    },
    {
        exp: /#!\/edit\/([a-zA-Z0-9\-]+)/,
        fun: editDoc
    },
    {
        exp: /#!\/show\/([a-zA-Z0-9\-]+)/,
        fun: showDoc
    }
];

// Current hash value to action.

function dispatch(hash) {
    
    console.log('Dispatch to ' + hash);
    
    for (var i = 0; i < routes.length; i++) {
        
        var route = routes[i];
        
        var match = hash.match(route.exp);
        
        if (match) {
            
            route.fun.apply(null, match.slice(1));
            
            break;
        }
    }
}

// Needed for clicking the same link multiple times
// as no changing hash triggers hashchange event.

document.addEventListener('click', function(e) {
    
    if (e.target.nodeName.toLowerCase() === 'a') {
                
        var hash = e.target.getAttribute('href');
        
        if (hash === window.location.hash) {
            
            dispatch(hash);
        }
    }
    
}, false);

// Hashchange event for dispatching.

window.addEventListener('hashchange', function() {
    
    dispatch(window.location.hash);
    
}, false);

// Dispatch at start. Redirect to #!/types

window.addEventListener('load', function() {
        
    dispatch(window.location.hash);
    
}, false);
