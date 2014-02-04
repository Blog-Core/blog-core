// Sets the main contents of the page.

function setContent(dom) {
    
    var content = document.getElementById('content');

    content.innerHTML = '';
    content.appendChild(dom);
}

// Sets popup for the page.

function setPopup(dom) {
    
    var content = document.createElement('div');
    
    content.className = 'modal-content';
    
    content.appendChild(dom);
    
    var modal = document.createElement('div');
    
    modal.className = 'modal';
    modal.appendChild(content);
    
    document.body.appendChild(modal);
}

// Removes the currently shown popup.
// Does nothing when there is no popup.

function removePopup() {
    
    var popup = document.querySelector('body > .modal');
    
    popup.parentNode.removeChild(popup);
}

// Shows the loading spinner.

function showSpinner() {

    var spinner = document.getElementById('spinner');

    spinner.style.display = 'block';
}

// Hides the loading spinner.

function hideSpinner() {

    setTimeout(function() {

        var spinner = document.getElementById('spinner');

        spinner.style.display = 'none';

    }, 100);
}

api.onbegin = showSpinner;
api.onend = hideSpinner;

// Shows page with collections.

function showCollections() {
    
    api.types(auth.key(), function(err, list) {
        
        setContent(collections.create(list));
    });
}

// Displays the table with entries
// from the given collection.

function showCollection(name) {
    
    api.colType(name, auth.key(), function(err, type) {
        
        api.collection(name, auth.key(), function(err, docs) {
            
            // Sort documents when the sort order is specified.
            
            if (typeof type.order === 'object') {

                console.log(type.order);
                
                var desc = type.order.direction === 'desc';
                
                util.sortByProperty(docs, type.order.property, desc);
            }
            
            setContent(list.create(docs, type));
        });
    });
}

// Displays the given document.

function showDoc(id) {
    
    api.doc(id, auth.key(), function(err, doc) {
        
        api.docType(id, auth.key(), function(err, type) {
            
            setContent(detail.create(doc, type));
        });
    });
}

// Displays the edit form for the document.

function editDoc(id) {
    
    api.doc(id, auth.key(), function(err, doc) {
        
        api.docType(id, auth.key(), function(err, type) {
            
            setContent(edit.create(doc, type));
        });
    });  
}

// Displays the edit form for a new document.

function newDoc(name) {

    api.colType(name, auth.key(), function(err, type) {
        
        setContent(create.create(type));
    });
}

var routes = [
    { exp: /#!\/list\/(\w+)/, fun: showCollection },
    { exp: /#!\/new\/(\w+)/, fun: newDoc },
    { exp: /#!\/collections/, fun: showCollections },
    { exp: /#!\/edit\/([a-zA-Z0-9\-]+)/, fun: editDoc },
    { exp: /#!\/show\/([a-zA-Z0-9\-]+)/, fun: showDoc }
];

// Current hash value to action.

function dispatch(hash) {
    
    console.log('Dispatch to ' + hash);
    
    // Do authentication when needed.
    
    if (!auth.hasKey()) {
        
        setPopup(auth.create(function() {
            
            // Re-dispatch to same location
            // after auth succeeds.
            
            removePopup();
            
            dispatch(hash);
        }));
        
    } else {
    
        var found = false;
        
        for (var i = 0; i < routes.length; i++) {
            
            var route = routes[i];
            
            var match = hash.match(route.exp);
            
            if (match) {
                
                found = true;
                
                route.fun.apply(null, match.slice(1));
                
                break;
            }
        }
        
        // When no suitable route is found, go
        // to collections page.
        
        if (!found) {
            
            window.location.hash = '#!/collections';
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
