var api = require('./lib/api');
var message = require('./lib/message');
var login = require('./lib/pages/login');
var posts = require('./lib/pages/posts');
var post = require('./lib/pages/post');
var users = require('./lib/pages/users');
var user = require('./lib/pages/user');
var email = require('./lib/pages/email');
var comments = require('./lib/pages/comments');
var trash = require('./lib/pages/trash');
var config = require('./lib/pages/config');
var analytics = require('./lib/pages/analytics');

// Errors binding.
require('./lib/form_error');

// Global to format dates.
window.formatDate = function(ts) {
    return new Date(1000 * ts).toISOString().substring(0, 10);
};

// The page menu.
var menu = {
    active: ko.observable(),
    types: ko.observable(),
    load: function() {
        if (menu.types()) {
            // Menu updated.
            return Promise.resolve(menu.types());
        }

        return api.types().then(function(types) {
            menu.types(types);
            return types;
        });
    }
};

// Binds menu.
ko.applyBindings(menu,
    document.getElementById('menu'));

// Returns true when user is authenticated.
// Redirects user if he/she is not
// authenticated.
function authenticated() {
    if (api.hasKey()) {
        // If user is authenticated then
        // check if custom menu entries need
        // reloading.
        menu.load().catch(message.error);
        return true;
    } else {
        route.go('login');
    }
    return false;
}

var recovered;

// Offers recovery option and returns
// true when user accepts it.
function recovery() {
    // Offer recover option to the user.
    var autosave = localStorage.getItem('autosave');
    if (autosave) {
        var data = JSON.parse(autosave);
        // Remove entry from localStorage.
        localStorage.removeItem('autosave');
        // Ask confirmation.
        if (confirm('You have unsaved entry "' + data.title + '",' +
            ' would you like to recover it?')) {
            // Assign global data that is used for
            // actual recovery.
            recovered = data;
            // Redirect to transient route.
            route.go('recover');
            return true;
        }
    }
    return false;
}

// Transient route for recovering
// an entry.
route(/^recover$/, function() {
    if (authenticated() && !recovery()) {
        if (recovered) {
            if (recovered.$id) {
                // Recover existing post.
                route.go('entry/' + recovered.type +
                    '/' + recovered.$id);
            } else {
                // Recover new post.
                route.go('new/' + recovered.type);
            }
        } else {
            route.go('landing');
        }
    }
});

route(/^config$/, function() {
    if (authenticated() && !recovery()) {
        menu.active('config');
        config.create().catch(message.error);
    }
});

route(/^entries\/([^\/]+)/, function(type) {
    if (authenticated() && !recovery()) {
        menu.active(type);
        posts.create(type).catch(message.error);
    }
});

route(/^entry\/([^\/]+)\/([^\/]+)/, function(type, id) {
    if (authenticated() && !recovery()) {
        menu.active(type);
        // Include recovery data (can be undefined).
        post.create(type, id, recovered).catch(message.error);
        recovered = undefined;
    }
});

route(/^new\/([^\/]+)/, function(type) {
    if (authenticated() && !recovery()) {
        menu.active(type);
        // Include recovery data (can be undefined).
        post.create(type, null, recovered).catch(message.error);
        recovered = undefined;
    }
});

route(/^comments\/([^\/]+)\/([^\/]+)/, function(type, id) {
    if (authenticated() && !recovery()) {
        menu.active(type);
        comments.create(type, id).catch(message.error);
    }
});

route(/^trash/, function() {
    if (authenticated() && !recovery()) {
        menu.active('trash');
        trash.create().catch(message.error);
    }
});

route(/^users/, function() {
    if (authenticated() && !recovery()) {
        menu.active('users');
        users.create().catch(message.error);
    }
});

route(/^user\/new/, function() {
    if (authenticated() && !recovery()) {
        menu.active('users');
        user.create().catch(message.error);
    }
});

route(/^user\/([^\/]+)/, function(id) {
    if (authenticated() && !recovery()) {
        menu.active('users');
        user.create(id).catch(message.error);
    }
});

route(/^analytics/, function() {
    if (authenticated() && !recovery()) {
        menu.active('analytics');
        analytics.create().catch(message.error);
    }
});

route(/^login/, function() {
    menu.active(null);
    login.create().catch(message.error);
});

route(/^email/, function() {
    if (authenticated() && !recovery()) {
        menu.active('email');
        email.create().catch(message.error);
    }
});

route(/^logout/, function() {
    sessionStorage.removeItem('api-key');
    localStorage.removeItem('api-key');
    window.location = '/admin';
});

// Temporary route that decides
// where to go.
route(/^landing/, function() {
    menu.active(null);
    menu.load().then(function(types) {
        if (types.length > 0) {
            // Select first type.
            route.go('entries/' + types[0].name);
        }
    }).catch(message.error);
});

route(/.*/, function() {
    route.go(api.hasKey() ? 'landing' : 'login');
});

document.body.addEventListener('click', function(e) {
    if (e.target.href && e.target.href.match(/#logout$/)) {
        if (confirm('Do you want to log out?')) {
            return true;
        } else {
            e.stopPropagation();
            e.preventDefault();
            return false;
        }
    }
}, false);
