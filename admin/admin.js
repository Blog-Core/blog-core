var api = require('./lib/api');
var message = require('./lib/message');
var login = require('./lib/pages/login');
var posts = require('./lib/pages/posts');
var post = require('./lib/pages/post');
var users = require('./lib/pages/users');
var user = require('./lib/pages/user');
var comments = require('./lib/pages/comments');

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

// Redirects user if he/she is not
// authenticated.

function authenticated() {

    if (api.hasKey()) {

        // If user if authenticated then
        // check if custom menu entries need
        // reloading.

        menu.load();

    } else {

        route.go('login');
    }
}

route(/^entries\/([^\/]+)/, function(type) {

    authenticated();

    menu.active(type);

    posts.create(type).catch(message.error);
});

route(/^entry\/([^\/]+)\/([^\/]+)/, function(type, id) {

    authenticated();

    post.create(type, id).catch(message.error);
});

route(/^new\/([^\/]+)/, function(type) {

    authenticated();

    menu.active(type);

    post.create(type).catch(message.error);
});

route(/^comments\/([^\/]+)\/([^\/]+)/, function(type, id) {

    authenticated();

    menu.active(type);

    comments.create(type, id).catch(message.error);
});

route(/^users/, function() {

    authenticated();

    menu.active('users');

    users.create().catch(message.error);
});

route(/^user\/new/, function() {

    authenticated();

    menu.active('users');

    user.create().catch(message.error);
});

route(/^user\/([^\/]+)/, function(id) {

    authenticated();

    menu.active('users');

    user.create(id).catch(message.error);
});

route(/^login/, function() {

    authenticated();

    menu.active(null);

    login.create().catch(message.error);
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
    });
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
