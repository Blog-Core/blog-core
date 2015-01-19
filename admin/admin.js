var hex = require('./hex');
var api = require('./api');
var message = require('./message');

// Menu component.

require('./components/menu');

// Login component.

require('./components/login');

// Posts list component.

require('./components/posts');

// Posts edit component.

require('./components/post');

// Users list component.

require('./components/users');

// User edit component.

require('./components/user');

// Comments list component.

require('./components/comments');

// Directory listing component.

require('./components/directory');

// File view component.

require('./components/file');

// Global to format dates.

window.formatDate = function(ts) {

    return new Date(1000 * ts).toISOString().substring(0, 10);
};

// Helper to trim whitespace from values.

ko.subscribable.fn.trimmed = function() {

    return ko.computed({

        read: function() {

            return this().trim();
        },

        write: function(value) {

            this(value.trim());
            this.valueHasMutated();
        },

        owner: this
    });
};

// The main model.

var model = {

    component: ko.observable(),

    // FIXME changing this before component
    // might have side effects.

    params: ko.observable({}),

    menu: {

        active: ko.observable(),

        types: ko.observable()
    }
};

// Shows given component with params.

model.show = function(name, params, menu) {

    model.menu.active(menu);
    model.params(params);
    model.component(name);
};

ko.applyBindings(model);

// Returns a Promise.

function loadMenu() {

    if (model.menu.types()) {

        // Menu updated.

        return Promise.resolve(model.menu.types());
    }

    return api.types().then(function(types) {

        model.menu.types(types);

        return types;
    });
}

// Redirects user if he/she is not
// authenticated.

function authenticated() {

    if (api.hasKey()) {

        // If user if authenticated then
        // check if custom menu entries need
        // reloading.

        loadMenu();

    } else {

        route.go('login');
    }
}

route(/^entries\/([^\/]+)/, function(type) {

    authenticated();

    model.show('posts', { type: type }, type);
});

route(/^entry\/([^\/]+)\/([^\/]+)/, function(type, id) {

    authenticated();

    model.show('post', { id: id }, type);
});

route(/^new\/([^\/]+)/, function(type) {

    authenticated();

    model.show('post', { type: type }, type);
});

route(/^files/, function() {

    authenticated();

    route.go('directory/' + hex.hex('/'));
});

route(/^directory\/([^\/]+)/, function(directory) {

    authenticated();

    model.show('directory', { directory: directory }, 'files');
});

route(/^file\/([^\/]+)/, function(file) {

    authenticated();

    model.show('file', { file: file }, 'files');
});

route(/^comments\/([^\/]+)\/([^\/]+)/, function(type, id) {

    authenticated();

    model.show('comments', { id: id }, type);
});

route(/^users/, function() {

    authenticated();

    model.show('users', {}, 'users');
});

route(/^user\/new/, function() {

    authenticated();

    model.show('user', {}, 'users');
});

route(/^user\/([^\/]+)/, function(id) {

    authenticated();

    model.show('user', { id: id }, 'users');
});

route(/^login/, function() {

    authenticated();

    model.show('login', {});
});

route(/^logout/, function() {

    sessionStorage.removeItem('api-key');

    window.location = '/admin';
});

// Temporary route that decides
// where to go.

route(/^landing/, function() {

    model.show(null, {}, 'landing');

    loadMenu().then(function(types) {

        if (types.length > 0) {

            // Select first type.

            route.go('entries/' + types[0].name);
        }
    });
});

route(/.*/, function() {

    route.go(api.hasKey() ? 'landing' : 'login');
});
