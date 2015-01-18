var hex = require('./hex');
var api = require('./api');

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

        active: ko.observable()
    }
};

// Shows given component with params.

model.show = function(name, params, menu) {

    model.menu.active(menu);
    model.params(params);
    model.component(name);
};

ko.applyBindings(model);

route(/^entries\/([^\/]+)/, function(type) {

    model.show('posts', { type: type }, type + 's');
});

route(/^entry\/([^\/]+)\/([^\/]+)/, function(type, id) {

    model.show('post', { id: id }, type + 's');
});

route(/^new\/([^\/]+)/, function(type) {

    model.show('post', { type: type }, type + 's');
});

route(/^files/, function() {

    route.go('directory/' + hex.hex('/'));
});

route(/^directory\/([^\/]+)/, function(directory) {

    model.show('directory', { directory: directory }, 'files');
});

route(/^file\/([^\/]+)/, function(file) {

    model.show('file', { file: file }, 'files');
});

route(/^comments\/([^\/]+)\/([^\/]+)/, function(type, id) {

    model.show('comments', { id: id }, type + 's');
});

route(/^users/, function() {

    model.show('users', {}, 'users');
});

route(/^user\/new/, function() {

    model.show('user', {}, 'users');
});

route(/^user\/([^\/]+)/, function(id) {

    model.show('user', { id: id }, 'users');
});

route(/^login/, function() {

    model.show('login', {});
});

route(/^logout/, function() {

    sessionStorage.removeItem('api-key');

    route.go('login');
});

route(/.*/, function() {

    // FIXME go to first entries.

    route.go(api.hasKey() ? 'entries/post' : 'login');
});
