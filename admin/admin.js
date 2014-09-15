var auth = require('./controller/auth');
var post = require('./controller/post');
var file = require('./controller/file');
var user = require('./controller/user');
var comment = require('./controller/comment');
var hex = require('./hex');
var api = require('./api');
var menu = require('./menu');
var message = require('./message');

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

route(/^posts/, function() {

    menu.active('posts');
    post.list('post').catch(message.error);
});

route(/^pages/, function() {

    menu.active('pages');
    post.list('page').catch(message.error);
});

route(/^blocks/, function() {

    menu.active('blocks');
    post.list('block').catch(message.error);
});

route(/^post\/([^\/]+)/, function(id) {

    menu.active('posts');
    post.edit(id).catch(message.error);
});

route(/^page\/([^\/]+)/, function(id) {

    menu.active('pages');
    post.edit(id).catch(message.error);
});

route(/^block\/([^\/]+)/, function(id) {

    menu.active('blocks');
    post.edit(id).catch(message.error);
});

route(/^new\/([^\/]+)/, function(type) {

    menu.active(type + 's');
    post.create(type).catch(message.error);
});

route(/^files/, function() {

    menu.active('files');
    route.go('directory/' + hex.hex('/'));
});

route(/^directory\/([^\/]+)/, function(directory) {

    menu.active('files');
    file.directory(hex.string(directory)).catch(message.error);
});

route(/^file\/([^\/]+)/, function(filename) {

    menu.active('files');
    file.file(hex.string(filename)).catch(message.error);
});

route(/^comments\/([^\/]+)/, function(id) {

    menu.active('posts');
    comment.list(id).catch(message.error);
});

route(/^users/, function() {

    menu.active('users');
    user.list().catch(message.error);
});

route(/^user\/new/, function() {

    menu.active('users');
    user.create().catch(message.error);
});

route(/^user\/([^\/]+)/, function(id) {

    menu.active('users');
    user.edit(id).catch(message.error);
});

route(/^login/, function() {

    menu.active();
    auth.form().catch(message.error);
});

route(/^logout/, function() {

    menu.active();
    auth.logout();
});

route(/.*/, function() {

    route.go(api.hasKey() ? 'posts' : 'login');
});
