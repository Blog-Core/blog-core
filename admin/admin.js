var ko = require('./lib/knockout');
var auth = require('./controller/auth');
var post = require('./controller/post');
var file = require('./controller/file');
var user = require('./controller/user');
var comment = require('./controller/comment');
var route = require('./lib/router');
var hex = require('./hex');
var api = require('./api');
var menu = require('./menu');

route(/^posts/, function() {

    menu.active('posts');
    post.list('post').done();
});

route(/^pages/, function() {

    menu.active('pages');
    post.list('page').done();
});

route(/^blocks/, function() {

    menu.active('blocks');
    post.list('block').done();
});

route(/^post\/([^\/]+)/, function(id) {

    menu.active('posts');
    post.edit(id).done();
});

route(/^new\/([^\/]+)/, function(type) {

    menu.active(type + 's');
    post.create(type).done();
});

route(/^files/, function() {

    menu.active('files');
    route.go('directory/' + hex.hex('/'));
});

route(/^directory\/([^\/]+)/, function(directory) {

    menu.active('files');
    file.directory(hex.string(directory)).done();
});

route(/^file\/([^\/]+)/, function(filename) {

    menu.active('files');
    file.file(hex.string(filename)).done();
});

route(/^comments\/([^\/]+)/, function(id) {

    menu.active('posts');
    comment.list(id).done();
});

route(/^users/, function() {

    menu.active('users');
    user.list().done();
});

route(/^user\/new/, function() {

    menu.active('users');
    user.create().done();
});

route(/^user\/([^\/]+)/, function(id) {

    menu.active('users');
    user.edit(id).done();
});

route(/^login/, function() {

    menu.active();
    auth.form().done();
});

route(/.*/, function() {

    route.go(api.hasKey() ? 'posts' : 'login');
});
