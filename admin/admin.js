var ko = require('./lib/knockout');
var auth = require('./controller/auth');
var post = require('./controller/post');
var file = require('./controller/file');
var route = require('./lib/router');
var hex = require('./hex');

route(/^posts/, function() {

    post.list('post').done();
});

route(/^pages/, function() {

    post.list('page').done();
});

route(/^blocks/, function() {

    post.list('block').done();
});

route(/^post\/([^\/]+)/, function(id) {

    post.edit(id).done();
});

route(/^new/, function(id) {

    post.create().done();
});

route(/^files/, function() {

    route.go('directory/' + hex.hex('/'));
});

route(/^directory\/([^\/]+)/, function(directory) {

    file.directory(hex.string(directory)).done();
});

route(/^file\/([^\/]+)/, function(filename) {

    file.file(hex.string(filename)).done();
});

route(/^login/, function() {

    auth.form().done();
});
