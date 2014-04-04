var ko = require('./lib/knockout');
var auth = require('./controller/auth');
var post = require('./controller/post');
var route = require('./lib/router');

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

route(/^login/, function() {

    auth.form().done();
});
