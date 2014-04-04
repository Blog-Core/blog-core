var view = require('../view');
var api = require('../api');
var ko = require('../lib/knockout');
var postVm = require('../vm/post_vm');

exports.list = function(type) {

    var type = type || 'post';

    return api.posts(type).then(function(posts) {

        posts.sort(function(post1, post2) {

            return post2.date_updated - post1.date_updated;
        });

        var start = 0;

        var model = {

            posts: ko.observableArray([]),

            more: function() {

                start += 5;

                var end = Math.min(start + 5, posts.length);

                for (var i = start; i < end; i++) {

                    model.posts.push(posts[i]);
                }
            }
        };

        model.posts(posts.slice(0, 5));

        view.show('posts', model);
    });
};

exports.edit = function(id) {

    return api.post(id).then(function(post) {

        return view.show('post', postVm.create(post)).then(function() {

            // Autoset initial textarea height.

            var editor = document.getElementById('post-content');

            editor.style.height = (editor.scrollHeight + 10) + 'px';
        });
    });
};

exports.create = function() {

    return view.show('post', postVm.create());
};
