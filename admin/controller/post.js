var view = require('../view');
var api = require('../api');
var message = require('../message');
var ko = require('../lib/knockout');
var route = require('../lib/router');
var postVm = require('../vm/post_vm');

exports.list = function(type) {

    var mytype = sessionStorage.getItem('user-type');

    var myid = sessionStorage.getItem('user-id');

    return api.posts(type).then(function(posts) {

        posts.sort(function(post1, post2) {

            return post2.date_updated - post1.date_updated;
        });

        posts.forEach(function(post) {

            post.expanded = ko.observable(false);

            post.expand = function() {

                if (post.expanded()) {

                    post.expanded(false);

                } else {

                    post.expanded(true);
                }
            };

            post.editable = mytype === 'admin' || myid === post.author;

            post.remove = function() {

                if (confirm('Remove the post?')) {

                    api.removePost(post.$id).then(function(response) {

                        if (response.status === 'success') {

                            message.info('The post was removed.');

                            route.refresh();

                        } else {

                            message.error(response.message);
                        }

                    }, message.error).done();
                }
            };
        });

        var start = 0;

        var model = {

            type: type,

            posts: ko.observableArray([]),

            hasMore: ko.observable(start + 5 < posts.length),

            more: function() {

                start += 5;

                var end = Math.min(start + 5, posts.length);

                for (var i = start; i < end; i++) {

                    model.posts.push(posts[i]);
                }

                model.hasMore(start + 5 < posts.length);
            }
        };

        model.posts(posts.slice(0, 5));

        view.show('posts', model);
    });
};

exports.edit = function(id) {

    return api.users().then(function(users) {

        return api.post(id).then(function(post) {

            return view.show('post', postVm.create(users, post)).then(function() {

                // Autoset initial textarea height.

                var editor = document.getElementById('post-content');

                editor.style.height = (editor.scrollHeight + 10) + 'px';
            });
        });
    });
};

exports.create = function(type) {

    return api.users().then(function(users) {

        var model = postVm.create(users);

        model.type(type);

        return view.show('post', model);
    });
};
