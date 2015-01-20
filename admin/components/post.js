var fs = require('fs');
var api = require('../api');
var post = require('../vm/post');
var message = require('../message');

// Component for the post edit page.

function page(params) {

    var model = {

        post: ko.observable()
    };

    var tasks;

    if (params.id) {

        // Edit existing post.

        api.userInfo().then(function(info) {

            if (info.type !== 'admin') {

                // User that is not an admin
                // cannot create posts owned by others.

                var users = [{

                    $id: info.$id,

                    fullname: info.fullname
                }];

                tasks = [ Promise.resolve(users), api.post(params.id), api.types() ];

            } else {

                tasks = [ api.users(), api.post(params.id), api.types() ];
            }

            return Promise.all(tasks).then(function(data) {

                var users = data[0], postData = data[1], types = data[2];

                model.post(post.create(info, types, users, postData));

                // Autoset initial textarea height.

                var editor = document.getElementById('post-content');

                editor.style.height = (editor.scrollHeight + 10) + 'px';

            });

        }).catch(message.error);

    } else {

        // Create a new post.

        api.userInfo().then(function(info) {

            if (info.type !== 'admin') {

                // User that is not an admin
                // cannot create posts owned by others.

                var users = [{

                    $id: info.$id,

                    fullname: info.fullname
                }];

                tasks = [ Promise.resolve(users), api.types() ];

            } else {

                tasks = [ api.users(), api.types() ];
            }

            return Promise.all(tasks).then(function(data) {

                var users = data[0], types = data[1];

                model.post(post.create(info, types, users));

                model.post().type(params.type);

                var title = document.querySelector('#post-title');

                title.focus();

                if (typeof title.setSelectionRange === 'function') {

                    title.setSelectionRange(0, title.value.length);
                }
            });

        }).catch(message.error);
    }

    return model;
}

ko.components.register('post', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/post.html', { encoding: 'utf8' })
});
