var fs = require('fs');
var api = require('../api');
var post = require('../vm/post');
var message = require('../message');
var resolveObject = require('../resolve_object');

// Component for the post edit page.

function page(params) {

    var model = {

        post: ko.observable()
    };

    var tasks;

    if (params.id) {

        // Edit existing post.

        api.userInfo().then(function(userInfo) {

            var requests = {

                post: api.post(params.id),

                types: api.types()

            };

            if (userInfo.type === 'admin') {

                requests.users = api.users();

            } else {

                // User that is not an admin
                // cannot create posts owned by others.

                requests.users = [{

                    $id: userInfo.$id,

                    fullname: userInfo.fullname

                }];
            }

            return resolveObject(requests).then(function(data) {

                model.post(post.create(userInfo, params.type, data.types, data.users, data.post));

                // Autoset initial textarea height.

                var editor = document.getElementById('post-content');

                editor.style.height = (editor.scrollHeight + 10) + 'px';

            });

        }).catch(message.error);

    } else {

        // Create a new post.

        api.userInfo().then(function(userInfo) {

            var requests = {

                types: api.types()

            };

            if (userInfo.type === 'admin') {

                requests.users = api.users();

            } else {

                // User that is not an admin
                // cannot create posts owned by others.

                requests.users = [{

                    $id: userInfo.$id,

                    fullname: userInfo.fullname

                }];
            }

            return resolveObject(requests).then(function(data) {

                model.post(post.create(userInfo, params.type, data.types, data.users));

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
