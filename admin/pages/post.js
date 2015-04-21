var fs = require('fs');
var api = require('../api');
var post = require('../vm/post');
var view = require('../view');
var resolveObject = require('../resolve_object');

var template = fs.readFileSync(__dirname + '/post.html', { encoding: 'utf8' });

// The post edit page.

exports.create = function(type, id) {

    var model = {

        post: ko.observable()
    };

    return postData(id).then(function(data) {

        // data.post will be undefined when id is not set.

        model.post(post.create(data.userInfo, type, data.types, data.users, data.post));

        view.show(template, model);

        if (id) {

            // Existing post.
            // Autoset initial textarea height.

            var editor = document.getElementById('post-content');

            editor.style.height = (editor.scrollHeight + 10) + 'px';

        } else {

            // Set focus to title.

            var title = document.querySelector('#post-title');

            title.focus();

            if (typeof title.setSelectionRange === 'function') {

                title.setSelectionRange(0, title.value.length);
            }
        }
    });
};

// Retrieves data relevant to
// the post. When id is not set
// then retrieves data needed for the
// new post.

function postData(id) {

    return api.userInfo().then(function(userInfo) {

        var requests = {

            types: api.types(),

            users: authors(userInfo),

            userInfo: Promise.resolve(userInfo)
        };

        if (id) {

            post: api.post(id)
        }

        return resolveObject(requests);
    });
}

// Retrieves selectable authors.
// userInfo - the current user info data.

function authors(userInfo) {

    if (userInfo.type === 'admin') {

        return api.users();

    } else {

        // Non-admin can only select
        // itself as the author.

        return Promise.resolve([{

            $id: userInfo.$id,

            fullname: userInfo.fullname

        }]);
    }
}
