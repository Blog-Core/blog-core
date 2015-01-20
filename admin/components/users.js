var fs = require('fs');
var api = require('../api');
var message = require('../message');
var users_item = require('../vm/users_item');

// Component for displaying the users list.

function page() {

    var model = {

        users: ko.observableArray([]),

        permission: ko.observable(false),

        loaded: ko.observable(false)
    };

    api.userInfo().then(function(info) {

        if (info.type !== 'admin') {

            model.loaded(true);

        } else {

            return api.users().then(function(users) {

                model.users(users.map(function(data) {

                    var user = users_item.create(data);

                    user.editable = info.type === 'admin' || user.$id === info.$id;

                    return user;

                }));

                model.permission(true);

                model.loaded(true);
            });
        }

    }).catch(message.error);

    return model;
}

ko.components.register('users', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/users.html', { encoding: 'utf8' })
});
