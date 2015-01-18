var fs = require('fs');
var api = require('../api');
var message = require('../message');
var users_item = require('../vm/users_item');

// Component for displaying the users list.

function page() {

    var model = {

        users: ko.observableArray([]),

        type: ko.observable()
    };

    // Loads users and current user info.

    var tasks = [ api.userInfo(), api.users() ];

    Promise.all(tasks).then(function(data) {

        var info = data[0], users = data[1];

        model.type(info.type);

        model.users(users.map(function(data) {

            var user = users_item.create(data);

            user.editable = info.type === 'admin' || user.$id === info.$id;

            return user;

        }));

    }).catch(message.error);

    return model;
}

ko.components.register('users', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/users.html', { encoding: 'utf8' })
});
