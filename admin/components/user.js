var fs = require('fs');
var api = require('../api');
var focus = require('../focus');
var user = require('../vm/user');
var message = require('../message');

// Component for user edit page.

function page(params) {

    var model = {

        user: ko.observable()
    };

    if (params.id) {

        // Edit existing user.

        var tasks = [ api.userInfo(), api.user(params.id) ];

        Promise.all(tasks).then(function(data) {

            var info = data[0], userData = data[1];

            model.user(user.create(info, userData));

            focus('#user-fullname');

        }).catch(message.error);

    } else {

        // Create a new user.

        api.userInfo().then(function(info) {

            model.user(user.create(info));

            focus('#user-fullname');

        }).catch(message.error);
    }

    return model;
}

ko.components.register('user', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/user.html', { encoding: 'utf8' })
});
