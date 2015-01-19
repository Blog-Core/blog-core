var fs = require('fs');
var api = require('../api');
var user = require('../vm/user');
var message = require('../message');

// Component for user edit page.

function page(params) {

    var model = {

        user: ko.observable()
    };

    if (params.id) {

        // Edit existing user.

        var tasks = [ api.userInfo(), api.roles(), api.user(params.id) ];

        Promise.all(tasks).then(function(data) {

            var info = data[0], roles = data[1], userData = data[2];

            model.user(user.create(info, roles, userData));

        }).catch(message.error);

    } else {

        // Create a new user.

        var tasks = [ api.userInfo(), api.roles() ];

        Promise.all(tasks).then(function(data) {

            var info = data[0], roles = data[1];

            model.user(user.create(info, roles));

            var fullname = document.querySelector('#user-fullname');

            fullname.focus();

            if (typeof fullname.setSelectionRange === 'function') {

                fullname.setSelectionRange(0, fullname.value.length);
            }

        }).catch(message.error);
    }

    return model;
}

ko.components.register('user', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/user.html', { encoding: 'utf8' })
});
