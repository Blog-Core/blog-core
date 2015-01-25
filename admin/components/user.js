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

        var tasks = [ api.roles(), api.user(params.id) ];

        Promise.all(tasks).then(function(data) {

            var roles = data[0], userData = data[1];

            model.user(user.create(roles, userData));

        }).catch(message.error);

    } else {

        // Create a new user.

        var tasks = [ api.roles() ];

        Promise.all(tasks).then(function(data) {

            var roles = data[0];

            model.user(user.create(roles));

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
