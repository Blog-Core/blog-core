var fs = require('fs');
var api = require('../api');
var message = require('../message');
var validate = require('../validate');

// Creates view model for the login page.

function page() {

    var model = {

        username: ko.observable('').trimmed(),

        password: ko.observable('').trimmed()
    };

    model.login = function(form) {

        validate.clear(form);

        var username = model.username();

        var password = model.password();

        if (username === '') {

            validate.error('username', 'Username is not entered.');

        } else if (!username.match(/^[^@]+@[^@]+$/)) {

            validate.error('username', 'Username must be an email.');
        }

        if (password === '') {

            validate.error('password', 'Password is not entered.');
        }

        if (validate.hasError(form)) {

            return false;
        }

        api.login(username, password).then(function(res) {

            if (res.status === 'success') {

                sessionStorage.setItem('api-key', res.data.key);

                route.go('landing');

            } else {

                validate.formError(form, res.message);
            }

        }).catch(message.error);
    };

    return model;
}

ko.components.register('login', {

    viewModel: { createViewModel: page },

    template: fs.readFileSync(__dirname + '/login.html', { encoding: 'utf8' })
});
