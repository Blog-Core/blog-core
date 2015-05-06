var api = require('../api');
var message = require('../message');

exports.create = function(roles, data) {

    var user = {

        username: ko.observable(''),
        fullname: ko.observable(''),
        type: ko.observable('author'),
        link: ko.observable(''),
        password: ko.observable(''),
        password_edit: ko.observable(true),
        password_text: ko.observable(false),
        error: ko.observable(''),
        creating: true,
        roles: roles,

        errors: {

            username: ko.observableArray([]),
            fullname: ko.observableArray([]),
            password: ko.observableArray([]),
            link: ko.observableArray([]),
            type: ko.observableArray([])
        },

        save: function(form) {

            // Clear errors.

            Object.keys(user.errors).forEach(function(key) {

                user.errors[key]([]);
            });

            if (user.password_edit()) {

                var password = user.password();

                if (password === '') {

                    user.errors.password.push('Password is not set.');

                } else if (password.length < 6) {

                    user.errors.password.push('Password length must be at least 6.');
                }
            }

            var username = user.username();

            if (username === '') {

                user.errors.username.push('Username is not set.');

            } else {

                if (!username.match(/^[^@]+@[^@]+$/)) {

                    user.errors.username.push('Username must be an email address.');
                }
            }

            var fullname = user.fullname();

            if (fullname === '') {

                user.errors.fullname.push('Full name is not set.');
            }

            var link = user.link();

            if (link !== '') {

                if (!link.match(/https?:\/\//)) {

                    user.errors.link.push('Link must start with http:// or https:// prefix.');
                }
            }

            var input = form.querySelector(
                '.has-error input, .has-error textarea,' +
                ' .has-error checkbox, .has-error select');

            if (input) {

                input.focus();

                return false;
            }

            if (user.$id) {

                api.updateUser(user.$id, user.toJS()).then(function() {

                    message.info('User "' + user.username() + '" has been updated.');

                    route.go('users');

                }).catch(handleSaveError.bind(null, user));

            } else {

                api.saveUser(user.toJS()).then(function() {

                    message.info('User "' + user.username() + '" has been added.');

                    route.go('users');

                }).catch(handleSaveError.bind(null, user));
            }

            return false;
        },

        toJS: function() {

            var js = {

                username: user.username(),
                fullname: user.fullname(),
                type: user.type(),
                link: user.link()
            };

            if (user.password_edit()) {

                js.password = user.password();
            }

            return js;
        }
    };

    user.password_text.subscribe(function(value) {

        if (value) {

            document.getElementById('user-password').type = 'text';

        } else {

            document.getElementById('user-password').type = 'password';
        }
    });

    if (data) {

        user.$id = data.$id;
        user.creating = false;
        user.username(data.username);
        user.fullname(data.fullname);
        user.type(data.type);
        user.link(data.link);
        user.password_edit(false);

    } else {

        user.fullname('Unnamed');
    }

    return user;
};

function handleSaveError(user, err) {

    var string = err.toString();

    if (string.match(/Cannot remove the last admin/)) {

        user.errors.type.push('Cannot remove the last admin.');

    } else if (string.match(/username exists/)) {

        user.errors.username.push('The username exists.');

    } else {

        message.error(err);
    }
}
