var api = require('../api');
var view = require('../view');
var message = require('../message');
var template = require('./login.html');

// The login page.
exports.create = function() {

    var model = {
        username: ko.observable(''),
        password: ko.observable(''),
        remember: ko.observable(false),
        errors: {
            username: ko.observableArray([]),
            password: ko.observableArray([])
        }
    };

    model.login = function(form) {
        // Clear errors.
        Object.keys(model.errors).forEach(function(key) {
            model.errors[key]([]);
        });
        var username = model.username();
        var password = model.password();
        if (username === '') {
            model.errors.username.push('Username is not entered.');
        } else if (!username.match(/^[^@]+@[^@]+$/)) {
            model.errors.username.push('Username must be an email.');
        }
        if (password === '') {
            model.errors.password.push('Password is not entered.');
        }
        var input = form.querySelector(
            '.has-error input, .has-error textarea, .has-error checkbox');
        if (input) {
            input.focus();
            return false;
        }
        api.login(username, password).then(function(res) {
            if (model.remember()) {
                localStorage.setItem('api-key', res.key);
                sessionStorage.removeItem('api-key');
            } else {
                sessionStorage.setItem('api-key', res.key);
                localStorage.removeItem('api-key');
            }
            route.go('landing');
        }).catch(function(err) {
            if (err.toString().match(/Invalid auth credentials/)) {
                model.errors.username.push('Invalid auth credentials.');
                model.errors.password.push('Invalid auth credentials.');
            } else {
                message.error(err);
            }
        });
    };

    view.show(template, model);
    return Promise.resolve();
};
