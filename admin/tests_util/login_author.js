var login = require('./login');
var add_user = require('./add_user');

// Helper to add login steps for non-admin user.

module.exports = function(casper, test) {

    login(casper, test);

    add_user(casper, test);

    // Log out and in as author.

    casper.thenClick('a[href="#logout"]');

    casper.waitForUrl(/#login$/);

    casper.then(function() {

        this.fillSelectors('form', {

            'input[name="username"]': 'new_user@example.com',
            'input[name="password"]': 'new123'

        }, false);
    });

    casper.thenClick('button[type="submit"]');

    casper.waitForSelector('a[href="#logout"]');
};
