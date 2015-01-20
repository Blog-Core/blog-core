var login = require('../tests_util/login');
var add_user = require('../tests_util/add_user');

casper.test.begin('Only admin can manage users', function suite(test) {

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

    casper.thenClick('a[href="#users"]');

    casper.waitForUrl(/#users$/);

    casper.waitForSelector('#content .alert-warning');

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Users');

        test.assertSelectorHasText('#content .alert-warning', 'You do not have permissions to manage users.');
    });

    casper.run(function() {

        test.done();
    });
});
