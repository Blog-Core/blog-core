var login = require('../tests_util/login');
var add_user = require('../tests_util/add_user');

casper.test.begin('Update user', function suite(test) {

    login(casper, test);

    add_user(casper, test);

    casper.then(function() {

        test.assertSelectorHasText('ul > li:last-child h4', 'New user');
    });

    casper.thenClick('ul > li:last-child h4');

    casper.then(function() {

        test.assertSelectorHasText('ul > li:last-child .btn:first-child', 'Edit');

        test.assertSelectorHasText('ul > li:last-child .btn:last-child', 'Remove');
    });

    casper.thenClick('ul > li:last-child .btn:first-child');

    casper.waitForUrl(/#user\/([^\/]+)$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'New user');
    });

    casper.then(function() {

        this.fillSelectors('form', {

            '#user-fullname': 'New user updated'

        }, false);
    });

    casper.then(function() {

        test.assertSelectorHasText('h2', 'New user updated');
    });

    casper.thenClick('button[type=submit]');

    casper.waitForUrl(/#users$/);

    casper.then(function() {

        test.assertSelectorHasText('#messages .message:last-child', 'User "new_user@example.com" has been updated.');

        test.assertSelectorHasText('h2', 'Users');
    });

    casper.run(function() {

        test.done();
    });
});
