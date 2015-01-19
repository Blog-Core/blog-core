// Adds a new user (New user).

module.exports = function(casper, test) {

    casper.thenClick('a[href="#users"]');

    casper.waitForUrl(/#users$/);

    casper.thenClick('a[href="#user/new"]');

    casper.waitForUrl(/#user\/new$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Unnamed');
    });

    casper.then(function() {

        this.fillSelectors('form', {

            '#user-fullname': 'New user',
            '#user-username': 'new_user@example.com',
            '#user-password': 'new123'

        }, false);
    });

    casper.then(function() {

        test.assertSelectorHasText('h2', 'New user');
    });

    casper.thenClick('button[type=submit]');

    casper.waitForUrl(/#users$/);

    casper.then(function() {

        test.assertSelectorHasText('#messages .message:last-child', 'User "new_user@example.com" has been added.');

        test.assertSelectorHasText('h2', 'Users');
    });
};
