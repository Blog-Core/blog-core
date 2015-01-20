var login_author = require('../tests_util/login_author');

casper.test.begin('Only admin can manage users', function suite(test) {

    login_author(casper, test);

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
