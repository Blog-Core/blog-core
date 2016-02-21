var host = require('../host');

casper.test.begin('Check validation', function(test) {

    // The start page. Should be quick to load.

    casper.start(host + '/comment.html', function() {

        test.assertTitle('Blog-Core comment system test');

        casper.viewport(1366, 1200);
    });

    // Checks that no validation error exists.

    casper.then(function() {

        test.assertTextDoesntExist('Email must match email pattern.');
        test.assertTextDoesntExist('Site must match URL pattern.');
        test.assertTextDoesntExist('Author must be set.');
        test.assertTextDoesntExist('Content must be set.');
        test.assertTextDoesntExist('Answer must be set.');
    });

    casper.thenClick('button[type="submit"]');

    // Default inputs.

    casper.then(function() {

        test.assertTextExists('Author must be set.');
        test.assertTextExists('Content must be set.');
        test.assertTextExists('Answer must be set.');
    });

    // Insert author.

    casper.then(function() {

        this.fill('#comment-form', { author: 'Raivo' }, true);
    });

    casper.then(function() {

        test.assertTextExists('Content must be set.');
        test.assertTextExists('Answer must be set.');
    });

    // Insert invalid mail.

    casper.then(function() {

        this.fill('#comment-form', { email: 'invalid'}, true);
    });

    casper.then(function() {

        test.assertTextExists('Email must match email pattern.');
    });

    // Insert valid mail.

    casper.then(function() {

        this.fill('#comment-form', { email: 'info@infdot.com'}, true);
    });

    casper.then(function() {

        test.assertTextDoesntExist('Email must match email pattern.');
    });

    // Insert invalid site.

    casper.then(function() {

        this.fill('#comment-form', { site: 'invalid'}, true);
    });

    casper.then(function() {

        test.assertTextExists('Site must match URL pattern.');
    });

    // Insert valid site.

    casper.then(function() {

        this.fill('#comment-form', { site: 'http://example.com'}, true);
    });

    casper.then(function() {

        test.assertTextDoesntExist('Site must match URL pattern.');
    });

    casper.run(function() {

        test.done();
    });
});
