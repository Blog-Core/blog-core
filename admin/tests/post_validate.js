var new_post_page = require('../tests_util/new_post_page');

casper.test.begin('New post', function suite(test) {

    new_post_page(casper, test);

    // Removes untitled.

    casper.then(function() {

        this.fillSelectors('form', {

            '#post-title': '',
            '#post-update': 'invalid',
            '#post-date': 'invalid'

        }, false);
    });

    casper.thenClick('button[type="submit"]');

    casper.then(function() {

        test.assertSelectorHasText('#post-title ~ .error-message', 'Title is not entered.');

        test.assertSelectorHasText('#post-slug ~ .error-message', 'Slug is not entered.');

        test.assertSelectorHasText('#post-content ~ .error-message', 'Content is not entered.');

        test.assertSelectorHasText('#post-date ~ .error-message', 'Date must be in the YYYY-MM-DD format.');

        test.assertSelectorHasText('#post-update ~ .error-message', 'Update date must be in the YYYY-MM-DD format.');
    });

    casper.run(function() {

        test.done();
    });
});
