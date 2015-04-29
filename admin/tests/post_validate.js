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

        test.assertSelectorHasText('#post-title ~ .ko-input-error', 'Title is not entered.');

        test.assertSelectorHasText('#post-slug ~ .ko-input-error', 'Slug is not entered.');

        test.assertSelectorHasText('#post-content ~ .ko-input-error', 'Content is not entered.');

        test.assertSelectorHasText('#post-date ~ .ko-input-error', 'Date must be in the YYYY-MM-DD format.');

        test.assertSelectorHasText('#post-update ~ .ko-input-error', 'Update date must be in the YYYY-MM-DD format.');
    });

    casper.run(function() {

        test.done();
    });
});
