var new_post_page = require('../tests_util/new_post_page');

casper.test.begin('New post', function suite(test) {

    new_post_page(casper, test);

    // Change title.

    casper.then(function() {

        this.fillSelectors('form', {

            '#post-title': 'Hello World'

        }, false);
    });

    casper.then(function() {

        // Check that slug is generated correctly.

        test.assertFieldCSS('#post-slug', 'hello-world');

        test.assertSelectorHasText('h2', 'Hello World');
    });

    casper.run(function() {

        test.done();
    });
});
