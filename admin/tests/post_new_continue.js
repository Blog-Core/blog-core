var new_post_page = require('../tests_util/new_post_page');

casper.test.begin('New post saved and continuing editing', function suite(test) {

    new_post_page(casper, test);

    casper.then(function() {

        this.fillSelectors('form', {

            '#post-title': 'Hello World',
            '#post-content': 'This is **Hello World** post.',
            '#post-description': 'Test post.',
            '#post-tags': 'hello, world'

        }, false);
    });

    casper.thenClick('button[type="submit"]');

    casper.waitForSelector('#messages .message');

    casper.then(function() {

        test.assertSelectorHasText('#messages .message:last-child', 'The entry "Hello World" has been saved.');

        test.assertSelectorHasText('h2', 'Hello World');
    });

    casper.run(function() {

        test.done();
    });
});
