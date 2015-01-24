var new_post_page = require('./new_post_page');

// Helper to add steps to new post page.

module.exports = function(casper, test) {

    new_post_page(casper, test);

    casper.then(function() {

        this.fillSelectors('form', {

            '#post-title': 'Hello World',
            '#post-content': 'This is **Hello World** post.',
            '#post-description': 'Test post.',
            '#post-tags': 'hello, world'

        }, false);
    });

    casper.thenClick('button:not([type="submit"])');

    casper.waitForUrl(/#entries\/post$/);

    casper.then(function() {

        test.assertSelectorHasText('#messages .message:last-child', 'The entry "Hello World" has been saved.');

        test.assertSelectorHasText('h2', 'Posts');
    });
};
