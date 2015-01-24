var new_post = require('../tests_util/new_post');

casper.test.begin('Update post', function suite(test) {

    new_post(casper, test);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Posts');
    });

    casper.then(function() {

        test.assertSelectorHasText('ul > li > h4', 'Hello World');
    });

    casper.thenClick('ul > li > h4');

    casper.then(function() {

        test.assertSelectorHasText('ul > li .btn:nth-child(1)', 'Edit');

        test.assertSelectorHasText('ul > li .btn:nth-child(2)', 'Comments');

        test.assertSelectorHasText('ul > li .btn:nth-child(3)', 'Remove');
    });

    casper.thenClick('ul > li .btn:nth-child(1)');

    casper.waitForUrl(/#entry\/post\/([^\/]+)$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Hello World');

        test.assertFieldCSS('#post-title', 'Hello World');
        test.assertFieldCSS('#post-slug', 'hello-world');
        test.assertFieldCSS('#post-description', 'Test post.');
        test.assertFieldCSS('#post-content', 'This is **Hello World** post.');
        test.assertFieldCSS('#post-type', 'post');
        test.assertFieldCSS('#post-content-type', 'markdown');
        test.assertFieldCSS('#post-tags', 'hello, world');
        test.assertFieldCSS('#post-update', new Date().toISOString().substring(0, 10));
        test.assertFieldCSS('#post-date', new Date().toISOString().substring(0, 10));
        test.assertFieldCSS('#post-language', 'en');
    });

    casper.then(function() {

        this.fillSelectors('form', {

            '#post-content': 'This is updated **Hello World** post.',
            '#post-description': 'Updated test post.'

        }, false);
    });

    casper.thenClick('button:not([type="submit"])');

    casper.waitForUrl(/#entries\/post/);

    casper.then(function() {

        test.assertSelectorHasText('#messages .message:last-child', 'The entry "Hello World" has been updated.');

        test.assertSelectorHasText('h2', 'Posts');
    });

    casper.run(function() {

        test.done();
    });
});
