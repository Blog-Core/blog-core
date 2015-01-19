var new_post = require('../tests_util/new_post');

casper.test.begin('Remove post', function suite(test) {

    new_post(casper, test);

    casper.then(function() {

        test.assertSelectorHasText('ul > li > h4', 'Hello World');
    });

    casper.thenClick('ul > li > h4');

    casper.then(function() {

        test.assertSelectorHasText('ul > li .btn:nth-child(1)', 'Edit');

        test.assertSelectorHasText('ul > li .btn:nth-child(2)', 'Comments');

        test.assertSelectorHasText('ul > li .btn:nth-child(3)', 'Remove');
    });

    casper.thenClick('ul > li .btn:nth-child(3)');

    casper.waitForSelector('#messages .message');

    casper.then(function() {

        test.assertSelectorHasText('#messages .message:last-child', 'The post "Hello World" has been removed.');

        test.assertSelectorHasText('h2', 'Posts');
    });

    casper.run(function() {

        test.done();
    });
});
