var new_post_page = require('../tests_util/new_post_page');

casper.test.begin('New post default values', function suite(test) {

    new_post_page(casper, test);

    casper.then(function() {

        // Test default values.

        test.assertFieldCSS('#post-type', 'post');
        test.assertFieldCSS('#post-language', 'en');
        test.assertFieldCSS('#post-content-type', 'markdown');
        test.assertFieldCSS('#post-update', new Date().toISOString().substring(0, 10));
        test.assertFieldCSS('#post-date', new Date().toISOString().substring(0, 10));
    });

    casper.run(function() {

        test.done();
    });
});
