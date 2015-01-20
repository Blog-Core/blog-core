var login_author = require('../tests_util/login_author');

casper.test.begin('New post by author', function suite(test) {

    login_author(casper, test);

    casper.thenClick('a[href="#entries/post"]');

    casper.waitForUrl(/#entries\/post$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Posts');

        test.assertSelectorHasText('.bc-controls > a.btn:nth-child(1)', 'Show more');

        test.assertSelectorHasText('.bc-controls > a.btn:nth-child(2)', 'Show all');

        test.assertSelectorHasText('.bc-controls > a.btn:nth-child(3)', 'Add');
    });

    casper.thenClick('.bc-controls > a.btn:nth-child(3)');

    casper.waitForUrl(/#new\/post$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Untitled');
    });

    casper.then(function() {

        var name = casper.page.evaluate(function() {

            var select = document.querySelector('#post-author');

            return select.options[select.selectedIndex].text;
        });

        test.assert(name === 'New user');
    });

    casper.run(function() {

        test.done();
    });
});
