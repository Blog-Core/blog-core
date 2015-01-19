var login = require('./login');

// Helper to add steps to new post page.

module.exports = function(casper, test) {

    login(casper, test);

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
};
