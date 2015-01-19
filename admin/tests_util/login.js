// Helper to add login steps.

module.exports = function(casper, test) {

    // Drop current database content.

    casper.start('http://localhost:18008/reset', function() {

        test.assert(casper.page.plainText === 'OK');
    });

    // Clears localStorage and sessionStorage.

    casper.evaluate(function() {

        localStorage.clear();
        sessionStorage.clear();
    });

    casper.thenOpen('http://localhost:18008/admin');

    casper.waitForUrl(/#login$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Login');
    });

    casper.then(function() {

        this.fillSelectors('form', {

            'input[name="username"]': 'admin@example.com',
            'input[name="password"]': 'admin'

        }, false);
    });

    casper.thenClick('button[type=submit]');

    casper.waitForSelector('a[href="#logout"]');
};
