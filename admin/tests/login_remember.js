casper.test.begin('Login/logout with remember', function suite(test) {

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

        casper.fillSelectors('form', {

            'input[name="username"]': 'admin@example.com',
            'input[name="password"]': 'admin'

        }, false);

        // Toggle checkbox.

        casper.page.evaluate(function() {

            document.querySelector('input[name="remember"]').click();
        });
    });

    casper.thenClick('button[type="submit"]');

    casper.waitForSelector('a[href="#logout"]');

    casper.then(function() {

        var key = casper.page.evaluate(function() {

            return localStorage.getItem('api-key');

        });

        test.assert(key.length === 36);
    });

    casper.thenClick('a[href="#logout"]');

    casper.waitForUrl(/#login$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Login');

        var key = casper.page.evaluate(function() {

            return localStorage.getItem('api-key') || '';

        });

        test.assert(key.length === 0);
    });

    casper.run(function() {

        test.done();
    });
});
