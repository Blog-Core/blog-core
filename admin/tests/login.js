casper.test.begin('Login/logout', function suite(test) {

    casper.start('http://localhost:18008/admin');

    casper.waitFor(function() {

        return this.exists('button[type="submit"]');
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Login');
    });

    casper.then(function() {

        this.fillSelectors('form', {

            'input[name="username"]': 'admin@example.com',
            'input[name="password"]': 'admin'

        }, false);
    });

    casper.thenClick('button[type=submit]');

    casper.waitFor(function() {

        return this.fetchText('h2') === 'Posts';
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Posts');
    });

    casper.thenClick('#menu a:last-child');

    casper.waitFor(function() {

        return this.fetchText('h2') === 'Login';
    });

    casper.then(function() {

        test.assertTitle('Blog-Core Administration');
        test.assertSelectorHasText('h2', 'Login');
    });

    casper.run(function() {

        test.done();
    });
});
