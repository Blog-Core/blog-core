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

        this.fillSelectors('form#login', {

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

    /*
    casper.thenOpen('http://mail.infdot.com/', function() {

        test.assert('https://mail.infdot.com/' === this.getCurrentUrl(), 'Canonical redirect');
    });

    casper.thenOpen('https://mail.infdot.com/data/settings/settings.xml', function() {

        test.assertTitle('403 Forbidden');
    });*/

    casper.run(function() {

        test.done();
    });
});
