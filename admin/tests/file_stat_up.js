var stat_file = require('../tests_util/stat_file');

casper.test.begin('A file info page up button/link', function suite(test) {

    stat_file(casper, test);

    casper.thenClick('.bc-controls > .btn:nth-child(1)');

    casper.waitForUrl(/#directory\/2f$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /');
    });

    casper.run(function() {

        test.done();
    });
});
