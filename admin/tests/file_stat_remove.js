var stat_file = require('../tests_util/stat_file');

casper.test.begin('A file info page remove button', function suite(test) {

    stat_file(casper, test);

    casper.thenClick('.bc-controls > .btn:nth-child(3)');

    casper.waitForUrl(/#directory\/2f$/);

    casper.then(function() {

        test.assertSelectorHasText('#messages .message:last-child', 'File "/test.txt" has been removed.');

        test.assertSelectorHasText('h2', 'Directory /');
    });

    casper.run(function() {

        test.done();
    });
});
