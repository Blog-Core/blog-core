var list_files_sub = require('../tests_util/list_files_sub');

casper.test.begin('File list go up from subdirectory', function suite(test) {

    list_files_sub(casper, test);

    casper.thenClick('.bc-controls > .btn:nth-child(1)');

    casper.waitForUrl(/#directory\/2f$/);

    casper.then(function() {

        test.assertSelectorHasText('h2', 'Directory /');
    });

    casper.run(function() {

        test.done();
    });
});
