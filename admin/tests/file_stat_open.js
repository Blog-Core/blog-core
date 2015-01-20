var stat_file = require('../tests_util/stat_file');

casper.test.begin('A file info page file open link', function suite(test) {

    stat_file(casper, test);

    // Cannot actually open in CasperJS as it opens
    // in _blank.

    casper.then(function() {

        test.assertExists('a[href*="test.txt"]');
    });

    casper.run(function() {

        test.done();
    });
});
