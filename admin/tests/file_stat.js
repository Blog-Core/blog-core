var stat_file = require('../tests_util/stat_file');

casper.test.begin('A file info page', function suite(test) {

    stat_file(casper, test);

    casper.run(function() {

        test.done();
    });
});
