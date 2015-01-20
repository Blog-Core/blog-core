var list_files_sub = require('../tests_util/list_files_sub');

casper.test.begin('File list go to subdirectory', function suite(test) {

    list_files_sub(casper, test);

    casper.run(function() {

        test.done();
    });
});
