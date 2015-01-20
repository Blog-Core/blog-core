var list_files = require('../tests_util/list_files');

casper.test.begin('File list browser', function suite(test) {

    list_files(casper, test);

    casper.run(function() {

        test.done();
    });
});
