var new_post = require('../tests_util/new_post');

casper.test.begin('New post', function suite(test) {

    new_post(casper, test);

    casper.run(function() {

        test.done();
    });
});
