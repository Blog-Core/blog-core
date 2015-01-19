var login = require('../tests_util/login');
var add_user = require('../tests_util/add_user');

casper.test.begin('New user', function suite(test) {

    login(casper, test);

    add_user(casper, test);

    casper.run(function() {

        test.done();
    });
});
