var fs = require('fs');

// Creates view model for the main menu.

function menu(params) {

    var model = {

        active: params.active
    };

    return model;
}

ko.components.register('bc-menu', {

    viewModel: { createViewModel: menu },

    template: fs.readFileSync(__dirname + '/menu.html', { encoding: 'utf8' })
});
