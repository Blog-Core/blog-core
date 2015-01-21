var fs = require('fs');

// Creates view model for the main menu.

function menu(params) {

    var model = {

        active: params.active,

        types: params.types
    };

    return model;
}

ko.components.register('menu', {

    viewModel: { createViewModel: menu },

    template: fs.readFileSync(__dirname + '/menu.html', { encoding: 'utf8' })
});
