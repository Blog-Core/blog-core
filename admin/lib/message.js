// Shows informational message.

exports.info = function(text) {

    var messages = document.getElementById('messages');
    var message = document.createElement('div');

    message.className = 'alert alert-success message';
    message.innerHTML = text;

    messages.appendChild(message);

    setTimeout(function() {

        messages.removeChild(message);

    }, 2000);
};

// Shows error message.

exports.error = function(err) {

    var button = document.createElement('button');

    button.className = 'close';
    button.innerHTML = '&times;';

    var text = document.createElement('span');
    text.innerHTML = err.toString();

    var messages = document.getElementById('messages');
    var message = document.createElement('div');

    message.className = 'alert alert-danger message';
    message.appendChild(button);
    message.appendChild(text);

    button.addEventListener('click', function() {

        messages.removeChild(message);

    }, false);

    messages.appendChild(message);

    if (err instanceof Error) {

        console.error(err.stack);
    }
};
