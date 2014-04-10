// Shows informational message.

exports.info = function(text) {

    var messages = document.getElementById('messages');
    var message = document.createElement('div');

    message.className = 'alert alert-success';
    message.innerHTML = text;

    messages.appendChild(message);

    setTimeout(function() {

        messages.removeChild(message);

    }, 2000);
};

// Shows error message.

exports.error = function(err) {

    var messages = document.getElementById('messages');
    var message = document.createElement('div');

    message.className = 'alert alert-danger';
    message.innerHTML = err.toString();

    messages.appendChild(message);
};
