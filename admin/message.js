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
