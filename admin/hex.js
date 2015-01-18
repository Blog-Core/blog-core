exports.hex = function(string) {

    var hex = '';

    for(var i = 0; i < string.length; i++) {

        hex += string.charCodeAt(i).toString(16);
    }

    return hex;
};

// FIXME this is broken encoding.

exports.string = function(hex) {

    if (typeof hex !== 'string') {

        hex = hex.toString();
    }

    var string = '';

    for (var i = 0; i < hex.length; i += 2) {

        string += String.fromCharCode(parseInt(hex.substr(i, 2), 16));
    }

    return string;
};
