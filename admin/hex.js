exports.hex = function(string) {

    var hex = '';

    // This encodes string to escaped UTF-8 octets
    // interleaved with normal ASCII characters.

    var encoded = encodeURIComponent(string), i = 0;

    while (i < encoded.length) {

        var ch = encoded.charAt(i);

        if (ch === '%') {

            i += 1;

            hex += encoded.charAt(i);

            i += 1;

            hex += encoded.charAt(i);

        } else {

            hex += encoded.charCodeAt(i).toString(16);
        }

        i += 1;
    }

    return hex.toLowerCase();
};

exports.string = function(hex) {

    if (typeof hex !== 'string') {

        hex = hex.toString();
    }

    var encoded = '';

    // Turns into percent-encoded
    // UTF-8.

    while (hex) {

        encoded += '%' + hex.substring(0, 2);

        hex = hex.substring(2);
    }

    // Decodes to internal representation.

    return decodeURIComponent(encoded);
};
