exports.draw = function(id) {
    function resizeHandler() {
        var canvas = document.getElementById(id);
        if (canvas) {
            var data = extractData(canvas);
            drawInAnimationFrame(canvas, data);
        } else {
            window.removeEventListener('resize', resizeHandler);
        }
    }
    var canvas = document.getElementById(id);
    if (canvas && canvas.dataset.controlled) {
        // It already has resize handler.
        // Just redraw.
        resizeHandler();
    } else {
        // Install resize handler.
        canvas.dataset.controlled = true;
        window.addEventListener('resize', resizeHandler);
        resizeHandler();
    }    
}

function extractData(canvas) {
    var values = canvas.getAttribute('data-values');
    if (values === '') {
        data = [];
    } else if (values) {
        data = values.split(',').map(function(value) {
            return parseFloat(value);
        });
    }
    return data;
}

function drawInAnimationFrame(canvas, data) {        
    window.requestAnimationFrame(function() {
        canvas.width = Math.floor(canvas.offsetWidth);
        canvas.height = Math.floor(canvas.offsetHeight);            
        var ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.imageSmoothingEnabled = true;
        drawData(ctx, 0, data);
    });
}

function drawData(ctx, padding, values) {
    var canvas = ctx.canvas;
    var height = canvas.height;
    var width = canvas.width;
    var points = normalizeInvert(values);
    var innerWidth = width - 2 * padding;
    var innerHeight = height - 2 * padding;
    var xDelta = innerWidth / points.length;
    var minmax = arrayMinMax(values);
    var first = points[0];
    function yLocation(value) {
        return padding + value * innerHeight;
    }
    ctx.save();
    ctx.lineWidth = 1;
    ctx.strokeStyle = '#999';
    ctx.setLineDash([2]);
    // Draws zero line.
    if (minmax.min < 0 && minmax.max > 0) {
        var yZero = Math.floor(yLocation(1 - minmax.apply(0))) + 0.5;
        ctx.beginPath();
        ctx.moveTo(padding, yZero);
        ctx.lineTo(width - padding, yZero);
        ctx.stroke();
    }
    // Draws min line.
    var yMin = padding + innerHeight + 0.5;
    ctx.beginPath();
    ctx.moveTo(padding, yMin);
    ctx.lineTo(width - padding, yMin);
    ctx.stroke();
    // Draws max line.
    var yMax = padding + 0.5;
    ctx.beginPath();
    ctx.moveTo(padding, yMax);
    ctx.lineTo(width - padding, yMax);
    ctx.stroke();
    ctx.restore();
    // Draws graph line.
    ctx.beginPath();
    ctx.strokeStyle = '#5cb85c';
    ctx.lineWidth = 2;
    ctx.lineJoin = 'round'; 
    ctx.moveTo(padding, yLocation(first));
    points.forEach(function(point, i) {
        if (i > 0) {
            ctx.lineTo(padding + i * xDelta, yLocation(point));
        }
    });
    ctx.stroke();    
    // Draws min/max labels.
    ctx.font = '14px Arial';
    ctx.fillText(minmax.max.toFixed(2), padding, 20);
    ctx.fillText(minmax.min.toFixed(2), padding, height - 10);
}

function arrayMinMax(array) {
    var min = Number.POSITIVE_INFINITY;
    var max = Number.NEGATIVE_INFINITY;
    array.forEach(function(value) {
        if (value < min) {
            min = value;
        }
        if (value > max) {
            max = value;
        }
    });
    var scale = Math.abs(max - min);
    return {
        min: min,
        max: max,
        scale: scale,
        apply: function(value) {
            return (value - min) / scale;
        }
    };
}

function normalizeInvert(array) {
    var minmax = arrayMinMax(array);
    return array.map(function(value) {
        return 1 - minmax.apply(value);
    });
}
