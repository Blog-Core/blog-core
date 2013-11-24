var dateEdit = (function(exports) {
    
    exports.create = function(value) {
        
        if (typeof value === 'undefined') {
            
            value = Math.round(Date.now() / 1000);
        }                
        
        var opt;
        
        var day = document.createElement('select');
        
        for (var d = 1; d <= 31; d++) {
            
            opt = document.createElement('option');
            
            opt.value = d;
            opt.textContent = d;
            
            day.appendChild(opt);
        }
    
        var months = ['January', 'February', 'March', 'April', 'May', 'June',
            'July', 'August', 'September', 'October', 'November', 'December'];
        
        var month = document.createElement('select');
        
        months.forEach(function(name, i) {
            
            opt = document.createElement('option');
            
            opt.value = i;
            opt.textContent = name;
            
            month.appendChild(opt);
        });
        
        var year = document.createElement('select');
        
        for (var y = 1970; y < 2036; y++) {
            
            opt = document.createElement('option');
            
            opt.value = y;
            opt.textContent = y;
            
            year.appendChild(opt);
        }
        
        var hour = document.createElement('select');
        
        for (var h = 0; h < 24; h++) {
            
            opt = document.createElement('option');
            
            opt.value = h;
            opt.textContent = pad(h);
            
            hour.appendChild(opt);
        }
        
        var minute = document.createElement('select');
        
        for (var m = 0; m < 60; m++) {
            
            opt = document.createElement('option');
            
            opt.value = m;
            opt.textContent = pad(m);
            
            minute.appendChild(opt);
        }
        
        function pad(num) {
            
            return num < 10 ? '0' + num : num;
        }
        
        var date = new Date(value * 1000);
        
        day.value = date.getDate();
        month.value = date.getMonth();
        year.value = date.getFullYear();
        hour.value = date.getHours();
        minute.value = date.getMinutes();
        
        var editor = document.createElement('div');
        
        editor.className = 'date-edit';
        
        editor.appendChild(day);
        editor.appendChild(document.createTextNode('-'));
        editor.appendChild(month);
        editor.appendChild(document.createTextNode('-'));
        editor.appendChild(year);
        editor.appendChild(document.createTextNode(' '));
        editor.appendChild(hour);
        editor.appendChild(document.createTextNode(':'));
        editor.appendChild(minute);
    
        return {
            
            dom: editor,
            
            value: function() {
                
                var date = new Date();
                
                date.setHours(parseInt(hour.value, 10), parseInt(minute.value, 10), 0, 0);
                date.setFullYear(parseInt(year.value, 10), parseInt(month.value, 10),
                    parseInt(day.value, 10));
                
                return Math.round(date.getTime() / 1000);
            }
        };
    }
    
    return exports;
    
})({});

