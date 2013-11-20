// Describes different property types
// and display/editor views.

// FIXME ellipsis

var types = (function(exports) {
    
    // Single-line text field.
    
    exports.line = {
        
        // Show value in the list view.
        // Simply a text is shown.
        
        list: function(value, prop) {
            
            return document.createTextNode(value);
        },
        
        detail: function(value, prop) {
            
            return document.createTextNode(value);
        },

        // Property editor. Returns
        // dom for the editor and accessor for the edited value.
        
        edit: function(value, prop, id) {
            
            var input = document.createElement('input');
                
            input.type = 'text';
            input.id = id;
            input.value = value;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        },
        
        create: function(prop, id) {
            
            var input = document.createElement('input');
                
            input.type = 'text';
            input.id = id;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        }
    };
    
    // Multiline text field.
    
    exports.multiline = {
        
        // Show shortened text in list view.
        
        list: function(value, prop) {
            
            return document.createTextNode(value);
        },
        
        detail: function(value, prop) {
            
            var pre = document.createElement('pre');
            
            pre.appendChild(document.createTextNode(value));
            
            return pre;
        },
        
        edit: function(value, prop, id) {
            
            var input = document.createElement('textarea');
            
            input.cols = 60;
            input.rows = 20;
            input.id = id;
            input.value = value;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        },
        
        create: function(prop, id) {
            
            var input = document.createElement('textarea');
            
            input.cols = 60;
            input.rows = 20;
            input.id = id;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        }
    };
    
    // Password editor.
    
    exports.password = {
        
        // Show just ****.
        
        list: function(value, prop) {
            
            return document.createTextNode('****');
        },
        
        detail: function(value, prop) {
            
            return document.createTextNode(value);
        },
        
        edit: function(value, prop, id) {
            
            var input = document.createElement('input');
                
            input.type = 'password';
            input.id = id;
            input.value = value;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        },
        
        create: function(prop, id) {
            
            var input = document.createElement('input');
                
            input.type = 'password';
            input.id = id;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        }
    };
    
    // Boolean with checkbox.
    
    exports.boolean = {
        
        list: function(value, prop) {
            
            return document.createTextNode(value ? 'yes' : 'no');
        },
        
        detail: function(value, prop) {
            
            return document.createTextNode(value ? 'yes' : 'no');
        },
        
        edit: function(value, prop, id) {
            
            var input = document.createElement('input');
            
            input.type = 'checkbox';
            input.id = id;
            input.checked = value;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.checked;
                }
            };
        },
        
        create: function(prop, id) {
            
            var input = document.createElement('input');
            
            input.type = 'checkbox';
            input.id = id;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.checked;
                }
            };
        }
    };
    
    // Single value from multiple choices.
    
    exports.choice = {
        
        list: function(value, prop) {
            
            return document.createTextNode(value);
        },
        
        detail: function(value, prop) {
            
            return document.createTextNode(value);
        },
        
        edit: function(value, prop, id) {
            
            var input = document.createElement('select');
            
            prop.values.forEach(function(value) {
                
                var option = document.createElement('option');
                
                option.value = value;
                option.textContent = util.nameToLabel(value);
                
                input.appendChild(option);
            });
            
            input.id = id;
            input.value = value;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        },
        
        create: function(prop, id) {
            
            var input = document.createElement('select');
            
            prop.values.forEach(function(value) {
                
                var option = document.createElement('option');
                
                option.textContent = util.nameToLabel(value);
                
                input.appendChild(option);
            });
            
            input.id = id;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return input.value;
                }
            };
        }
    };
    
    // Date with time.
    
    exports.datetime = {
        
        list: function(value, prop) {
            
            var date = new Date(value * 1000);
            
            return document.createTextNode(date.toLocaleString());
        },
        
        detail: function(value, prop) {
            
            var date = new Date(value * 1000);
            
            return document.createTextNode(date.toLocaleString());
        },
        
        edit: function(value, prop, id) {
            
            var date = new Date(value * 1000);
            
            var input = document.createElement('input');
            
            input.type = 'text';
            input.id = id;
            input.value = date.toLocaleString();
            input.disabled = true;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return value;
                }
            };
        },
        
        create: function(prop, id) {
            
            var date = new Date();
            
            var input = document.createElement('input');
            
            input.type = 'text';
            input.id = id;
            input.value = date.toLocaleString();
            input.disabled = true;
            
            return {
                
                dom: input,
             
                value: function() {
                    
                    return Math.round(date.getTime() / 1000);
                }
            };
        }
    };
    
    // Reference to another document.
    // Currently has only 'list' and 'detail' views.
    
    exports.ref = {
        
        list: function(value, prop) {
            
            var a = document.createElement('a');
            
            a.title = value;
            a.href = '#!/show/' + value;
            a.textContent = value.substring(0, 8);
            a.className = 'reference';
            
            return a;
        },
        
        detail: function(value, prop) {
            
            var a = document.createElement('a');
            
            a.title = value;
            a.href = '#!/show/' + value;
            a.textContent = value.substring(0, 8);
            a.className = 'reference';
            
            return a;
        }        
    };
    
    // List of document references.
    // Currently only has detail view.
    
    exports.reflist = {
        
        detail: function(value, prop) {
            
            var ul = document.createElement('ul');
                
            value.forEach(function(ref) {
                
                var a = document.createElement('a');
                
                a.textContent = ref;
                a.href = '#!/show/' + ref;
                a.className = 'reference';
                
                var li = document.createElement('li');
                
                li.appendChild(linkElem);
                
                ul.appendChild(liElem);                    
            });
            
            return ul;
        }        
    };
    
    return exports;
    
})({});
