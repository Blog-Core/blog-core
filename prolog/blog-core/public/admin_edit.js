var edit = (function(exports) {
    
    var idCounter = 0;
    
    exports.create = function(doc, type) {
        
        // Mapping with value getters.
        
        var getters = {};
        
        // Creates form.
        
        var form = document.createElement('form');
        
        form.addEventListener('submit', function(e) {
            
            e.preventDefault();
            
            var newDoc = {
                
                $id: doc.$id
            };
            
            var changes = false;
            
            type.edit.forEach(function(name) {
                
                var value = getters[name]();
                
                // Only update value that was changed.
                
                if (value !== doc[name]) {
                
                    newDoc[name] = value;
                    
                    changes = true;
                }
            });
            
            if (changes) {
            
                api.update(newDoc, function(err) {
                    
                    // FIXME error handling.
                });
            }
        });
        
        // Creates property editors.
        
        type.edit.forEach(function(name) {
                                    
            var prop = type.props[name];
            
            var id = 'edit' + idCounter++;
            
            var label = document.createElement('label');
            
            label.textContent = util.nameToLabel(name);
            label.htmlFor = id;
            
            var editor = types[prop.type].edit(doc[name], prop, id);
            
            getters[name] = editor.value;
            
            var div = document.createElement('div');
            
            div.className = 'editor';
            
            div.appendChild(label);
            div.appendChild(editor.dom);
            
            form.appendChild(div);
        });
        
        var submit = document.createElement('button');
        
        submit.textContent = 'Save';
        
        form.appendChild(submit);
        
        var title = document.createElement('h2');
        
        title.textContent = util.nameToLabel(type.name) + ': ' + doc[type.title];
        
        var view = document.createElement('a');
        
        view.textContent = 'Back to view';
        view.href = '#!/show/' + doc.$id;
        
        var collection = document.createElement('a');
        
        collection.textContent = 'Back to collection';
        collection.href = '#!/list/' + type.name;
            
        var links = document.createElement('p');

        links.appendChild(view);
        links.appendChild(document.createTextNode(' '));
        links.appendChild(collection);
            
        var div = document.createElement('div');
        
        div.appendChild(title);
        div.appendChild(links);
        div.appendChild(form);
        div.appendChild(links.cloneNode(true));
        
        return div;  
    };
    
    return exports;
    
})({});
