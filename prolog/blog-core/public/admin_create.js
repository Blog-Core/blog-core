var create = (function(exports) {
    
    var idCounter = 0;
    
    exports.create = function(type) {
        
        // Mapping with value getters.
        
        var getters = {};
        
        // Creates form.
        
        var form = document.createElement('form');
        
        form.addEventListener('submit', function(e) {
            
            e.preventDefault();
            
            var newDoc = {};
            
            type.edit.forEach(function(name) {
                
                newDoc[name] = getters[name]();
            });
            
            api.create(type.name, newDoc, function(err) {
                
                // FIXME error handling.
            });
        });
        
        // Creates property editors.
        
        type.edit.forEach(function(name) {
                                    
            var prop = type.props[name];
            
            var id = 'edit' + idCounter++;
            
            var label = document.createElement('label');
            
            label.textContent = util.nameToLabel(name);
            label.htmlFor = id;
            
            var editor = types[prop.type].create(prop, id);
            
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
        
        title.textContent = 'New document in collection ' + util.nameToLabel(type.name);
        
        function links() {
            
            var collection = document.createElement('a');
            
            collection.textContent = 'Back to collection';
            collection.href = '#!/list/' + type.name;
                
            var links = document.createElement('p');

            links.appendChild(collection);
            
            return links;
        }
            
        var div = document.createElement('div');
        
        div.appendChild(title);
        div.appendChild(links());
        div.appendChild(form);
        div.appendChild(links());
        
        return div;        
    };
    
    return exports;
    
})({});
