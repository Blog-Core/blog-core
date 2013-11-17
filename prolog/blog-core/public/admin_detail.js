var detail = (function(exports) {
    
    // Creates detail view for the given
    // document.
    
    exports.create = function(doc, type) {
        
        // Creates header for the table.
        
        function header() {
            
            var name = document.createElement('th');
                
            name.textContent = 'Name';
            
            var value = document.createElement('th');
            
            value.textContent = 'Value';
            
            var header = document.createElement('tr');    
            
            header.appendChild(name);
            header.appendChild(value);
            
            return header;
        }                                
        
        var table = document.createElement('table');
        
        table.appendChild(header());
        
        type.detail.forEach(function(name) {
            
            var prop = type.props[name];
            
            var label = document.createElement('td');
            
            label.textContent = util.nameToLabel(name);
            label.className = 'detail-label';
            
            var tr = document.createElement('tr');
            
            var value = document.createElement('td');
            
            value.appendChild(types[prop.type].detail(doc[name], prop));
            
            tr.appendChild(label);
            tr.appendChild(value);
            
            table.appendChild(tr);            
        });
        
        function links() {
            
            var edit = document.createElement('a');
            
            edit.textContent = 'Edit';
            edit.href = '#!/edit/' + doc.$id;
            
            var remove = document.createElement('a');
            
            remove.textContent = 'Delete';
            remove.href = '#';
            
            remove.onclick = function(e) {
                
                e.preventDefault();
                
                if (confirm('Delete this document?')) {
                
                    api.remove(doc.$id, function(err) {
                        
                        // FIXME error handling.
                        
                        window.location.hash = '#!/list/' + type.name;
                    });
                }
            };
            
            var collection = document.createElement('a');
            
            collection.textContent = 'Back to collection';
            collection.href = '#!/list/' + type.name;
                
            var links = document.createElement('p');
                
            links.appendChild(edit);
            links.appendChild(document.createTextNode(' '));
            links.appendChild(remove);
            links.appendChild(document.createTextNode(' '));
            links.appendChild(collection);
            
            return links;
        }
        
        var title = document.createElement('h2');
        
        title.textContent = util.nameToLabel(type.name) + ': ' + doc[type.title];                
            
        var div = document.createElement('div');
        
        div.appendChild(title);
        div.appendChild(links());
        div.appendChild(table);
        div.appendChild(links());
        
        return div;        
    };
    
    return exports;
    
})({});
