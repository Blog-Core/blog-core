var list = (function(exports) {
    
    // Creates the list view from documents
    // docs with the given type.
    
    exports.create = function(docs, type) {

        // Creates the list view table header.
        
        function header() {
            
            var tr = document.createElement('tr');            
            var id = document.createElement('th');
            
            id.textContent = 'ID';
            
            tr.appendChild(id);
            
            type.list.forEach(function(name) {
                
                var th = document.createElement('th');
                
                th.textContent = util.nameToLabel(name);
                
                tr.appendChild(th);
            });
            
            return tr;
        }
        
        // Creates row cell for the
        // id property.
        
        function idCell(id) {                        
            
            var a = document.createElement('a');
            
            a.title = id;
            a.href = '#!/show/' + id;
            a.textContent = id.substring(0, 8);
            a.className = 'reference';
            
            var td = document.createElement('td');
            
            td.appendChild(a);
            
            return td;
        }
        
        // Creates row cell for the given
        // document property.
        
        function propCell(value, prop) {
            
            var td = document.createElement('td');
            
            td.appendChild(types[prop.type].list(value, prop));
            
            return td;
        }
        
        // Builds the table.
        
        var table = document.createElement('table');
        
        table.appendChild(header());
        
        docs.forEach(function(doc) {
            
            var tr = document.createElement('tr');
            
            tr.appendChild(idCell(doc.$id));
            
            type.list.forEach(function(name) {
                
                tr.appendChild(propCell(doc[name], type.props[name]));
            });
            
            table.appendChild(tr);
        });
        
        var title = document.createElement('h2');
        
        title.textContent = util.nameToLabel(type.name);
        
        function links() {
            
            var add = document.createElement('a');
            
            add.textContent = 'New';
            
            add.href = '#!/new/' + type.name;
            
            var links = document.createElement('p');
            
            links.appendChild(add);
            
            return links;
        }
            
        var div = document.createElement('div');                
        
        div.appendChild(title);
        div.appendChild(links());
        div.appendChild(table);
        div.appendChild(links());
        
        return div;
    };
    
    return exports;
    
})({});
