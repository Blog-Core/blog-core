var collections = (function(exports) {
    
    exports.create = function(list) {
        
        function header() {
            
            var name = document.createElement('th');
            
            name.textContent = 'Name';
            
            var desc = document.createElement('th');
            
            desc.textContent = 'Description';
            
            var tr = document.createElement('tr');            
            
            tr.appendChild(name);
            tr.appendChild(desc);
            
            return tr;
        }
        
        var table = document.createElement('table');
        
        table.appendChild(header());
        
        list.forEach(function(type) {
                
            var a = document.createElement('a');
            
            a.textContent = util.nameToLabel(type.name);
            a.href = '#!/list/' + type.name;
            
            var name = document.createElement('td');
            
            name.appendChild(a);
            
            var desc = document.createElement('td');
            
            desc.textContent = type.description;
            
            var tr = document.createElement('tr');
            
            tr.appendChild(name);
            tr.appendChild(desc);
            
            table.appendChild(tr);
        });
        
        var title = document.createElement('h2');
        
        title.textContent = 'Document collections';
        
        function links() {
            
            var links = document.createElement('p');
            
            var logout = document.createElement('a');
            
            logout.textContent = 'Logout';
            logout.href = '#';
            logout.addEventListener('click', function(e) {
                
                e.preventDefault();
                
                auth.unsetKey();
                
                window.location.hash = '';
                
            }, false);
            
            links.appendChild(logout);
            
            return links;
        }
        
        var content = document.createElement('div');
        
        content.appendChild(title);
        content.appendChild(links());
        content.appendChild(table);
        content.appendChild(links());
        
        return content;        
    };
    
    return exports;
    
})({});
