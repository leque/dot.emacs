window.addEventListener('load', function(ev) {
    var content = document.getElementById('content').textContent;
    var html = Asciidoctor().convert(
        content,
        {
            attributes: {
                showtitle: true
            }
        });
    document.getElementById('body').innerHTML = html;
    Prism.highlightAll();
});
