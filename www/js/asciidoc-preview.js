window.addEventListener('load', function(ev) {
    document.getElementById('body').innerHTML =
        Asciidoctor().convert(
            document.getElementById('content').textContent,
            {
                attributes: {
                    showtitle: true
                }
            });
});
