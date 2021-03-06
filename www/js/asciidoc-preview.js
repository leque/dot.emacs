/* global Asciidoctor Prism */
window.addEventListener('load', (_ev) => {
    const content = document.getElementById('content').textContent;
    const html = Asciidoctor().convert(
        content,
        {
            attributes: {
                showtitle: true
            }
        });
    document.getElementById('body').innerHTML = html;
    Prism.highlightAll();
});
