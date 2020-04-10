window.addEventListener('load', function(ev) {
    document.querySelectorAll('pre[lang]').forEach(function(elem) {
        elem.classList.add('language-' + elem.getAttribute('lang'));
    });
    Prism.highlightAll();
});
