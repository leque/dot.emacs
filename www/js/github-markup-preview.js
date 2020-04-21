window.addEventListener('load', (ev) => {
    document.querySelectorAll('pre[lang]').forEach((elem) => {
        elem.classList.add(`language-${elem.getAttribute('lang')}`);
    });
    Prism.highlightAll();
});
