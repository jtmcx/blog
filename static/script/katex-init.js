document.querySelectorAll(".math").forEach(function (el) {
  katex.render(el.textContent, el, {
    displayMode: el.classList.contains("display"),
    throwOnError: false,
    macros: { "\\arraystretch": "1.25" },
  });
});
