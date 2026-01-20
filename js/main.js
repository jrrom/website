(() => {
    // We assume that the data-theme has already been dealt with
    // inside inline head.

    function createThemeSelect(themes = ["dark", "light"]) {
        const select = document.createElement("select");
        const current = document.documentElement.dataset.theme ?? themes[0];

        themes.forEach(theme => {
            select.add(
                new Option(theme, theme, theme === current, theme === current)
            );
        });
        select.className = "theme-dropdown";

        return select;
    }

    const themeDropdown = createThemeSelect();

    themeDropdown.addEventListener("change", e => {
        document.documentElement.dataset.theme = e.target.value;
        localStorage.setItem("theme", e.target.value);
    });


    document
        .querySelector(".header__nav")
        .prepend(themeDropdown);
})();
