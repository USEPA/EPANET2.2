# conf.py
# pip install sphinx sphinx-rtd-theme

# -- Project info -----------------------------------------------------
project = "EPANET 2.2"
author = "USEPA"
extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.intersphinx",
]

# Intersphinx mapping must be a dict (you had a warning earlier about this)
intersphinx_mapping = {
    "python": ("https://docs.python.org/3", {}),
}

# If you don't have a _static folder, either create it or set this to []
html_static_path = ["_static"]  # create docs/_static or change to []

# If you saw the source_suffix warning, set it explicitly like this:
source_suffix = {
    ".rst": "restructuredtext",
}

# -- Theme ------------------------------------------------------------
html_theme = "sphinx_rtd_theme"  # html_theme_path not needed

html_theme_options = {
    "collapse_navigation": False,  # keep the full tree expanded
    "navigation_depth": 4,         # how many levels to show
    "sticky_navigation": True,     # nav stays visible on scroll
    "titles_only": False,          # show section titles too
}

# Optional: if you don't use translations, remove/omit locale settings entirely
# locale_dirs = ['locales']  # ONLY if you actually have this directory


