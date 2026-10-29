# Configuration file for the Sphinx documentation builder.
#
# This file only contains the most commonly used options. For a full list see:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

import os
import sys

# -- Project information -----------------------------------------------------
project = "ggcirclize"
copyright = "2026, Jun Zhang"
author = "Jun Zhang"
release = "0.0.3"

# -- General configuration ---------------------------------------------------
extensions = [
    "myst_parser",
    "sphinx_copybutton",
    "sphinx.ext.autodoc",
    "sphinx.ext.napoleon",
]

templates_path = ["_templates"]
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store", "README.md"]

myst_enable_extensions = [
    "colon_fence",
    "deflist",
    "html_image",
]
myst_heading_anchors = 3
myst_all_links_external = True

# -- Options for HTML output -------------------------------------------------
html_theme = "sphinx_book_theme"
html_title = "ggcirclize Reference"
html_theme_options = {
    "repository_url": "https://github.com/junjunlab/ggcirclize",
    "use_repository_button": True,
    "use_issues_button": True,
    "path_to_docs": "docs",
    "show_toc_level": 2,
}
html_static_path = ["_static"]
html_css_files = ["custom.css"]
html_favicon = "_static/favicon.svg"
html_logo = "_static/favicon.svg"

copybutton_prompt_text = ">>> "
copybutton_prompt_is_regexp = False
