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
# Classic Read the Docs look: blue header, dark navigation sidebar.
html_theme = "sphinx_rtd_theme"
html_title = "ggcirclize Reference"

html_theme_options = {
    "logo_only": False,
    "prev_next_buttons_location": "bottom",
    "style_external_links": True,
    "collapse_navigation": False,
    "sticky_navigation": True,
    "navigation_depth": 3,
    "includehidden": True,
    "titles_only": False,
}

# Powers the "Edit on GitHub" link in the top-right corner.
html_context = {
    "display_github": True,
    "github_user": "junjunlab",
    "github_repo": "ggcirclize",
    "github_version": "master",
    "conf_py_path": "/docs/",
}

html_static_path = ["_static"]
html_css_files = ["custom.css"]
html_favicon = "_static/favicon.svg"
html_logo = "_static/favicon.svg"

html_show_sourcelink = False
html_last_updated_fmt = "%Y-%m-%d"

copybutton_prompt_text = ">>> "
copybutton_prompt_is_regexp = False
