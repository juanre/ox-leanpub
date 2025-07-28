# ox-leanpub.el

**Org-mode backend to export Markdown for publication with Leanpub**

> **Note:** This is the original ox-leanpub project created in 2014. The project is now actively maintained by [Diego Zamboni](https://zzamboni.org/).
>
> - **Original repository (archived):** [github.com/juanre/ox-leanpub](https://github.com/juanre/ox-leanpub)
> - **Current maintained version:** [github.com/zzamboni/ox-leanpub](https://github.com/zzamboni/ox-leanpub)
>
> For new projects and active development, please use Diego's version which includes many improvements and bug fixes.

## Overview

Small adaptation of `ox-md.el` to make the exported markdown work better for [Leanpub](http://leanpub.com) publication. 

### Features

- **Footnotes**: Properly formatted footnote references and definitions
- **Code blocks**: Source code separated from its output, with output displayed without line numbers
- **HTML blocks**: Ignored during export
- **LaTeX fragments**: Formatted with `{$$}` `{$$}` notation
- **Cross-references**: Better support for internal links (when using IDs)

## Documentation

For detailed information on how it works, limitations, and installation instructions, visit:
[juanreyero.com/open/ox-leanpub/](http://juanreyero.com/open/ox-leanpub/index.html)

## Disclaimer

I am in no way associated with Leanpub. Leanpub is not responsible for this code. Leanpub is not responsible for any damage it may cause in your computer, and neither am I.