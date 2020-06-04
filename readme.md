Multi-line editor control for Lazarus, which was made as an alternative to SynEdit.
It has totally different structure and is not based on SynEdit
(author: I even don't know base structure of SynEdit, it is very complex and very blurry,
structure of ATSynEdit is much simpler, it has less base classes, storages, helper classes,
but of course internal/hidden complexity of classes is big).

It implements lot of features that modern text editors need.
CudaText editor is rather modern in 2020 year, it is based on this component.
Implements word-wrapping from birth.
Implements multi-carets + multi-selections from birth.

Includes copy of TRegExpr engine, which was improved by Alexey Torgashin
(TRegExpr development was continued in 2019 by Alexey, because CudaText users needed it,
all patches were later merged to Free Pascal repo).

* Full documentation: http://wiki.freepascal.org/ATSynEdit
* Adapter for EControl lexers: http://wiki.freepascal.org/ATSynEdit_EControl_adapter

Screenshot of main demo:

![img](img/screen.png?raw=true)

Screenshots of demo with various EControl lexers:

![img](img/syntax_pas.png?raw=true)
![img](img/syntax_cs.png?raw=true)
![img](img/syntax_css.png?raw=true)
![img](img/syntax_py.png?raw=true)

Feature called "inter-line gaps" allows to show bitmaps between lines:

![img](img/pics.png?raw=true)
