Multi-line editor control for Lazarus, which was made by Alexey Torgashin
as an alternative to SynEdit. It is not based on SynEdit, it has totally different structure.
Alexey (me) does not know base structure of SynEdit, it is very complex and blurry,
structure of ATSynEdit is much simpler, it has less base classes, storages, helper classes,
but of course internal/hidden complexity of classes is big.

It implements lot of features that modern text editors need.
CudaText editor is rather modern in 2020 year, it is based on this component.
Implements word-wrap from birth.
Implements multi-carets + multi-selections from birth.
Implements mini-map which mimics Sublime Text behaviour.
Supports painting of pictures in the inter-line "gaps".
And lot more features.

Includes renamed copy of TRegExpr engine, which was improved by Alexey Torgashin.
TRegExpr development was continued in 2019 by Alexey, because CudaText users needed it.
Some patches were later merged to Free Pascal repo, but not all patches yet.

- Full documentation: http://wiki.freepascal.org/ATSynEdit
- Adapter for EControl lexers: http://wiki.freepascal.org/ATSynEdit_EControl_adapter

Screenshot of main demo:

![img](img/screen.png?raw=true)

Screenshots of demo with various EControl lexers:

![img](img/syntax_pas.png?raw=true)
![img](img/syntax_cs.png?raw=true)
![img](img/syntax_css.png?raw=true)
![img](img/syntax_py.png?raw=true)

Feature called "inter-line gaps" allows to show bitmaps between lines:

![img](img/pics.png?raw=true)
