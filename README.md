# uax-15

This package provides a common lisp unicode normalization function using nfc, nfd, nfkc and nfkd as per Unicode Standard Annex #15 found at [http://www.unicode.org/reports/tr15/tr15-22.html](http://www.unicode.org/reports/tr15/tr15-22.html).

This is a fork of a subset of work done by Takeru Ohta in 2010. Future work is intended to provide support for https://tools.ietf.org/html/rfc8264 and https://tools.ietf.org/html/rfc7564.

# Implementation Notes
This has been successfully tested on sbcl, ccl, ecl, clisp, abcl, allegro and cmucl against the unicode test file found at [http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt](http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt)

# Usage
It has one major exported function:

  * (normalize (str unicode-normalization-method))

The currently supported normalization methods are :nfc :nfkc :nfd :nfkd

Normalization example with reference to relevant xkcd [https://www.xkcd.com/936/](https://www.xkcd.com/936/)

```common-lisp
    (normalize "正しい馬バッテリーステープル" :nfkc)
    "正しい馬バッテリーステープル"

    (normalize "الحصان الصحيح البطارية التيلة" :nfkc)
    "الحصان الصحيح البطارية التيلة"

    (normalize "اstáplacha ceart ceallraí capall" :nfkc)
    "اstáplacha ceart ceallraí capall"
```

# To Do list
  * Implement and validate against https://tools.ietf.org/html/rfc8264 and https://tools.ietf.org/html/rfc7564
  * Optimization?

More relevant xkcd [https://xkcd.com/1726/](https://xkcd.com/1726/), [https://xkcd.com/1953/](https://xkcd.com/1953/), [https://www.xkcd.com/1209/](https://www.xkcd.com/1209/), [https://xkcd.com/1137/](https://xkcd.com/1137/)

# Data Files
  * UnicodeData.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/UnicodeData.txt](http://www.unicode.org/Public/UNIDATA/UnicodeData.txt)
  * The file CompositionExclusions.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt](http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt)
  * The file DerivedNormalizationProps.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt](http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt)
  * The test file NormalizationTest.txt was downloaded from [http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt](http://www.unicode.org/Public/UNIDATA/NormalizationTest.txt)

# Other References
  * [https://tools.ietf.org/html/rfc5802](https://tools.ietf.org/html/rfc5802)
  * [http://www.unicode.org/reports/tr15/#References](http://www.unicode.org/reports/tr15/#References)
  * [https://www.unicode.org/reports/tr41/tr41-24.html](https://www.unicode.org/reports/tr41/tr41-24.html)
  * [https://www.unicode.org/charts/normalization/](https://www.unicode.org/charts/normalization/)
  * [https://towardsdatascience.com/difference-between-nfd-nfc-nfkd-and-nfkc-explained-with-python-code-e2631f96ae6c](https://towardsdatascience.com/difference-between-nfd-nfc-nfkd-and-nfkc-explained-with-python-code-e2631f96ae6c)
  * [https://en.wikipedia.org/wiki/Unicode_equivalence](https://en.wikipedia.org/wiki/Unicode_equivalence)
  * [http://www.unicode.org/faq/normalization.html](http://www.unicode.org/faq/normalization.html)
  * [https://github.com/edicl/cl-unicode/blob/master/specials.lisp](https://github.com/edicl/cl-unicode/blob/master/specials.lisp)
  * [https://perldoc.perl.org/Unicode/Normalize.html](https://perldoc.perl.org/Unicode/Normalize.html)
  * [https://www.mkssoftware.com/docs/perl/lib/Unicode/Normalize.asp](https://www.mkssoftware.com/docs/perl/lib/Unicode/Normalize.asp)
  * [https://github.com/Wisdom/Awesome-Unicode](https://github.com/Wisdom/Awesome-Unicode)
