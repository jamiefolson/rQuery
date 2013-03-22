rQuery
======

Bringing the [`jQuery`](http://api.jquery.com/) syntax to R.

Install with `devtools`:

```r
library(devtools)
install_github("rQuery","jamiefolson",subdir="pkg")
```

To show how things work, consider the following document fragment:
```html
<html>
<body>
<ul class="level-1">
  <li class="item-i">I</li>
  <li class="item-ii">II
    <ul class="level-2">
      <li class="item-a">A</li>
      <li class="item-b">B
        <ul class="level-3">
          <li class="item-1">1</li>
          <li class="item-2">2</li>
          <li class="item-3">3</li>
        </ul>
      </li>
      <li class="item-c">C</li>
    </ul>
  </li>
  <li class="item-iii">III</li>
</ul>
</body>
</html>
```

Currently only a few `jQuery` functions are implemented.  Since `$` is a pretty
important generic function, I thought it best not to overwrite it.  Instead,
you can access the basic `jQuery` selector with the function `rQuery` or its
alias `%$%`.

```r
> library(XML)
> library(rQuery)
Loading required package: selectr
> doc = htmlParse("tmp.html")
> `%$%`(doc,"ul.level-2")
[[1]]
<ul class="level-2"><li class="item-a">A</li>
      <li class="item-b">B
        <ul class="level-3"><li class="item-1">1</li>
          <li class="item-2">2</li>
          <li class="item-3">3</li>
        </ul></li>
      <li class="item-c">C</li>
    </ul> 
```

`jQuery` methods are implemented as `S4` methods on the class `rQueryResult`.

```r
> showMethods(classes="rQueryResult")
...
Function: rQuery.add (package rQuery)
rquery="rQueryResult"

Function: rQuery.addClass (package rQuery)
rquery="rQueryResult"
...
> rQuery.class(`%$%`(doc,"ul"))
[1] "level-1"
```

In addition to the regular R syntax for calling methods, `rQuery` emulates the `jQuery` "chaining" syntax.

```r
> `%$%`(doc,"body")$find("ul.level-2")$attr(x="value")$addClass("myclass")$prepend("<li>New item</li>")$addClass("AB")
[[1]]
<li class="AB">New item</li> 

> doc
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">
<html><body><ul class="level-1">
<li class="item-i">I</li>
  <li class="item-ii">II
    <ul class="level-2,myclass" x="value">
<li class="AB">New item</li>
<li class="item-a">A</li>
      <li class="item-b">B
        <ul class="level-3">
<li class="item-1">1</li>
          <li class="item-2">2</li>
          <li class="item-3">3</li>
        </ul>
</li>
      <li class="item-c">C</li>
    </ul>
</li>
  <li class="item-iii">III</li>
</ul></body></html>
```

## Package Documentation
You can find the generated package documentation [here](http://jamiefolson.github.com/rQuery/man/00Index.html).
