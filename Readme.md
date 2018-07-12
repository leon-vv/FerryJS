FerryJS
============================

This package provides a metaphorical ferry between Idris-land and JavaScript-land. ⛴ 
JavaScript values can be easily converted to Idris standard types and vice versa.

For example, the function to access the command line arguments looks like (FerryJS.Util module):

```Idris
export
getArgs : JS_IO (List String)
getArgs =
  toIdrisUnsafe {to=List String}
    <$> jscall "process.argv" (JS_IO Ptr)
```


Usage
-----------------------------
Make sure to install the latest version of the Idris compiler. This package has a dependency on the [Record](https://github.com/leon-vv/Record) package. So install that one first.
```idris --install ferryjs.ipkg```
To use the library in another file use:
```idris -p record_ -p ferryjs Main.idr```

Documentation
----------------------------
```idris --mkdoc ./ferryjs.ipkg```

See `Main.idr` for inspiration.

License
----------------------------
Mozilla Public License, v. 2.0
