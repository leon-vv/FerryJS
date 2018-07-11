FerryJS
============================

This package provides a ferry between Idris-land and JavaScript-land. ⛴ 
JavaScript values can be easily converted to Idris standard types and vice versa.

Usage
-----------------------------
Make sure to install the latest version of the Idris compiler. This package has a dependency of the Record package. So install that one first.
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
