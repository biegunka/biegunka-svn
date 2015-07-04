biegunka-svn
============
[![Build Status](https://secure.travis-ci.org/biegunka/biegunka-svn.png?branch=master)](http://travis-ci.org/biegunka/biegunka-svn)

[SVN](https://subversion.apache.org/) repositories as `'Source`s.

Installation
------------

Use [Nix](https://nixos.org/nix/).

Example
-------

```haskell
script :: Script 'Sources ()
script =
  namespace "svn" $ do

    svn (url "https://example.com/svn/project1/trunk" . path "project1") $ do
      ...

    svn (url "https://example.com/svn/project2/branches/1.0" . path "project2-1.0" . ignoreExternals) $ do
      ...
```
