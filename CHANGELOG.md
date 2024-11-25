Unreleased
==========

1.3.0
=======

- Add an `HasEndpoint` instance for `RawM`
- Add an `HasEndpoint` instance for `WithResource`
- Fix `HasEndpoint` instance for `CaptureAll`
  - Previously paths would never be matched since the instance
  did not consume the rest of the path like `CaptureAll` does.
  The rest of the path is now captured and replaced with a `*`
  place holder and this is also the case for enumerating the endpoint.

1.2.0
=======

- Add an `HasEndpoint` instance for `AuthProtect`

1.1.0
=======

- Support GHC 9.4

1.0.0
=======

Initial release.
