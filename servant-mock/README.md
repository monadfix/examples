# Network-less mocking with Servant

This example provides a translation from Servant's requests to WAI requests,
and from WAI responses to Servant's responses. It allows calling a
Servant-produced `Application` directly _without seizing a port or doing any
networking at all._

* [Main.hs][] -- defining a simple mock-server and calling it.
* [Mock.hs][] -- request-translating machinery.

[Main.hs]: https://github.com/monadfix/examples/blob/master/servant-mock/src/Main.hs
[Mock.hs]: https://github.com/monadfix/examples/blob/master/servant-mock/src/Mock.hs

## Limitations

* This is a proof-of-concept: streaming is not supported yet.

* Cookies are not handled faithfully (see servant-client's [`performRequest`][]).

[`performRequest`]: https://hackage.haskell.org/package/servant-client-0.14/docs/src/Servant.Client.Internal.HttpClient.html#performRequest
