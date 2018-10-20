# Mocking with Servant

This example provides a translation from Servant's requests to WAI requests,
and from WAI responses to Servant's responses. It allows calling a
Servant-produced `Application` directly without seizing a port or doing any
networking at all.

* [Main.hs][] -- defining a simple mock-server and calling it.
* [Mock.hs][] -- request-translating machinery.

[Main.hs]: https://github.com/monadfix/examples/blob/master/servant-mock/src/Main.hs
[Mock.hs]: https://github.com/monadfix/examples/blob/master/servant-mock/src/Mock.hs
