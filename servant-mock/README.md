# Mocking with Servant

This example provides a translation from Servant's requests to WAI requests,
and from WAI responses to Servant's responses. It allows calling a
Servant-produced `Application` directly without seizing a port or doing any
networking at all.
