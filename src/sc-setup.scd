(
s.options.maxLogins = 2;
o = Server.default.options;
o.inDevice_("ASIO : MOTU Pro Audio");
o.outDevice_("ASIO : MOTU Pro Audio");
s.boot;
)

a = {SinOsc.ar()}
qa.play