(
s.options.maxLogins = 2;
o = Server.default.options;
o.inDevice_("ASIO : MOTU Pro Audio");
o.outDevice_("ASIO : MOTU Pro Audio");
Server.local.options.numOutputBusChannels = 4;    // change number of input and output channels
Server.local.options.numInputBusChannels = 4;
Server.internal.options.numOutputBusChannels = 4;
Server.internal.options.numInputBusChannels = 4;
s.boot;
)

a = {SinOsc.ar(E)}
a.play


(
{
    PanAz.ar(
        numChans: 4,
        in: ClipNoise.ar(0.1),
        pos: LFSaw.kr(MouseX.kr(0.2, 8, 'exponential')),
        level: 0.5,
        width: 3,
        orientation: 0.5
    );
}.scope
)