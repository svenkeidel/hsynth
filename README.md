```
   ___      ___     ___      ___     ___      ___
  /\__\    /\  \   /\__\    /\__\   /\  \    /\__\
 /:/__/_  /::\  \ |::L__L  /:| _|_  \:\  \  /:/__/_
/::\/\__\/\:\:\__\|:::\__\/::|/\__\ /::\__\/::\/\__\
\/\::/  /\:\:\/__//:;;/__/\/|::/  //:/\/__/\/\::/  /
  /:/  /  \::/  / \/__/     |:/  / \/__/     /:/  /
  \/__/    \/__/            \/__/            \/__/
```

HSynth is a purely functional library for audio synthesis. Its main features are

 * A arrow-based DSL for creating and composing audio signals
 * Optimization of audio signals with the
   [_causal commutative arrows_](http://cs-www.cs.yale.edu/c2/images/uploads/ICFP-CCA.pdf)
   technique [1]
 * Support for MIDI
 * Plotting of audio signals

# Getting Started

 * The requirements to run HSynth programs is the audio server
   [jack2](http://www.jackaudio.org/).

   ```
   git clone github.com/svenkeidel/hsynth2.git
   cd hsynth2

   cabal sandbox init
   cabal configure -fexamples -fjack
   cabal install

   # Start the jack audio server
   jack_control start

   # Start the audio example
   ./dist/build/example-jack/example-jack

   ```

# Roadmap

 * **Creating a nice community that invents and shares new instruments**
 * Making HSynth a great learning resource for digital audio synthesis

# References

[1]: Liu, Hai, Eric Cheng, and Paul Hudak. "Causal commutative arrows." Journal of Functional Programming 21.4-5 (2011): 467-496.