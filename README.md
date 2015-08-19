```
    ___      ___     ___      ___     ___      ___
   /\__\    /\  \   /\__\    /\__\   /\  \    /\__\
  /:/__/_  /::\  \ |::L__L  /:| _|_  \:\  \  /:/__/_
 /::\/\__\/\:\:\__\|:::\__\/::|/\__\ /::\__\/::\/\__\
 \/\::/  /\:\:\/__//:;;/__/\/|::/  //:/\/__/\/\::/  /
   /:/  /  \::/  / \/__/     |:/  / \/__/     /:/  /
   \/__/    \/__/            \/__/            \/__/
```

HSynth is a purely functional audio synthesis library and music composition. Its
main features are

 * Lazy infinite streams to represent audio signals
 * Stream fusion
 * Support for multiple audio drivers: Jack, Stdout/SoX, ALSA, Pulseaudio, SDL
 * Support for MIDI
 * Plotting of audio signals
 * Support for multiple tuning systems: 12-tone equal temperament, pythagorean tuning

# Getting Started

 * Clone the repository and change into the directory.

    ```
    git clone github.com/svenkeidel/hsynth.git
    cd hsynth
    ```

 * Build the project inside a sandbox and listen

    ```
    cabal sandbox init
    cabal configure -fexamples
    cabal build

    # Install SoX and listen
    ./play-sox.sh ./dist/build/example-flute/example-flute

    # You can also play around with the parameters of the flute
    ./play-sox.sh ./dist/build/example-flute/example-flute \
        --breath 0.15 \
	--pressure 0.85 \
	--frequency 400
    ```

# Roadmap

 * **Building a nice community that invents and shares new instruments**
 * Support for Live-Programming (maybe with `StaticPtr`)
 * Making HSynth a great learning resource for digital audio synthesis
