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
 * Support for multiple audio drivers: Jack, ALSA, Pulseaudio, SDL, Stdout/SoX
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
    cabal configure -fjack -fexamples
    cabal build

    # start the jack server
    ./dist/build/example-jack/example-jack

    # if this does not work, you can use SoX to play the audio
    ./dist/build/example-stdout/example-stdout | play -t raw -b 16 -e signed -c 1 -r 48000 -
    ```

# Roadmap

 * **Building a nice community that invents and shares new instruments**
 * Support for Live-Programming (maybe with `StaticPtr`)
 * Making HSynth a great learning resource for digital audio synthesis
