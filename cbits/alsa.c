#include<stdio.h>
#include<alsa/asoundlib.h>

snd_rawmidi_t *open_rawmidi(const char *device) {

  snd_rawmidi_t *input;

  if (snd_rawmidi_open(&input, 0, device, 0) < 0) {
    fprintf(stderr, "Error opening ALSA midi device.\n");
    exit(1);
  }
  return input;
}

void close_rawmidi(snd_rawmidi_t *handle) {
  if(snd_rawmidi_close(handle) < 0) {
    fprintf(stderr, "Error closing ALSA sequencer.\n");
    exit(1);
  }
}
