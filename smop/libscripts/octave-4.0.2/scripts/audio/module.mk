FCN_FILE_DIRS += audio

audio_FCN_FILES = \
  audio/lin2mu.m \
  audio/mu2lin.m \
  audio/record.m \
  audio/sound.m \
  audio/soundsc.m \
  audio/wavread.m \
  audio/wavwrite.m \
  audio/@audioplayer/__get_properties__.m \
  audio/@audioplayer/audioplayer.m \
  audio/@audioplayer/display.m \
  audio/@audioplayer/get.m \
  audio/@audioplayer/isplaying.m \
  audio/@audioplayer/pause.m  \
  audio/@audioplayer/play.m \
  audio/@audioplayer/playblocking.m \
  audio/@audioplayer/resume.m \
  audio/@audioplayer/set.m \
  audio/@audioplayer/stop.m \
  audio/@audioplayer/subsasgn.m \
  audio/@audioplayer/subsref.m \
  audio/@audiorecorder/__get_properties__.m \
  audio/@audiorecorder/audiorecorder.m \
  audio/@audiorecorder/display.m \
  audio/@audiorecorder/get.m \
  audio/@audiorecorder/getaudiodata.m \
  audio/@audiorecorder/getplayer.m \
  audio/@audiorecorder/isrecording.m \
  audio/@audiorecorder/pause.m \
  audio/@audiorecorder/play.m \
  audio/@audiorecorder/record.m \
  audio/@audiorecorder/recordblocking.m \
  audio/@audiorecorder/resume.m \
  audio/@audiorecorder/set.m \
  audio/@audiorecorder/stop.m \
  audio/@audiorecorder/subsasgn.m \
  audio/@audiorecorder/subsref.m

FCN_FILES += $(audio_FCN_FILES)

PKG_ADD_FILES += audio/PKG_ADD

DIRSTAMP_FILES += audio/$(octave_dirstamp)
