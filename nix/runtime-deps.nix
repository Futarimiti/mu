{
  pkgs ? import <nixpkgs> { },
  ...
}:
with pkgs;
[
  mpv
  yt-dlp
]
