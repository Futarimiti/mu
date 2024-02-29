{ cannotInferDefaultPlayerByOS = \(os : Text) -> "Cannot infer a default music player for your OS: ${os}"
, cannotInferMusicDir = "Cannot infer a default directory to store audios"
, mustManuallySetDownloader = "Cannot infer a default downloader, you must set it manually"
, notSupportedUpdateFileFormat = \(format : Text) -> "INTERNAL ERROR: update file format \"${format}\" is not supported"
, beginDownload = \(songname : Text) -> \(url : Text) -> "Downloading song \"${songname}\" from ${url}..."
, beginDelete = \(songname : Text) -> "Deleting song \"${songname}\"..."
, beginReinstall = \(songname : Text) -> \(url : Text) -> "Reinstalling song \"${songname}\" from ${url}..."
, illegalEmptyCommand = "Illegal empty list provided as command"
, songNotExist = \(songname : Text) -> "No such song: ${songname}"
, currentPlaying = \(songname : Text) -> "-> ${songname}"
}

