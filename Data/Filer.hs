module Data.Filer
  where

data AudioFormat = AudioFormat Bool Bool Bool
    deriving Show

listAudioFormat :: AudioFormat -> [String]
listAudioFormat fmt = [name | (name, test) <- items, test fmt]
    where items =   [(".ogg", \(AudioFormat b _ _) -> b)
                    ,(".flac", \(AudioFormat _ b _) -> b)
                    ,(".mp3", \(AudioFormat _ _ b) -> b)
                    ]



