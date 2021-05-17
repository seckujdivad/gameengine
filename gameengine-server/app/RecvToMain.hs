module RecvToMain (RecvToMain (RecvToMain), RecvToMainMsg (Close, Message)) where

data RecvToMain = RecvToMain Integer RecvToMainMsg
data RecvToMainMsg = Close | Message String