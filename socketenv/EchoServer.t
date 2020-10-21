module EchoServer where

import POSIX

port = Port 12345

root w = class
    env = new posix w
    result action
      env.inet.tcp.listen port (server env.stdout)

server logfile sock = class

   n := 1

   p = show sock.remoteHost

   log str = logfile.write ('[':str ++ "]\n")

   echo str = action
      sock.outFile.write (show n ++"> "++str)
      n := n+1

   close = request
      log (p ++ " closing")
      result ()

   neterror str = action log ("Neterror: "++str)

   established = action
      log ("Connected from " ++ p)
      sock.inFile.installR echo

   result Connection{..}