namespace NetWolf
{
    class Py
    {
        public static readonly string Name = "wlserver.py";
        public static readonly string Code = @"import socket
import _thread
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl, wlexpr


def on_new_client(clientsocket, addr):
    while True:
        try:
            msg = clientsocket.recv(1024)
            #print(addr, ' >> ', msg)
            result = session.evaluate(wlexpr(msg))
            clientsocket.send(str.encode(str(result)))
        except:
            print('Close connection from', addr)
            clientsocket.close()
            break


s = socket.socket()
host = socket.gethostname()
port = 1642

print('Server started!')
print('Waiting for clients...')

s.bind((host, port))
s.listen(5)
print('Starting WolframLanguageSession...')
session = WolframLanguageSession()
session.evaluate(wlexpr('0'))
print('WolframLanguageSession() ok')

try:
    while True:
        c, addr = s.accept()
        print('Got connection from', addr)
        _thread.start_new_thread(on_new_client, (c, addr))
except:
    s.close()
    session.terminate()
";
    }
}