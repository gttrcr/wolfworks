import socket
from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl, wlexpr


def server_program():
    print("Starting WolframLanguageSession...")
    session = WolframLanguageSession()
    session.evaluate(wlexpr('0'))
    print("WolframLanguageSession() ok")
    while True:
        try:
            host = socket.gethostname()
            port = 5000
            server_socket = socket.socket()
            server_socket.bind((host, port))
            print("gethostname() and bind() ok")
            server_socket.listen(1)
            conn, address = server_socket.accept()
            print("Connection from: " + str(address))
            i = 0
            while True:
                i += 1
                data = conn.recv(1024).decode()
                if not data:
                    continue
                received = str(data)
                #print("from connected user: " + received)
                result = session.evaluate(wlexpr(received))
                # print(result)
                conn.send(str.encode(str(result)))
                #print(i)
        except KeyboardInterrupt:
            conn.close()
            session.terminate()
        except:
            print("Disconnection from: " + str(address))
            conn.close()


if __name__ == '__main__':
    server_program()
