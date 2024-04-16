import messages_pb2 as Messages
import socket

def register(user,pas):
    register = Messages.Register()
    register.username = user
    register.password = pas

    messageRegister = Messages.Message()
    messageRegister.type = Messages.Type.REGISTER
    messageRegister.register.CopyFrom(register)
    return messageRegister

def login(user,pas):
    login = Messages.Login()
    login.username = user
    login.password = pas

    messageLogin = Messages.Message()
    messageLogin.type = Messages.Type.LOGIN
    messageLogin.login.CopyFrom(login)
    return messageLogin

def createAlbum(name):
    albumCreate = Messages.AlbumCreate()
    albumCreate.name = name

    messageAlbumCreate = Messages.Message()
    messageAlbumCreate.type = Messages.Type.ALBUMCREATE
    messageAlbumCreate.albumCreate.CopyFrom(albumCreate)
    return messageAlbumCreate

def logout():
    messageLogOut = Messages.Message()
    messageLogOut.type = Messages.Type.LOGOUT
    return messageLogOut

def listAlbums():
    messageAlbumList = Messages.Message()
    messageAlbumList.type = Messages.Type.ALBUMSLIST
    return messageAlbumList

def getAlbum(name):
    message = Messages.Message()
    message.type = Messages.Type.ALBUMGET
    message.albumGet.name = name
    return message

def startConnecting(ip,port,tokens):
    nodeInfo = Messages.NodeInfo()
    nodeInfo.ip = ip
    nodeInfo.port = port
    nodeInfo.tokens.extend(tokens)
    
    message = Messages.Message()
    message.type = Messages.Type.STARTENTRANCE
    message.nodeInfo.CopyFrom(nodeInfo)
    return message

def endConnecting(ip,port,tokens):
    nodeInfo = Messages.NodeInfo()
    nodeInfo.ip = ip
    nodeInfo.port = port
    nodeInfo.tokens.extend(tokens)
    
    message = Messages.Message()
    message.type = Messages.Type.ENDENTRANCE
    message.nodeInfo.CopyFrom(nodeInfo)
    return message

def read(token):    
    message = Messages.Message()
    message.type = Messages.Type.READ
    message.token = token
    return message

def write(token):    
    message = Messages.Message()
    message.type = Messages.Type.WRITE
    message.token = token
    return message

# Define the address and port to connect to
server_address = ('localhost', 4321)

def sendAndRecieve(sock,msg):
    sock.sendall(msg.SerializeToString())
        
    # Wait for a response (you might want to handle the response here)
    response = sock.recv(1024)
    m = Messages.Message()
    m.ParseFromString(response)
    print(m)


try:
    # Create a TCP socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        # Connect to the server
        sock.connect(server_address)
        
        #sendAndRecieve(sock,register("123","123"))
        #sendAndRecieve(sock,login("123","123"))
        #sendAndRecieve(sock,createAlbum("F1"))
        #sendAndRecieve(sock,listAlbums())
        #sendAndRecieve(sock,getAlbum("F1"))
        #sendAndRecieve(sock,logout())
        
        print("startConnect")
        sendAndRecieve(sock,startConnecting("localhost","8888",["136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77","442ed285407b635427559a3f1f1695b260b46ded"]))
        
        print("read")
        sendAndRecieve(sock,read("e1b40557d67bb000f7bd9e0e2c760687540073be"))
        
        print("write")
        sendAndRecieve(sock,write("e1b40557d67bb000f7bd9e0e2c760687540073be"))
        
        print("endConnect")
        sendAndRecieve(sock,endConnecting("localhost","8888",["136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77","442ed285407b635427559a3f1f1695b260b46ded"]))
        
        print("read")
        sendAndRecieve(sock,read("e1b40557d67bb000f7bd9e0e2c760687540073be"))
        
        print("write")
        sendAndRecieve(sock,write("e1b40557d67bb000f7bd9e0e2c760687540073be"))
        
except ConnectionRefusedError:
    print("Connection refused. Make sure the server is running.")
except Exception as e:
    print("An error occurred:", e)