import messages_pb2 as Messages
import socket


def getSimpleMessage(type):
    message = Messages.Message()
    message.type = type
    return message


def register(user,pas):
    data = Messages.UserData()
    data.username = user
    data.password = pas

    message = getSimpleMessage(Messages.Type.REGISTER)
    message.user_data.CopyFrom(data)
    return message

def login(user,pas):
    data = Messages.UserData()
    data.username = user
    data.password = pas

    message = getSimpleMessage(Messages.Type.LOGIN)
    message.user_data.CopyFrom(data)
    return message

def logout():
    return getSimpleMessage(Messages.Type.LOGOUT)

def album_list():
    return getSimpleMessage(Messages.Type.ALBUMSLIST)

def album_create(name):
    message = getSimpleMessage(Messages.Type.ALBUMCREATE)
    message.album_name = name
    return message

def album_edit(name):
    message = getSimpleMessage(Messages.Type.ALBUMEDIT)
    message.album_name = name
    return message


def nodeInfo(ip,port,tokens):
    node_info = Messages.NodeInfo()
    node_info.ip = ip
    node_info.port = port
    node_info.tokens.extend(tokens)
    return node_info

def startConnecting(ip,port,tokens):
    message = getSimpleMessage(Messages.Type.STARTENTRANCE)
    message.node_info.CopyFrom(nodeInfo(ip,port,tokens))
    return message

def endConnecting(ip,port,tokens):
    message = getSimpleMessage(Messages.Type.ENDENTRANCE)
    message.node_info.CopyFrom(nodeInfo(ip,port,tokens))
    return message

def read(token):    
    message = getSimpleMessage(Messages.Type.READ)
    message.token = token
    return message

def write(token):    
    message = getSimpleMessage(Messages.Type.WRITE)
    message.token = token
    return message


def classification(user,grade):
    classification = Messages.Classification()
    classification.username = user
    classification.value = grade
    return classification

def file(name,hash,classifications):
    file = Messages.File()
    file.name = name
    file.hash = hash
    c=[]
    for u,g in classifications:
        c.append(classification(u,g))
    file.classifications.extend(c)
    return file

def leave(name,clock,position,files,users):
    album = Messages.Album()
    album.users.extend(users)
    f=[]
    for n,h,c in files:
        f.append(file(n,h,c))
    album.files.extend(f)
    
    data = Messages.LeaveData()
    data.clock = clock
    data.position = position
    
    message = getSimpleMessage(Messages.Type.LEAVE)
    message.leave_data.CopyFrom(data)
    message.album_name = name
    message.album.CopyFrom(album)
    return message


def toHash(str):
    return int(str, 16) % (2**32)

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
        
        print("register")
        sendAndRecieve(sock,register("123","123"))
        print("register")
        sendAndRecieve(sock,register("MAX","VERSTAPPEN"))
        print("login")
        sendAndRecieve(sock,login("123","123"))
        print("album_create")
        sendAndRecieve(sock,album_create("F1"))
        print("album_list")
        sendAndRecieve(sock,album_list())
        print("album_edit")
        sendAndRecieve(sock,album_edit("F1"))
        print("leave")
        sendAndRecieve(sock,leave("F1",10,0,[("perdi",toHash("136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77"),[("123",10)]),("perdi2",toHash("136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77"),[("123",10)])],["123","MAX"]))
        print("logout")
        sendAndRecieve(sock,logout())
        print("login")
        sendAndRecieve(sock,login("MAX","VERSTAPPEN"))
        print("album_list")
        sendAndRecieve(sock,album_list())
        print("album_edit")
        sendAndRecieve(sock,album_edit("F1"))
        
        #print("startConnect")
        #sendAndRecieve(sock,startConnecting("localhost",8888,[toHash("136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77"),toHash("442ed285407b635427559a3f1f1695b260b46ded")]))
        #
        #print("endConnect")
        #sendAndRecieve(sock,endConnecting("localhost",8888,[toHash("136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77"),toHash("442ed285407b635427559a3f1f1695b260b46ded")]))
        #
        #print("startConnect")
        #sendAndRecieve(sock,startConnecting("localhost",7777,[toHash("136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77"),toHash("442ed285407b635427559a3f1f1695b260b46ded")]))
        #
        #print("endConnect")
        #sendAndRecieve(sock,endConnecting("localhost",7777,[toHash("136e3cf1ab6a4dc9cd25784ffe7ab05af45d9f77"),toHash("442ed285407b635427559a3f1f1695b260b46ded")]))
        
except ConnectionRefusedError:
    print("Connection refused. Make sure the server is running.")
except Exception as e:
    print("An error occurred:", e)