//package org.client.threads;
//
//import org.zeromq.SocketType;
//import org.zeromq.ZContext;
//import org.zeromq.ZMQ;
//
//public class Server {
//    public static void main(String[] args) {
//        try (ZContext context = new ZContext();
//             ZMQ.Socket pull_message = context.createSocket(SocketType.PULL);
//             ZMQ.Socket pub_message = context.createSocket(SocketType.PUB))
//        {
//            pull_message.bind("tcp://localhost:5002");
//            pub_message.bind("tcp://localhost:5003");
//
//            while (true) {
//                byte[] msg = pull_message.recv();
//                String msg_received = new String(msg);
//                System.out.println("Received: " + msg_received);
//                pub_message.send(msg_received);
//            }
//        }
//    }
//
//}
