package org.client.controllers;

import org.client.crdts.Album;
import org.messages.central.*;

public class ServerOperations {

    public static Message register(String data) {

        String r = data.substring("/register".length());
        String[] split = r.split(" ");

        String username = split[1];
        String password = split[2];

        UserData reg = UserData.newBuilder().setUsername(username).setPassword(password).build();
        return Message.newBuilder().setType(Type.REGISTER).setUserData(reg).build();
    }

    public static Message login(String data, String clientIp, int clientPort) {

        String r = data.substring("/login".length());
        String[] split = r.split(" ");

        String username = split[1];
        String password = split[2];

        UserData l = UserData.newBuilder().setUsername(username).setPassword(password).build();
        Address a = Address.newBuilder().setPort(clientPort).setIp(clientIp).build();
        return Message.newBuilder().setType(Type.LOGIN).setUserData(l).setAddress(a).build();
    }

    public static Message logout(String data) {
        return Message.newBuilder().setType(Type.LOGOUT).build();
    }

    public static Message listAlbums(String data) {
        return Message.newBuilder().setType(Type.ALBUMSLIST).build();
    }

    public static Message createAlbum(String data) {

        String r = data.substring("/createAlbum".length());
        String[] split = r.split(" ");

        String albumName = split[1];

        return Message.newBuilder().setType(Type.ALBUMCREATE).setAlbumName(albumName).build();
    }

    /*public static Message getAlbum(String data) {

        String r = data.substring("/getAlbum".length());
        String[] split = r.split(" ");

        String albumName = split[1];

        return Message.newBuilder().setType(Type.ALBUMGET).setAlbumName(albumName).build();
    }*/

    public static Message editAlbum(String data){
        String r = data.substring("/editAlbum".length());
        String[] split = r.split(" ");

        String albumName = split[1];

        Message m = Message.newBuilder().setType(Type.ALBUMEDIT).setAlbumName(albumName).build();
        return m;
    }

    public static Message leaveAlbum(String data, int clock, int position, Album album){
        String r = data.substring("/leaveAlbum".length());
        String[] split = r.split(" ");

        String albumName = split[1];

        LeaveData lD = LeaveData.newBuilder()
                .setClock(clock+2)
                .setPosition(position)
                .build();

        Message m = Message.newBuilder()
                .setType(Type.LEAVE)
                .setAlbumName(albumName)
                .setLeaveData(lD)
                .setAlbum(Album.getInstance().toAlbumMessage())
                .build();

        return m;
    }

}
