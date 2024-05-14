package org.client.controllers.server;

import org.client.crdts.Album;
import org.messages.central.*;

public class ServerOperations {

    public static Message getWriteAddress(String hash) {
        // todo: token is supposed to be a string not a uint64
        return Message.newBuilder().setType(Type.WRITE).setToken(0).build();
    }

    public static Message getReadAddress(String hash) {
        // todo: token is supposed to be a string not a uint64
        return Message.newBuilder().setType(Type.READ).setToken(0).build();
    }

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

        return Message.newBuilder().setType(Type.ALBUMEDIT).setAlbumName(albumName).build();
    }

    public static Message leaveAlbum(String data, int clock, int position, Album album){
        String r = data.substring("/leaveAlbum".length());                 // todo: dangling album ?
        String[] split = r.split(" ");

        String albumName = split[1];

        LeaveData lData = LeaveData.newBuilder()
                .setClock(clock)
                .setPosition(position)
                .build();

        return Message.newBuilder()
                .setType(Type.LEAVE)
                .setAlbumName(albumName)
                .setLeaveData(lData)
                .setAlbum(Album.getInstance().toAlbumMessage())
                .build();
    }

}
