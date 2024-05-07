package org.client.controllers;

import org.messages.central.*;

public class ServerOperations {

    public static Message register(String data) {

        String r = data.substring("/register".length());
        String[] split = r.split(" ");

        String username = split[1];
        String password = split[2];

        Register reg = Register.newBuilder().setUsername(username).setPassword(password).build();
        return Message.newBuilder().setType(Type.REGISTER).setRegister(reg).build();
    }

    public static Message login(String data) {

        String r = data.substring("/login".length());
        String[] split = r.split(" ");

        String username = split[1];
        String password = split[2];

        Login l = Login.newBuilder().setUsername(username).setPassword(password).build();
        return Message.newBuilder().setType(Type.LOGIN).setLogin(l).build();
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

        AlbumCreate a = AlbumCreate.newBuilder().setName(albumName).build();
        return Message.newBuilder().setType(Type.ALBUMCREATE).setAlbumCreate(a).build();
    }

    public static Message getAlbum(String data) {

        String r = data.substring("/getAlbum".length());
        String[] split = r.split(" ");

        String albumName = split[1];

        AlbumCreate a = AlbumCreate.newBuilder().setName(albumName).build();
        return Message.newBuilder().setType(Type.ALBUMGET).setAlbumCreate(a).build();
    }

}
