/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/NTLM.java,v 1.12 2003/02/11 03:41:14 jsdever Exp $
 * $Revision: 1.12 $
 * $Date: 2003-02-11 04:41:14 +0100 (Tue, 11 Feb 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

import java.io.UnsupportedEncodingException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.Security;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.httpclient.util.Base64;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Provides an implementation of the NTLM authentication protocol.
 * <p>
 * This class provides methods for generating authentication
 * challenge responses for the NTLM authentication protocol.  The NTLM
 * protocol is a proprietary Microsoft protocol and as such no RFC
 * exists for it.  This class is based upon the reverse engineering
 * efforts of a wide range of people.</p>
 *
 * @deprecated this class will be made package access for 2.0beta2
 *
 * @author <a href="mailto:adrian@ephox.com">Adrian Sutton</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 *
 * @version $Revision: 1.12 $ $Date: 2003-02-11 04:41:14 +0100 (Tue, 11 Feb 2003) $
 * @since 2.0alpha2
 */
public final class NTLM {

    /** The current response */
    private byte[] currentResponse;

    /** The current position */
    private int currentPosition = 0;

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(NTLM.class);

    /** Character encoding */
    public static final String DEFAULT_CHARSET = "ASCII";

    //Initialize the security provider
    static {
        //TODO: do not use System properties
        final String secProviderName 
            = System.getProperty("httpclient.security.provider",
                "com.sun.crypto.provider.SunJCE");
        try {
            java.security.Provider secProvider = (java.security.Provider)
                Class.forName(secProviderName).newInstance();
            Security.addProvider(secProvider);
        } catch (ClassNotFoundException e) {
            LOG.error("Specified security provider " + secProviderName 
                + " could not be found by the class loader", e);
        } catch (ClassCastException e) {
            LOG.error("Specified security provider " + secProviderName 
                + " is not of type java.security.Provider", e);
        } catch (InstantiationException e) {
            LOG.error("Specified security provider " + secProviderName 
                + " could not be instantiated", e);
        } catch (IllegalAccessException e) {
            LOG.error("Specified security provider " + secProviderName 
                + " does not allow access to the constructor", e);
        }
    }

    /**
     * Returns the response for the given message.
     *
     * @param message the message that was received from the server.
     * @param username the username to authenticate with.
     * @param password the password to authenticate with.
     * @param host The host.
     * @param domain the NT domain to authenticate in.
     * @return The response.
     * @throws HttpException If the messages cannot be retrieved.
     */
    public final String getResponseFor(String message,
            String username, String password, String host, String domain)
            throws HttpException {
                
        final String response;
        if (message == null || message.trim().equals("")) {
            response = getType1Message(host, domain);
        } else {
            response = getType3Message(username, password, host, domain,
                    parseType2Message(message));
        }
        return response;
    }

    /**
     * Return the cipher for the specified key.
     * @param key The key.
     * @return Cipher The cipher.
     * @throws HttpException If the cipher cannot be retrieved.
     */
    private Cipher getCipher(byte[] key) throws HttpException {
        try {
            final Cipher ecipher = Cipher.getInstance("DES/ECB/NoPadding");
            key = setupKey(key);
            ecipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "DES"));
            return ecipher;
        } catch (NoSuchAlgorithmException e) {
            throw new HttpException("DES encryption is not available.");
        } catch (InvalidKeyException e) {
            throw new HttpException("Invalid key for DES encryption.");
        } catch (NoSuchPaddingException e) {
            throw new HttpException(
                "NoPadding option for DES is not available.");
        }
    }

    /** 
     * Adds parity bits to the key.
     * @param key56 The key
     * @return The modified key.
     */
    private byte[] setupKey(byte[] key56) {
        byte[] key = new byte[8];
        key[0] = (byte) ((key56[0] >> 1) & 0xff);
        key[1] = (byte) ((((key56[0] & 0x01) << 6) 
            | (((key56[1] & 0xff) >> 2) & 0xff)) & 0xff);
        key[2] = (byte) ((((key56[1] & 0x03) << 5) 
            | (((key56[2] & 0xff) >> 3) & 0xff)) & 0xff);
        key[3] = (byte) ((((key56[2] & 0x07) << 4) 
            | (((key56[3] & 0xff) >> 4) & 0xff)) & 0xff);
        key[4] = (byte) ((((key56[3] & 0x0f) << 3) 
            | (((key56[4] & 0xff) >> 5) & 0xff)) & 0xff);
        key[5] = (byte) ((((key56[4] & 0x1f) << 2) 
            | (((key56[5] & 0xff) >> 6) & 0xff)) & 0xff);
        key[6] = (byte) ((((key56[5] & 0x3f) << 1) 
            | (((key56[6] & 0xff) >> 7) & 0xff)) & 0xff);
        key[7] = (byte) (key56[6] & 0x7f);
        
        for (int i = 0; i < key.length; i++) {
            key[i] = (byte) (key[i] << 1);
        }
        return key;
    }

    /**
     * Encrypt the data.
     * @param key The key.
     * @param bytes The data
     * @return byte[] The encrypted data
     * @throws HttpException If {@link Cipher.doFinal(byte[])} fails
     */
    private byte[] encrypt(byte[] key, byte[] bytes)
        throws HttpException {
        Cipher ecipher = getCipher(key);
        try {
            byte[] enc = ecipher.doFinal(bytes);
            return enc;
        } catch (IllegalBlockSizeException e) {
            throw new HttpException("Invalid block size for DES encryption.");
        } catch (BadPaddingException e) {
            throw new HttpException(
                    "Data not padded correctly for DES encryption.");
        }
    }

    /** 
     * Prepares the object to create a response of the given length.
     * @param length the length of the response to prepare.
     */
    private void prepareResponse(int length) {
        currentResponse = new byte[length];
        currentPosition = 0;
    }

    /** 
     * Adds the given byte to the response.
     * @param b the byte to add.
     */
    private void addByte(byte b) {
        currentResponse[currentPosition] = b;
        currentPosition++;
    }

    /** 
     * Adds the given bytes to the response.
     * @param bytes the bytes to add.
     */
    private void addBytes(byte[] bytes) {
        for (int i = 0; i < bytes.length; i++) {
            currentResponse[currentPosition] = bytes[i];
            currentPosition++;
        }
    }

    /** 
     * Returns the response that has been generated after shrinking the array if
     * required and base64 encodes the response.
     * @return The response as above.
     */
    private String getResponse() {
        byte[] resp;
        if (currentResponse.length > currentPosition) {
            byte[] tmp = new byte[currentPosition];
            for (int i = 0; i < currentPosition; i++) {
                tmp[i] = currentResponse[i];
            }
            resp = tmp;
        } else {
            resp = currentResponse;
        }
        return HttpConstants.getString(Base64.encode(resp));
    }
    
    /**
     * TODO: Figure out what this method really does.
     * @param host The host
     * @param domain The domain
     * @return String
     */
    private String getType1Message(String host, String domain) {
        host = host.toUpperCase();
        domain = domain.toUpperCase();
        byte[] hostBytes = getBytes(host);
        byte[] domainBytes = getBytes(domain);

        int finalLength = 32 + hostBytes.length + domainBytes.length;
        prepareResponse(finalLength);
        
        // The initial id string.
        byte[] protocol = getBytes("NTLMSSP");
        addBytes(protocol);
        addByte((byte) 0);

        // Type
        addByte((byte) 1);
        addByte((byte) 0);
        addByte((byte) 0);
        addByte((byte) 0);

        // Flags
        addByte((byte) 6);
        addByte((byte) 82);
        addByte((byte) 0);
        addByte((byte) 0);

        // Domain length (first time).
        int iDomLen = domainBytes.length;
        byte[] domLen = convertShort(iDomLen);
        addByte(domLen[0]);
        addByte(domLen[1]);

        // Domain length (second time).
        addByte(domLen[0]);
        addByte(domLen[1]);

        // Domain offset.
        byte[] domOff = convertShort(hostBytes.length + 32);
        addByte(domOff[0]);
        addByte(domOff[1]);
        addByte((byte) 0);
        addByte((byte) 0);

        // Host length (first time).
        byte[] hostLen = convertShort(hostBytes.length);
        addByte(hostLen[0]);
        addByte(hostLen[1]);

        // Host length (second time).
        addByte(hostLen[0]);
        addByte(hostLen[1]);

        // Host offset (always 32).
        byte[] hostOff = convertShort(32);
        addByte(hostOff[0]);
        addByte(hostOff[1]);
        addByte((byte) 0);
        addByte((byte) 0);

        // Host String.
        addBytes(hostBytes);

        // Domain String.
        addBytes(domainBytes);

        return getResponse();
    }

    /** 
     * Extracts the server nonce out of the given message type 2.
     * 
     * @param message the String containing the base64 encoded message.
     * @return an array of 8 bytes that the server sent to be used when
     * hashing the password.
     */
    private byte[] parseType2Message(String message) {
        // Decode the message first.
        byte[] msg = Base64.decode(getBytes(message));
        byte[] nonce = new byte[8];
        // The nonce is the 8 bytes starting from the byte in position 24.
        for (int i = 0; i < 8; i++) {
            nonce[i] = msg[i + 24];
        }
        return nonce;
    }

    /** 
     * Creates the type 3 message using the given server nonce.
     * @param user The user.
     * @param password The password.
     * @param host The host.
     * @param domain The domain.
     * @param nonce the 8 byte array the server sent.
     * @return The type 3 message.
     * @throws HttpException If {@encrypt(byte[],byte[])} fails.
     */
    private String getType3Message(String user, String password,
            String host, String domain, byte[] nonce)
    throws HttpException {

        int ntRespLen = 0;
        int lmRespLen = 24;
        domain = domain.toUpperCase();
        host = host.toUpperCase();
        user = user.toUpperCase();
        byte[] domainBytes = getBytes(domain);
        byte[] hostBytes = getBytes(host);
        byte[] userBytes = getBytes(user);
        int domainLen = domainBytes.length;
        int hostLen = hostBytes.length;
        int userLen = userBytes.length;
        int finalLength = 64 + ntRespLen + lmRespLen + domainLen 
            + userLen + hostLen;
        prepareResponse(finalLength);
        byte[] ntlmssp = getBytes("NTLMSSP");
        addBytes(ntlmssp);
        addByte((byte) 0);
        addByte((byte) 3);
        addByte((byte) 0);
        addByte((byte) 0);
        addByte((byte) 0);

        // LM Resp Length (twice)
        addBytes(convertShort(24));
        addBytes(convertShort(24));

        // LM Resp Offset
        addBytes(convertShort(finalLength - 24));
        addByte((byte) 0);
        addByte((byte) 0);

        // NT Resp Length (twice)
        addBytes(convertShort(0));
        addBytes(convertShort(0));

        // NT Resp Offset
        addBytes(convertShort(finalLength));
        addByte((byte) 0);
        addByte((byte) 0);

        // Domain length (twice)
        addBytes(convertShort(domainLen));
        addBytes(convertShort(domainLen));
        
        // Domain offset.
        addBytes(convertShort(64));
        addByte((byte) 0);
        addByte((byte) 0);

        // User Length (twice)
        addBytes(convertShort(userLen));
        addBytes(convertShort(userLen));

        // User offset
        addBytes(convertShort(64 + domainLen));
        addByte((byte) 0);
        addByte((byte) 0);

        // Host length (twice)
        addBytes(convertShort(hostLen));
        addBytes(convertShort(hostLen));

        // Host offset
        addBytes(convertShort(64 + domainLen + userLen));

        for (int i = 0; i < 6; i++) {
            addByte((byte) 0);
        }

        // Message length
        addBytes(convertShort(finalLength));
        addByte((byte) 0);
        addByte((byte) 0);

        // Flags
        addByte((byte) 6);
        addByte((byte) 82);
        addByte((byte) 0);
        addByte((byte) 0);

        addBytes(domainBytes);
        addBytes(userBytes);
        addBytes(hostBytes);
        addBytes(hashPassword(password, nonce));
        return getResponse();
    }

    /** 
     * Creates the LANManager and NT response for the given password using the
     * given nonce.
     * @param password the password to create a hash for.
     * @param nonce the nonce sent by the server.
     * @return The response.
     * @throws HttpException If {@link #encrypt(byte[],byte[])} fails.
     */
    private byte[] hashPassword(String password, byte[] nonce)
        throws HttpException {
        byte[] passw = getBytes(password.toUpperCase());
        byte[] lmPw1 = new byte[7];
        byte[] lmPw2 = new byte[7];

        int len = passw.length;
        if (len > 7) {
            len = 7;
        }

        int idx;
        for (idx = 0; idx < len; idx++) {
            lmPw1[idx] = passw[idx];
        }
        for (; idx < 7; idx++) {
            lmPw1[idx] = (byte) 0;
        }

        len = passw.length;
        if (len > 14) {
            len = 14;
        }
        for (idx = 7; idx < len; idx++) {
            lmPw2[idx - 7] = passw[idx];
        }
        for (; idx < 14; idx++) {
            lmPw2[idx - 7] = (byte) 0;
        }

        // Create LanManager hashed Password
        byte[] magic = {
            (byte) 0x4B, (byte) 0x47, (byte) 0x53, (byte) 0x21, 
            (byte) 0x40, (byte) 0x23, (byte) 0x24, (byte) 0x25
        };

        byte[] lmHpw1;
        lmHpw1 = encrypt(lmPw1, magic);

        byte[] lmHpw2 = encrypt(lmPw2, magic);

        byte[] lmHpw = new byte[21];
        for (int i = 0; i < lmHpw1.length; i++) {
            lmHpw[i] = lmHpw1[i];
        }
        for (int i = 0; i < lmHpw2.length; i++) {
            lmHpw[i + 8] = lmHpw2[i];
        }
        for (int i = 0; i < 5; i++) {
            lmHpw[i + 16] = (byte) 0;
        }

        // Create the responses.
        byte[] lmResp = new byte[24];
        calcResp(lmHpw, nonce, lmResp);

        return lmResp;
    }

    /** 
     * Takes a 21 byte array and treats it as 3 56-bit DES keys.  The 8 byte
     * plaintext is encrypted with each key and the resulting 24 bytes are
     * stored in the results array.
     * 
     * @param keys The keys.
     * @param plaintext The plain text to encrypt.
     * @param results Where the results are stored.
     * @throws HttpException If {@link #encrypt(byte[],byte[])} fails.
     */
    private void calcResp(byte[] keys, byte[] plaintext, byte[] results)
        throws HttpException {
        byte[] keys1 = new byte[7];
        byte[] keys2 = new byte[7];
        byte[] keys3 = new byte[7];
        for (int i = 0; i < 7; i++) {
            keys1[i] = keys[i];
        }

        for (int i = 0; i < 7; i++) {
            keys2[i] = keys[i + 7];
        }

        for (int i = 0; i < 7; i++) {
            keys3[i] = keys[i + 14];
        }
        byte[] results1 = encrypt(keys1, plaintext);

        byte[] results2 = encrypt(keys2, plaintext);

        byte[] results3 = encrypt(keys3, plaintext);

        for (int i = 0; i < 8; i++) {
            results[i] = results1[i];
        }
        for (int i = 0; i < 8; i++) {
            results[i + 8] = results2[i];
        }
        for (int i = 0; i < 8; i++) {
            results[i + 16] = results3[i];
        }
    }

    /** 
     * Converts a given number to a two byte array in little endian order.
     * @param num the number to convert.
     * @return The new array.
     */
    private byte[] convertShort(int num) {
        byte[] val = new byte[2];
        String hex = Integer.toString(num, 16);
        while (hex.length() < 4) {
            hex = "0" + hex;
        }
        String low = hex.substring(2, 4);
        String high = hex.substring(0, 2);

        val[0] = (byte) Integer.parseInt(low, 16);
        val[1] = (byte) Integer.parseInt(high, 16);
        return val;
    }
    
    /**
     * Convert a string to a byte array.
     * @param s The string
     * @return byte[] The resulting byte array.
     */
    private static byte[] getBytes(final String s) {
        if (s == null) {
            throw new IllegalArgumentException("Parameter may not be null"); 
        }
        try {
            return s.getBytes(DEFAULT_CHARSET); 
        } catch (UnsupportedEncodingException unexpectedEncodingException) {
            throw new RuntimeException("NTLM requires ASCII support"); 
        }
    }
}
