package org.apache.commons.httpclient;

import java.io.IOException;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * A utility class for parsing http header values.
 * 
 * @author Michael Becke
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @since 2.0beta1
 */
public class HttpParser {

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HttpParser.class);
    
    /** Log for any wire messages. */
    private static final Log WIRE_LOG = LogFactory.getLog("httpclient.wire");
    /**
     * Constructor for HttpParser.
     */
    private HttpParser() {}

    /**
     * Return byte array from an (unchunked) input stream.
     * Stop reading when <tt>"\r\n"</tt> terminator encountered 
     * If the stream ends before the line terminator is found,
     * the last part of the string will still be returned.
     * '\r' and '\n' are allowed to appear individually in the stream.
     *
     * @param inputStream the stream to read from
     *
     * @throws IOException if an I/O problem occurs
     * @return a byte array from the stream
     */
    public static byte[] readRawLine(InputStream inputStream) throws IOException {
        LOG.trace("enter HttpConnection.readRawLine()");

        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        int ch;
        while ((ch = inputStream.read()) >= 0) {
            buf.write(ch);
            if (ch == '\r') {
                ch = inputStream.read();
                if (ch < 0) {
                    break;
                }
                buf.write(ch);
                if (ch == '\n') {
                    break;
                }
            }
        }
        if (WIRE_LOG.isDebugEnabled()) {
            WIRE_LOG.debug("<< \"" + buf.toString() + (ch>0 ? "\" [\\r\\n]" : ""));
        }
        if (buf.size() == 0) {
            return null;
        }
        return buf.toByteArray();
    }

    /**
     * Read up to <tt>"\r\n"</tt> from an (unchunked) input stream.
     * If the stream ends before the line terminator is found,
     * the last part of the string will still be returned.
     * '\r' and '\n' are allowed to appear individually in the stream.
     *
     * @param inputStream the stream to read from
     *
     * @throws IOException if an I/O problem occurs
     * @return a line from the stream
     */

    public static String readLine(InputStream inputStream) throws IOException {
        LOG.trace("enter HttpConnection.readLine()");
        byte[] rawdata = readRawLine(inputStream);
        if (rawdata == null) {
            return null;
        }
        int len = rawdata.length;
        if (( len >= 2) && (rawdata[len - 2] == '\r') && (rawdata[len - 1] == '\n')) {
            return HttpConstants.getString(rawdata, 0, rawdata.length - 2);
        } else {
            return HttpConstants.getString(rawdata);
        }
    }

    /**
     * Parses headers from the given stream.  Headers with the same name are not
     * combined.
     * 
     * @param is the stream to read headers from
     * 
     * @return an array of headers in the order in which they were parsed
     * 
     * @throws IOException if an IO error occurs while reading from the stream
     * @throws HttpException if there is an error parsing a header value
     */
    public static Header[] parseHeaders(InputStream is) throws IOException, HttpException {
        LOG.trace("enter HeaderParser.parseHeaders(HttpConnection, HeaderGroup)");

        ArrayList headers = new ArrayList();
        String name = null;
        StringBuffer value = null;
        for (; ;) {
            String line = HttpParser.readLine(is);
            if ((line == null) || (line.length() < 1)) {
                break;
            }

            // Parse the header name and value
            // Check for folded headers first
            // Detect LWS-char see HTTP/1.0 or HTTP/1.1 Section 2.2
            // discussion on folded headers
            if ((line.charAt(0) == ' ') || (line.charAt(0) == '\t')) {
                // we have continuation folded header
                // so append value
                value.append(' ');
                value.append(line.trim());
            } else {
                // make sure we save the previous name,value pair if present
                if (name != null) {
                    headers.add(new Header(name, value.toString()));
                }

                // Otherwise we should have normal HTTP header line
                // Parse the header name and value
                int colon = line.indexOf(":");
                if (colon < 0) {
                    throw new HttpException("Unable to parse header: " + line);
                }
                name = line.substring(0, colon).trim();
                value = new StringBuffer(line.substring(colon + 1).trim());
            }

        }

        // make sure we save the last name,value pair if present
        if (name != null) {
            headers.add(new Header(name, value.toString()));
        }
        
        return (Header[]) headers.toArray(new Header[headers.size()]);    
    }
    
}
