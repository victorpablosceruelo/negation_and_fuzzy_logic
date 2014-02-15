/*
 * $Header: $
 * $Revision: $
 * $Date: $
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
 * 4. The names "The Jakarta Project", "HttpClient", and "Apache Software
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

import org.apache.commons.httpclient.*;
import org.apache.commons.httpclient.methods.*;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.text.html.*;

/**
 * 
 * This is a Swing application that demonstrates
 * how to use the Jakarta HttpClient API.
 * 
 * @author Sean C. Sullivan
 * @author Ortwin Glück
 */
public class ClientApp {

    public static void main(String[] args) {
		HttpClientMainFrame f = new HttpClientMainFrame();
		f.setTitle("HttpClient demo application");
		f.setSize(700, 500);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setVisible(true);
	}

    public static class HttpClientMainFrame extends javax.swing.JFrame {
		private HttpClientPanel m_panel;

        public HttpClientMainFrame() {
			m_panel = new HttpClientPanel();
			this.getContentPane().add(m_panel);
		}
	}

    public static class HttpClientPanel extends JPanel {
		private static final String strTenSpaces = "          ";
		private static final String strFortySpaces =
			strTenSpaces + strTenSpaces + strTenSpaces + strTenSpaces;
		private static final String strEightySpaces =
			strFortySpaces + strFortySpaces;

        public HttpClientPanel() {
			final JPanel panInput = new JPanel();
			panInput.setLayout(new FlowLayout());

			final JPanel panDisplay = new JPanel();
			panDisplay.setLayout(new BorderLayout());

            String[] aURLs = {
					"http://www.apache.org/",
					"http://www.google.com/",
					"http://www.opensource.org/",
					"http://www.anybrowser.org/",
					"http://jakarta.apache.org/",
                    "http://www.w3.org/"
            };

			final JComboBox cmbURL = new JComboBox(aURLs);
			cmbURL.setToolTipText("Enter a URL");
			cmbURL.setPrototypeDisplayValue(strEightySpaces);
			cmbURL.setEditable(true);
			cmbURL.setSelectedIndex(0);

			final JTextArea taTextResponse = new JTextArea();
			taTextResponse.setEditable(false);
			taTextResponse.setCaretPosition(0);

			final JLabel lblURL = new JLabel("URL:");

			final JButton btnGET = new JButton("GET");

            cmbURL.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent ae)
				{
					btnGET.doClick();
				}
			});

            btnGET.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
					final String strRawURL = (String) cmbURL.getSelectedItem();
                    if (strRawURL.length() > 0) {
						final URL u;
                        try {
							u = new URL(strRawURL.trim());
                            Thread t = new Thread() {
                                public void run() {
                                    HttpClient client = new HttpClient(new MultiThreadedHttpConnectionManager());
									GetMethod get = new GetMethod();
									HeadMethod head = new HeadMethod();
                                    HostConfiguration hc = new HostConfiguration();
                                    try {
                                        hc.setHost(new URI(u));
                                    } catch(URIException e) {
                                        throw new RuntimeException(e.toString());
                                    }
                                    client.setHostConfiguration(hc);
                                    client.setConnectionTimeout(30000);
									int iGetResultCode;
									int iHeadResultCode;
                                    try {
                                        //to execute two methods in a row without reading the first method's response
                                        //we *need* the MultiThreadedHttpConnectionManager as set in the HttpClient's
                                        //constructor.
										iGetResultCode = client.executeMethod(get);
										iHeadResultCode = client.executeMethod(head);
										
										final String strGetResponseBody =
											get.getResponseBodyAsString();
                                        get.releaseConnection();
											
										final String strHeadResponseBody =
											head.getResponseBodyAsString();
                                        head.releaseConnection();
											
                                        if (strGetResponseBody != null) {
                                            Runnable r = new Runnable() {
                                                public void run() {
													panDisplay.removeAll();
		
													taTextResponse.setText(
														strGetResponseBody);
													taTextResponse.setCaretPosition(0);
													taTextResponse.requestFocus();
		
														JEditorPane htmlPane =
                                                        new JEditorPane("text/html",
                                                            strGetResponseBody);
														htmlPane.setEditable(false);
			
														final JSplitPane splitResponsePane =
															new JSplitPane(
																JSplitPane.HORIZONTAL_SPLIT,
																new JScrollPane(taTextResponse),
																new JScrollPane(htmlPane));
														splitResponsePane
															.setOneTouchExpandable(
															false);
														panDisplay.add(
															splitResponsePane,
															BorderLayout.CENTER);
			
														splitResponsePane
															.setDividerLocation(
															panDisplay.getWidth() / 2);
			
														panDisplay.validate();
													}
											};
                                            try {
												SwingUtilities.invokeAndWait(r);
                                            } catch (InvocationTargetException ex) {
												ex.printStackTrace();
                                            } catch (InterruptedException ex) {
												ex.printStackTrace();
											}
										}
									}
                                    catch (HttpException ex) {
										ex.printStackTrace();
                                    } catch (IOException ex) {
										ex.printStackTrace();
									}
								}
							};
							t.start();
                        } catch (MalformedURLException ignored) {
							// ignore
						}
					}
				}
			});

			panInput.add(lblURL);
			panInput.add(cmbURL);
			panInput.add(btnGET);

			this.setLayout(new BorderLayout());

			this.add(panInput, BorderLayout.NORTH);
			this.add(panDisplay, BorderLayout.CENTER);
		}
	}
}
