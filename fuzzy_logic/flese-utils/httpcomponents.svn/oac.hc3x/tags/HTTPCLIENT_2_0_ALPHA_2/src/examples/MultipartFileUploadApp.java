/*
 * $Header: $
 * $Revision: 1.2 $
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
 * how to use the Jakarta HttpClient multipart POST method
 * for uploading files
 * 
 * @author Sean C. Sullivan
 *
 */
public class MultipartFileUploadApp {

    public static void main(String[] args) {
		MultipartFileUploadMainFrame f = new MultipartFileUploadMainFrame();
		f.setTitle("HTTP multipart file upload application");
		f.setSize(700, 500);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setVisible(true);
	}

    public static class MultipartFileUploadMainFrame extends javax.swing.JFrame {
		private MultipartFileUploadPanel m_panel;

        public MultipartFileUploadMainFrame() {
			m_panel = new MultipartFileUploadPanel();
			this.getContentPane().add(m_panel);
		}
	}

    public static class MultipartFileUploadPanel extends JPanel {
		private static final String strTenSpaces = "          ";
		private static final String strFortySpaces =
			strTenSpaces + strTenSpaces + strTenSpaces + strTenSpaces;
		private static final String strEightySpaces =
			strFortySpaces + strFortySpaces;

		private File targetFile = null;
		
        public MultipartFileUploadPanel() {
			final JPanel panInput = new JPanel();
			
            String[] aURLs = {
					"http://localhost:8080/foo/bar"
				};

			final JComboBox cmbURL = new JComboBox(aURLs);
			cmbURL.setToolTipText("Enter a URL");
			cmbURL.setPrototypeDisplayValue(strEightySpaces);
			cmbURL.setEditable(true);
			cmbURL.setSelectedIndex(0);

			final JLabel lblTargetFile = new JLabel();
			lblTargetFile.setText("");

			final JButton btnDoUpload = new JButton("Upload");
			btnDoUpload.setEnabled(false);

			final JButton btnSelectFile = new JButton("Select a file...");
            btnSelectFile.addActionListener(new ActionListener() {
                    public void actionPerformed(final ActionEvent evt) {
						final JFileChooser chooser = new JFileChooser();
						chooser.setFileHidingEnabled(false);
						chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
						chooser.setMultiSelectionEnabled(false);
						chooser.setDialogType(JFileChooser.OPEN_DIALOG);
						chooser.setDialogTitle("Choose a file...");
                        if (chooser.showOpenDialog(MultipartFileUploadPanel.this) == JFileChooser.APPROVE_OPTION) {
							targetFile = chooser.getSelectedFile();
							lblTargetFile.setText("Selected file:  " + targetFile.toString());
							btnDoUpload.setEnabled(true);
						}
					}
				});
			
			final JTextArea taTextResponse = new JTextArea();
			taTextResponse.setEditable(false);
			taTextResponse.setCaretPosition(0);

			final JLabel lblURL = new JLabel("URL:");


            btnDoUpload.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
					URL targetURL = null;

                    try {
						targetURL = new URL(cmbURL.getSelectedItem().toString());
                    } catch (MalformedURLException ex) {
						// todo - code here
						ex.printStackTrace();
					}

										
					MultipartPostMethod filePost = new MultipartPostMethod();
                    if (targetURL.getPath() == null) {
						filePost.setPath("/");
                    } else {
						filePost.setPath(targetURL.getPath());
					}
					
                    try {
						filePost.addParameter(
								targetFile.getName(),
								targetFile);
						HttpClient client = new HttpClient();
                        HostConfiguration hc = new HostConfiguration();
                        hc.setHost(new URI(targetURL));
                        client.setHostConfiguration(hc);
						client.executeMethod(filePost);
                        filePost.releaseConnection();
                    } catch (FileNotFoundException ex) {
						// todo - put real code here
						ex.printStackTrace();
                    } catch (HttpException ex) {
						// todo - put real code here
						ex.printStackTrace();
                    } catch (java.io.IOException ex) {
						// todo - put real code here
						ex.printStackTrace();
					}
					
				}
			});

			JPanel panURLInput = new JPanel();
			panURLInput.setLayout(new FlowLayout());
			panURLInput.add(lblURL);
			panURLInput.add(cmbURL);
			
			JPanel panFile = new JPanel();
			panFile.setLayout(new FlowLayout());
			panFile.add(lblTargetFile);
			panFile.add(btnSelectFile);
			
			panInput.setLayout(new GridLayout(3, 1));

			panInput.add(panURLInput);
			panInput.add(panFile);
			panInput.add(btnDoUpload);
			
			this.setLayout(new BorderLayout());

			this.add(panInput, BorderLayout.CENTER);
		}
	}
}
