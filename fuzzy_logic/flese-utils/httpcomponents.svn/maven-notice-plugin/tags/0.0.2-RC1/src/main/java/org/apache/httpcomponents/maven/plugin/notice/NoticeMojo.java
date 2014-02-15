/*
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */
package org.apache.httpcomponents.maven.plugin.notice;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Calendar;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.maven.model.License;
import org.apache.maven.model.Resource;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;

/**
 * @phase generate-resources
 * @goal generate
 */
public final class NoticeMojo extends AbstractMojo {

    private final static String UTF_8 = "UTF-8";

    /**
     * The Maven Project Object
     *
     * @parameter expression="${project}"
     * @required
     */
    protected MavenProject project;

    /**
     * This is where build results go.
     *
     * @parameter default-value="${project.build.directory}"
     * @required
     * @readonly
     */
    private File directory;

    /**
     * The project title in NOTICE. Optional.
     *
     * @parameter expression="${projectTitle}"
     */
    private String projectTitle;

    /**
     * The copyright holder in NOTICE. Optional.
     *
     * @parameter expression="${copyrightHolder}"
     */
    private String copyrightHolder;

    public NoticeMojo() {
        super();
        this.project = new MavenProject();
    }

    public void execute() throws MojoExecutionException, MojoFailureException {
        if (this.project.getPackaging().equalsIgnoreCase("pom")) {
            return;
        }
        getLog().info("Generating NOTICE and LICENSE resources");

        File nalDir = new File(this.directory, "notice-and-license");
        if (!nalDir.exists()) {
            nalDir.mkdirs();
        }

        List licenses = this.project.getLicenses();
        for (int i = 0; i < licenses.size(); i++) {
            License license = (License) licenses.get(i);
            try {
                URI uri = new URI(license.getUrl());
                String s = "LICENSE";
                if (i > 0) {
                    s = s + i;
                }
                s = s + ".txt";
                File out = new File(nalDir, s);
                if (!uri.isAbsolute()) {
                    File file = findFile(license.getUrl());
                    if (file != null) {
                        FileUtils.copyFile(file, out);
                    }
                } else {
                    FileUtils.copyURLToFile(uri.toURL(), out);
                }
            } catch (URISyntaxException ex) {
                throw new MojoFailureException("Invalid URI: " +
                        ex.getMessage(), ex);
            } catch (IOException ex) {
                throw new MojoFailureException("I/O error copying LICENSE file: " +
                        ex.getMessage(), ex);
            }
        }
        File file = findFile("NOTICE.txt");
        if (file != null) {
            File out = new File(nalDir, "NOTICE.txt");
            try {
                rewriteNotice(file, out);
            } catch (IOException ex) {
                throw new MojoFailureException("I/O error copying NOTICE file: " +
                        ex.getMessage(), ex);
            }
        }

        Resource nalRes = new Resource();
        nalRes.setDirectory(nalDir.getAbsolutePath());
        nalRes.setFiltering(false);
        nalRes.setTargetPath("META-INF");
        this.project.addResource(nalRes);
        this.project.addTestResource(nalRes);
    }

    private File findFile(final String resource) {
        MavenProject current = this.project;
        while (current != null) {
            File basedir = current.getBasedir();
            File file = new File(basedir, resource);
            if (file.exists()) {
                return file;
            }
            current = current.getParent();
        }
        return null;
    }

    private void rewriteNotice(final File in, final File out) throws IOException  {
        BufferedReader reader = new BufferedReader(new InputStreamReader(
                new FileInputStream(in), UTF_8));
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(out), UTF_8));
            try {
                if (this.projectTitle != null) {
                    writer.write(this.projectTitle);
                    writer.write(" ");
                }
                writer.write(this.project.getName());
                writer.write("\r\n");
                writer.write("Copyright ");
                String fromYear = this.project.getInceptionYear();
                if (fromYear != null && fromYear.length() > 0) {
                    writer.write(fromYear);
                    writer.write("-");
                }
                Calendar cal = Calendar.getInstance();
                String toYear = Integer.toString(cal.get(Calendar.YEAR));
                writer.write(toYear);

                String copyrightHolder = this.copyrightHolder;
                if (copyrightHolder == null) {
                    if (this.project.getOrganization() != null) {
                        copyrightHolder = this.project.getOrganization().getName();
                    }
                }
                if (copyrightHolder != null) {
                    writer.write(" ");
                    writer.write(copyrightHolder);
                }
                writer.write("\r\n");

                int count = 0;
                String line;
                while ((line = reader.readLine()) != null) {
                    count++;
                    if (count > 2) {
                        writer.write(line);
                        writer.write("\r\n");
                    }
                }
                writer.flush();
            } finally {
                writer.close();
            }
        } finally {
            reader.close();
        }
    }

}
