/*
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

import org.apache.maven.artifact.versioning.ArtifactVersion
import org.apache.maven.artifact.versioning.DefaultArtifactVersion
import org.jdom2.Comment
import org.jdom2.Document
import org.jdom2.Element
import org.jdom2.Namespace
import org.jdom2.filter.ContentFilter
import org.jdom2.input.SAXBuilder
import org.jdom2.output.Format
import org.jdom2.output.XMLOutputter

import java.util.regex.Matcher

class Release {

    static String upgradeVersion(String version) {
        ArtifactVersion ver = new DefaultArtifactVersion(version)
        int major = ver.majorVersion
        int minor = ver.minorVersion
        int patch = ver.incrementalVersion
        String qualifier = ver.qualifier

        if (qualifier) {
            if (qualifier == 'SNAPSHOT') {
                qualifier = null
            } else if (qualifier.endsWith('-SNAPSHOT')) {
                qualifier = qualifier - '-SNAPSHOT'
            }
        }

        if (qualifier) {
            Matcher m = qualifier =~ '(alpah|beta|rc)(\\d)'
            if (m.find()) {
                int n = Integer.parseInt(m.group(2))
                qualifier = m.group(1) + (++n)
            } else {
                patch++
            }
        } else {
            patch++
        }
        StringBuilder buf = new StringBuilder()
        buf.append(major).append('.').append(minor)
        if (patch > 0) {
            buf.append('.').append(patch)
        }
        if (qualifier) {
            buf.append('-').append(qualifier)
        }
        buf.append('-SNAPSHOT')
        buf.toString()
    }

    static URI rewriteAsReleaseTag(URI uri, String ver) {
        String path = uri.path
        Matcher m1 = path =~ '(.*)/trunk'
        if (m1.find()) {
            path = m1.group(1) + '/' + 'tags/' + ver
        } else {
            Matcher m2 = path =~ '(.*)/branches/[A-Za-z0-9_\\-\\.]'
            if (m2.find()) {
                path = m2.group(1) + '/' + 'tags/' + ver
            }
        }
        new URI(uri.scheme, uri.userInfo, uri.host, uri.port, path, null, null)
    }

    // Copied from Maven release manager
    static private fixLineDelimitersInComments(Document document) {
        ContentFilter filter = new ContentFilter(ContentFilter.COMMENT)
        for (Iterator it = document.getDescendants(filter); it.hasNext(); ) {
            Comment c = (Comment) it.next();
            c.setText(c.getText().replaceAll('\n', Line.DELIM));
        }
    }

    static void rewritePom(File dir, String version, URI repoUrl) {
        File pomFile = new File(dir, 'pom.xml')
        SAXBuilder parser = new SAXBuilder()

        Namespace ns = Namespace.getNamespace("http://maven.apache.org/POM/4.0.0")

        Document maindoc = parser.build(pomFile)

        Element rootEl = maindoc.rootElement
        Element versionEl = rootEl.getChild('version', ns)
        if (versionEl) {
            versionEl.text = version
        }

        Element scmEl = rootEl.getChild('scm', ns)
        if (scmEl) {
            Element connectionEl = scmEl.getChild('connection', ns)
            if (connectionEl) {
                connectionEl.text = 'scm:svn:' + repoUrl.toASCIIString()
            }
            Element devConnectionEl = scmEl.getChild('developerConnection', ns)
            if (devConnectionEl) {
                devConnectionEl.text = 'scm:svn:' + repoUrl.toASCIIString()
            }
            Element urlEl = scmEl.getChild('url', ns)
            if (urlEl) {
                urlEl.text = repoUrl.toASCIIString()
            }
        }

        Format format = Format.getRawFormat()
        format.lineSeparator = Line.DELIM

        Element modulesEl = rootEl.getChild('modules', ns)
        if (modulesEl) {
            List<Element> moduleEls = modulesEl.getChildren('module', ns)
            for (Element moduleEl: moduleEls) {
                File moduleDir = new File(dir, moduleEl.text)
                File modulePomFile = new File(moduleDir, 'pom.xml')
                Document doc = parser.build(modulePomFile)
                Element moduleParentEl = doc.rootElement.getChild('parent', ns)
                if (moduleParentEl) {
                    Element moduleVersionEl = moduleParentEl.getChild('version', ns)
                    if (moduleVersionEl) {
                        moduleVersionEl.text = version
                    }
                }
                fixLineDelimitersInComments(doc)
                XMLOutputter xmlOutputter = new XMLOutputter(format)
                modulePomFile.withWriter('UTF-8') { Writer writer ->
                    xmlOutputter.output(doc, writer)
                }
            }
        }

        fixLineDelimitersInComments(maindoc)
        XMLOutputter xmlOutputter = new XMLOutputter(format)
        pomFile.withWriter('UTF-8') { Writer writer ->
            xmlOutputter.output(maindoc, writer)
        }
    }

    static void rewritePomParent(File dir, Pom newParent) {
        File pomFile = new File(dir, 'pom.xml')
        SAXBuilder parser = new SAXBuilder()
        Document doc = parser.build(pomFile)

        Namespace ns = Namespace.getNamespace("http://maven.apache.org/POM/4.0.0")
        Element rootEl = doc.rootElement
        Element parentEl = rootEl.getChild('parent', ns)
        if (parentEl != null) {

            Element groupIdEl = parentEl.getChild('groupId', ns)
            if (!groupIdEl) {
                groupIdEl = new Element('groupId', ns)
                parentEl.addContent(groupIdEl)
            }
            groupIdEl.setText(newParent.groupId)

            Element artifactIdEl = parentEl.getChild('artifactId', ns)
            if (!artifactIdEl) {
                artifactIdEl = new Element('artifactId', ns)
                parentEl.addContent(artifactIdEl)
            }
            artifactIdEl.setText(newParent.artifactId)

            Element versionEl = parentEl.getChild('version', ns)
            if (!versionEl) {
                versionEl = new Element("version", ns);
                parentEl.addContent(versionEl)
            }
            versionEl.setText(newParent.version)

            parentEl.removeChild('relativePath', ns)

            fixLineDelimitersInComments(doc)
            Format format = Format.getRawFormat()
            format.lineSeparator = Line.DELIM
            XMLOutputter xmlOutputter = new XMLOutputter(format)
            pomFile.withWriter('UTF-8') { Writer writer ->
                xmlOutputter.output(doc, writer)
            }
        }
    }

}
