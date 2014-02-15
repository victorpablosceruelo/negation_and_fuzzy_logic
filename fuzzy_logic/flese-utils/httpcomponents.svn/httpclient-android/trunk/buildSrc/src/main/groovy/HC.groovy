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




import com.google.common.collect.Multimap
import org.reflections.Reflections
import org.reflections.util.ClasspathHelper
import org.reflections.util.ConfigurationBuilder
import org.tmatesoft.svn.core.io.ISVNEditor
import org.tmatesoft.svn.core.io.SVNRepository

import java.lang.reflect.Method
import java.util.regex.Matcher

class HC {

    static String LINE_DELIM = System.getProperty("line.separator")

    static void createAndroidBranch(
            URI repoRoot,
            String coreVersion,
            String clientVersion,
            String branchName) {
        SVNRepository hcRepo = Svn.getRepository(repoRoot)

        try {
            ISVNEditor commitEditor = hcRepo.getCommitEditor(
                    "Creating Android port branch based on HttpCore ${coreVersion} and HttpClient ${clientVersion}", null)
            commitEditor.openRoot(-1)
            commitEditor.openDir(
                    'httpclient-android/branches', -1)
            commitEditor.addDir(
                    "httpclient-android/branches/${branchName}", null, -1)
            commitEditor.addDir(
                    "httpclient-android/branches/${branchName}/src", null, -1)
            commitEditor.addDir(
                    "httpclient-android/branches/${branchName}/src/main", null, -1)
            commitEditor.addDir(
                    "httpclient-android/branches/${branchName}/src/main/java",
                    "httpcore/tags/${coreVersion}/httpcore/src/main/java/", -1)
            commitEditor.openDir(
                    "httpclient-android/branches/${branchName}/src/main/java/org", -1)
            commitEditor.openDir(
                    "httpclient-android/branches/${branchName}/src/main/java/org/apache", -1)
            commitEditor.openDir(
                    "httpclient-android/branches/${branchName}/src/main/java/org/apache/http", -1)

            ['auth', 'cookie', 'conn', 'client'].each { String module ->
                commitEditor.addDir(
                        "httpclient-android/branches/${branchName}/src/main/java/org/apache/http/${module}",
                        "httpclient/tags/${clientVersion}/httpclient/src/main/java/org/apache/http/${module}", -1)
                commitEditor.closeDir()
            }

            commitEditor.openDir(
                    "httpclient-android/branches/${branchName}/src/main/java/org/apache/http/impl", -1)
            ['auth', 'cookie', 'conn', 'execchain', 'client'].each { String module ->
                commitEditor.addDir(
                        "httpclient-android/branches/${branchName}/src/main/java/org/apache/http/impl/${module}",
                        "httpclient/tags/${clientVersion}/httpclient/src/main/java/org/apache/http/impl/${module}", -1)
                commitEditor.closeDir()
            }
            commitEditor.closeDir()

            commitEditor.closeDir()
            commitEditor.closeDir()
            commitEditor.closeDir()
            commitEditor.closeDir()
            commitEditor.closeDir()
            commitEditor.closeDir()
            commitEditor.closeDir()
            commitEditor.closeDir()
            commitEditor.closeEdit()

        } finally {
            hcRepo.closeSession()
        }
    }

    static Set<Class<?>> getClasses(String namespace, Collection<File> jars) {
        URL[] urls = jars.collect { File file ->
            file.toURI().toURL()
        }
        URLClassLoader urlClassLoader = new URLClassLoader(urls)

        Reflections reflections = new ConfigurationBuilder()
                .setUrls(ClasspathHelper.forPackage(namespace, urlClassLoader))
                .setScanners(new GetAllScanner())
                .build()
        Multimap<String, String> multimap = reflections.getStore().get(GetAllScanner)
        Set<Class<?>> allClasses = new HashSet<Class<?>>()
        for (String key: multimap.keySet()) {
            Class<?> clazz = urlClassLoader.loadClass(key)
            if (!clazz.memberClass && !clazz.anonymousClass) {
                allClasses.add(clazz)
            }
        }
        allClasses
    }

    static Set<Class<?>> getApiClasses(Set<Class<?>> allClasses) {
        Set<Class<?>> apiClasses = new HashSet<Class<?>>()
        for (Class<?> clazz: allClasses) {
            if (clazz.interface) {
                apiClasses.add(clazz)
                Method[] methods = clazz.methods
                for (Method method: methods) {
                    Class<?>[] parameterTypes = method.parameterTypes
                    for (Class<?> parameterType: parameterTypes) {
                        if (allClasses.contains(parameterType)) {
                            apiClasses.add(parameterType)
                        }
                    }
                    Class<?> returnType = method.returnType
                    if (returnType && allClasses.contains(returnType)) {
                        apiClasses.add(returnType)
                    }
                    Class<?>[] exceptionTypes = method.exceptionTypes
                    for (Class<?> exceptionType: exceptionTypes) {
                        if (allClasses.contains(exceptionType)) {
                            apiClasses.add(exceptionType)
                        }
                    }
                }
            }
        }
        apiClasses
    }

    static void replacePatterns(File file, List<Replacement> replacements) {
        File tmpFile = File.createTempFile('temp', ".java")
        try {
            boolean modified = false
            tmpFile.withWriter { Writer w ->
                file.eachLine { String line ->
                    for (Replacement replacement in replacements) {
                        Matcher m = line =~ replacement.pattern
                        if (m.find()) {
                            modified = true
                            line = m.replaceAll(replacement.text)
                        }
                    }
                    w << line + LINE_DELIM
                }
            }
            if (modified) {
                file.bytes = tmpFile.bytes
            }
        } finally {
            if (tmpFile.exists()) {
                tmpFile.delete()
            }
        }
    }

}
