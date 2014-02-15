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

class Pom {

    private final String name
    private final PomArtifact parent
    private final PomArtifact artifact
    private final List<String> modules

    Pom(String name, PomArtifact parent, PomArtifact artifact, modules) {
        this.name = name
        this.parent = parent
        this.artifact = artifact
        this.modules = modules
    }

    String getName() {
        return name
    }

    String getGroupId() {
        if (artifact.groupId) {
            artifact.groupId
        } else {
            parent ? parent.groupId : null
        }
    }

    String getArtifactId() {
        artifact.id
    }

    String getVersion() {
        artifact.version
    }

    int getMajor() {
        artifact.major
    }

    int getMinor() {
        artifact.minor
    }

    int getIncremental() {
        artifact.incremental
    }

    int getBuildNumber() {
        artifact.buildNumber
    }

    String getQualifier() {
        artifact.qualifier
    }

    List<String> getModules() {
        modules
    }

}
