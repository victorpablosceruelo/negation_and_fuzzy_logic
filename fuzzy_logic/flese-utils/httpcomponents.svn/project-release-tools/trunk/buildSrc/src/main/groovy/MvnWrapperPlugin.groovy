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

import org.gradle.api.InvalidUserDataException
import org.gradle.api.Plugin
import org.gradle.api.Project

/**
 * This plugin adds the following extensions to the project model:
 *
 * 'mvn' - Maven wrapper
 */
class MvnWrapperPlugin implements Plugin<Project> {

    void apply(Project project) {
        String MAVEN_HOME = project.getProperties().get('MAVEN_HOME')
        if (!MAVEN_HOME) {
            throw new InvalidUserDataException("MAVEN_HOME not set")
        }
        File mvnHomeDir = new File(MAVEN_HOME)
        if (!mvnHomeDir.exists()) {
            throw new InvalidUserDataException("Maven not found at ${mvnHomeDir}")
        }
        Mvn mvn = new Mvn(project, mvnHomeDir)
        project.extensions.add('mvn', mvn)
    }

}
