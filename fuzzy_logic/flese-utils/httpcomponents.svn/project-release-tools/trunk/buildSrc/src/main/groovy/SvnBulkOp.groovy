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

class SvnBulkOp {

    final File path;
    final File copyFrom;
    final long revision;

    protected SvnBulkOp(File path, File copyFrom, long revision) {
        this.path = path
        this.copyFrom = copyFrom
        this.revision = revision
    }

    static class Rm extends SvnBulkOp {

        Rm(File path) {
            super(path, null, -1)
        }

        Rm(String path) {
            super(new File(path), null, -1)
        }
    }

    static class Mkdir extends SvnBulkOp {

        Mkdir(File path) {
            super(path, null, -1)
        }

        Mkdir(String path) {
            super(new File(path), null, -1)
        }

    }

    static class CpFile extends SvnBulkOp {

        CpFile(File path, File copyFrom, long revision) {
            super(path, copyFrom, revision)
        }

        CpFile(File path, File copyFrom) {
            super(path, copyFrom, -1)
        }

        CpFile(String path, String copyFrom, long revision) {
            super(new File(path), new File(copyFrom), revision)
        }

        CpFile(String path, String copyFrom) {
            super(new File(path), new File(copyFrom), -1)
        }

    }

    static class CpDir extends SvnBulkOp {

        CpDir(File path, File copyFrom, long revision) {
            super(path, copyFrom, revision)
        }

        CpDir(File path, File copyFrom) {
            super(path, copyFrom, -1)
        }

        CpDir(String path, String copyFrom, long revision) {
            super(new File(path), new File(copyFrom), revision)
        }

        CpDir(String path, String copyFrom) {
            super(new File(path), new File(copyFrom), -1)
        }

    }

}