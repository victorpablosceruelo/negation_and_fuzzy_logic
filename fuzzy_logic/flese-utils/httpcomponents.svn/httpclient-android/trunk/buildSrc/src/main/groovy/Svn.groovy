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

import org.tmatesoft.svn.cli.SVNConsoleAuthenticationProvider
import org.tmatesoft.svn.cli.svn.SVNCommandEnvironment
import org.tmatesoft.svn.cli.svn.SVNNotifyPrinter
import org.tmatesoft.svn.cli.svn.SVNStatusPrinter
import org.tmatesoft.svn.core.SVNCommitInfo
import org.tmatesoft.svn.core.SVNDepth
import org.tmatesoft.svn.core.SVNException
import org.tmatesoft.svn.core.SVNURL
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager
import org.tmatesoft.svn.core.internal.wc17.SVNWCContext
import org.tmatesoft.svn.core.internal.wc2.compat.SvnCodec
import org.tmatesoft.svn.core.io.SVNRepository
import org.tmatesoft.svn.core.io.SVNRepositoryFactory
import org.tmatesoft.svn.core.wc.SVNRevision
import org.tmatesoft.svn.core.wc.SVNWCUtil
import org.tmatesoft.svn.core.wc2.ISvnObjectReceiver
import org.tmatesoft.svn.core.wc2.SvnCheckout
import org.tmatesoft.svn.core.wc2.SvnCommit
import org.tmatesoft.svn.core.wc2.SvnCopy
import org.tmatesoft.svn.core.wc2.SvnCopySource
import org.tmatesoft.svn.core.wc2.SvnGetStatus
import org.tmatesoft.svn.core.wc2.SvnOperationFactory
import org.tmatesoft.svn.core.wc2.SvnRemoteCopy
import org.tmatesoft.svn.core.wc2.SvnRemoteDelete
import org.tmatesoft.svn.core.wc2.SvnRevert
import org.tmatesoft.svn.core.wc2.SvnScheduleForAddition
import org.tmatesoft.svn.core.wc2.SvnScheduleForRemoval
import org.tmatesoft.svn.core.wc2.SvnStatus
import org.tmatesoft.svn.core.wc2.SvnTarget
import org.tmatesoft.svn.core.wc2.SvnUpdate

class Svn {

    private static SVNCommandEnvironment getSVNCommandEnvironment() {
        new SVNCommandEnvironment("SVN", System.out, System.err, System.in);
    }

    private static SvnOperationFactory createOperationFactory(SVNCommandEnvironment env) {
        SvnOperationFactory opfactory = new SvnOperationFactory()
        ISVNAuthenticationManager authmanager = SVNWCUtil.createDefaultAuthenticationManager()
        authmanager.setAuthenticationProvider(new SVNConsoleAuthenticationProvider(false))
        opfactory.setAuthenticationManager(authmanager)
        opfactory.setEventHandler(new SVNNotifyPrinter(env))
        opfactory
    }

    static SVNRepository getRepository(URI src) {
        SVNURL url = SVNURL.parseURIEncoded(src.toASCIIString())
        SVNRepository repo = SVNRepositoryFactory.create(url)
        ISVNAuthenticationManager authmanager = SVNWCUtil.createDefaultAuthenticationManager()
        authmanager.setAuthenticationProvider(new SVNConsoleAuthenticationProvider(false))
        repo.setAuthenticationManager(authmanager)
        repo
    }

    static void checkout(URI src, File dst) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnCheckout checkoutOp = opfactory.createCheckout()
            checkoutOp.setSource(SvnTarget.fromURL(SVNURL.parseURIEncoded(src.toASCIIString())))
            checkoutOp.setSingleTarget(SvnTarget.fromFile(dst))
            checkoutOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void update(File dir) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnUpdate updateOp = opfactory.createUpdate()
            updateOp.setSingleTarget(SvnTarget.fromFile(dir))
            updateOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void status(File dir) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SVNStatusPrinter statusPrinter = new SVNStatusPrinter(env)
            SVNWCContext context = opfactory.getWcContext();
            SvnGetStatus statusOp = opfactory.createGetStatus()
            statusOp.setSingleTarget(SvnTarget.fromFile(dir))
            statusOp.setReportAll(false)
            statusOp.setReceiver(new ISvnObjectReceiver<SvnStatus>() {

                @Override
                void receive(SvnTarget target, SvnStatus object) throws SVNException {
                    String root = dir.getAbsoluteFile();
                    String f = target.getFile().getAbsolutePath()
                    if (f.startsWith(root)) {
                        f = f.substring(root.length())
                        if (f.startsWith('/')) {
                            f = f.substring(1, f.length() - 1)
                        }
                    }
                    statusPrinter.printStatus(f,
                            SvnCodec.status(context, object), false, true, true, false)
                }

            })
            statusOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void revert(File dir) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnRevert revertOp = opfactory.createRevert()
            revertOp.setSingleTarget(SvnTarget.fromFile(dir))
            revertOp.setDepth(SVNDepth.INFINITY)
            revertOp.setPreserveModifiedCopies(false)
            revertOp.setRevertMissingDirectories(true)
            revertOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void scheduleForAddition(File dir) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnScheduleForAddition schedulingOp = opfactory.createScheduleForAddition()
            schedulingOp.setSingleTarget(SvnTarget.fromFile(dir))
            schedulingOp.setDepth(SVNDepth.INFINITY)
            schedulingOp.setForce(true)
            schedulingOp.setAddParents(true)
            schedulingOp.setIncludeIgnored(false)
            schedulingOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void scheduleForAddition(Collection<File> files) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnScheduleForAddition schedulingOp = opfactory.createScheduleForAddition()
            files.each { File file ->
                schedulingOp.addTarget(SvnTarget.fromFile(file))
            }
            schedulingOp.setDepth(SVNDepth.INFINITY)
            schedulingOp.setAddParents(true)
            schedulingOp.setForce(true)
            schedulingOp.setIncludeIgnored(false)
            schedulingOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void scheduleForRemoval(File file) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnScheduleForRemoval schedulingOp = opfactory.createScheduleForRemoval()
            schedulingOp.addTarget(SvnTarget.fromFile(file))
            schedulingOp.setDepth(SVNDepth.INFINITY)
            schedulingOp.setDeleteFiles(true)
            schedulingOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void scheduleForRemoval(Collection<File> files) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnScheduleForRemoval schedulingOp = opfactory.createScheduleForRemoval()
            files.each { File file ->
                schedulingOp.addTarget(SvnTarget.fromFile(file))
            }
            schedulingOp.setDepth(SVNDepth.INFINITY)
            schedulingOp.setDeleteFiles(true)
            schedulingOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static long commit(File dir, String message) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnCommit commitOp = opfactory.createCommit()
            commitOp.setSingleTarget(SvnTarget.fromFile(dir))
            commitOp.setDepth(SVNDepth.INFINITY)
            commitOp.setCommitMessage(message)
            SVNCommitInfo result = commitOp.run()
            result.newRevision
        } finally {
            opfactory.dispose()
        }
    }

    static void copy(File src, File dst, String message) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnCopy copyOp = opfactory.createCopy()
            copyOp.addCopySource(
                    SvnCopySource.create(SvnTarget.fromFile(src), SVNRevision.WORKING))
            copyOp.setSingleTarget(SvnTarget.fromFile(dst))
            copyOp.setFailWhenDstExists(true)
            copyOp.setMakeParents(true)
            copyOp.setMove(false)
            copyOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static void move(File src, File dst) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnCopy copyOp = opfactory.createCopy()
            copyOp.addCopySource(
                    SvnCopySource.create(SvnTarget.fromFile(src), SVNRevision.WORKING))
            copyOp.setSingleTarget(SvnTarget.fromFile(dst))
            copyOp.setFailWhenDstExists(true)
            copyOp.setMakeParents(true)
            copyOp.setMove(true)
            copyOp.run()
        } finally {
            opfactory.dispose()
        }
    }

    static long copyRemote(URI src, URI dst, String message) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnRemoteCopy copyOp = opfactory.createRemoteCopy()
            copyOp.addCopySource(
                    SvnCopySource.create(SvnTarget.fromURL(
                            SVNURL.parseURIEncoded(src.toASCIIString())), SVNRevision.HEAD))
            copyOp.setSingleTarget(SvnTarget.fromURL(SVNURL.parseURIEncoded(dst.toASCIIString())))
            copyOp.setFailWhenDstExists(true)
            copyOp.setCommitMessage(message)
            SVNCommitInfo result = copyOp.run()
            result.newRevision
        } finally {
            opfactory.dispose()
        }
    }

    static void deleteRemote(URI src, String message) {
        SVNCommandEnvironment env = getSVNCommandEnvironment()
        SvnOperationFactory opfactory = createOperationFactory(env)
        try {
            SvnRemoteDelete deleteOp = opfactory.createRemoteDelete()
            deleteOp.setSingleTarget(SvnTarget.fromURL(SVNURL.parseURIEncoded(src.toASCIIString())))
            deleteOp.setCommitMessage(message)
            deleteOp.run()
        } finally {
            opfactory.dispose()
        }
    }

}
