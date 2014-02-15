/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta1/module-nio/src/test/java/org/apache/http/mockup/ByteSequence.java $
 * $Revision: 599808 $
 * $Date: 2007-11-30 13:52:38 +0100 (Fri, 30 Nov 2007) $
 *
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

package org.apache.http.mockup;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class ByteSequence {

    private final List<byte []> data;
    
    public ByteSequence() {
        super();
        this.data = new ArrayList<byte []>();
    }
    
    public void addBytes(byte[] bytes) {
        this.data.add(bytes);
    }
    
    public int size() {
        return this.data.size();
    }

    public byte[] getBytes(int index) {
        return this.data.get(index);
    }
    
    public void rnd(int count) {
        Random rnd = new Random();
        for (int i = 0; i < count; i++) {
            int size = rnd.nextInt(5000);
            byte[] data = new byte[size];
            rnd.nextBytes(data);
            this.data.add(data);
        }
    }
    
}
