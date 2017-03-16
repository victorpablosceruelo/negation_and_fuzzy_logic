:- module(factorized_opcodes,[opcode/3]).

:- dynamic wide_flag/1.

wide_flag(off).

wide_flag_on :-
	retract(wide_flag(_)),
	assert(wide_flag(on)).

wide_flag_off :-
	retract(wide_flag(_)),
	assert(wide_flag(off)).


opcode(0x00,[nop],[]) :- !.
opcode(0x01,[aconst_null],[]) :- !.
opcode(0x02,[const,primitiveType(int),-1],[]) :- !.
opcode(0x03,[const,primitiveType(int),0],[]) :- !.
opcode(0x04,[const,primitiveType(int),1],[]) :- !.
opcode(0x05,[const,primitiveType(int),2],[]) :- !.
opcode(0x06,[const,primitiveType(int),3],[]) :- !.
opcode(0x07,[const,primitiveType(int),4],[]) :- !.
opcode(0x08,[const,primitiveType(int),5],[]) :- !.
opcode(0x09,[const,primitiveType(long),0],[]) :- !.
opcode(0x0a,[const,primitiveType(long),1],[]) :- !.
opcode(0x0b,[const,primitiveType(float),0],[]) :- !.
opcode(0x0c,[const,primitiveType(float),1],[]) :- !.
opcode(0x0d,[const,primitiveType(float),2],[]) :- !.
opcode(0x0e,[const,primitiveType(double),0],[]) :- !.
opcode(0x0f,[const,primitiveType(double),1],[]) :- !.
opcode(0x10,[const,primitiveType(int)],[-1]) :- !.
opcode(0x11,[const,primitiveType(int)],[-2]) :- !.
opcode(0x12,[const],[1]) :- !.
opcode(0x13,[const],[2]) :- !.
opcode(0x14,[const],[2]) :- !.
opcode(0x15,[iload],[1]) :- wide_flag(off), !.
opcode(0x15,[iload],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x16,[iload],[1]) :- wide_flag(off), !. %lload
opcode(0x16,[iload],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x17,[iload],[1]) :- wide_flag(off), !.%fload
opcode(0x17,[iload],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x18,[iload],[1]) :- wide_flag(off), !.%dload
opcode(0x18,[iload],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x19,[aload],[1]) :- wide_flag(off), !.
opcode(0x19,[aload],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x1a,[iload,0],[]) :- !.
opcode(0x1b,[iload,1],[]) :- !.
opcode(0x1c,[iload,2],[]) :- !.
opcode(0x1d,[iload,3],[]) :- !.
opcode(0x1e,[iload,0],[]) :- !.%lload
opcode(0x1f,[iload,1],[]) :- !.%lload
opcode(0x20,[iload,2],[]) :- !.%lload
opcode(0x21,[iload,3],[]) :- !.%lload
opcode(0x22,[iload,0],[]) :- !.%fload
opcode(0x23,[iload,1],[]) :- !.%fload
opcode(0x24,[iload,2],[]) :- !.%fload
opcode(0x25,[iload,3],[]) :- !.%fload
opcode(0x26,[iload,0],[]) :- !.%double
opcode(0x27,[iload,1],[]) :- !.%double
opcode(0x28,[iload,2],[]) :- !.%double
opcode(0x29,[iload,3],[]) :- !.%double
opcode(0x2a,[aload,0],[]) :- !.
opcode(0x2b,[aload,1],[]) :- !.
opcode(0x2c,[aload,2],[]) :- !.
opcode(0x2d,[aload,3],[]) :- !.
opcode(0x2e,[iaload],[]) :- !.
opcode(0x2f,[laload],[]) :- !.
opcode(0x30,[faload],[]) :- !.
opcode(0x31,[daload],[]) :- !.
opcode(0x32,[aaload],[]) :- !.
opcode(0x33,[baload],[]) :- !.
opcode(0x34,[caload],[]) :- !.
opcode(0x35,[saload],[]) :- !.
opcode(0x36,[istore],[1]) :- wide_flag(off), !.
opcode(0x36,[istore],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x37,[istore],[1]) :- wide_flag(off), !. %lstore
opcode(0x37,[istore],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x38,[istore],[1]) :- wide_flag(off), !.%fstore
opcode(0x38,[istore],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x39,[istore],[1]) :- wide_flag(off), !.%dstore
opcode(0x39,[istore],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0x3a,[astore],[1]) :- wide_flag(off), !.
opcode(0x3a,[astore],[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x3b,[istore,0],[]) :- !.
opcode(0x3c,[istore,1],[]) :- !.
opcode(0x3d,[istore,2],[]) :- !.
opcode(0x3e,[istore,3],[]) :- !.
opcode(0x3f,[istore,0],[]) :- !.%lstore
opcode(0x40,[istore,1],[]) :- !.%lstore
opcode(0x41,[istore,2],[]) :- !.%lstore
opcode(0x42,[istore,3],[]) :- !.%lstore
opcode(0x43,[istore,0],[]) :- !.%fstore
opcode(0x44,[istore,1],[]) :- !.%fstore
opcode(0x45,[istore,2],[]) :- !.%fstore
opcode(0x46,[istore,3],[]) :- !.%fstore
opcode(0x47,[istore,0],[]) :- !.%dstore
opcode(0x48,[istore,1],[]) :- !.%dstore
opcode(0x49,[istore,2],[]) :- !.%dstore
opcode(0x4a,[istore,3],[]) :- !.%dstore
opcode(0x4b,[astore,0],[]) :- !.
opcode(0x4c,[astore,1],[]) :- !.
opcode(0x4d,[astore,2],[]) :- !.
opcode(0x4e,[astore,3],[]) :- !.
opcode(0x4f,[iastore],[]) :- !.
opcode(0x50,[lastore],[]) :- !.
opcode(0x51,[fastore],[]) :- !.
opcode(0x52,[dastore],[]) :- !.
opcode(0x53,[aastore],[]) :- !.
opcode(0x54,[bastore],[]) :- !.
opcode(0x55,[castore],[]) :- !.
opcode(0x56,[sastore],[]) :- !.
opcode(0x57,[pop],[]) :- !.
opcode(0x58,[pop2],[]) :- !.
opcode(0x59,[dup],[]) :- !.
opcode(0x5a,[dup_x1],[]) :- !.
opcode(0x5b,[dup_x2],[]) :- !.
opcode(0x5c,[dup2],[]) :- !.
opcode(0x5d,[dup2_x1],[]) :- !.
opcode(0x5e,[dup2_x2],[]) :- !.
opcode(0x5f,[swap],[]) :- !.
opcode(0x60,[ibinop,addInt],[]) :- !.
%opcode(0x61,[ibinop,addLong],[]) :- !.
opcode(0x61,[ibinop,addInt],[]) :- !.%Long
opcode(0x62,[ibinop,addInt],[]) :- !.%Float
opcode(0x63,[ibinop,addInt],[]) :- !.%Double
opcode(0x64,[ibinop,subInt],[]) :- !.
opcode(0x65,[ibinop,subInt],[]) :- !.%Long
opcode(0x66,[ibinop,subInt],[]) :- !.%Float
opcode(0x67,[ibinop,subInt],[]) :- !.%Double
opcode(0x68,[ibinop,mulInt],[]) :- !.
opcode(0x69,[ibinop,mulInt],[]) :- !.%Long
opcode(0x6a,[ibinop,mulInt],[]) :- !.%Float
opcode(0x6b,[ibinop,mulInt],[]) :- !.%Double
opcode(0x6c,[ibinop,divInt],[]) :- !.
opcode(0x6d,[ibinop,divInt],[]) :- !.%Long
opcode(0x6e,[ibinop,divInt],[]) :- !.%Float
opcode(0x6f,[ibinop,divInt],[]) :- !.%Double
opcode(0x70,[ibinop,remInt],[]) :- !.
opcode(0x71,[ibinop,remInt],[]) :- !.%Long
opcode(0x72,[ibinop,remInt],[]) :- !.%Float
opcode(0x73,[ibinop,remInt],[]) :- !.%Double
opcode(0x74,[ineg],[]) :- !.           %%??
opcode(0x75,[lneg],[]) :- !.
opcode(0x76,[fneg],[]) :- !.
opcode(0x77,[dneg],[]) :- !.
opcode(0x78,[ibinop,shlInt],[]) :- !.
opcode(0x79,[ibinop,shlInt],[]) :- !.%Long
opcode(0x7a,[ibinop,shrInt],[]) :- !.
opcode(0x7b,[ibinop,shrInt],[]) :- !.%Long
opcode(0x7c,[ibinop,ushrInt],[]) :- !.
opcode(0x7d,[ibinop,ushrInt],[]) :- !.%Long
opcode(0x7e,[ibinop,andInt],[]) :- !.
opcode(0x7f,[ibinop,andInt],[]) :- !.%Long
opcode(0x80,[ibinop,orInt],[]) :- !.
opcode(0x81,[ibinop,orInt],[]) :- !.%Long
opcode(0x82,[ibinop,xorInt],[]) :- !.
opcode(0x83,[ibinop,xorInt],[]) :- !.%Long
opcode(0x84,[iinc],[1,-1]) :- wide_flag(off),!.
opcode(0x84,[iinc],[2,-2]) :- wide_flag(on),wide_flag_off,!.
opcode(0x85,[nop],[]) :- !.%i2l
opcode(0x86,[nop],[]) :- !.%i2f
opcode(0x87,[nop],[]) :- !.%i2d
opcode(0x88,[l2i],[]) :- !.
opcode(0x89,[nop],[]) :- !.%l2f
opcode(0x8a,[nop],[]) :- !.%l2d
opcode(0x8b,[f2i],[]) :- !.
opcode(0x8c,[f2i],[]) :- !.%f2l
opcode(0x8d,[nop],[]) :- !.%f2d
opcode(0x8e,[f2i],[]) :- !.%d2i
opcode(0x8f,[f2i],[]) :- !.%d2l
opcode(0x90,[nop],[]) :- !.%d2f
opcode(0x91,[i2b],[]) :- !.
opcode(0x92,[i2c],[]) :- !.
opcode(0x93,[i2s],[]) :- !.
opcode(0x94,[lcmp],[]) :- !.
opcode(0x95,[lcmp],[]) :- !.%fcmpl
opcode(0x96,[lcmp],[]) :- !.%fcmpg
opcode(0x97,[lcmp],[]) :- !.%dcmpl
opcode(0x98,[lcmp],[]) :- !.%dcmpg
opcode(0x99,[if0,eqInt],[-2]) :- !.
opcode(0x9a,[if0,neInt],[-2]) :- !.
opcode(0x9b,[if0,ltInt],[-2]) :- !.
opcode(0x9c,[if0,geInt],[-2]) :- !.
opcode(0x9d,[if0,gtInt],[-2]) :- !.
opcode(0x9e,[if0,leInt],[-2]) :- !.
opcode(0x9f,[if_icmp,eqInt],[-2]) :- !.
opcode(0xa0,[if_icmp,neInt],[-2]) :- !.
opcode(0xa1,[if_icmp,ltInt],[-2]) :- !.
opcode(0xa2,[if_icmp,geInt],[-2]) :- !.
opcode(0xa3,[if_icmp,gtInt],[-2]) :- !.
opcode(0xa4,[if_icmp,leInt],[-2]) :- !.
opcode(0xa5,[if_acmpeq],[-2]) :- !.                  %%??
opcode(0xa6,[if_acmpne],[-2]) :- !.
opcode(0xa7,[goto],[-2]) :- !.
opcode(0xa8,[jsr],[-2]) :- !.
opcode(0xa9,[ret],[1]) :- wide_flag(off), !.
opcode(0xa9,[ret],[2]) :- wide_flag(on), wide_flag_off, !.
opcode(0xaa,[tableswitch],tableswitch) :- !.
opcode(0xab,[lookupswitch],lookupswitch) :- !.
opcode(0xac,[ireturn],[]) :- !.
opcode(0xad,[ireturn],[]) :- !.%lreturn
opcode(0xae,[ireturn],[]) :- !.%freturn
opcode(0xaf,[ireturn],[]) :- !.%dreturn
opcode(0xb0,[areturn],[]) :- !.
opcode(0xb1,[return],[]) :- !.
opcode(0xb2,[getstatic],[2]) :- !.
opcode(0xb3,[putstatic],[2]) :- !.
opcode(0xb4,[getfield],[2]) :- !.
opcode(0xb5,[putfield],[2]) :- !.
opcode(0xb6,[invokevirtual],[2]) :- !.           %%??
opcode(0xb7,[invokespecial],[2]) :- !.
opcode(0xb8,[invokestatic],[2]) :- !.
opcode(0xb9,[invokeinterface],[2,1,1]) :- !.
opcode(0xba,[xxxunusedxxx1],[]) :- !.
opcode(0xbb,[new],[2]) :- !.
opcode(0xbc,[newarray],[1]) :- !.
opcode(0xbd,[anewarray],[2]) :- !.
opcode(0xbe,[arraylength],[]) :- !.
opcode(0xbf,[athrow],[]) :- !.
opcode(0xc0,[checkcast],[2]) :- !.
opcode(0xc1,[instanceof],[2]) :- !.
opcode(0xc2,[monitorenter],[]) :- !.
opcode(0xc3,[monitorexit],[]) :- !.
opcode(0xc4,[wide],[]) :- wide_flag_on, !.
opcode(0xc5,[multianewarray],[2,1]) :- !.
opcode(0xc6,[ifnull],[-2]) :- !.
opcode(0xc7,[ifnonnull],[-2]) :- !.
opcode(0xc8,[goto_w],[-4]) :- !.
opcode(0xc9,[jsr_w],[-4]) :- !.
opcode(0xca,[breakpoint],_) :- !.
opcode(0xfe,[impdep1],_) :- !.
opcode(0xff,[impdep2],_) :- !.
