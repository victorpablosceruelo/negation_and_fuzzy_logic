:- module(opcodes,[opcode/3]).

:- dynamic wide_flag/1.

wide_flag(off).

wide_flag_on :-
	retract(wide_flag(_)),
	assert(wide_flag(on)).

wide_flag_off :-
	retract(wide_flag(_)),
	assert(wide_flag(off)).



opcode(0x00,nop,[]) :- !.
opcode(0x01,aconst_null,[]) :- !.
opcode(0x02,iconst_m1,[]) :- !.
opcode(0x03,iconst_0,[]) :- !.
opcode(0x04,iconst_1,[]) :- !.
opcode(0x05,iconst_2,[]) :- !.
opcode(0x06,iconst_3,[]) :- !.
opcode(0x07,iconst_4,[]) :- !.
opcode(0x08,iconst_5,[]) :- !.
opcode(0x09,lconst_0,[]) :- !.
opcode(0x0a,lconst_1,[]) :- !.
opcode(0x0b,fconst_0,[]) :- !.
opcode(0x0c,fconst_1,[]) :- !.
opcode(0x0d,fconst_2,[]) :- !.
opcode(0x0e,dconst_0,[]) :- !.
opcode(0x0f,dconst_1,[]) :- !.
opcode(0x10,bipush,[-1]) :- !.
opcode(0x11,sipush,[-2]) :- !.
opcode(0x12,ldc,[1]) :- !.
opcode(0x13,ldc_w,[2]) :- !.
opcode(0x14,ldc2_w,[2]) :- !.

opcode(0x15,iload,[1]) :- wide_flag(off), !.
opcode(0x15,iload,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x16,lload,[1]) :- wide_flag(off), !.
opcode(0x16,lload,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x17,fload,[1]) :- wide_flag(off), !.
opcode(0x17,fload,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x18,dload,[1]) :- wide_flag(off), !.
opcode(0x18,dload,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x19,aload,[1]) :- wide_flag(off), !.
opcode(0x19,aload,[2]) :- wide_flag(on), wide_flag_off, !.


opcode(0x1a,iload_0,[]) :- !.
opcode(0x1b,iload_1,[]) :- !.
opcode(0x1c,iload_2,[]) :- !.
opcode(0x1d,iload_3,[]) :- !.
opcode(0x1e,lload_0,[]) :- !.
opcode(0x1f,lload_1,[]) :- !.
opcode(0x20,lload_2,[]) :- !.
opcode(0x21,lload_3,[]) :- !.
opcode(0x22,fload_0,[]) :- !.
opcode(0x23,fload_1,[]) :- !.
opcode(0x24,fload_2,[]) :- !.
opcode(0x25,fload_3,[]) :- !.
opcode(0x26,dload_0,[]) :- !.
opcode(0x27,dload_1,[]) :- !.
opcode(0x28,dload_2,[]) :- !.
opcode(0x29,dload_3,[]) :- !.
opcode(0x2a,aload_0,[]) :- !.
opcode(0x2b,aload_1,[]) :- !.
opcode(0x2c,aload_2,[]) :- !.
opcode(0x2d,aload_3,[]) :- !.
opcode(0x2e,iaload,[]) :- !.
opcode(0x2f,laload,[]) :- !.
opcode(0x30,faload,[]) :- !.
opcode(0x31,daload,[]) :- !.
opcode(0x32,aaload,[]) :- !.
opcode(0x33,baload,[]) :- !.
opcode(0x34,caload,[]) :- !.
opcode(0x35,saload,[]) :- !.

opcode(0x36,istore,[1]) :- wide_flag(off), !.
opcode(0x36,istore,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x37,lstore,[1]) :- wide_flag(off), !.
opcode(0x37,lstore,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x38,fstore,[1]) :- wide_flag(off), !.
opcode(0x38,fstore,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x39,dstore,[1]) :- wide_flag(off), !.
opcode(0x39,dstore,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x3a,astore,[1]) :- wide_flag(off), !.
opcode(0x3a,astore,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0x3b,istore_0,[]) :- !.
opcode(0x3c,istore_1,[]) :- !.
opcode(0x3d,istore_2,[]) :- !.
opcode(0x3e,istore_3,[]) :- !.
opcode(0x3f,lstore_0,[]) :- !.
opcode(0x40,lstore_1,[]) :- !.
opcode(0x41,lstore_2,[]) :- !.
opcode(0x42,lstore_3,[]) :- !.
opcode(0x43,fstore_0,[]) :- !.
opcode(0x44,fstore_1,[]) :- !.
opcode(0x45,fstore_2,[]) :- !.
opcode(0x46,fstore_3,[]) :- !.
opcode(0x47,dstore_0,[]) :- !.
opcode(0x48,dstore_1,[]) :- !.
opcode(0x49,dstore_2,[]) :- !.
opcode(0x4a,dstore_3,[]) :- !.
opcode(0x4b,astore_0,[]) :- !.
opcode(0x4c,astore_1,[]) :- !.
opcode(0x4d,astore_2,[]) :- !.
opcode(0x4e,astore_3,[]) :- !.
opcode(0x4f,iastore,[]) :- !.
opcode(0x50,lastore,[]) :- !.
opcode(0x51,fastore,[]) :- !.
opcode(0x52,dastore,[]) :- !.
opcode(0x53,aastore,[]) :- !.
opcode(0x54,bastore,[]) :- !.
opcode(0x55,castore,[]) :- !.
opcode(0x56,sastore,[]) :- !.
opcode(0x57,pop,[]) :- !.
opcode(0x58,pop2,[]) :- !.
opcode(0x59,dup,[]) :- !.
opcode(0x5a,dup_x1,[]) :- !.
opcode(0x5b,dup_x2,[]) :- !.
opcode(0x5c,dup2,[]) :- !.
opcode(0x5d,dup2_x1,[]) :- !.
opcode(0x5e,dup2_x2,[]) :- !.
opcode(0x5f,swap,[]) :- !.
opcode(0x60,iadd,[]) :- !.
opcode(0x61,ladd,[]) :- !.
opcode(0x62,fadd,[]) :- !.
opcode(0x63,dadd,[]) :- !.
opcode(0x64,isub,[]) :- !.
opcode(0x65,lsub,[]) :- !.
opcode(0x66,fsub,[]) :- !.
opcode(0x67,dsub,[]) :- !.
opcode(0x68,imul,[]) :- !.
opcode(0x69,lmul,[]) :- !.
opcode(0x6a,fmul,[]) :- !.
opcode(0x6b,dmul,[]) :- !.
opcode(0x6c,idiv,[]) :- !.
opcode(0x6d,ldiv,[]) :- !.
opcode(0x6e,fdiv,[]) :- !.
opcode(0x6f,ddiv,[]) :- !.
opcode(0x70,irem,[]) :- !.
opcode(0x71,lrem,[]) :- !.
opcode(0x72,frem,[]) :- !.
opcode(0x73,drem,[]) :- !.
opcode(0x74,ineg,[]) :- !.
opcode(0x75,lneg,[]) :- !.
opcode(0x76,fneg,[]) :- !.
opcode(0x77,dneg,[]) :- !.
opcode(0x78,ishl,[]) :- !.
opcode(0x79,lshl,[]) :- !.
opcode(0x7a,ishr,[]) :- !.
opcode(0x7b,lshr,[]) :- !.
opcode(0x7c,iushr,[]) :- !.
opcode(0x7d,lushr,[]) :- !.
opcode(0x7e,iand,[]) :- !.
opcode(0x7f,land,[]) :- !.
opcode(0x80,ior,[]) :- !.
opcode(0x81,lor,[]) :- !.
opcode(0x82,ixor,[]) :- !.
opcode(0x83,lxor,[]) :- !.

opcode(0x84,iinc,[1,-1]) :- 
	wide_flag(off),
	!.
opcode(0x84,iinc,[2,-2]) :- 
	wide_flag(on),
	wide_flag_off,
	!.

opcode(0x85,i2l,[]) :- !.
opcode(0x86,i2f,[]) :- !.
opcode(0x87,i2d,[]) :- !.
opcode(0x88,l2i,[]) :- !.
opcode(0x89,l2f,[]) :- !.
opcode(0x8a,l2d,[]) :- !.
opcode(0x8b,f2i,[]) :- !.
opcode(0x8c,f2l,[]) :- !.
opcode(0x8d,f2d,[]) :- !.
opcode(0x8e,d2i,[]) :- !.
opcode(0x8f,d2l,[]) :- !.
opcode(0x90,d2f,[]) :- !.
opcode(0x91,i2b,[]) :- !.
opcode(0x92,i2c,[]) :- !.
opcode(0x93,i2s,[]) :- !.
opcode(0x94,lcmp,[]) :- !.
opcode(0x95,fcmpl,[]) :- !.
opcode(0x96,fcmpg,[]) :- !.
opcode(0x97,dcmpl,[]) :- !.
opcode(0x98,dcmpg,[]) :- !.
opcode(0x99,ifeq,[-2]) :- !.
opcode(0x9a,ifne,[-2]) :- !.
opcode(0x9b,iflt,[-2]) :- !.
opcode(0x9c,ifge,[-2]) :- !.
opcode(0x9d,ifgt,[-2]) :- !.
opcode(0x9e,ifle,[-2]) :- !.
opcode(0x9f,if_icmpeq,[-2]) :- !.
opcode(0xa0,if_icmpne,[-2]) :- !.
opcode(0xa1,if_icmplt,[-2]) :- !.
opcode(0xa2,if_icmpge,[-2]) :- !.
opcode(0xa3,if_icmpgt,[-2]) :- !.
opcode(0xa4,if_icmple,[-2]) :- !.
opcode(0xa5,if_acmpeq,[-2]) :- !.
opcode(0xa6,if_acmpne,[-2]) :- !.
opcode(0xa7,goto,[-2]) :- !.
opcode(0xa8,jsr,[-2]) :- !.

opcode(0xa9,ret,[1]) :- wide_flag(off), !.
opcode(0xa9,ret,[2]) :- wide_flag(on), wide_flag_off, !.

opcode(0xaa,tableswitch,tableswitch) :- !.
opcode(0xab,lookupswitch,lookupswitch) :- !.
opcode(0xac,ireturn,[]) :- !.
opcode(0xad,lreturn,[]) :- !.
opcode(0xae,freturn,[]) :- !.
opcode(0xaf,dreturn,[]) :- !.
opcode(0xb0,areturn,[]) :- !.
opcode(0xb1,return,[]) :- !.
opcode(0xb2,getstatic,[2]) :- !.
opcode(0xb3,putstatic,[2]) :- !.
opcode(0xb4,getfield,[2]) :- !.
opcode(0xb5,putfield,[2]) :- !.
opcode(0xb6,invokevirtual,[2]) :- !.
opcode(0xb7,invokespecial,[2]) :- !.
opcode(0xb8,invokestatic,[2]) :- !.
opcode(0xb9,invokeinterface,[2,1,1]) :- !.
opcode(0xba,xxxunusedxxx1,[]) :- !.
opcode(0xbb,new,[2]) :- !.
opcode(0xbc,newarray,[1]) :- !.
opcode(0xbd,anewarray,[2]) :- !.
opcode(0xbe,arraylength,[]) :- !.
opcode(0xbf,athrow,[]) :- !.
opcode(0xc0,checkcast,[2]) :- !.
opcode(0xc1,instanceof,[2]) :- !.
opcode(0xc2,monitorenter,[]) :- !.
opcode(0xc3,monitorexit,[]) :- !.

opcode(0xc4,wide,[]) :- wide_flag_on, !.

opcode(0xc5,multianewarray,[2,1]) :- !.
opcode(0xc6,ifnull,[-2]) :- !.
opcode(0xc7,ifnonnull,[-2]) :- !.
opcode(0xc8,goto_w,[-4]) :- !.
opcode(0xc9,jsr_w,[-4]) :- !.
opcode(0xca,breakpoint,_) :- !.
opcode(0xfe,impdep1,_) :- !.
opcode(0xff,impdep2,_) :- !.
