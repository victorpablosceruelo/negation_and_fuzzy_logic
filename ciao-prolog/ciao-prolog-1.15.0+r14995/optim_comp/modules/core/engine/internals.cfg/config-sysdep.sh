#!/bin/sh
# Input: CIAOARCH

# Get OSNAME and ARCHNAME
case ${CIAOARCH} in
    SYMMi86)           OSNAME=Dynix; ARCHNAME=i86 ;;
    IRIXmips)          OSNAME=IRIX; ARCHNAME=mips ;;
    BSDi86)            OSNAME=LINUX; ARCHNAME=i86 ;; # TODO: correct???
    DARWINppc)         OSNAME=DARWIN; ARCHNAME=ppc ;;
    DARWINi86)         OSNAME=DARWIN; ARCHNAME=i86 ;;
    DARWINx86_64)      OSNAME=DARWIN; ARCHNAME=x86_64 ;;
    DARWINx86_64m32)   OSNAME=DARWIN; ARCHNAME=x86_64m32 ;;
    LINUXalpha)        OSNAME=LINUX; ARCHNAME=alpha ;;
    LINUXi86)          OSNAME=LINUX; ARCHNAME=i86 ;;
    LINUXx86_64)       OSNAME=LINUX; ARCHNAME=x86_64 ;;
    LINUXx86_64m32)    OSNAME=LINUX; ARCHNAME=x86_64m32 ;;
    LINUXppc)          OSNAME=LINUX; ARCHNAME=ppc ;;
    LINUXppc64)        OSNAME=LINUX; ARCHNAME=ppc64 ;;
    LINUXppc64m32)     OSNAME=LINUX; ARCHNAME=ppc64m32 ;;
    LINUXSparc)        OSNAME=LINUX; ARCHNAME=Sparc ;;
    LINUXSparc64)      OSNAME=LINUX; ARCHNAME=Sparc64 ;;
    LINUXarmv4l)       OSNAME=LINUX; ARCHNAME=armv4l ;;
    LINUXarmv5tel)     OSNAME=LINUX; ARCHNAME=armv5tel ;;
    Solarisi86)        OSNAME=Solaris; ARCHNAME=i86 ;;
    SolarisSparc)      OSNAME=Solaris; ARCHNAME=Sparc ;;
    SolarisSparc64)    OSNAME=Solaris; ARCHNAME=Sparc64 ;;
    SolarisSparc64m32) OSNAME=Solaris; ARCHNAME=Sparc64m32 ;;
    SunOS4Sparc)       OSNAME=SunOS4; ARCHNAME=Sparc ;;
    Win32alpha)        OSNAME=Win32; ARCHNAME=alpha ;;
    Win32i86)          OSNAME=Win32; ARCHNAME=i86 ;;
esac

# Set CC (C compiler) and LD (linker)
case ${CIAOARCH} in
    SYMMi86)           CC=cc;  LD=ld  ;;
    IRIXmips)          CC=cc;  LD=ld  ;;
    BSDi86)            CC=gcc; LD=ld  ;;
    DARWINppc)         CC=cc;  LD=cc  ;;
    DARWINi86)         CC=cc;  LD=cc  ;;
    DARWINx86_64)      CC=cc;  LD=cc  ;;
    DARWINx86_64m32)   CC=cc;  LD=cc  ;;
    LINUXalpha)        CC=gcc; LD=gcc ;;
    LINUXi86)          CC=gcc; LD=gcc  ;;
    LINUXx86_64)       CC=gcc; LD=gcc  ;;
    LINUXx86_64m32)    CC=gcc; LD=gcc  ;;
    LINUXppc)          CC=gcc; LD=gcc ;;
    LINUXppc64)        CC=gcc; LD=gcc ;;
    LINUXppc64m32)     CC=gcc; LD=gcc ;;
    LINUXSparc)        CC=gcc; LD=gcc ;;
    LINUXSparc64)      CC=gcc; LD=gcc ;;
    LINUXarmv4l)       CC=gcc; LD=ld ;;
    LINUXarmv5tel)     CC=arm-linux-gcc; LD=arm-linux-gcc; LCC=gcc; LLD=ld; ;; # TODO: LCC and LLD are for cross compiling... make sure it works!
    Solarisi86)        CC=gcc; LD=ld  ;;
    SolarisSparc)      CC=gcc; LD=ld  ;;
    SolarisSparc64)    CC=gcc; LD=ld  ;;
    SolarisSparc64m32) CC=gcc; LD=ld  ;;
    SunOS4Sparc)       CC=gcc; LD=ld  ;;
    Win32alpha)        CC=gcc; LD=gcc ;;
    Win32i86)          CC=gcc; LD=gcc ;;
esac

# Does this machine has dynamic linking abilities?
case ${CIAOARCH} in
    SYMMi86)    FOREIGN_FILES_FLAG= ;;
    Win32alpha) FOREIGN_FILES_FLAG= ;;
    Win32i86)   FOREIGN_FILES_FLAG= ;;
    *)          FOREIGN_FILES_FLAG="-DFOREIGN_FILES" ;;
esac

# Libraries and flags to use threads
if test x"${USE_THREADS}" = x"yes"; then
    case ${CIAOARCH} in
	SolarisSparc*)
	    case "`/bin/uname -r`" in
		5.[123456]* ) SOLARIS_VERSION=pre_7 ;;
		5.* )         SOLARIS_VERSION=equal_post_7 ;;
	    esac
	    THREAD_FLAG="-D_REENTRANT"
	    if test x"${SOLARIS_VERSION}" = x"pre_7"; then
		LD_THREAD_LIB="-lpthread"
	    else
		LD_THREAD_LIB="-lpthread -lrt"
	    fi
	    ;;
	IRIXmips)     
	    THREAD_FLAG=""
	    LD_THREAD_LIB="-lpthread" ;;
	BSDi86)       
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	DARWINppc)    
	    THREAD_FLAG="-D_REENTRANT"
	    ;;
	DARWINi86)    
	    THREAD_FLAG="-D_REENTRANT"
	    ;;
	DARWINx86_64)    
	    THREAD_FLAG="-D_REENTRANT"
	    ;;
	DARWINx86_64m32)    
	    THREAD_FLAG="-D_REENTRANT"
	    ;;
	LINUXalpha)   
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXi86)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXx86_64)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXx86_64m32)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXppc)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXppc64)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXppc64m32)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXSparc)   
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXSparc64) 
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXarmv4l)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	LINUXarmv5tel)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	Solarisi86)   
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	Win32i86)     
	    THREAD_FLAG="-D_REENTRANT"
	    LD_THREAD_LIB="-lpthread" ;;
	SunOS4Sparc)  
	    # threads not available?
	    THREAD_FLAG=
	    LD_THREAD_LIB= ;;
	Win32alpha)   
	    # threads not available?
	    THREAD_FLAG=
	    LD_THREAD_LIB= ;;
	SYMMi86)
	    # threads not available?
	    THREAD_FLAG=
	    LD_THREAD_LIB= ;;
    esac
fi

# C compiler options for generating shared libraries code
# Linker options for shared objects
case ${CIAOARCH} in
    SYMMi86)           CCSHARED= ; LDSHARED= ;;
    IRIXmips)          CCSHARED= ; LDSHARED="-shared" ;;
    BSDi86)            CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    DARWINppc)         CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -bundle -undefined suppress" ;;
    DARWINi86)         CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -bundle -undefined suppress" ;;
    DARWINx86_64)      CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -bundle -undefined suppress" ;;
    DARWINx86_64m32)   CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -bundle -undefined suppress -m32" ;;
    LINUXalpha)        CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXi86)          CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXx86_64)       CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXx86_64m32)    CCSHARED="-fPIC" ; LDSHARED="-shared -melf_i386 -m32" ;;
    LINUXppc)          CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXppc64)        CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXppc64m32)     CCSHARED="-fPIC" ; LDSHARED="-shared -melf32ppclinux -m32" ;;
    LINUXSparc)        CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXSparc64)      CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXarmv4l)       CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    LINUXarmv5tel)     CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    Solarisi86)        CCSHARED= ; LDSHARED="-G" ;;
    SolarisSparc)      CCSHARED= ; LDSHARED="-G" ;;
    SolarisSparc64)    CCSHARED= ; LDSHARED="-G" ;;
    SolarisSparc64m32) CCSHARED= ; LDSHARED="-G -m32" ;;
    SunOS4Sparc)       CCSHARED="-fPIC" ; LDSHARED= ;;
    Win32alpha)        CCSHARED= ; LDSHARED="-c" ;;
    Win32i86)          CCSHARED= ; LDSHARED="-c" ;;
esac

# POSIX locks
if test x"${OSNAME}" = x"Solaris"; then
    if test x"${USE_POSIX_LOCKS}" = x"yes"; then
	LD_LOCK_LIB="-lposix4"
    fi
fi

# Debug options
if test x"${DEBUG_LEVEL}" = x"debug"; then
  OPTIM_LEVEL="debug"
fi

# Optimizations
M_OR_F_OPTIONS="`${CONFIGURE_DIR}/gcc_m_or_f_options`"
case ${CIAOARCH} in
    SYMMi86)           OPTIM_FLAGS0= ;;
    IRIXmips)          OPTIM_FLAGS0="" ;;
    BSDi86)            OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    DARWINppc)         OPTIM_FLAGS0="" ;;
    DARWINi86)         OPTIM_FLAGS0="" ;;
    DARWINx86_64)      OPTIM_FLAGS0="" ;;
    DARWINx86_64m32)   OPTIM_FLAGS0="" ;;
    LINUXalpha)        OPTIM_FLAGS0="" ;;
    LINUXi86)          OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXx86_64)       OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXx86_64m32)    OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
    LINUXppc)          OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXppc64)        OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXppc64m32)     OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXSparc)        OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXSparc64)      OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXarmv4l)       OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    LINUXarmv5tel)     OPTIM_FLAGS0="-fomit-frame-pointer" ;;
    Solarisi86)        OPTIM_FLAGS0="" ;;
    SolarisSparc)      OPTIM_FLAGS0="" ;;
    SolarisSparc64)    OPTIM_FLAGS0="" ;;
    SolarisSparc64m32) OPTIM_FLAGS0="" ;;
    SunOS4Sparc)       OPTIM_FLAGS0="" ;;
    Win32alpha)        OPTIM_FLAGS0="" ;;
    Win32i86)          OPTIM_FLAGS0="-fomit-frame-pointer ${M_OR_F_OPTIONS}" ;;
esac
if test x"${OPTIM_LEVEL}" = x"optimized"; then
    OPTIM_FLAGS="-fno-strict-aliasing -O2 ${OPTIM_FLAGS0}"
else
    OPTIM_FLAGS="-O2"
fi

# Memory management options
MEM_MNG_FLAGS=""
case ${CIAOARCH} in
    DARWINppc)         MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    DARWINi86)         MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    DARWINx86_64m32)   MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    LINUXi86)          MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    LINUXx86_64m32)    MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    LINUXppc)          MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    LINUXppc64m32)     MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    SolarisSparc)      MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    SolarisSparc64)    MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    SolarisSparc64m32) MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    Win32i86)          MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
esac

# Architecture options
case ${CIAOARCH} in
    DARWINx86_64m32)   ARCH_FLAGS="-m32" ;;
    LINUXx86_64m32)    ARCH_FLAGS="-m32" ;;
    LINUXppc64m32)     ARCH_FLAGS="-m32" ;;
    SolarisSparc64)    ARCH_FLAGS="-m64" ;;
    SolarisSparc64m32) ARCH_FLAGS="-m32" ;;
    # Symmetry C compiler and linker specific options
    SYMMi86)           ARCH_FLAGS="-i" ;;
    # TODO: WHY??
    Solarisi86)        ARCH_FLAGS="-fPIC" ;;
    *)                 ARCH_FLAGS="" ;;
esac

# Set LDFLAGS
case ${CIAOARCH} in
    BSDi86)         LDFLAGS0="-rdynamic" ;;
    DARWINi86)       LDFLAGS0="-rdynamic" ;;
    DARWINx86_64)    LDFLAGS0="-rdynamic" ;;
    DARWINx86_64m32) LDFLAGS0="-rdynamic -m32" ;;
    LINUXalpha)     LDFLAGS0="-rdynamic" ;;
    LINUXi86)       LDFLAGS0="-rdynamic" ;;
    LINUXx86_64)    LDFLAGS0="-rdynamic" ;;
    LINUXx86_64m32) LDFLAGS0="-rdynamic -m32" ;;
    LINUXppc)       LDFLAGS0="-rdynamic" ;;
    LINUXppc64)     LDFLAGS0="-rdynamic" ;;
    LINUXppc64m32)  LDFLAGS0="-rdynamic -m32" ;;
    LINUXSparc)     LDFLAGS0="-rdynamic" ;;
    LINUXSparc64)   LDFLAGS0="-rdynamic" ;;
    LINUXarmv4l)    LDFLAGS0="-rdynamic" ;;
    LINUXarmv5tel)  LDFLAGS0="-rdynamic" ;;
    SolarisSparc64)    LDFLAGS0="-m64" ;;
    SolarisSparc64m32) LDFLAGS0="-m32" ;;
    Win32alpha)     LDFLAGS0="-static" ;;
    Win32i86)       LDFLAGS0="-static" ;;
esac

# Other libs
case ${CIAOARCH} in
    SYMMi86)           LIBS0="-lseq -lpps" ;;
    IRIXmips)          LIBS0="-lm" ;;
    BSDi86)            LIBS0="-lm" ;;
    Solarisi86)        LIBS0="-ldl -lm -lnsl" ;;
    SolarisSparc)      LIBS0="-ldl -lm -lnsl" ;;
    SolarisSparc64)    LIBS0="-ldl -lm -lnsl" ;;
    SolarisSparc64m32) LIBS0="-ldl -lm -lnsl" ;;
    DARWINi86)         LIBS0="-ldl -lm" ;;
    DARWINx86_64)      LIBS0="-ldl -lm" ;;
    DARWINx86_64m32)   LIBS0="-ldl -lm" ;;
    LINUXalpha)        LIBS0="-ldl -lm" ;;
    LINUXi86)          LIBS0="-ldl -lm" ;;
    LINUXx86_64)       LIBS0="-ldl -lm" ;;
    LINUXx86_64m32)    LIBS0="-ldl -lm" ;;
    LINUXppc)          LIBS0="-ldl -lm" ;;
    LINUXppc64)        LIBS0="-ldl -lm" ;;
    LINUXppc64m32)     LIBS0="-ldl -lm" ;;
    LINUXSparc)        LIBS0="-ldl -lm" ;;
    LINUXSparc64)      LIBS0="-ldl -lm" ;;
    LINUXarmv4l)       LIBS0="-ldl -lm" ;;
    LINUXarmv5tel)     LIBS0="-ldl -lm" ;;
    SunOS4Sparc)       LIBS0="-ldl -lm" ;;
esac

# Libraries used when creating so files
case ${OSNAME} in
    LINUX)             SOLIBS="-lc"
esac

LIBS="${LIBS0} ${LD_THREAD_LIB} ${LD_LOCK_LIB}"

