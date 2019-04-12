      program PANDORA
C
C
C
C
C
C
C
C
C
C
C
C
C     PANDORA is a comprehensive model stellar atmosphere
C     and radiative line transfer program.
C
C
C
C
C
C     Built for Eugene Avrett by Rudolf Loeser
C
C     Center for Astrophysics
C     Harvard and Smithsonian Observatories
C     Cambridge, Massachusetts
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C.......................................................................
C
C     PANDORA ('all gifts'): According to Hesiod, Pandora was the
C             first woman and was created by Zeus to punish man after
C             Prometheus had created and helped the human race. She
C             came with a box or storage-jar in which all evils and
C             diseases were stored, and when Prometheus's guileless
C             brother Epimetheus married her and opened the box, all
C             these escaped, leaving only hope at the bottom to be
C             some alleviation of the troubles let loose upon the
C             world. 'Pandora's box' became proverbial. ...
C
C                                 from: "Who's Who in the Ancient World"
C                                        by Betty Radice (Penguin Books)
C.......................................................................
C     !EJECT
C     G e n e r a l   b a c k g r o u n d                  (2001 Nov 02)
C
C
C     Originally built for the CONTROL DATA 6400, beginning in 1966.
C     This system had a small memory, necessitating the use of
C     overlays for code, and much scratch I/O for data. However, its
C     single-precision floating point operand had a large exponent
C     range and sufficient precision for PANDORA.
C       Transferred, during 1979, to the DEC VAX11/780 running VMS.
C     This system had a large virtual memory and consequently did
C     not provide an overlay mechanism. Calculations had to be done
C     in IEEE-standard double precision ("G-floating"): (a) to achieve
C     sufficient precision where needed, and (b) to obtain a sufficient
C     exponent range.
C       Transferred, during Spring 1996, to the DEC Alpha running
C     DEC Unix and the DEC Unix f77 compiler.
C       Transferred, during Fall 2001, to a SUN workstation running
C     Solaris and the SUN f77 compiler (which accepts many non-
C     standard extensions of DEC's compiler).
C
C
C     S o u r c e   C o d e                                (2002 Aug 13)
C
C
C     Currently the source code is close to the Fortran-77 standard.
C                            H O W E V E R :
C     names use up to 8 characters (i.e. more than 6 characters), and
C     labelled common blocks are used in a non-standard way (see below:
C     "Shared data structures").
C
C       All PANDORA source files have the suffix ff (i.e. ---.ff). They
C     all must use the "alpha" preprocessor before they can be compiled
C     ("alpha" produces the actual compilable source file), and should
C     use the "omega" post-processor after compilation ("omega" produces
C     a clean listing). Subroutine calls are checked for correctness by
C     the JANUS2 system.
C       I use the script " f o g " to "compile" PANDORA source files:
C     "fog" accepts a "source" file (---.ff), invokes alpha, f77, and
C     omega to produce listing (---.lis) and object (---.o) files,
C     and then uses JANUS2 to check subroutine calls.
C
C       PANDORA source files are grouped into three classes and are kept
C     in three different directories:
C
C        1) PANDORA-specific modules are in directory "pan" (with object
C           modules in panlib.a; listings are in directory "panlis");
C
C        2) General utility modules are in directory "zoo" (with object
C           modules in zoolib.a; listings are in directory "zoolis");
C
C        3) System-dependent modules are in directory "sys" (with object
C           modules in syslib.a; listings are in directory "syslis").
C
C     (When PANDORA is linked, these libraries should be searched in
C     this order: first panlib.a, then zoolib.a, finally syslib.a .)
C     !EJECT
C     S h a r e d   d a t a   s t r u c t u r e s          (2000 Nov 28)
C
C
C     Almost all of PANDORA's working data reside in many "labelled
C     common" blocks. These are described, collected and initialized
C     in this main program and in subroutine BOX.
C
C     Important Note:
C
C     PANDORA uses labelled common blocks in a  N O N - S T A N D A R D
C     manner: not all specifications (i.e. occurences of) any given
C     block are identical. Indeed,
C        (1) names and uses of particular parts in them may differ, and
C        (2) lengths may differ.
C
C     As far as (1) goes, it is up to the programmer (i.e. me) to see
C     to it that things "make sense."
C
C     As far as (2) goes, it is  E X P E C T E D / R E Q U I R E D
C     that the linker allocates enough memory to accommodate the longest
C     version of each block. To help insure that this happens,
C        (a) every labelled common block is referred to in one of two
C            key modules: in this main program, or in subroutine BOX;
C        (b) the versions of the labelled common blocks used in these
C            two modules should always be the latest ones; thus, one
C            or the other of these modules must be recompiled when a
C            labelled common block is changed;
C        (c) the object files for PANDORA and BOX must be loaded first
C            (and all other modules thereafter).
C
C     (This procedure makes it unnecessary to recompile EVERY module
C     using a given labelled common block when that block is changed
C     in a way which does not affect how it is used in other modules.)
C
C
C     S c r a t c h  I / O                                 (2001 Nov 02)
C
C
C     ---of which PANDORA does a great deal---is 'done' "in-memory"
C     and/or to disk. In-memory scratch I/O occurs if it is enabled
C     (i.e. the input parameter ISCRS retained its default value, 0)
C     and as long as there is sufficient room in the memory-resident
C     scratch I/O buffer (see below: "Memory usage"). If that buffer
C     is "full" (or if its use was never enabled), then scratch I/O
C     uses (or overflows to) the disk file 01.
C       In-memory scratch I/O uses the MEMOIR subroutine package while
C     disk scratch I/O uses the RAFAEL subroutine package (both in
C     directory "sys"); these routines are accessed via the VISAYAS
C     subroutine package (in directory "pan").
C     !EJECT
C     M e m o r y   U s a g e                              (2002 Mar 19)
C
C
C     Three large blocks of memory, allocated when this main program
C     source file is compiled, largely determine the size of the program
C     address space the linker must claim for PANDORA. The sizes of
C     these blocks are set by the parameters LNGTHX, LNGTHI, and LENMEM
C     (defined below). Memory within blocks "X" and "IX" (the general
C     data blocks and scratch areas) is allocated dynamically at
C     run-time (only the amount then actually needed is claimed); if
C     "too much" is ever needed for a particular run, the run aborts.
C
C
C     Run-time storage allocation
C
C     Both permanent floating point (real*8) data arrays, as well as
C     temporary scratch arrays needed at various times during the
C     calculation, are consolidated into a single array X, of length
C     LNGTHX (this value is set at compile-time; see below). Portions
C     of X needed for such permanent and scratch arrays are allocated
C     at run-time, when actual needs are known. (Thus the amount of
C     reserved-but-unused storage is minimized.)
C       Permanent arrays are allocated first, in the low-index region
C     of X. When all of these have been set up (in subroutine CHURN;
C     see the main program source code, below), elements of X(i) up to
C     i = IBSCR are committed. The remaining portion of X, which is
C     available for scratch, is thence referred to by the name W.
C       Scratch arrays are allocated in W as needed, as follows. The
C     value of the index IS designates the next available free element
C     of W. Three subroutines are used to manage storage in W:
C     1) WGET returns the current value of IS;
C     2) WLCK increases the value of IS by a specified amount;
C     3) WRLS reduces the value of IS to what it was before the
C             corresponding call to WLCK.
C     (The necessary history of successive IS values is kept in a
C     pushdown stack; this stack, and associated control parameters,
C     are in labelled common block WORLD.)
C
C     Permanent integer (integer*4) data arrays and scratch arrays are
C     provided separately, but in the same manner: the consolidated
C     array is IX (aka IW); the permanent region extends up to JBSCR;
C     the management routines are IGET, ILCK, and IRLS; and the
C     associated labelled common block is IWORLD.
C
C     (The first elements of X and IX remain reserved and are not
C     allocated; they are used for run-time consistency checks that
C     guard against errors.)
C
C     PANDORA runs usually do not use all of X and IX as allocated
C     here, leaving a sizeable chunk of reserved-but-unused memory.
C     Still, this procedure for allocating arrays at run-time has
C     proven to be economical, effective, and flexible. Computers
C     using 64-bit processors routinely have so much installed RAM
C     that X and/or IX (and/or SIOBUF, the in-memory scratch buffer;
C     see below) could be made much bigger if desired. (Problems may
C     arise with some operating systems that have arbitrary limits
C     for program size or swap file size.)
C     !EJECT
C     E x a m p l e
C
C     To understand how this works, consider an actual example:
C     subroutine WHEEL, whose main purpose is to set up scratch storage
C     for BUDDHA.
C       WHEEL sets up floating point scratch storage arrays by calling
C     the "allocator" KRISHNA to compute the values of array starting
C     indices collected in the "allocation table" IN. KRISHNA calls
C     WGET for IS, which becomes the value of the first element of IN.
C     The remaining elements of IN are then computed from the known
C     lengths of the various desired scratch arrays. Finally KRISHNA
C     computes MUX, which becomes the new value of IS when KRISHNA
C     calls WLCK with it: thus the required amount of scratch storage
C     is "locked" up. (Note that almost every allocator is custom-built
C     for the routine that uses it [very few actual allocators are used
C     by more than one caller in PANDORA].)
C       WHEEL sets up integer scratch storage analogously, by calling
C     the allocator RANULF to initialize the allocation table JN.
C       WHEEL then calls BUDDHA; most of the arguments in the calling
C     sequence are indexed array references that transfer the addresses
C     of the first element of various arrays. (Note that several of
C     these are "permanent" arrays in X; the allocation table for
C     these is called IZOQ and is kept in labelled common "MANAGER".)
C       Thus WHEEL transmits to BUDDHA the addresses of the scratch
C     arrays (carved out of W and IW) that BUDDHA wants. Look at the
C     formal parameter list of BUDDHA to see how these array addresses
C     arrive there, and how BUDDHA uses them. (BUDDHA hardly uses them
C     at all, passing them on to its callees which in turn then modify
C     the values of some of the array elements.)
C       When control returns from BUDDHA, the scratch storage arrays
C     are no longer needed and WHEEL releases them by calling WGIVE
C     and IGIVE, which call WRLS and IRLS, respectively.
C       Note that BUDDHA also receives W and IW themselves. This is
C     because some of BUDDHA's callees also set up additional scratch
C     storage for themselves. For example subroutine WEATHER, which is
C     called from LARISA via CARINA, sets up integer scratch storage.
C     Look at WEATHER to see that it, among other things, sets up
C     scratch storage for OOBLECK using this same procedure.
C       (By the way: each module that requests allocation of scratch
C     storage thus MUST release that allocation before returning to
C     its caller.)
C
C
C
C
C     And so throughout PANDORA. Indeed, allocators MANAGE and MINIGE
C     are used (in CHURN, see below), in just this way, to set up the
C     allocation tables IZOQ and JZOQ for the permanent floating point
C     and integer data arrays, respectively. This requires PANDORA to
C     know all run-time array lengths before it can allocate X and IX.
C     This is insured by requiring that input values of array lengths
C     may be specified only in part "B" of the input (which is read
C     and processed before any array element values are read).
C     !EJECT
C     I / O   f i l e s                                    (2002 Mar 18)
C
C
C     PANDORA maintains a one-to-one correspondence between I/O files
C     and logical unit numbers (LUN). Thus a file is known by its LUN.
C     Files are named, defined and described in the following table:
C
C     LUMA      01   Scratch file
C
C     LUGI      02   "General" data input file
C     LUIN      03   Regular data input file
C     LUMO      04   Model atmosphere data input file
C     LUAT      07   Atomic model data input file
C     LURE      08   Iterative restart data input file
C     LUJI      09   Iterative PRD Jnu values input file
C     LUKU      10   Statistical line opacity data input file
C     LUCM      11   Composite line opacity data input file
C     LUKA      12   Averaged line opacity data input file
C
C     LURO      15   Regular printout file
C     LUIX      31   Printout index file
C     LUEO (16) 15   Error printout file
C     LURS (17) 15   "Switched" regular printout file (not used yet)
C     LUDO      18   "Deferred" regular printout file (not used yet)
C
C     LURR      19   Iterative restart data output file
C     LUMR      20   Miscellaneous restart data output file
C     LUPR      21   Populations restart data output file
C     LUJO      22   Iterative PRD Jnu restart output file
C     LUIS      30   "Iterative studies" output file
C     LUCS      32   Checksums output file
C     LUSO      23   Special spectrum data save file
C     LUCR      24   Cooling rates output file
C     LUNC      25   "Spectrum Summary Plots" data output file
C     LUSM      26   Sample matrices output file
C     LUSF      27   Source-Function-related data
C
C     LUPD      28   Performance data output (archive) file
C     LUCA      29   NUDEAL (input reading) journal file
C     LUWM      97   world/iworld dump output file
C     LUSD      98   "system data" input file (written by 'uname -a')
C     LUHB      99   Hi/Bye/Abort-system dump output file
C
C     These LUN definitions are kept in labelled common block LUNITS.
C     !EJECT
C     !DASH
      real*8 SIOBUF, X
      integer IX, LNGTHI, LNGTHX, MELIMT
C     !DASH
C
C
C
C     On this and the following page are declared the
C
C     M a j o r   S t o r a g e   B l o c k s
C
C
C
      parameter ( L N G T H X  = 50 000 000)
C
      dimension   X(LNGTHX)
      common      /TERRA/ X
C
C---- WORLD       as of 2002 Jun 04
C
      integer     LISTK
      parameter   (LISTK = 100)
      integer     ISTCK,INEXT,ILMIT,IUMAX,IUKNT
      dimension   ISTCK(LISTK)
      common      /WORLD/ ISTCK,INEXT,ILMIT,IUMAX,IUKNT
C     Management of floating point working/scratch storage in X
C     - ISTCK is the allocation stack
C     - INEXT is the stack index for the next allocation
C     - ILMIT is the length of X
C     - IUMAX and IUKNT are cumulative usage statistics.
C     .
      data        ILMIT /LNGTHX/
C
C---- WSAFE       as of 1997 Oct 31
      real*8      XSIGNAL
      common      /WSAFE/ XSIGNAL
C     .
      data        XSIGNAL /317.D0/
C
C---- SALLOC      as of 1997 Oct 31
      integer     ISALLOC
      common      /SALLOC/ ISALLOC
C     Index of first allocatable cell in WORLD and IWORLD
C     (To reserve initial cells for error checking).
C     .
      data        ISALLOC /8/
C     !EJECT
      parameter ( L N G T H I  = 2 000 000)
C
      dimension   IX(LNGTHI)
      common      /JERRY/ IX
C
C---- IWORLD      as of 2002 Jun 04
C
      integer     LJSTK
      parameter   (LJSTK = 100)
      integer     JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
      dimension   JSTCK(LJSTK)
      common      /IWORLD/ JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
C     Management of integer working/scratch storage in IX
C     - JSTCK is the allocation stack
C     - JNEXT is the stack index for the next allocation
C     - JLMIT is the length of IX
C     - JUMAX and JUKNT are cumulative usage statistics.
C     .
      data        JLMIT /LNGTHI/
C
C---- IWSAFE      as of 1997 Oct 31
      integer     IXSIGNL
      common      /IWSAFE/ IXSIGNL
C     .
      data        IXSIGNL /317/
C
      parameter ( L E N M E M  = 25 000 000)
C
      dimension   SIOBUF(LENMEM)
      common      /CORTEX3/ MELIMT
      common      /CORTEX4/ SIOBUF
      data        MELIMT /LENMEM/
C     !EJECT
      external BOX, HUMBER, CHURN, YARTY, LARK, TAW
C
C     !BEG
C
C
C---- Initialize universal and derived constants
      call BOX
C
C---- Initialization 1:
C     read Parts A & B of the input file(s)
C     (so that table lengths are known for the next step);
C  ** Also: initialize Hi/Bye/Abort System
C     (which depends on part B input)
      call HUMBER (X, IX)
C
C---- Allocate general storage blocks X and IX, and
C     set up scratch/working storage blocks W (in X) and IW (in IX)
      call CHURN  (X, IX)
C
C---- Initialization 2:
C     read rest of input file(s), and do other initializations
      call YARTY  (X, IX, X, IX)
C
C
C---- Execute
      call LARK   (X, IX, X, IX)
C
C
C---- Wrap up
C  ** Also: close out Hi/Bye/Abort System
      call TAW
C
C---- That's all!
      stop 'PANDORA done'
C
C
C     !END
C
      end
