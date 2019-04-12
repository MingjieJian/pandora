      subroutine MOLLY
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1985 Jul 18
C---- Allocates scratch storage for PENNY.
C     (This is version 3 of MOLLY.)
C     !DASH
      save
C     !DASH
      integer IN, IS, KK, KTRN, LDLMX, MUX, N, NK, NKLD, NLD
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS(20),KTRN )
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(33),LDLMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MOLLY')
C     !BEG
      call WGET (IS, CALLER)
C
      KK   = KTRN*KTRN
      NK   = N*KTRN
      NLD  = N*LDLMX
      NKLD = NK*LDLMX
C     !EJECT
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+KTRN
      IN( 4) = IN( 3)+NK
      IN( 5) = IN( 4)+NLD
      IN( 6) = IN( 5)+NLD
      IN( 7) = IN( 6)+NKLD
      IN( 8) = IN( 7)+NKLD
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+NK
      IN(11) = IN(10)+NLD
C
      IN(12) = IN(11)+NK
      IN(13) = IN(12)+NK
      IN(14) = IN(13)+NK
      IN(15) = IN(14)+NK
      IN(16) = IN(15)+NK
      IN(17) = IN(16)+NK
      IN(18) = IN(17)+NK
      IN(19) = IN(18)+NK
      IN(20) = IN(19)+NK
      IN(21) = IN(20)+NK
C
      IN(22) = IN(21)+NK
      IN(23) = IN(22)+NK
      IN(24) = IN(23)+KK
      IN(25) = IN(24)+KTRN
      IN(26) = IN(25)+KTRN
      IN(27) = IN(26)+N
      IN(28) = IN(27)+NK
      IN(29) = IN(28)+NK
      IN(30) = IN(29)+N
      IN(31) = IN(30)+NK
C
      IN(32) = IN(31)+N
      IN(33) = IN(32)+NK
      IN(34) = IN(33)+NK
      IN(35) = IN(34)+NK
      IN(36) = IN(35)+N
      MUX    = IN(36)+NK
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MOLLY')
C
      return
      end
