      subroutine JET
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1999 Nov 17
C---- Allocates scratch storage for NADINE.
C     (This is version 6 of JET.)
C     !DASH
      save
C     !DASH
      integer IN, IS, KBTMX, KRTMX, KSTMX, LDLMX, MUX, N, NT, NTLDL,
     $        NTN
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C
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
      equivalence (LEST(60),KBTMX)
      equivalence (LEST(61),KRTMX)
      equivalence (LEST(62),KSTMX)
C     !DASH
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JET')
C     !BEG
      call WGET (IS,  CALLER)
C
      NTLDL = NT*LDLMX
      NTN   = NT*N
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NT
      IN( 3) = IN( 2)+NT
      IN( 4) = IN( 3)+NTLDL
      IN( 5) = IN( 4)+NTLDL
      IN( 6) = IN( 5)+NTLDL
      IN( 7) = IN( 6)+NT
      IN( 8) = IN( 7)+NTN
      IN( 9) = IN( 8)+NTN
      IN(10) = IN( 9)+NTLDL
      IN(11) = IN(10)+NTLDL
C
      IN(12) = IN(11)+NTLDL
      IN(13) = IN(12)+NT
      IN(14) = IN(13)+NT
      IN(15) = IN(14)+NT
      IN(16) = IN(15)+NT
      IN(17) = IN(16)+NT
      IN(18) = IN(17)+NT
      IN(19) = IN(18)+NT
      IN(20) = IN(19)+NT*KBTMX
      IN(21) = IN(20)+NT*KRTMX
C
      IN(22) = IN(21)+NT*KSTMX
      IN(23) = IN(22)+NT
      IN(24) = IN(23)+NTN
      IN(25) = IN(24)+N
      MUX    = IN(25)+NT
C
      call WLCK (MUX,  CALLER)
C     !END
      call BYE ('JET')
C
      return
      end
