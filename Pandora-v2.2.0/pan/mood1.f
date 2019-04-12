      subroutine MOOD1
     $(IN,MUX)
C
C     Rudolf Loeser, 1999 Dec 03
C---- Allocates the Line Intensity data block, part 1.
C     !DASH
      save
C     !DASH
      integer IN, JUBT, JURT, JUST, KBTMX, KM, KRTMX, KS, KSTMX, LDLMX,
     $        MUX, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(36),KS )
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
      external  HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MOOD1')
C     !BEG
      JUST = max(KSTMX,KS)
      JUBT = max(KBTMX,JUST)
      JURT = max(KRTMX,JUST)
C
      IN( 1) = 1
C
      IN( 2) = IN( 1)+1
      IN( 3) = IN( 2)+1
      IN( 4) = IN( 3)+KM
      IN( 5) = IN( 4)+1
      IN( 6) = IN( 5)+1
      IN( 7) = IN( 6)+1
      IN( 8) = IN( 7)+LDLMX
      IN( 9) = IN( 8)+LDLMX
      IN(10) = IN( 9)+LDLMX
      IN(11) = IN(10)+1
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N*LDLMX
      IN(14) = IN(13)+N
      IN(15) = IN(14)+KM
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      IN(19) = IN(18)+N
      IN(20) = IN(19)+N
      IN(21) = IN(20)+N
C
      IN(22) = IN(21)+N
      IN(23) = IN(22)+N
      IN(24) = IN(23)+N
      IN(25) = IN(24)+N
      IN(26) = IN(25)+N
      IN(27) = IN(26)+N
      IN(28) = IN(27)+N
      IN(29) = IN(28)+N
      IN(30) = IN(29)+N
      IN(31) = IN(30)+N
C
      IN(32) = IN(31)+LDLMX
      IN(33) = IN(32)+N
      IN(34) = IN(33)+N
      IN(35) = IN(34)+LDLMX
      IN(36) = IN(35)+N
      IN(37) = IN(36)+N
      IN(38) = IN(37)+1
      IN(39) = IN(38)+1
      IN(40) = IN(39)+1
      IN(41) = IN(40)+1
C
      IN(42) = IN(41)+1
      IN(43) = IN(42)+1
      IN(44) = IN(43)+1
      IN(45) = IN(44)+1
      IN(46) = IN(45)+N
      IN(47) = IN(46)+N
      IN(48) = IN(47)+N
      IN(49) = IN(48)+N
      IN(50) = IN(49)+1
      IN(51) = IN(50)+1
C
      IN(52) = IN(51)+N
      IN(53) = IN(52)+N
      IN(54) = IN(53)+1
      IN(55) = IN(54)+3*LDLMX
      IN(56) = IN(55)+LDLMX
      IN(57) = IN(56)+JUBT
      IN(58) = IN(57)+JURT
      IN(59) = IN(58)+JUST
      IN(60) = IN(59)+KM
      IN(61) = IN(60)+1
C
      IN(62) = IN(61)+1
      IN(63) = IN(62)+1
      IN(64) = IN(63)+1
      IN(65) = IN(64)+N
      IN(66) = IN(65)+N
      IN(67) = IN(66)+N
      MUX    = IN(67)+N
C     !END
      call BYE ('MOOD1')
C
      return
      end
