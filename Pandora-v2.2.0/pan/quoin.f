      subroutine QUOIN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1998 Jun 29
C---- Allocates scratch storage for OBELISK.
C     !DASH
      save
C     !DASH
      integer IN, IS, M, M2, MM, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external  WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('QUOIN')
C     !BEG
      call WGET (IS ,CALLER)
C
      M  = 2*N
      MM = M**2
      M2 = M+M
C     !EJECT
      IN( 1) = IS
      IN( 2) = IN( 1)+MM
      IN( 3) = IN( 2)+MM
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      IN(19) = IN(18)+M2
      IN(20) = IN(19)+M2
      IN(21) = IN(20)+N
C
      IN(22) = IN(21)+N
      IN(23) = IN(22)+4*N
      IN(24) = IN(23)+N
      IN(25) = IN(24)+N
      IN(26) = IN(25)+N
      IN(27) = IN(26)+N
      IN(28) = IN(27)+N
      IN(29) = IN(28)+N
      IN(30) = IN(29)+N
      IN(31) = IN(30)+N
C
      IN(32) = IN(31)+N
      IN(33) = IN(32)+N
      IN(34) = IN(33)+N
      IN(35) = IN(34)+N
      IN(36) = IN(35)+N
      IN(37) = IN(36)+N
      IN(38) = IN(37)+N
      IN(39) = IN(38)+N
      IN(40) = IN(39)+N
      IN(41) = IN(40)+N
C
      MUX    = IN(41)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('QUOIN')
C
      return
      end
