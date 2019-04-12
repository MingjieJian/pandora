      subroutine MOUND
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Allocates scratch storage for HOTROD.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NDT, NNDT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(21),NDT)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('MOUND')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNDT = N*NDT
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNDT
      IN( 3) = IN( 2)+NNDT
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+NDT
      IN( 6) = IN( 5)+NDT
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+NNDT
      IN(10) = IN( 9)+NNDT
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N*N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+MIKLEN
      IN(18) = IN(17)+N
      IN(19) = IN(18)+NNDT
      IN(20) = IN(19)+NNDT
      IN(21) = IN(20)+NNDT
C
      IN(22) = IN(21)+NDT
      IN(23) = IN(22)+NDT
      IN(24) = IN(23)+NDT
      IN(25) = IN(24)+NDT
      MUX    = IN(25)+3*NDT
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MOUND')
C
      return
      end
