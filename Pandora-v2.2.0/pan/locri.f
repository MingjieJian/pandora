      subroutine LOCRI
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2004 Aug 18
C---- Allocates scratch storage for LIME.
C     (This is version 2 of LOCRI.)
C     !DASH
      save
C     !DASH
      integer IN, IS, K, KK, KKX, MUX, N, NCR, NKK, NL, NN, NNCR, NNKK,
     $        NNL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(10),KK )
      equivalence (JZQ(32),NCR)
      equivalence (JZQ(12),KKX)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('LOCRI')
C     !BEG
      call WGET (IS,  CALLER)
C
      K = max(KK,KKX)
C
      NN   = N*N
      NKK  = N*K
      NNL  = N*NL
      NNCR = N*NCR
      NNKK = N*NKK
C     !EJECT
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NKK
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NN
      IN( 5) = IN( 4)+NKK
      IN( 6) = IN( 5)+NKK
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+NKK
      IN(11) = IN(10)+NKK
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      IN(19) = IN(18)+NNCR
      IN(20) = IN(19)+N
      IN(21) = IN(20)+NKK
C
      IN(22) = IN(21)+NKK
      IN(23) = IN(22)+NKK
      IN(24) = IN(23)+NNL
      IN(25) = IN(24)+N
      IN(26) = IN(25)+N
      IN(27) = IN(26)+N
      IN(28) = IN(27)+N
      IN(29) = IN(28)+N
      IN(30) = IN(29)+N
      IN(31) = IN(30)+NKK
C
      IN(32) = IN(31)+NKK
      IN(33) = IN(32)+N
      IN(34) = IN(33)+NNKK
      IN(35) = IN(34)+MIKLEN
      IN(36) = IN(35)+N
      IN(37) = IN(36)+N
      IN(38) = IN(37)+N
      IN(39) = IN(38)+K
      IN(40) = IN(39)+K
      IN(41) = IN(40)+K
C
      MUX    = IN(41)+NKK
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LOCRI')
C
      return
      end
