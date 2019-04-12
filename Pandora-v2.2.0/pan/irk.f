      subroutine IRK
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 10
C---- Allocates scratch storage for FLIP.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NLM, NLMS, NLS, NNT, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('IRK')
C     !BEG
      call WGET (IS ,CALLER)
C
      NLM  = NL-1
      NLMS = NLM**2
      NLS  = NL**2
      NNT  = N*NT
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NLM
      IN( 3) = IN( 2)+NLS
      IN( 4) = IN( 3)+NLMS
      IN( 5) = IN( 4)+NLMS
      IN( 6) = IN( 5)+NNT
      IN( 7) = IN( 6)+NNT
      MUX    = IN( 7)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IRK')
C
      return
      end
