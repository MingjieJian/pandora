      subroutine JIMSON
     $(L,XLM,TE,HN,BD,TERM)
C
C     Rudolf Loeser, 1974 May 24
C---- Computes a term for GINSENG.
C     (This is version 4 of JIMSON.)
C     !DASH
      save
C     !DASH
      real*8 BD, HN, HNUKT, ONE, POFL, RAT, SE, TE, TERM, XLM, ZERO
      integer L
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external TARGET, PROD, DIVIDE, HI, BYE
C
      call HI ('JIMSON')
C     !BEG
      call TARGET   (L,XLM,TERM)
      if(TERM.ne.ZERO) then
        call PROD   (TE,XLM,2,HNUKT,SE)
        call DIVIDE (SE,BD,RAT)
        POFL = (L**5)
        TERM = (HN/POFL)*(ONE-RAT)*TERM
      end if
C     !END
      call BYE ('JIMSON')
C
      return
      end
