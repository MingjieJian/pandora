      subroutine HAMRY
     $(NL,NSL,NTE,RFAC,CII,CEI)
C
C     Rudolf Loeser, 2002 Apr 29
C---- Global adjustment of collision rates.
C     !DASH
      save
C     !DASH
      real*8 CEI, CII, ONE, RFAC
      integer MUL, NL, NSL, NTE, NVCE, NVCI
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external CONMUL, HI, BYE
C
C               CII(NTE,NSL), CEI(NTE,MUL)
      dimension CII(*),       CEI(*)
C
      call HI ('HAMRY')
C     !BEG
      if(RFAC.ne.ONE) then
C
        NVCI = NTE*NSL
        call CONMUL (RFAC, CII, NVCI)
C
        MUL  = (NL*(NL-1))/2
        NVCE = NTE*MUL
        call CONMUL (RFAC, CEI, NVCE)
C
      end if
C     !END
      call BYE ('HAMRY')
C
      return
      end
