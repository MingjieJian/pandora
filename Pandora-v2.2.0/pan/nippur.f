      subroutine NIPPUR
     $(TI,TJ,TN,Y,IRF,GIJ)
C
C     Rudolf Loeser, 1981 Mar 30
C---- Computes GIJ for interior columns.
C     !DASH
      save
C     !DASH
      real*8 D, GIJ, GP, TI, TJ, TN, TWO, Y
      logical IRF
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external BEST, HI, BYE
C
      call HI ('NIPPUR')
C     !BEG
      call BEST   (TI,TJ,Y,GIJ)
C
      if(IRF) then
        D = TWO*TN-TI
        call BEST (D,TJ,Y,GP)
        GIJ = GIJ-GP
      end if
C     !END
      call BYE ('NIPPUR')
C
      return
      end
