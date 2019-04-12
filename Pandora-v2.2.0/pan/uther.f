      subroutine UTHER
     $(FIN,FAN,DL,FIS)
C
C     Rudolf Loeser, 1983 Mar 22
C---- Computes "Line without continuum".
C     !DASH
      save
C     !DASH
      real*8 DL, FAN, FIN, FIS, TWO, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
      call HI ('UTHER')
C     !BEG
      if(FIN.eq.ZERO) then
        FIS = ZERO
      else
        FIS = FIN-TWO*abs(DL)*FAN
      end if
C     !END
      call BYE ('UTHER')
C
      return
      end
