      subroutine NINLIL
     $(TI,TN,IRF,IFN,GI1)
C
C     Rudolf Loeser, 1981 Mar 30
C---- Computes GIJ for the first column.
C     !DASH
      save
C     !DASH
      real*8 D, E3, GI1, HALF, T1N, TI, TN, TWO, dummy
      logical IFN, IRF
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external EXPINT, HI, BYE
C
      call HI ('NINLIL')
C     !BEG
      call EXPINT   (3,TI,E3,dummy)
C
      GI1 = HALF*E3
C
C
      if(IFN) then
        if(IRF) then
          T1N = TWO*TN
        else
          T1N = TN
        end if
        D = T1N-TI
        call EXPINT (3,D,E3,dummy)
C
        GI1 = GI1-HALF*E3
C
      end if
C     !END
      call BYE ('NINLIL')
C
      return
      end
