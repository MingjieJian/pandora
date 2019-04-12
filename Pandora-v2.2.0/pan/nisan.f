      subroutine NISAN
     $(TI,TN,IRF,IFN,GIN)
C
C     Rudolf Loeser, 1981 Mar 30
C---- Computes GIJ for the last column.
C     !DASH
      save
C     !DASH
      real*8 D, E3, E4, GIN, HALF, THIRD, TI, TN, TWO, dummy
      logical IFN, IRF
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(13),THIRD )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external EXPINT, HI, BYE
C
      call HI ('NISAN')
C     !BEG
      call EXPINT     (4,TI,E4,dummy)
C
      GIN = THIRD-HALF*E4
C
      if(IFN) then
        if(IRF) then
          D = TWO*TN-TI
          call EXPINT (4,D,E4,dummy)
C
          GIN = GIN+HALF*E4
C
C
        end if
        D = TN-TI
        call EXPINT   (4,D,E4,dummy)
        if(IRF) then
C
          GIN = GIN-E4
C
        else
          call EXPINT (3,D,E3,dummy)
C
          GIN = GIN-HALF*(E4+TN*E3)
C
        end if
      end if
C     !END
      call BYE ('NISAN')
C
      return
      end
