      subroutine UNIFORM
     $(MET, N,PEN,FEN,EPN,EFN,BSN, PE,FE,EP,EF,BS, KNT)
C
C     Rudolf Loeser, 1984 Oct 24
C---- Selects the Statistical Equilibrium results indicated by MET.
C     (This is version 2 of UNIFORM.)
C     !DASH
      save
C     !DASH
      real*8 BS, BSN, EF, EFN, EP, EPN, FE, FEN, PE, PEN
      integer J, KNT, MET, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MOVE1, HALT, HI, BYE
C
C               PEN(N,KNT), PE(N), FEN(N,KNT), FE(N), EPN(N,KNT), EP(N),
      dimension PEN(N,*),   PE(*), FEN(N,*),   FE(*), EPN(N,*),   EP(*),
C
C               EFN(N,KNT), EF(N), BSN(N,KNT), BS(N)
     $          EFN(N,*),   EF(*), BSN(N,*),   BS(*)
C
      call HI ('UNIFORM')
C     !BEG
      if((MET.lt.0).or.(MET.ge.KNT)) then
        write (MSSLIN(1),100) MET,KNT
  100   format('MET =',I12,', KNT =',I12,'; MET is out-of-range.')
        call HALT ('UNIFORM',1)
      end if
C
      J = MET+1
C
      call MOVE1  (PEN(1,J),N,PE)
      call MOVE1  (FEN(1,J),N,FE)
      call MOVE1  (EPN(1,J),N,EP)
      call MOVE1  (EFN(1,J),N,EF)
      call MOVE1  (BSN(1,J),N,BS)
C     !END
      call BYE ('UNIFORM')
C
      return
      end
