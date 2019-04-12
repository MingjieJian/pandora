      subroutine KRAAL
     $(C,SUM,F,N,NOPAC,KONFRM)
C
C     Rudolf Loeser, 1973 Feb 06
C---- Sets up a contributions array for printing.
C     !DASH
      save
C     !DASH
      real*8 C, F, FAC, SUM
      integer J, KONFRM, N, NOPAC
C     !DASH
      external DIVIDE, CONMUL, HI, BYE
C
C               SUM(N), C(Nopac,N)
      dimension SUM(*), C(NOPAC,*)
C
      call HI ('KRAAL')
C     !BEG
      if(KONFRM.eq.2) then
        do 100 J = 1,N
          call DIVIDE (F, SUM(J), FAC)
          call CONMUL (FAC, C(1,J), NOPAC)
  100   continue
      end if
C     !END
      call BYE ('KRAAL')
C
      return
      end
