      subroutine BUTTEM
     $(A,INCA,N,SUM)
C     Rudolf Loeser, 1997 Dec 22
C---- Gets running integrals, for BUSH.
C     !DASH
      save
C     !DASH
      real*8 A, SUM
      integer I, IA, INCA, IP, N, NA, NMO
C     !DASH
      dimension A(*)
C
C     !BEG
      NMO = N-1
      if(N.gt.3) then
        IA = 1+INCA
        do 100 I = 3,NMO
          IP = IA
          IA = IA+INCA
          A(IA) = A(IP)+A(IA)
  100   continue
      end if
      NA = 1+INCA*NMO
      A(NA) = SUM
C     !END
C
      return
      end
