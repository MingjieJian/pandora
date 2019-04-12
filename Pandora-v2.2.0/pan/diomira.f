      subroutine DIOMIRA
     $(K,TMUN,N,CNDT,YNT,YNTA)
C
C     Rudolf Loeser, 2000 Jul 19
C---- Includes the radiation from the illuminating star in the
C     computed emergent line spectrum.
C     (This is version 2 of DIOMIRA.)
C     !DASH
      save
C     !DASH
      real*8 CNDT, EX, TMUN, YNT, YNTA
      integer I, K, N
C     !DASH
      external HI, BYE
C
C               TMUN(KM), CNDT(N), YNT(KM), YNTA(KM)
      dimension TMUN(*),  CNDT(*), YNT(*),  YNTA(*)
C
      call HI ('DIOMIRA')
C     !BEG
      do 100 I = 1,K
        EX      = exp(-TMUN(I))
        YNTA(I) = YNT(I)+EX*CNDT(N)
  100 continue
C     !END
      call BYE ('DIOMIRA')
C
      return
      end
