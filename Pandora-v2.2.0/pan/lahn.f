      subroutine LAHN
     $(ITAU,N,NL,XM,CQSI,B)
C
C     Rudolf Loeser, 1988 Jul 28
C---- Computes b's, for RHEIN.
C     (This is version 2 of LAHN.)
C     !DASH
      save
C     !DASH
      real*8 B, CQSI, XM
      integer I, ITAU, N, NL
C     !DASH
      external SUMPROD, HI, BYE
C
C               XM(NL,NL), CQSI(N,NSL), B(NL)
      dimension XM(NL,*),  CQSI(N,*),   B(*)
C
      call HI ('LAHN')
C     !BEG
      do 100 I = 1,NL
        call SUMPROD (B(I),XM(I,1),NL,CQSI(ITAU,1),N,NL)
  100 continue
C     !END
      call BYE ('LAHN')
C
      return
      end
