      subroutine EPEE
     $(ITAU,IN,IDG,IDB,GMI,BDI,N,GB)
C
C     Rudolf Loeser, 1987 Dec 11
C---- Computes a common term, for COMPLEX.
C     (This is version 2 of EPEE.)
C     !DASH
      save
C     !DASH
      real*8 B, BDI, G, GB, GMI
      integer IDB, IDG, IN, ITAU, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               GMI(N,NSL), BDI(N,NL)
      dimension GMI(N,*),   BDI(N,*)
C
      call HI ('EPEE')
C     !BEG
      call DIVIDE (BDI(ITAU,IN), BDI(ITAU,IDB), B)
      call DIVIDE (GMI(ITAU,IN), GMI(ITAU,IDG), G)
C
      GB = G*B
C     !END
      call BYE ('EPEE')
C
      return
      end
