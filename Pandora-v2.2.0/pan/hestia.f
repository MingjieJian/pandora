      subroutine HESTIA
     $(N,NSHL,CSHL,XASHL,WNSHL,MRR,CDSK,XADSK,WNDSK,XM)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Computes an XM matrix, for angle-dependent Continuum Calculations.
C     (See also NOTIUM.)
C     !DASH
      save
C     !DASH
      real*8 CDSK, CSHL, WNDSK, WNSHL, XADSK, XASHL, XM
      integer I, M, MRR, N, N2, NSHL
C     !DASH
      external  ZERO1, ARTEMIS, TATAR, NEGATE, UNITADD, HI, BYE
C
C               CSHL(N,NSHL), XASHL(N,N,NSHL), WNDSK(N,N,MRR), XM(N,N),
      dimension CSHL(N,*),    XASHL(N,N,*),    WNDSK(N,N,*),   XM(*),
C
C               CDSK(N,MRR), XADSK(N,N,MRR), WNSHL(N,N,NSHL)
     $          CDSK(N,*),   XADSK(N,N,*),   WNSHL(N,N,*)
C
      call HI ('HESTIA')
C     !BEG
C---- Initialize
      N2 = N**2
      call ZERO1     (XM,N2)
C---- Accumulate angle sums
C     Shell part
      I  = 0
      do 100 M = 1,NSHL
        call TATAR   (I)
        call ARTEMIS (I,N,CSHL(1,M),XASHL(1,1,M),WNSHL(1,1,M),XM)
  100 continue
C     Disk part
      do 101 M = 1,MRR
        call ARTEMIS (N,N,CDSK(1,M),XADSK(1,1,M),WNDSK(1,1,M),XM)
  101 continue
C---- Final subtraction from unit matrix
      call NEGATE    (XM,N2)
      call UNITADD   (1,N,XM,N)
C     !END
      call BYE ('HESTIA')
C
      return
      end
