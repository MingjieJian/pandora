      subroutine HIP
     $(I,N,NL,M,GMI,GM,XND,XN,NOK,BDI,BD)
C
C     Rudolf Loeser, 1974 Mar 20
C---- Gets various parameters at depth I, for HAMMER.
C     !DASH
      save
C     !DASH
      real*8 BD, BDI, GM, GMI, XN, XND
      integer I, LGT, M, N, NL
      logical NOK
C     !DASH
      external MOVED, PLUSD, HI, BYE
C
C               GMI(N,NSL), XND(N,NL), BDI(N,NL), GM(NL), XN(NL), BD(NL)
      dimension GMI(N,*),   XND(N,*),  BDI(N,*),  GM(*),  XN(*),  BD(*)
C
      call HI ('HIP')
C     !BEG
      call MOVED   (GMI(I,1), N, NL, GM, 1, NL)
      call MOVED   (XND(I,1), N, NL, XN, 1, NL)
      call MOVED   (BDI(I,1), N, NL, BD, 1, NL)
C
      if(I.eq.1) then
        call PLUSD (XND(1,M), 1, N, LGT)
        NOK = LGT.eq.N
      end if
C     !END
      call BYE ('HIP')
C
      return
      end
