      subroutine BISHOP
     $(N,NLH,HNI,TE,XNE,XNP,LIMP,W)
C
C     Rudolf Loeser, 1981 May 29
C---- Sets up default values of Hydrogen number densities.
C     NLH is the number of the highest level for which data
C     have already been computed.
C     !DASH
      save
C     !DASH
      real*8 HNI, TE, W, XNE, XNP
      integer IAA, IBB, IEE, IFF, IN, IS, LIMP, MOX, N, NLH
C     !DASH
      external JUPITER, MADDER, MULLEIN, MALLOW, WGIVE, HI, BYE
C
      dimension W(*)
C
C               HNI(N,LIMP), TE(N), XNE(N), XNP(N)
      dimension HNI(*),      TE(*), XNE(*), XNP(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IAA   ),(IN( 2),IBB   ),(IN( 3),IEE   ),(IN( 4),IFF   )
C
      call HI ('BISHOP')
C     !BEG
      if(NLH.lt.LIMP) then
C       (Get, and allocate, W allotment)
        call JUPITER   (IN,IS,MOX,'BISHOP')
C
C----   Compute for last level.
        call MADDER    (LIMP,N,TE,XNE,XNP,HNI)
C
        if((NLH+1).lt.LIMP) then
C----     Yes, more than just the last level is needed
C
C----     Compute level-dependent intermediates.
          call MULLEIN (LIMP,NLH,W(IAA),W(IBB),W(IEE),W(IFF))
C----     Compute for remaining levels.
          call MALLOW  (TE,N,NLH,LIMP,HNI,W(IAA),W(IBB),W(IEE),W(IFF))
        end if
C
C       (Give back W allotment)
        call WGIVE     (W,'BISHOP')
      end if
C     !END
      call BYE ('BISHOP')
C
      return
      end
