      subroutine KOA
     $(XK,KK,Y,AK,W,IW,KODE)
C
C     Rudolf Loeser, 1968 May 06
C---- Controls the calculation of the weights AK for Lyman source
C     function frequency summations.
C
C---- Upon return, KODE = 1 if this calculation seems OK; = 0 if not.
C     !DASH
      save
C     !DASH
      real*8 AK, ONE, W, XK, Y
      integer IFF, IGG, IN, IS, IW, IXI, KK, KODE, MOX
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  JONAH, CHERRY, WGIVE, HI, BYE
      intrinsic max,min
C
      dimension W(*), IW(*)
C
C               XK(KK), AK(KK)
      dimension XK(*),  AK(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IFF   ),(IN( 2),IGG   ),(IN( 3),IXI   )
C
      call HI ('KOA')
C     !BEG
      if(KK.eq.1) then
        AK(KK) = ONE
        KODE = 1
      else
C       (Get, and allocate, W allotment)
        call JONAH  (IN, IS, MOX, 'KOA', KK)
C
        call CHERRY (XK, KK, Y, AK, W(IFF), W(IGG), W(IXI), W, IW,
     $               KODE)
C
C       (Give back W allotment)
        call WGIVE  (W, 'KOA')
      end if
      KODE = max(min(KODE,1),0)
C     !END
      call BYE ('KOA')
C
      return
      end
