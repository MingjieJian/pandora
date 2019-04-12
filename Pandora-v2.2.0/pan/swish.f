      subroutine SWISH
     $(KAMB,MN1,HND,H1,HEND,HE1,HE21,Z,G,F,ZXH,H,N1MET,IVLSW,VEC,
     $ W,IW,DUMP)
C
C     Rudolf Loeser, 1997 Aug 05
C---- Computes h (and ZXH) and IVLSW, for diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 F, G, H, H1, HE1, HE21, HEND, HND, VEC, W, Z, ZXH
      integer IVLSW, IW, KAMB, MN1, N1MET
      logical DUMP
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, SIGMA, LOMBERS, ARRDIV, ARRMUL, ARRSUB, HI, BYE
C
      dimension W(*), IW(*)
C
C               H1(N), ZXH(N), HE21(N), Z(N), G(N), F(N), VEC(N), H(N),
      dimension H1(*), ZXH(*), HE21(*), Z(*), G(*), F(*), VEC(*), H(*),
C
C               HE1(N), HND(N), HEND(N)
     $          HE1(*), HND(*), HEND(*)
C
      call HI ('SWISH')
C     !BEG
C---- Compute ZXH
      if(KAMB.eq.1) then
        call ARRDIV (H1,   HND,  VEC, MN1)
      else if(KAMB.eq.2) then
        call ARRDIV (HE1,  HEND, VEC, MN1)
      else if(KAMB.eq.3) then
        call ARRDIV (HE21, HEND, VEC, MN1)
      else
        write (MSSLIN(1),100) KAMB
  100   format('KAMB =',I12,', which is not 1, 2, or 3.')
        call HALT ('SWISH', 1)
      end if
      call SIGMA    (MN1, Z, VEC, ZXH, 'ZXH', W, IW)
C
C---- Compute h
      call ARRMUL   (F, ZXH, H, MN1)
      call ARRSUB   (G, H,   H,  MN1)
C
C---- Set up VELSW
      call LOMBERS  (H, ZXH, MN1, N1MET, IVLSW, DUMP)
C     !END
      call BYE ('SWISH')
C
      return
      end
