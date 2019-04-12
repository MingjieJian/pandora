      subroutine MUDIR
     $(HND,HK,H1,HEND,HE1,BETA,HE2K,RH,RS,R1,RK,XK1,XK2,XK3,XK4,XK5)
C
C     Rudolf Loeser, 1989 Oct 13
C---- Computes coefficients for diffusion calculations.
C     (This is version 2 of MUDIR.)
C     !DASH
      save
C     !DASH
      real*8 BETA, H1, HE1, HE2K, HEND, HK, HND, R1, RAT, RH, RK, RS,
     $       XK1, XK2, XK3, XK4, XK5
C     !DASH
      external RUDIM, DIVIDE, HI, BYE
C
      call HI ('MUDIR')
C     !BEG
      call RUDIM  (HND,HEND,RH,RS)
C
      call DIVIDE (H1,HND,R1)
      call DIVIDE (HK,HND,RK)
C
      call DIVIDE ((BETA+HE2K),HEND ,XK1)
      call DIVIDE (HE1        ,HEND ,XK2)
      call DIVIDE (HE2K       ,HEND ,XK3)
      call DIVIDE (HE1+BETA   ,HEND ,XK4)
C
      call DIVIDE (HE2K,HE1,RAT)
      XK5 = -RAT*XK4
C     !END
      call BYE ('MUDIR')
C
      return
      end
