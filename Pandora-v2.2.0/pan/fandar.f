      subroutine FANDAR
     $(LU,KODE,LAB,N,Z,JQ,JX,VE,VP,VH,V1,V2,V3)
C
C     Rudolf Loeser, 1990 Apr 29
C---- Plots diffusion velocities
C     !DASH
      save
C     !DASH
      real*8 V1, V2, V3, VE, VH, VMAX, VMIN, VP, Z
      integer JQ, JX, KODE, LU, N
      logical OK
      character LAB*(*)
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external NAMMA, BORLEY, NORI, ESAN, HI, BYE
C
C               Z(N), VE(N), VP(N), VH(N), V1(N), V2(N), V3(N)
      dimension Z(*), VE(*), VP(*), VH(*), V1(*), V2(*), V3(*)
C
      call HI ('FANDAR')
C     !BEG
C---- Get function limits
      call NAMMA  (VE,VP,VH,V1,V2,V3,JQ,JX,VMIN,VMAX)
C---- Initialize plot image
      call BORLEY (IMAGE,VMIN,VMAX,Z,JQ,JX,KODE,OK)
      if(OK) then
C----   Enter data
        call NORI (IMAGE,JQ,JX,Z,VE,ALPHS( 5),KODE)
        call NORI (IMAGE,JQ,JX,Z,VP,ALPHS(16),KODE)
        call NORI (IMAGE,JQ,JX,Z,VH,ALPHS( 8),KODE)
        call NORI (IMAGE,JQ,JX,Z,V1,NUMBS( 2),KODE)
        call NORI (IMAGE,JQ,JX,Z,V2,NUMBS( 3),KODE)
        call NORI (IMAGE,JQ,JX,Z,V3,NUMBS( 4),KODE)
C----   Print plot
        call ESAN (LU,IMAGE,LAB)
      end if
C     !END
      call BYE ('FANDAR')
C
      return
      end
