      subroutine DRUMMER
     $(N,J,K,DL,WVL,EMU,VXA,VP,DV,DLS,DLSMIN,DLSMAX,IMIN,IMAX,KRED,DLR,
     $ SNUR,S,CALLER)
C
C     Rudolf Loeser, 1983 Sep 08
C---- Dumps, for shifted-SNU calculation.
C     !DASH
      save
C     !DASH
      real*8 DL, DLR, DLS, DLSMAX, DLSMIN, DV, EMU, S, SNUR, VP, VXA,
     $       WVL
      integer I, IMAX, IMIN, J, K, KRED, LUEO, N
      character CALLER*(*), LAB*30
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, SULFUR, MASHED, HI, BYE
C
C               DL(K), EMU(N), VXA(N), VP(N), DV(N), DLS(N), DLR(KRED),
      dimension DL(*), EMU(*), VXA(*), VP(*), DV(*), DLS(*), DLR(*),
C
C               SNUR(N,KRED) S(N)
     $          SNUR(*),     S(*)
C
      data LAB /'Unshifted SNU (reduced table)'/
C     !EJECT
C
      call HI ('DRUMMER')
C     !BEG
      call LINER  (1, LUEO)
      write (LUEO,100) WVL,DL(J),J
  100 format(' ','Core Wavelength =',1PE16.8,', displacement ',
     $           '(Delta-Lambda) =',E16.8,5X,'(',I3,')')
C
      call VECOUT (LUEO, DL, K, 'Unshifted Delta-Lambda')
C
      call LINER  (2, LUEO)
      write (LUEO,102)
  102 format(' ',18X,'VXA',14X,'MU',3X,'projected VXA',11X,'shift',
     $           6X,'shifted DL',5X,'shifted SNU')
      call LINER  (1, LUEO)
      write (LUEO,103) (I,VXA(I),EMU(I),VP(I),DV(I),DLS(I),S(I),I=1,N)
  103 format(5(' ',I5,1P6E16.8/))
C
      call LINER  (1, LUEO)
      write (LUEO,104) DLSMIN,IMIN,DLSMAX,IMAX
  104 format(' ','DLSMIN =',1PE12.4,' (',I4,')',5X,
     $           'DLSMAX =',  E12.4,' (',I4,')')
C
      call LINER  (2, LUEO)
      call SULFUR (LUEO, N, KRED, SNUR, DLR, LAB)
C
      call MASHED (CALLER)
C     !END
      call BYE ('DRUMMER')
C
      return
      end
