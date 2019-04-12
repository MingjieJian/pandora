      subroutine BAKHA
     $(N,XRKH,XRLH,XRK,XRL)
C
C     Rudolf Loeser, 1990 Nov 21
C---- Sums upper-level charge exchange input data from various ions.
C     !DASH
      save
C     !DASH
      real*8 XRK, XRKH, XRL, XRLH
      integer J, K, N, NN
C     !COM
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
      external ZERO1, ARRADD, HI, BYE
C
C               XRKH(N,NPQLM,NXI), XRK(N,NPQLM), XRLH(N,NPQLM,NXI),
      dimension XRKH(N,NPQLM,*),   XRK(N,*),     XRLH(N,NPQLM,*),
C
C               XRL(N,NPQLM)
     $          XRL(N,*)
C
      call HI ('BAKHA')
C     !BEG
      NN = N*NPQMX
      call ZERO1        (XRK, NN)
      call ZERO1        (XRL, NN)
C
      if(NPQMX.ge.4) then
        do 101 K = 1,NXI
          do 100 J = 4,NPQMX
            call ARRADD (XRKH(1,J,K), XRK(1,J), XRK(1,J), N)
            call ARRADD (XRLH(1,J,K), XRL(1,J), XRL(1,J), N)
  100     continue
  101   continue
      end if
C     !END
      call BYE ('BAKHA')
C
      return
      end
