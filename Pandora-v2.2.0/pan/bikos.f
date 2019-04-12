      subroutine BIKOS
     $(N,NL,RKI,IQRK,RLI,IQRL,LUP,XRK,XRL,ESG)
C
C     Rudolf Loeser, 1990 Nov 27
C---- Applies upper level charge exchange effects to
C     RKI and RLI, for Hydrogen.
C     NL .gt. 3 is required!
C     !DASH
      save
C     !DASH
      real*8 ESG, RKI, RLI, XRK, XRL
      integer IQRK, IQRL, J, LUP, N, NL
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
      external ARRADD, DIVSUM, LUCAIR, HI, BYE
C
C               RKI(N,NSL), RLI(N,NSL), ESG(N,NL), IQRK(NSL), IQRL(NSL),
      dimension RKI(N,*),   RLI(N,*),   ESG(N,*),  IQRK(*),   IQRL(*),
C
C               XRK(N,NPQLM), XRL(N,NPQLM)
     $          XRK(N,*),     XRL(N,*)
C
      call HI ('BIKOS')
C     !BEG
      do 100 J = 4,NL
        if(IQRK(J).gt.0) then
          call ARRADD (RKI(1,J),XRK(1,J),RKI(1,J),N)
        end if
        if(IQRL(J).gt.0) then
          call DIVSUM (XRL(1,J),ESG(1,J),RLI(1,J),N)
        end if
  100 continue
C
C---- Print
      call LUCAIR     (N,NL,LUP,RKI,IQRK,RLI,IQRL,ESG)
C     !END
      call BYE ('BIKOS')
C
      return
      end
