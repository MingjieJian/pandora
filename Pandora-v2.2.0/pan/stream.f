      subroutine STREAM
     $(N,Z,HND,HEND,VBMB,FMV,FR, VM)
C
C     Rudolf Loeser, 1998 Mar 18
C---- Computes VM = mass motion velocity (km/s), for diffusion.
C
C     (If HEND is not available, use the following recipe:
C
C     call BRYMBO (N,RHEAB,HEND)
C     call ARRMUL (HEND,HND,HEND,N)  .)
C
C     !DASH
      save
C     !DASH
      real*8 CFH, CFHE, CMPKM, FMV, FR, HEND, HND, RAT, RH, RS, VBMB,
     $       VM, Z, dummy
      integer I, ION, MFMV, N
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(113),CFH  )
      equivalence (RZQ(152),CFHE )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(54),MFMV )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C     !DASH
      external ZERO1, PENDA, RUDIM, DIVIDE, ALKIMA, ARRMUL, HI, BYE
C
C               Z(N), HND(N), HEND(N), VBMB(N), FMV(N), FR(N), VM(N)
      dimension Z(*), HND(*), HEND(*), VBMB(*), FMV(*), FR(*), VM(*)
C     !EJECT
C
      call HI ('STREAM')
C     !BEG
      call ZERO1        (VM,N)
C
      call PENDA        (ION)
      if((ION.ge.1).and.(ION.le.3)) then
        if(ION.eq.1) then
C----     Hydrogen
          do 100 I = 1,N
            call RUDIM  (HND(I),HEND(I),dummy,RS)
            call DIVIDE (CFH,HND(I),RAT)
            VM(I) = (RAT-RS*VBMB(I))/CMPKM
  100     continue
        else
C----     Helium
          do 101 I = 1,N
            call RUDIM  (HND(I),HEND(I),RH,dummy)
            call DIVIDE (CFHE,HEND(I),RAT)
            VM(I) = (RAT+RH*VBMB(I))/CMPKM
  101     continue
        end if
C----   Geometrical term (FR .ne. 1 in spherical case)
        call ALKIMA (N,Z,HND,dummy,FR)
        do 102 I = 1,N
          call DIVIDE   (VM(I),(FR(I)**2),VM(I))
  102   continue
C----   Apply multiplier if needed
        if(MFMV.gt.0) then
          call ARRMUL   (VM,FMV,VM,N)
        end if
      end if
C     !END
      call BYE ('STREAM')
C
      return
      end
