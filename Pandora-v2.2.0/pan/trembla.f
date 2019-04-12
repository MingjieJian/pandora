      subroutine TREMBLA
     $(N,HND,HEND,VBMB,BETA,HE2K,XND,ZETA)
C
C     Rudolf Loeser, 1998 Jul 24
C---- Computes Helium-I ZETA, for "diffusion".
C     !DASH
      save
C     !DASH
      real*8 BETA, CFHE, CMPKM, HE2K, HEND, HND, RH, T1, T2, VBMB, XND,
     $       ZETA, dummy
      integer I, N
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
      equivalence (RZQ(152),CFHE )
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
      external RUDIM, HI, BYE
C
C               XND(N,NL), HEND(N), VBMB(N), BETA(N), HE2K(N), ZETA(N),
      dimension XND(N,*),  HEND(*), VBMB(*), BETA(*), HE2K(*), ZETA(*),
C
C               HND(N)
     $          HND(*)
C
      call HI ('TREMBLA')
C     !BEG
      do 100 I = 1,N
        call RUDIM (HND(I),HEND(I),RH,dummy)
        T1 = -((BETA(I)+HE2K(I))/HEND(I))*CFHE
        T2 = (XND(I,1)*VBMB(I))*RH
C
        ZETA(I) = (T1+T2)/CMPKM
  100 continue
C     !END
      call BYE ('TREMBLA')
C
      return
      end
