      subroutine CALLIAS
     $(N,HND,HEND,VBMB,XNK,XND,ZETA)
C
C     Rudolf Loeser, 1998 Jul 24
C---- Computes Hydrogen ZETA, for "diffusion".
C     (This is version 5 of CALLIAS.)
C     !DASH
      save
C     !DASH
      real*8 CFH, CMPKM, HEND, HND, RS, T1, T2, VBMB, XND, XNK, ZETA,
     $       dummy
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
      equivalence (RZQ(113),CFH  )
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
C               XND(N,NL), HND(N), HEND(N), VBMB(N), XNK(N), ZETA(N)
      dimension XND(N,*),  HND(*), HEND(*), VBMB(*), XNK(*), ZETA(*)
C
      call HI ('CALLIAS')
C     !BEG
      do 100 I = 1,N
        call RUDIM (HND(I),HEND(I),dummy,RS)
        T1 = -(XNK(I)/HND(I))*CFH
        T2 = -(XND(I,1)*VBMB(I))*RS
C
        ZETA(I) = (T1+T2)/CMPKM
  100 continue
C     !END
      call BYE ('CALLIAS')
C
      return
      end
