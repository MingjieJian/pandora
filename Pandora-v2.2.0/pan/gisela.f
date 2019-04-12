      subroutine GISELA
     $(N,HELABD,DGM,F,G)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Computes G for H.S.E.
C     (This is version 2 of GISELA.)
C     !DASH
      save
C     !DASH
      real*8 CGR, CON14, DGM, F, FAC, G, HELABD, HEMASS, ONE, XNUM
      integer I, N
      logical KILROY
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
      equivalence (RZQ( 16),CGR  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 5),HEMASS)
C     !DASH
C     !EJECT
      external RIGEL, DIVIDE, HI, BYE
C
C               HELABD(N), F(N), G(N), DGM(N)
      dimension HELABD(*), F(*), G(*), DGM(*)
C
      data KILROY /.true./
C
      call HI ('GISELA')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL  (14, CON14)
        FAC = CGR*CON14
      end if
C
      do 100 I = 1,N
        XNUM = FAC*(ONE+HEMASS*HELABD(I))*DGM(I)
        call DIVIDE (XNUM, F(I), G(I))
  100 continue
C     !END
      call BYE ('GISELA')
C
      return
      end
