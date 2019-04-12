      subroutine MEULAN
     $(N,IN1,HND,H1,HK,RHAB,HEND,HE1,HEK,BETA,HE21,HE2K,RHEAB,PNF,
     $ SHE,SHE2,BETAR,DZB)
C
C     Rudolf Loeser, 1997 May 30
C---- Prints "populations" for diffusion calculations.
C     (This is version 2 of MEULAN.)
C     !DASH
      save
C     !DASH
      real*8 BETA, BETAR, DZB, H1, HE1, HE21, HE2K, HEK, HEND, HK, HND,
     $       PNF, RHAB, RHEAB, SHE, SHE2
      integer IBTSW, IN1, IN1S, LUEO, N
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
      equivalence (KZQ(166),IBTSW)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, EMILIA, BUTTER, KUTUZ, HI, BYE
C
C               H1(N), HK(N), HE1(N), HEK(N), BETA(N), HE21(N), PNF(N),
      dimension H1(*), HK(*), HE1(*), HEK(*), BETA(*), HE21(*), PNF(*),
C
C               SHE2(N), HEND(N), RHEAB(N), RHAB(N), HE2K(N), BETAR(N),
     $          SHE2(*), HEND(*), RHEAB(*), RHAB(*), HE2K(*), BETAR(*),
C
C               HND(N), SHE(N), DZB(N)
     $          HND(*), SHE(*), DZB(*)
C
      data IN1S /-1/
C     !EJECT
C
      call HI ('MEULAN')
C     !BEG
      if(IN1S.ne.0) then
        call LINER  (2, LUEO)
        write (LUEO,100) IN1
  100   format(' ','Starting values of "Populations", N1-iter =',I3)
C
C----   Hydrogen data
        call LINER  (2, LUEO)
        call EMILIA (LUEO, N, HND, RHAB, H1, HK)
C
C----   Helium data, part 1
        call LINER  (2, LUEO)
        call BUTTER (LUEO, N, SHE, HEK, BETA, SHE2, HE2K, HE1, HE21,
     $               IBTSW)
C
C----   Helium data, part 2
        call LINER  (2, LUEO)
        call KUTUZ  (LUEO, N, SHE, BETA, HE2K, HEND, RHEAB, PNF,
     $               BETAR, DZB)
C
      end if
C
      IN1S = IN1
C     !END
      call BYE ('MEULAN')
C
      return
      end
