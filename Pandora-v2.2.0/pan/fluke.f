      subroutine FLUKE
     $(XNUU,XNUL,CVW)
C
C     Rudolf Loeser, 2005 Feb 23
C---- Computes the default value of CVW(u,l).
C     !DASH
      save
C     !DASH
      real*8 C1, C2, CVW, FAC, FEFF, TERM, XKVW, XNUK, XNUL, XNUU, ZEFF
      integer IONST
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
      equivalence (RZQ(  9),XNUK )
      equivalence (KZQ( 56),IONST)
C     !DASH
      external KNAVE, HI, BYE
C
      data FAC,C1,C2 /1.258D-5, 8.D-1, 2.5D0/
C
      call HI ('FLUKE')
C     !BEG
      call KNAVE (XNUU, FEFF)
C
      if(XNUU.ge.XNUK) then
        XKVW = C2
      else
        ZEFF = IONST
        TERM = (FEFF**2)/ZEFF
        XKVW = TERM**C1
      end if
C
      CVW = FAC*(XKVW/((XNUU-XNUL)**2))
C     !END
      call BYE ('FLUKE')
C
      return
      end
