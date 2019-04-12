      subroutine LAOS
     $(KCOMP,KPRNT,KPLOT, X,IX,W,IW)
C
C     Rudolf Loeser, 1997 Sep 26
C---- Number Densities and Departure Coefficients.
C     (This is version 3 of LAOS.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IHSLT, ITHSL, IW, IX, KCOMP, KPLOT, KPRNT, LU, MO, NOION
      logical COMP, LAST, PLOT
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
      equivalence (KZQ( 20),IHSLT)
      equivalence (KZQ( 94),NOION)
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
      equivalence (LEST(19),ITHSL)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external KNACK, BRAMBLE, DIAMOND, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('LAOS')
C     !BEG
      COMP = (KCOMP.gt.0)
      LAST = (MO.gt.0).and.(ITHSL.eq.IHSLT)
      PLOT = (KPLOT.gt.0).and.LAST
C
      if((COMP.or.PLOT).and.(NOION.le.0)) then
        call KNACK     (KPRNT, LU)
C
        if(COMP) then
          call BRAMBLE (X, IX, W, IW, LU)
        end if
C
        if(PLOT) then
          call DIAMOND (X, IX, W, IW, LU)
        end if
      end if
C     !END
      call BYE ('LAOS')
C
      return
      end
