      subroutine LARK
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1997 Jun 13
C---- Does the calculation.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, JSTCN, JSTIN
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
      equivalence (KZQ(  7),JSTIN)
      equivalence (KZQ( 35),JSTCN)
C     !DASH
      external SPEY, GLEN, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('LARK')
C     !BEG
      if(JSTIN.le.0) then
C
        if(JSTCN.gt.0) then
C----     Continuum-only run
          call GLEN (X, IX, W, IW)
        else
C----     Regular run (including the NOION case)
          call SPEY (X, IX, W, IW)
        end if
C
      end if
C     !END
      call BYE ('LARK')
C
      return
      end
