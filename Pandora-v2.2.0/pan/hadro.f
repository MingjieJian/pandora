      subroutine HADRO
     $(WPOP,WBD,JEDIT,IBNVW)
C
C     Rudolf Loeser, 1977 Jan 17
C---- Sets defaults of parameters for BD & ND calculations.
C     !DASH
      save
C     !DASH
      real*8 ONE, WBD, WPOP, ZERO
      integer IBNVW, JEDIT, N, NOION
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic min
C     !EJECT
C
      call HI ('HADRO')
C     !BEG
      if(NOION.le.0) then
        if(WPOP.lt.ZERO) then
          if(WBD.ge.ZERO) then
            WPOP = WBD
          else
            WPOP = ONE
            WBD  = ONE
          end if
        else
          if(WBD.lt.ZERO) then
            WBD = WPOP
          end if
        end if
        if((JEDIT.le.0).or.(JEDIT.gt.N)) then
          JEDIT = N/2
        end if
        if((IBNVW.le.0).or.(IBNVW.gt.N)) then
          IBNVW = min((JEDIT+1),N)
        end if
      end if
C     !END
      call BYE ('HADRO')
C
      return
      end
