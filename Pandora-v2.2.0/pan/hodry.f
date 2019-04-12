      subroutine HODRY
     $(CP,NSL,IQUTR,KSHEL)
C
C     Rudolf Loeser, 1980 Mar 03
C---- Sets KSHEL value.
C     !DASH
      save
C     !DASH
      real*8 CP, ZERO
      integer IQUTR, KSHEL, NOION, NSL
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
      equivalence (KZQ( 94),NOION)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
C               CP(NSL+1)
      dimension CP(*)
C     !EJECT
C
      call HI ('HODRY')
C     !BEG
      KSHEL = 0
      if((CP(NSL+1).ne.ZERO).and.(NOION.le.0)) then
        KSHEL = 1
C
        if(IQUTR.gt.0) then
          write (MSSLIN(1),100) IQUTR,CP(NSL+1)
  100     format('IQUTR =',I12,', CP =',1PE12.4,'; K-Shell is not ',
     $           'allowed when USETRIN = on.')
          call HALT ('HODRY',1)
        end if
C
      end if
C     !END
      call BYE ('HODRY')
C
      return
      end
