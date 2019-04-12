      subroutine CLIFF
     $(FNU,XINK,FINK,RAD)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Gets incident radiation term, for SPUME.
C     !DASH
      save
C     !DASH
      real*8 EXPT, FINK, FNU, HNUKT, ONE, RAD, SEF, TX, XINK, ZERO
      integer INK, IRTIS, MODE, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(35),INK)
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
      equivalence (RZQ( 18),TX   )
      equivalence (KZQ( 85),IRTIS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  LININT, PROD, DIVIDE, QEXP1, HALT, HI, BYE
      intrinsic min, max
C
C               XINK(INK), FINK(INK)
      dimension XINK(*),   FINK(*)
C     !EJECT
C
      call HI ('CLIFF')
C     !BEG
      if(INK.gt.1) then
        MODE = min(max(IRTIS,1),3)
        call LININT   (XINK,1,FINK,1,INK, FNU,RAD, MODE,1,jummy)
      else
        if(TX.gt.ZERO) then
          call PROD   (TX,FNU,1,HNUKT,EXPT)
          call QEXP1  (HNUKT,EXPT,1,SEF)
          call DIVIDE (EXPT,SEF,RAD)
        else
          write (MSSLIN(1),100) INK,TX
  100     format('Computing incident radiation. INK =',I12,5X,'TX =',
     $           1PE20.12)
          call HALT   ('CLIFF',1)
        end if
      end if
C     !END
      call BYE ('CLIFF')
C
      return
      end
