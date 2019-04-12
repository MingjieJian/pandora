      subroutine NESTLE
     $(NO)
C
C     Rudolf Loeser, 2005 Nov 02
C---- Prints H Ly lines normalization data, for TWEET.
C     (This is version 2 of NESTLE.)
C     !DASH
      save
C     !DASH
      real*8 ONE
      integer NO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- LIFFEY      as of 2005 Nov 02
      real*8      FLNRML
      dimension   FLNRML(15)
      common      /LIFFEY/ FLNRML
C     Background H Ly alpha & beta normalization factor for the
C     current value of wavelength (only #2 and #3 can differ from 1)
C     (FLNRML is set up by GROAN)
C     .
C     !DASH
      external LINER, HI, BYE
C
      call HI ('NESTLE')
C     !BEG
      if(NO.gt.0) then
        if(FLNRML(2).ne.ONE) then
          call LINER (1, NO)
          write (NO,100) 'alpha',FLNRML(2)
  100     format(' ','Lyman ',A,' source function normalizing ',
     $               'factor =',1PE12.5)
        else if(FLNRML(3).ne.ONE) then
          call LINER (1, NO)
          write (NO,100) 'beta',FLNRML(3)
        end if
      end if
C     !END
      call BYE ('NESTLE')
C
      return
      end
