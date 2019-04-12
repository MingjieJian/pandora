      subroutine ELGAR
     $(NO,Q,DEF)
C
C     Rudolf Loeser, 2002 Apr 30
C---- Prints a collisions data legend, for ATOM.
C     Upon input, Q = either "CI" or "CE".
C     !DASH
      save
C     !DASH
      real*8 RFAC
      integer NO
      logical DEF
      character Q*2
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
      equivalence (RZQ( 42),RFAC )
C     !DASH
      external HI, BYE
C
      call HI ('ELGAR')
C     !BEG
      if(NO.gt.0) then
        if(DEF) then
          write (NO,100) Q,Q,Q,Q,Q, Q,Q,Q,Q,Q, Q,Q, RFAC
  100     format(' ',8X,'* The raw computed default values of ',A,
     $               ' can be adjusted with M',A,' and A',A,
     $               ' such that'/
     $           ' ',10X,A,'(adjusted) = ',A,'(raw) * M',A,' + A',A/
     $           ' ',10X,'The default values are: M',A,' = 1 and A',
     $               A,' = 0; only values differing from these are ',
     $               'printed.'//
     $           ' ',10X,'Finally, all ',A,'(adjusted) have been ',
     $               'multiplied by the Reduction Factor for All ',
     $               'Collisions, RFAC, such that'/
     $           ' ',10X,A,'(final default) = RFAC * ',A,
     $               '(adjusted); in this run RFAC =',1PE10.3,'.')
        else
          write (NO,101) Q,RFAC
  101     format(' ',8X,'* All input ',A,' values have been ',
     $               'multiplied by the Reduction Factor for All ',
     $               'Transitions, RFAC =',1PE10.3,'.')
        end if
      end if
C     !END
      call BYE ('ELGAR')
C
      return
      end
