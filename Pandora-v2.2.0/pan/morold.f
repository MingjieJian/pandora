      subroutine MOROLD
     $(NO,MCOF,NAME)
C
C     Rudolf Loeser, 1994 Dec 20
C---- Prints an explanation for the ATOM printout.
C     (This is version 2 of MOROLD.)
C     !DASH
      save
C     !DASH
      real*8 RFAC, RFXNC
      integer MCOF, NO
      character NAME*2
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
      equivalence (REST( 5),RFXNC)
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      call HI ('MOROLD')
C     !BEG
      call LINER (1, NO)
C
      if(MCOF.gt.0) then
        write (NO,100) NAME,RFXNC,NAME,RFAC,NAME
  100   format(' ',12X,'The printed tables of ',A2,'-values show '
     $             'samples only,'/
     $         ' ',12X,'computed with NC =',1PE13.6,' (note input ',
     $             'parameter IRFNC);'/
     $         ' ',12X,'in this run, in place of the samples, ',A2,
     $             '-values will be computed on-the-fly as needed.'//
     $         ' ',12X,'These printed samples have all been ',
     $             'multiplied by the "Reduction Factor for All ',
     $             'Collisions," RFAC =',E10.3,'.'/
     $         ' ',12X,'RFAC will also be applied to all on-the-fly ',
     $             'values of ',A2,'.')
      else
        write (NO,101) NAME
  101   format(' ',8X,'* ',A2,'-values are obtained as needed by ',
     $             'interpolation in the tables printed above.')
      end if
C     !END
      call BYE ('MOROLD')
C
      return
      end
