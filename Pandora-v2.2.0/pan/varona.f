      subroutine VARONA
     $(NO)
C
C     Rudolf Loeser, 2003 Mar 06
C---- Preaches a sermon on Hydrogen model atomic data.
C     (This is version 2 of VARONA.)
C     !DASH
      save
C     !DASH
      integer JDKNT, JDNUK, JDXNU, NO, NSL
      logical HYD, LKT, LNK, LNU
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
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
      equivalence (QZQ(  2),QELSM)
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(17),JDNUK)
      equivalence (MEST(16),JDXNU)
      equivalence (MEST(21),JDKNT)
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      call HI ('VARONA')
C     !BEG
      HYD = QELSM(:3).eq.'H  '
      LNK = JDNUK.le.0
      LNU = JDXNU.lt.(NSL-1)
      LKT = JDKNT.gt.0
      if((NO.gt.0).and.HYD.and.(LNK.or.LNU.or.LKT)) then
        call LINER (2, NO)
        write (NO,100)
C                         ---------1---------2---------3---------4
  100   format(' ',49X,  'For H as the ion-of-the-run PANDORA will ',
     $                   'compute default values of'/
     $         ' ',49X,  'many of the atomic parameters: P, CP, CE ',
     $                   'and A'/
     $         ' ',49X,  '(and also line broadening: CRD, CVW, CSK ',
     $                   'and CRS);'/
     $         ' ',49X,  'but most especially of the level ',
     $                   'energies: NU and NUK.   ***')
C                         ---------1---------2---------3---------4
        write (NO,101)
  101   format(' ',49X,  'It is strongly recommended that these ',
     $                   'values not be specified'/
     $         ' ',49X,  'in the input, so that PANDORA will ',
     $                   'compute them.'/
     $         ' ',49X,  '(See Section 19 of "About PANDORA".)')
C                         ---------1---------2---------3---------4
        call LINER (1, NO)
        write (NO,102)
  102   format(' ',49X,  'Since input values were given for some ',
     $                   'of these parameters,')
        if(LNK.or.LNU) then
          write (NO,103)
  103     format(' ',49X,'particularly for NU and/or NUK,')
        end if
C                         ---------1---------2---------3---------4
        write (NO,104)
  104   format(' ',49X,  'it is important for the user to verify ',
     $                   'that these given values'/
     $         ' ',49X,  'are rightly related to their potential ',
     $                   'defaults.')
      end if
C     !END
      call BYE ('VARONA')
C
      return
      end
