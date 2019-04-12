      subroutine HAMRE
     $(X,IX,TE,XNC,NO)
C
C     Rudolf Loeser, 2006 Apr 12
C     Revised RL/SGK Jul 14 2014 
C---- Prints CI and CE defaults information.
C     (This is version 2 of HAMRE.)
C     !DASH
      save
C     !DASH
      real*8 CNC, CTE, TE, X, XNC
      integer INCEI, IX, IXNCS, N, NO, IQACC
      logical HYDR
      character QELSM*8
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(225),INCEI)
      equivalence (KZQ(144),IXNCS)
C
C---- OPTIONS     as of Jul 14 2014
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=346)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
C     .
      equivalence (IQQ(346), IQACC)
C
C---- SUBLET      as of 2006 Dec 04
      character   CITES*64, CQ*20
      dimension   CITES(11), CQ(11)
      common      /SUBLET/ CITES, CQ
C     CI/CE methods source citations for HAMRE et al.
C     .
      data CITES /
     $'M.B. Shah et al. 1987, J.Phys.B. 20, 3501',
     $'M. Arnaud & R. Rothenflug 1985, A&A 60, 425',
     $'G.S. Voronov 1997, Atommic Data & Nuclear Data Tables 65, 1',
     $'L. Vriens & A.H.M. Smeets 1980, Phys.Rev.A 72, 940',
     $'L.C. Johnson 1972, ApJ 174, 227',
     $'Clark et al. 1991, ApJ 381, 597',
     $'Scholz et al. 1990, MNRAS 242, 692',
     $'Przybilla & Butler 2004, ApJ 609, 1181',
     $'M.J. Seaton 1962, Proc.Phys.Soc. 79, 1105',
     $'H. van Regemorter 1962, ApJ 136, 906',
     $'Aggarwal et al. 1991, J.Phys.B 24, 1385'/
C
      data CQ /'H level 1  only     ', 'level 1 only        ',
     $         'non-H level 1 only  ', 'H only              ',
     $         'H only              ', '                    ',
     $         'H 2/1 only          ', 'H u/l only, u < 8   ',
     $         'neutral, A > 0, only', '                    ',
     $         'H u/l only, u < 6   '/
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external LINER, DASHER, CANIS, HAREM, HI, BYE
C
      dimension X(*), IX(*)
C
C               XNC(N), TE(N)
      dimension XNC(*), TE(*)
C
      call HI ('HAMRE')
C     !BEG
      if(NO.gt.0) then
        call LINER  (2, NO)
        call DASHER (NO)
        call LINER  (1,NO)
C
        if (IQACC.gt.0) then
          write(NO,9010)
 9010     format(1X, 'Complete set of CI and CE samples.',
     $          77X, '(Option ALLCICE)')
          
          HYDR = QELSM(:3).eq.'H  '
C
          call CANIS  (N, TE, INCEI)
          CTE = TE(INCEI)
          CNC = XNC(INCEI)
C
          call LINER  (1,NO)
          if(IXNCS.gt.0) then
            write (NO,100) INCEI,CTE,CNC
 100        format(' ','About CI (collisional ionization) and CE ',
     $           '(collisional de-excitation) coefficients,'/
     $           ' ','and comparisons computed for depth #',I4,
     $           ' (input parameter INCEI), where TE =',
     $           1PE12.5,:,' and NC =',E15.8)
          else
            write (NO,100) INCEI,CTE
          end if
C     
          call HAREM  (X, IX, NO, CTE, CNC, HYDR)
C     
          call LINER  (1, NO)
          call DASHER (NO)
          call LINER  (2, NO)
        end if
      end if
C     !END
      call BYE ('HAMRE')
C
      return
      end
