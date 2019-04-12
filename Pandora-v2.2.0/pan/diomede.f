      subroutine DIOMEDE
     $(EP1N,EP2N,METEP,T)
C
C     Rudolf Loeser, 1987 Nov 05
C---- Prints comparisons of calculated "Lyman" epsilons.
C     !DASH
      save
C     !DASH
      real*8 EP1N, EP2N, T
      integer IQEPC, IQLYA, METEP, MO, N
      character LAB*9
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
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
      equivalence (IQQ(  7),IQEPC)
      equivalence (IQQ(255),IQLYA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external ABJECT, ITURI, HI, BYE
C
C               EP1N(N,4), EP2N(N,4), T(4)
      dimension EP1N(*),   EP2N(*),   T(*)
C
      dimension LAB(4)
C
      data LAB /'  NOVA   ', 'COMPLEX-U', 'COMPLEX-L', '  CHAIN  '/
C
      call HI ('DIOMEDE')
C     !BEG
      if((IQEPC.gt.0).and.(MO.gt.0).and.(IQLYA.le.0)) then
        call ABJECT (MO)
        write (MO,100) LAB(METEP+1)
  100   format(' ','Comparison of calculated EP1 and EP2.',3X,
     $             'The final values are those from ',A9)
        call ITURI  (MO, EP1N, EP2N, N, T, LAB)
      end if
C     !END
      call BYE ('DIOMEDE')
C
      return
      end
