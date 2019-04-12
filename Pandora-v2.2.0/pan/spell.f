      subroutine SPELL
     $(LURR,RKI,RKW,RLI,EP1,EP2)
C
C     Rudolf Loeser, 1980 Jan 04
C---- Puts "Lyman" data into restart file.
C     !DASH
      save
C     !DASH
      real*8 EP1, EP2, RKI, RKW, RLI
      integer IQLYM, KOLEV, LURR, N
      character LABEL*17
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
      equivalence (KZQ( 33),KOLEV)
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
      equivalence (IQQ( 13),IQLYM)
C     !DASH
C     !EJECT
      external BUNT, HI, BYE
C
C               RKI(N,NL), RLI(N,NL), RKW(N), EP1(N), EP2(N)
      dimension RKI(N,*),  RLI(N,*),  RKW(*), EP1(*), EP2(*)
C
      call HI ('SPELL')
C     !BEG
      if(IQLYM.gt.0) then
C
  100   format('I',A2,'COMP ( I ',I2,' 0 ) >')
  101   format(A4,7X,I3,3X)
C
        write (LURR,100) 'RK',KOLEV
        write (LABEL,101) 'RK  ',KOLEV
        call BUNT (LURR,RKI(1,KOLEV),LABEL)
C
        write (LABEL,101) 'RKWT',KOLEV
        call BUNT (LURR,RKW         ,LABEL)
C
        write (LURR,100) 'RL',KOLEV
        write (LABEL,101) 'RL  ',KOLEV
        call BUNT (LURR,RLI(1,KOLEV),LABEL)
C
        call BUNT (LURR,EP1,'EP1')
        call BUNT (LURR,EP2,'EP2')
      end if
C     !END
      call BYE ('SPELL')
C
      return
      end
