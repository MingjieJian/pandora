      subroutine BRAZIL
     $(DUMP,KILROY,KOLEV,METHOD,KDGV,CALLER)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Prints header for Lyman-Epsilons detail printout.
C     !DASH
      save
C     !DASH
      integer IQEDP, KDGV, KOLEV, LDINT, LUEO, MO
      logical DUMP, KILROY
      character CALLER*(*), METHOD*(*)
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
      equivalence (KZQ( 48),LDINT)
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
      equivalence (IQQ(121),IQEDP)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('BRAZIL')
C     !BEG
      KILROY = .true.
      DUMP   = (IQEDP.gt.0).and.(LDINT.ne.0).and.(MO.gt.0)
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO,100) KOLEV,METHOD,KDGV
  100   format(' ','Details of Lyman Epsilons calculation.',10X,
     $             'KOLEV=',I2,10X,'Method: ',A,10X,'KDGV=',I1)
        KILROY = .false.
      end if
C     !END
      call BYE ('BRAZIL')
C
      return
      end
