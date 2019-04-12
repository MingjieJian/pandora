      subroutine IODINE
     $(IRCOMP,R,JLEV,IQJ)
C
C     Rudolf Loeser, 1989 Jul 31
C---- Determines whether or not to compute "Rate" R for level JLEV.
C     Sets IQJ=1 if yes, =0 if no.
C     (This is version 2 of IODINE.)
C     !DASH
      save
C     !DASH
      real*8 R
      integer IOVER, IQJ, IQLYM, IRCOMP, JLEV, KOLEV, KSHEL, N, NSL
      logical RZERO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 1),KSHEL)
C     !DASH
      external NAUGHTD, HI, BYE
C
C               IRCOMP(NSL), R(N,NSL)
      dimension IRCOMP(*),   R(N,*)
C
      call HI ('IODINE')
C     !BEG
      if((JLEV.ge.1).and.(JLEV.le.NSL)) then
C----   Provide the basic default
        IQJ = IRCOMP(JLEV)
C
        call NAUGHTD (R(1,JLEV), 1, N, RZERO)
        if(RZERO) then
C----     Since R(JLEV) does not exist (i.e. all members =0),
C         it must be computed
          IQJ = 1
        end if
C
        if(IOVER.gt.1) then
C----     Further considerations apply it this is not the
C         first iteration
          if(IQLYM.le.0) then
C----       Since there is NO Lyman calculation, every R MUST
C           be computed
            IQJ = 1
          else
C----       Since there IS a Lyman calculation, R(KOLEV) must
C           NOT be computed
            if(JLEV.eq.KOLEV) then
              IQJ = 0
            end if
          end if
        end if
C
      else
        if((KSHEL.gt.0).and.(JLEV.eq.(NSL+1))) then
C----     The K-shell rate must always be done
          IQJ = 1
        else
C----     (This is a non-existent level)
          IQJ = 0
        end if
      end if
C     !END
      call BYE ('IODINE')
C
      return
      end
