      subroutine ABRAHAM
     $(KODE)
C
C     Rudolf Loeser, 1987 Aug 20
C---- Initializes the main printout file, its index file (if needed),
C     and the error/warning printout file (if needed).
C     (This is version 2 of ABRAHAM.)
C     !DASH
      save
C     !DASH
      integer IH, IM, IQMIX, IS, KODE, LUEO, LUIX, NO
      character TYME*8
C     !COM
C---- ILION       as of 1987 Aug 20
      integer     NUMSCT
      common      /ILION/ NUMSCT
C     Next available identifier for printout sections.
C     .
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
      equivalence (IQQ(217),IQMIX)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS(11),LUIX )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DUCK, GET_TIME, HI, BYE
C     !EJECT
C
      call HI ('ABRAHAM')
C     !BEG
      if(KODE.eq.1) then
C----   Set up "error/warning" printout file = LUEO = .aer
C       as a separate file.
C       (This is a tricky matter, since error messages may already have
C        had to be written -- however, if so, then the run has already
C        been aborted, and control never gets here.
C        Thus,
C        in a normal run, when control gets here, no printout has yet
C        been issued.)
        LUEO = 16
        call DUCK     (LUEO,15)
      else
C       Set up LUEO to be the same as the main printout file, .aaa
        LUEO = 15
      end if
C
C---- Set up main printout file = NO = .aaa
      call DUCK       (NO,LUEO)
C
      if(IQMIX.gt.0) then
C----   Set up "printout index" file = LUIX = .aix
        call DUCK     (LUIX,LUEO)
C
C----   Set up serial number of first printout section
        call GET_TIME (TYME)
        read (TYME,100) IH,IM,IS
  100   format(I2,1X,I2,1X,I2)
        NUMSCT = (IH*100+IM)*100+IS
      end if
C     !END
      call BYE ('ABRAHAM')
C
      return
      end
