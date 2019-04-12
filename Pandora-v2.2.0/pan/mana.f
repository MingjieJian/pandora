      subroutine MANA
     $(IU,IL,LINPRD,LINLDL)
C
C     Rudolf Loeser, 2006 Jul 13
C---- Makes sure that LINPRD (= ICE) is properly set.
C     (This is version 3 of MANA.)
C     !DASH
      save
C     !DASH
      integer IL, IQPMH, IU, LINLDL, LINPRD, LUEO, NSPRD
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
      equivalence (KZQ(117),NSPRD)
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
      equivalence (IQQ(337),IQPMH)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, MASHED, HI, BYE
C
      call HI ('MANA')
C     !BEG
      if(LINPRD.gt.0) then
        if(IQPMH.gt.0) then
          if((LINLDL.gt.1).and.(NSPRD.eq.0)) then
C
            call MESHED ('MANA', 3)
            write (LUEO,100) IU,IL
  100       format(' ','PRD(',I2,'/',I2,'): Hubeny-Lites  may not be ',
     $                 'used with blended lines.')
            call MASHED ('MANA')
C
            LINPRD = 1
          else
            LINPRD = 2
          end if
        else
          LINPRD = 1
        end if
      end if
C     !END
      call BYE ('MANA')
C
      return
      end
