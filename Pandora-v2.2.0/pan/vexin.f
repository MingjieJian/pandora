      subroutine VEXIN
     $(KILROY,CALLER,RHW,LAB,N,WMN,WMX)
C
C     Rudolf Loeser, 1996 Feb 01
C---- Edits input values of RHO weights, if needed.
C     (This is version 2 of VEXIN.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, RHW, WMN, WMX
      integer I, IFLG, IQIRE, LUEO, N
      logical CHANGE, KILROY
      character CALLER*(*), LAB*(*)
C     !COM
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
      equivalence (IQQ(312),IQIRE)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external COMPD, MUSHED, LINER, HI, BYE
C
C               RHW(N)
      dimension RHW(*)
C
      data DELTA /1.D-8/
C
      call HI ('VEXIN')
C     !BEG
      if(IQIRE.gt.0) then
C
        CHANGE = .false.
        do 100 I = 1,N
C
          call COMPD   (RHW(I), WMX, DELTA, IFLG)
          if(IFLG.gt.0) then
            RHW(I) = WMX
            CHANGE = .true.
          else
            call COMPD (RHW(I), WMN, DELTA, IFLG)
            if(IFLG.lt.0) then
              RHW(I) = WMN
              CHANGE = .true.
            end if
          end if
C
  100   continue
C
        if(CHANGE) then
          call MUSHED  (CALLER, 3, KILROY)
          call LINER   (1, LUEO)
          write (LUEO,101) LAB
  101     format(' ','VEXIN: Input values of ',A,' were edited.')
        end if
C
      end if
C     !END
      call BYE ('VEXIN')
C
      return
      end
