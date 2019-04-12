      subroutine VICTOR
     $(MET,MO,LAST,SWITCH,KNT,ALL)
C
C     Rudolf Loeser, 1974 Mar 22
C---- Decides which method(s) to use for
C     the Statistical Equilibrium calculation.
C     (This is version 2 of VICTOR.)
C     !DASH
      save
C     !DASH
      integer I, IQEVR, IQSEC, KNT, MET, MO
      logical ALL, LAST, SET, SWITCH
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
      equivalence (IQQ(103),IQSEC)
      equivalence (IQQ( 19),IQEVR)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
C               SWITCH(KNT)
      dimension SWITCH(*)
C     !EJECT
C
      call HI ('VICTOR')
C     !BEG
      if((MET.lt.0).or.(MET.ge.KNT)) then
        write (MSSLIN(1),100) MET,KNT
  100   format('MET =',I12,', KNT =',I12,'; this is not OK.')
        call HALT ('VICTOR',1)
      end if
C
      ALL = .false.
      SET = .false.
      if((IQSEC.gt.0).and.((IQEVR.eq.1).or.(MO.gt.0))) then
        if(LAST) then
          ALL = .true.
          SET = .true.
        end if
      end if
C
      do 101 I = 1,KNT
        SWITCH(I) = SET
  101 continue
C
      if(.not.ALL) then
        SWITCH(MET+1) = .true.
      end if
C     !END
      call BYE ('VICTOR')
C
      return
      end
