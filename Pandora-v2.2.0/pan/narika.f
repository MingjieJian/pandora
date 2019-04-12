      subroutine NARIKA
     $(Y,FIN,KASE)
C
C     Rudolf Loeser, 1989 Oct 31
C---- Computes and checks KASE, the method selector, for LAMBDA.
C     KASE = 1 means: QR-method, direct;
C            2      : QR-method, mapped;
C            3      : RT-method, and
C            4      : GR-method.
C     !DASH
      save
C     !DASH
      real*8 Y
      integer IQEXA, IQSFS, KASE, LUEO, jummy
      logical FIN
      character EXA*3, SFS*3
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(169),IQEXA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external BRISTOL, ONOFF, HALT, MESHED, ABORT, HI, BYE
C
      call HI ('NARIKA')
C     !BEG
      call BRISTOL  (Y, KASE)
C
      if((IQSFS.gt.0).or.(IQEXA.gt.0)) then
        if(KASE.ne.4) then
          call ONOFF  (IQSFS, jummy, SFS)
          call ONOFF  (IQEXA, jummy, EXA)
          call MESHED ('NARIKA', 1)
          write (LUEO,100) Y,KASE
  100     format(' ','Error in LAMBDA/NARIKA: ',
     $               'Calculation of Lambda-minus-One operator.'//
     $           ' ','Source Function method selection parameter ',
     $               'Y =',1PE12.4,', (KASE =',I12,')'/
     $           ' ','is incompatible with options SPHERE =',A3,
     $               ' and/or EXPAND =',A3,'.')
          call ABORT
        end if
C
      else if(FIN.and.(KASE.ne.3)) then
        write (MSSLIN(1),101) KASE,Y
  101   format('KASE =',I12,', Y =',1PE24.16,' are invalid for a ',
     $         'FINITE slab.')
        call HALT   ('NARIKA', 1)
C
      end if
C     !END
      call BYE ('NARIKA')
C
      return
      end
