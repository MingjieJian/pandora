      subroutine HASP
     $(N,Z,GD,TITLE,CALLER)
C
C     Rudolf Loeser, 1980 Feb 20
C---- Prints, for RASP.
C     !DASH
      save
C     !DASH
      real*8 GD, R1GD, Z
      integer IQGDS, IQORT, LUEO, N
      character CALLER*(*), TITLE*(*), TYPE*22
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (REST( 2),R1GD )
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
      equivalence (IQQ( 60),IQGDS)
      equivalence (IQQ(135),IQORT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, LINER, VECOUT, PRAY, MASHED, HI, BYE
C
C               Z(N), GD(N,N)
      dimension Z(*), GD(*)
C
      call HI ('HASP')
C     !BEG
      if(IQGDS.gt.0) then
        TYPE = 'geometrical dilution  '
      end if
      if(IQORT.gt.0) then
        TYPE = 'outward-only radiation'
      end if
C
      call MESHED (CALLER, 2)
C
      write (LUEO,100) TYPE,TITLE
  100 format(' ','Matrix of correction terms for ',A22/
     $       ' ',A100)
      call VECOUT (LUEO, Z, N, 'Z table')
C
      call LINER  (1, LUEO)
      write (LUEO,102) R1GD
  102 format(' ','GD matrix',102X,'R1GD',1PE12.4)
      call PRAY   (LUEO, GD, N, N)
C
      call MASHED (CALLER)
C     !END
      call BYE ('HASP')
C
      return
      end
