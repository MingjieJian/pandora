      subroutine RASP
     $(Z,N,GD)
C
C     Rudolf Loeser, 1980 Feb 20
C---- Computes geometrical correction terms matrix.
C     (This is version 2 of RASP.)
C     !DASH
      save
C     !DASH
      real*8 GD, ONE, R1GD, RMZ, Z, ZERO, ZJI
      integer I, IQGDS, IQORT, J, N, jummy
      character GDS*3, ORT*3
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external DIVIDE, HALT, ONOFF, HI, BYE
C
C               Z(N), GD(N,N)
      dimension Z(*), GD(N,*)
C
      call HI ('RASP')
C     !BEG
      if(IQGDS.gt.0) then
C
        do 101 J = 1,N
          RMZ = R1GD-Z(J)
          do 100 I = 1,N
            if(J.le.I) then
              GD(I,J) = ONE
            else
              call DIVIDE (RMZ, (R1GD-Z(I)), ZJI)
              GD(I,J) = ZJI**2
            end if
  100     continue
  101   continue
C
      else if(IQORT.gt.0) then
C
        do 103 J = 1,N
          do 102 I = 1,N
            if(J.ge.I) then
              GD(I,J) = ONE
            else
              GD(I,J) = ZERO
            end if
  102     continue
  103   continue
C
      else
        call ONOFF        (IQGDS, jummy, GDS)
        call ONOFF        (IQORT, jummy, ORT)
        write (MSSLIN(1),104) GDS,ORT
  104   format('Option GDS is ',A3,' and option ORT is ',A3,'; ',
     $         'at least one of them must be on.')
        call HALT         ('RASP', 1)
      end if
C     !END
      call BYE ('RASP')
C
      return
      end
