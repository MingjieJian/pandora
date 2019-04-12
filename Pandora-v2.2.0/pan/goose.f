      subroutine GOOSE
     $(Z,N,GD,KODE,TITLE)
C
C     Rudolf Loeser, 1980 Feb 20
C---- Computes a matrix of correction terms, to multiply a
C     weight matrix on a term-by-term basis, for
C     the following purposes:
C     1) to account for geometrical dilution of internal radiation
C        fields (0-order approximation to spherical shells);
C     2) to account for "outward-only" flow of internal radiation.
C---- GD (length N*N) is the desired matrix.
C     Returns with KODE=1 if some terms of GD have values
C     other than 1.0, with KODE=0 if all terms of GD = 1.0.
C---- TITLE is a label (type character, .le. 100) to be printed
C     to identify an optional dump printout.
C     (This is version 2 of GOOSE.)
C     !DASH
      save
C     !DASH
      real*8 GD, Z
      integer IQGDP, IQGDS, IQORT, KODE, MO, N
      logical DUMP, SWITCH
      character TITLE*(*)
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
      equivalence (IQQ( 60),IQGDS)
      equivalence (IQQ(135),IQORT)
      equivalence (IQQ( 86),IQGDP)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external ONE1, RASP, HASP, HI, BYE
C
C               GD(N,N), Z(N)
      dimension GD(*),   Z(*)
C
      call HI ('GOOSE')
C     !BEG
      SWITCH = ((IQGDS.le.0).and.(IQORT.le.0))
C
      if(SWITCH) then
        KODE = 0
        call ONE1   (GD, (N**2))
C
      else
        DUMP = (IQGDP.gt.0).and.(MO.gt.0)
        KODE = 1
        call RASP   (Z, N, GD)
        if(DUMP) then
          call HASP (N, Z, GD, TITLE, 'GOOSE')
        end if
      end if
C     !END
      call BYE ('GOOSE')
C
      return
      end
