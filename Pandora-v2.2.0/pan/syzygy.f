      subroutine SYZYGY
     $(N,IBEG,IEND,Z,ZL,TAUIJ,INDX,NRAD,NO,KODE)
C
C     Rudolf Loeser, 1968 Oct 18
C---- Sets up Tau vs. Z plots.
C
C     KODE=1 is for BDI, =2 is for BDIJ.
C     !DASH
      save
C     !DASH
      real*8 TAUIJ, Z, ZL
      integer IBEG, IEND, IL, INDX, IQTPL, IU, J, KODE, N, NO, NRAD
      character STAR*1, SYM*1
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
      equivalence (IQQ(280),IQTPL)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external GARLIC, HALT, MAY, HI, BYE
C
C               TAUIJ(N,NRAD), Z(N), ZL(N), INDX(NRAD)
      dimension TAUIJ(N,*),    Z(*), ZL(*), INDX(*)
C     !EJECT
C
      call HI ('SYZYGY')
C     !BEG
      if(IQTPL.gt.0) then
        if((KODE.lt.1).or.(KODE.gt.2)) then
          write (MSSLIN(1),100) KODE
  100     format('KODE =',I12,', which is neither 1 nor 2.')
          call HALT    ('SYZYGY',1)
        end if
C
        do 101 J = 1,NRAD
          IU = INDX(J)/100
          IL = INDX(J)-100*IU
          if(KODE.eq.1) then
            SYM = STAR
          else
            call GARLIC (J,ALPHS,26,SYM)
          end if
          call MAY      (NO,ZL,TAUIJ(1,J),N,IBEG,IEND,SYM,IU,IL)
  101   continue
C
      end if
C     !END
      call BYE ('SYZYGY')
C
      return
      end
