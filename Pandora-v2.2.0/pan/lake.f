      subroutine LAKE
     $(XJA,XJB,KK,N,KOLEV)
C
C     Rudolf Loeser, 1977 Jan 18
C---- Prints comparisons for FAKE.
C     !DASH
      save
C     !DASH
      real*8 XJA, XJB
      integer I, IE, IQCRK, IQLYA, IS, J, KINT, KK, KOLEV, LU, MO, N
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
      equivalence (IQQ(126),IQCRK)
      equivalence (IQQ(255),IQLYA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external  ZEUS, LINER, ABJECT, SAKE, HI, BYE
      intrinsic min
C
C               XJA(N,KKX), XJB(N,KKX)
      dimension XJA(N,*),   XJB(N,*)
C
      call HI ('LAKE')
C     !BEG
      call ZEUS        (MO, IQCRK, LU)
C
      if((LU.gt.0).and.(IQLYA.le.0)) then
        call SAKE      (KK, KINT)
C
        call ABJECT    (LU)
        write (LU,100) KOLEV
  100   format(' ','Comparison of Jnus for RK-',I2,' calculations',
     $             ' (RK from J("B") is used).')
C
        IE = 0
  101   continue
          IS = IE+1
          IE = min(IE+10,N)
          call LINER   (2, LU)
          write (LU,102) (I,I=IS,IE)
  102     format(' ','Depth',2X,10I12)
C
          do 104 J = 1,KK,KINT
            call LINER (1, LU)
            write (LU,103) 'A',J,(XJA(I,J),I=IS,IE)
  103       format(' ',A1,'(',I4,')',1P10E12.4)
            write (LU,103) 'B',J,(XJB(I,J),I=IS,IE)
  104     continue
C
        if(IE.lt.N) goto 101
C
      end if
C     !END
      call BYE ('LAKE')
C
      return
      end
