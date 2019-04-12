      subroutine LAURA
     $(FB,IB,FE,IE,F,N,QNAME,I1,I2)
C
C     Rudolf Loeser, 1992 Apr 01
C---- Prints input table extrapolation warning message.
C     !DASH
      save
C     !DASH
      real*8 F, FB, FE, TEN
      integer I, I1, I2, IB, IE, IQIXW, IWRN, N, NN, NO
      logical KILROY
      character QNAME*8
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
      equivalence (IQQ(298),IQIXW)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external MESHED, MASHED, LORENZO, LINER, HI, BYE
C
C               F(N)
      dimension F(*)
C
      call HI ('LAURA')
C     !BEG
      if(IQIXW.gt.0) then
        KILROY = .true.
C
        if(IB.gt.0) then
          call LORENZO    (FB, F, IB, TEN, IWRN)
          if(IWRN.gt.0) then
            if(KILROY) then
              KILROY = .false.
              call MESHED ('LAURA', 3)
              write (NO,100) QNAME,I1,I2
  100         format(' ','Input table extrapolation warning for ',A,
     $                   2I4,58X,'(Option INPEXW)')
            end if
            write (NO,101) FB
  101       format(' ','First input value =',1PE16.8)
            write (NO,102)
  102       format(' ','Extrapolated values =')
            write (NO,103) (F(I),I=1,IB)
  103       format(' ',1P7E17.8)
          end if
        end if
C
        if(IE.gt.0) then
          NN = (N-IE)+1
          call LORENZO    (FE, F(IE), NN, TEN, IWRN)
          if(IWRN.gt.0) then
            if(KILROY) then
              call LINER  (1, NO)
              write (NO,100) QNAME,I1,I2
            end if
            write (NO,104) FE
  104       format(' ','Last input value =',1PE16.8)
            write (NO,102)
            write (NO,103) (F(I),I=IE,N)
          end if
        end if
C
        if(.not.KILROY) then
          call MASHED     ('LAURA')
        end if
      end if
C     !END
      call BYE ('LAURA')
C
      return
      end
