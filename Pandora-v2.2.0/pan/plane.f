      subroutine PLANE
     $(NO,DAMP)
C
C     Rudolf Loeser, 1982 Feb 12
C---- Prints numerological details concerning weight matrices for
C     source function calculations.
C     (This is version 3 of PLANE.)
C     !DASH
      save
C     !DASH
      real*8 DAMP, TBAR, TLRGE, TMS, TSMLL, YPRE
      integer IQSFS, KASE, NO
      character BLANK*1, CODY*10, MINUS*1, TYPE*31
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
      equivalence (RZQ( 44),TMS  )
      equivalence (RZQ( 24),TBAR )
      equivalence (RZQ( 63),TSMLL)
      equivalence (RZQ( 64),TLRGE)
      equivalence (RZQ( 20),YPRE )
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
      equivalence (IQQ( 31),IQSFS)
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(40),MINUS )
C     !DASH
      external LINER, BRISTOL, DASHER, HI, BYE
C
      dimension TYPE(4)
C
      data TYPE /'Quadratic Representation method',
     $           'mapped quadratic method',
     $           'analytic Ray Tracing method',
     $           'General Ray tracing method'/
C
      call HI ('PLANE')
C     !BEG
      if(NO.gt.0) then
        call LINER   (1, NO)
        call BRISTOL (DAMP, KASE)
        if(KASE.eq.1) then
          write (CODY,100) DAMP
  100     format(1PE10.2)
        else if(KASE.eq.2) then
          write (CODY,100) YPRE
        end if
C     !EJECT
        if(IQSFS.gt.0) then
          write (NO,101) ' spherical geometry, ',TYPE(KASE)
  101     format(' ','----- Concerning weight matrices:',A,A,:,10X,
     $               'Damping Parameter =',A)
        else
          if((KASE.eq.1).or.(KASE.eq.2)) then
            write (NO,101) BLANK,TYPE(KASE),CODY
          else
            write (NO,101) BLANK,TYPE(KASE)
          end if
        end if
C
        if((KASE.eq.1).or.(KASE.eq.2)) then
          write (NO,102) TSMLL,TLRGE
  102     format(' ','-- TSMALL =',1PE9.1,' and TLARGE =',E9.1,
     $               ' were used to omit small and large TAU values.')
        else if(KASE.eq.3) then
          write (NO,103) TSMLL
  103     format(' ','-- TSMALL =',1PE9.1,' was used to omit small ',
     $               'TAU values.')
        end if
C
        if(KASE.eq.2) then
          write (NO,104) TBAR
  104     format(' ','-- TAUBAR =',1PE9.1,' is the TAU where the ',
     $               'changeover between the Lambda and Lambda-1 ',
     $               'formulations occurs.')
        end if
C
        if((KASE.eq.3).or.(KASE.eq.4)) then
          write (NO,105) TMS
  105     format(' ','-- TMS =',1PE9.1,'. For TAU > TMS, the ',
     $               'representation of S is by quadratic segments ',
     $               'between the three depths centered on'/
     $           ' ','   the diagonal, and by linear segments ',
     $               'elsewhere. For smaller TAU, linear segments are ',
     $               'used to represent S everywhere.')
        end if
C
        call DASHER (NO)
      end if
C     !END
      call BYE ('PLANE')
C
      return
      end
