      subroutine CLOUNS
     $(LU,DORGVL,N,NL,PLK,GVL,RGVL,DIDRGVL)
C
C     Rudolf Loeser, 1998 Jun 22
C---- Computes (and prints) RGVL = GVL / PLK.
C     !DASH
      save
C     !DASH
      real*8 GVL, PLK, RGVL
      integer I, IQDFA, J, K, L, LU, N, NL
      logical DIDRGVL, DORGVL
      character BLANK*1, LINE*128, R*11
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
      equivalence (IQQ(234),IQDFA)
C     !DASH
      external  ARRDIV, LINER, CLOSUN, SHIM, HI, BYE
C
C               PLK(N,NL), GVL(N,NL), RGVL(N,NL)
      dimension PLK(N,*),  GVL(N,*),  RGVL(N,*)
C     !EJECT
C
      call HI ('CLOUNS')
C     !BEG
      DIDRGVL = DORGVL.and.(IQDFA.gt.0).and.(LU.gt.0)
      if(DIDRGVL) then
C----   Form ratio
        call ARRDIV         (GVL,PLK,RGVL,N*NL)
C
C----   Print
        call LINER          (3,LU)
        write (LU,100)
  100   format(' ','GVL / PLK = RGVL , for each level (printed when ',
     $             'option DIFFANA = on).')
        do 108 K = 1,NL,3
          LINE = BLANK
          L = -31
          do 102 J = K,(K+2)
            if(J.le.NL) then
              L = L+40
              write (LINE(L:),101) J,J,J
  101         format(9X,'GVL(',I2,')',5X,'PLK(',I2,')',
     $               4X,'RGVL(',I2,')')
            end if
  102     continue
          call LINER        (2,LU)
          write (LU,103) LINE(2:)
  103     format(' ',A127)
          call LINER        (1,LU)
C
          do 107 I = 1,N
            write (LINE,104) I
  104       format(I8,120X)
            L  = -31
            do 106 J = K,(K+2)
              if(J.le.NL) then
                L = L+40
                call CLOSUN (RGVL(I,J),R)
                write (LINE(L:),105) GVL(I,J),PLK(I,J),R
  105           format(5X,1P2E12.4,A11)
              end if
  106       continue
            write (LU,103) LINE(2:)
            call SHIM       (I,5,LU)
  107     continue
  108   continue
C
      end if
C     !END
      call BYE ('CLOUNS')
C
      return
      end
