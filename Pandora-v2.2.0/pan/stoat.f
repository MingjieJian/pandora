      subroutine STOAT
     $(LU,N,NL,GVL,GVI,TYPE)
C     Rudolf Loeser, 1987 Oct 06
C---- Prints, for TARPON.
C     !DASH
      save
C     !DASH
      real*8 GVI, GVL
      integer LU, N, NL
      logical PRNTZ
      character BLANK*1, TYPE*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, OMAR, HI, BYE
C
C               GVL(N,NL), GVI(N)
      dimension GVL(N,*),  GVI(*)
C
      data      PRNTZ /.false./
C
      call HI ('STOAT')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100) TYPE
  100   format(' ','GNV-I: sum of diffusion and velocity terms ',
     $             '- ion, ',A)
        call OMAR  (LU, N, 1, GVI, BLANK, PRNTZ)
C
        call LINER (2, LU)
        write (LU,101) TYPE
  101   format(' ','GNV-L: sum of diffusion and velocity terms ',
     $             '- levels, ',A)
        call OMAR  (LU, N, NL, GVL, 'Level ', PRNTZ)
      end if
C     !END
      call BYE ('STOAT')
C
      return
      end
