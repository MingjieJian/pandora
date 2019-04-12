      subroutine BIRGER
     $(NO,N,NL,DIJ)
C
C     Rudolf Loeser, 1998 Oct 29
C---- Prints the depth indices where the maxima of DIJ occur.
C     (This is version 2 of BIRGER.)
C     !DASH
      save
C     !DASH
      real*8 DIJ
      integer I, J, JE, JS, L, LIST, LL, N, NL, NO
      character BLANK*1, LAB*2
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  GIBERR, LINER, HI, BYE
      intrinsic min, mod
C
C               DIJ(N,NL**2)
      dimension DIJ(*)
C
      dimension LIST(30)
C     !EJECT
C
      call HI ('BIRGER')
C     !BEG
      if(NO.gt.0) then
        call LINER       (1,NO)
        write (NO,100)
  100   format(' ','Abbreviated printout: listing, for each ',
     $             'transition (I,J), the depth index of the ',
     $             'maximum value.')
        JE = 0
  101   continue
          JS = JE+1
          JE = min((JE+30),NL)
          LL = JE-JS+1
          call LINER     (2,NO)
          write (NO,102) (J,J=JS,JE)
  102     format(' ',5X,'J=',30I4)
          do 104 I = 1,NL
            call GIBERR  (DIJ,N,I,JS,JE,LIST)
            LAB = BLANK
            if(mod(I,5).eq.1) then
              call LINER (1,NO)
              LAB = 'I='
            end if
            write (NO,103) LAB,I,(LIST(L),L=1,LL)
  103       format(' ',A2,I3,2X,30I4)
  104     continue
        if(JE.lt.NL) goto 101
      end if
C     !END
      call BYE ('BIRGER')
C
      return
      end
