      subroutine DFHYCOF
     $(TE,PART,D01,D01T,DUMP)
C
C     Rudolf Loeser, 1990 Jul 05
C---- Computes Hydrogen results for DEEHEE (q.v.).
C     !DASH
      save
C     !DASH
      real*8 C1, C2, C3, C4, C5, COEF, D01, D01T, PART, POT, PX, RXH,
     $       TE, TEXP, TWO, XH
      integer LUEO
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      dimension PART(5)
C
      data TEXP,C1,C2 /1.76D0, 9.07D1, 6.41D1/
      data C3,C4,C5   /2.57D0, 2.D-2, 4.D3/
      data TWO        /2.D0/
C
      call HI ('DFHYCOF')
C     !BEG
      PX   = PART(1)+TWO*PART(2)
      XH   = PART(2)/PART(1)
      POT  = TE**TEXP
      RXH  = sqrt(XH)
      COEF = POT/PX
C
      D01  = C1*COEF
      D01T = C2*COEF*(XH+C3-C5/(TE*RXH))/(XH+C4/XH)
C
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) PX,D01,D01T,XH,RXH,COEF
  100   format(' ','PX=',1PE11.3,' D01=',E11.3,' D01T=',E11.3/
     $         ' ','XH=',E11.3,' RXH=',E11.3,' COEF=',E11.3)
      end if
C     !END
      call BYE ('DFHYCOF')
C
      return
      end
