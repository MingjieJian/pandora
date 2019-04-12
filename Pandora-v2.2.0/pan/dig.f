      subroutine DIG
     $(NO,N,NL,CIJ,PIJ)
C
C     Rudolf Loeser, 1985 Feb 08
C---- Sets up and prints RIJ.
C     (This is version 2 of DIG.)
C     !DASH
      save
C     !DASH
      real*8 CIJ, PIJ
      integer IE, IS, N, NL, NO
C     !DASH
      external  LINER, PIG, HI, BYE
      intrinsic min
C
C               CIJ(N,NL,NL), PIJ(N,NL,NL)
      dimension CIJ(*),       PIJ(*)
C
C
      call HI ('DIG')
C     !BEG
      call LINER (1,NO)
      write (NO,100)
  100 format(' ','RIJ = CIJ + PIJ'/
     $       ' ','When C > P, the fraction C/R appears above the ',
     $           'value of R;'/
     $       ' ','when P > C, the fraction P/R appears below the ',
     $           'value of R.')
C
      IE = 0
  101 continue
        IS = IE+1
        IE = min((IE+11),N)
C
        call PIG (NO,IS,IE,N,NL,CIJ,PIJ)
C
      if(IE.lt.N) goto 101
C     !END
      call BYE ('DIG')
C
      return
      end
