      subroutine SPALY
     $(LU,N,NL,NAME,HYDR,BDHM,BDI)
C
C     Rudolf Loeser, 1999 Aug 05
C---- Departure coefficients, for Kurucz.
C     !DASH
      save
C     !DASH
      real*8 BDHM, BDI
      integer J, LU, N, NL
      logical HYDR
      character BLANK*1, HM*6, NAME*6
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external YARDEN, BUNT, HI, BYE
C
C               BDHM(N), BDI(N,NL)
      dimension BDHM(*), BDI(N,*)
C
      data HM /'HMINUS'/
C
      call HI ('SPALY')
C     !BEG
      call YARDEN (LU,1,'KURUCZ - b')
C
      if(HYDR) then
        write (LU,100) HM,1,N
        call BUNT (LU,BDHM,BLANK)
      end if
C
  100 format(A6,2I10)
C
      do 101 J = 1,NL
        write (LU,100) NAME,J,N
        call BUNT (LU,BDI(1,J),BLANK)
  101 continue
C
      call YARDEN (LU,2,'KURUCZ - b')
C     !END
      call BYE ('SPALY')
C
      return
      end
