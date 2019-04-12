      subroutine MENTHA
     $(LU,IPRDD,N,LDL,DP,SAP)
C
C     Rudolf Loeser, 1988 Nov 09
C---- Prints, for PRD.
C     (This is version 2 of MENTHA.)
C     !DASH
      save
C     !DASH
      real*8 DP, SAP
      integer I, IPRDD, J, JE, JS, K, LDL, LU, N
      character BLANK*1, LINE*120
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, HI, BYE
      intrinsic min
C
C               DP(N,LDL), SAP(N,LDL)
      dimension DP(N,*),   SAP(N,*)
C     !EJECT
C
      call HI ('MENTHA')
C     !BEG
      JE = 0
  100 continue
        JS = JE+1
        JE = min((JE+4),LDL)
C
        call LINER (2,LU)
        write (LU,101) (BLANK,J,J=JS,JE)
  101   format(' ',4(1X,A1,'******  Component',I3,'  ******'))
        write (LU,102) (BLANK,  J=JS,JE)
  102   format(' ',4(3X,A1,11X,'DP',10X,'SAP'))
        call LINER (1,LU)
C
        LINE = BLANK
        do 106 I = 1,N,IPRDD
C
          K = -30
          do 104 J = JS,JE
            K = K+30
            write (LINE((K+1):(K+30)),103) I,DP(I,J),SAP(I,J)
  103       format(I4,1P2E13.5)
  104     continue
C
          write (LU,105) LINE
  105     format(' ',A120)
  106   continue
      if(JE.lt.LDL) goto 100
C     !END
      call BYE ('MENTHA')
C
      return
      end
