      subroutine BROME
     $(IPRDF,DL,K,J,PRINT)
C
C     Rudolf Loeser, 1987 Nov 02
C---- Determines whether to print PRD data for the J'th frequency.
C     (This is version 2 of BROME.)
C     !DASH
      save
C     !DASH
      real*8 DL
      integer I, IPRDF, J, JDN, JUP, K, KIND
      logical PRINT
C     !DASH
      external QUEBEC, HI, BYE
C
C               DL(K)
      dimension DL(*)
C
      call HI ('BROME')
C     !BEG
      PRINT = .true.
C
      if(J.eq.1) then
C       Initialize KIND, the index of DL = 0
        call QUEBEC (DL,K,'DL','BROME',KIND)
      end if
C
      if(J.eq.KIND) then
        goto 101
      end if
C
      JUP = KIND
      JDN = KIND
      do 100 I = 1,K,IPRDF
C
        JUP = JUP+IPRDF
        if(J.eq.JUP) then
          goto 101
        end if
C
        JDN = JDN-IPRDF
        if(J.eq.JDN) then
          goto 101
        end if
C
  100 continue
      PRINT = .false.
C
  101 continue
C     !END
      call BYE ('BROME')
C
      return
      end
