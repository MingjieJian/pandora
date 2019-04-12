      subroutine THATELL
     $(N,DTE,ZIN,ZAX,SDTE)
C
C     Rudolf Loeser, 2001 Nov 23
C---- Makes a shifted and scaled version of DTE, for ADARE.
C     !DASH
      save
C     !DASH
      real*8 ADD, C1, C2, DTE, DTM, FAC, SDTE, ZAX, ZERO, ZIN
      integer IMAX, IMIN, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOVE1, MINMAXD, CONADD, CONSUB, DIVIDE, CONMUL, HI, BYE
C
C               DTE(N), SDTE(N)
      dimension DTE(*), SDTE(*)
C
      data C1,C2 /0.9D0, 1.1D0/
C
      call HI ('THATELL')
C     !BEG
      call MOVE1   (DTE, N, SDTE)
      call MINMAXD (SDTE, 1, N, IMIN, IMAX)
C
      DTM = SDTE(IMIN)
      call CONSUB  (DTM, SDTE, N)
C
      FAC = C1*ZAX-C2*ZIN
      if(FAC.le.ZERO) then
        FAC = ZAX-ZIN
        ADD = ZIN
      else
        ADD = C2*ZIN
      end if
      call DIVIDE  (FAC, SDTE(IMAX), FAC)
C
      call CONMUL  (FAC, SDTE, N)
      call CONADD  (ADD, SDTE, N)
C     !END
      call BYE ('THATELL')
C
      return
      end
