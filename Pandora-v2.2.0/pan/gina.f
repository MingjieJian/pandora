      subroutine GINA
     $(NSHL,N,WZ,CSHL,WS,TITLE,LABEL,DUMP)
C
C     Rudolf Loeser, 1983 Mar 15
C---- Adds component matrices to obtain
C     total "Shell weight matrices", for FERREX.
C     (This is version 4 of GINA.)
C     !DASH
      save
C     !DASH
      real*8 CSHL, WS, WZ
      integer I, J, N, NSHL
      logical DUMP
      character CLAB*27, LABEL*33, TITLE*(*)
C     !DASH
      external ZERO1, TATAR, EVENKI, TOBA, IOTA, HI, BYE
C
C               WZ(N,N,NSHL), CSHL(N,NSHL), WS(N,N)
      dimension WZ(N,N,*),    CSHL(N,*),    WS(*)
C
      call HI ('GINA')
C     !BEG
      call ZERO1      (WS, (N**2))
C
      I = 0
      do 101 J = 1,NSHL
        write (CLAB,100) LABEL(26:33),J
  100   format(' ',A8,', reallocated:',I4)
C
        call TATAR    (I)
        if(DUMP) then
          call EVENKI (I, WZ(1,1,J), CSHL(1,J), CLAB)
        end if
        call TOBA     (WS, WZ(1,1,J), CSHL(1,J), I, N)
  101 continue
C
      if(DUMP) then
        call IOTA     (WS, N, TITLE, LABEL)
      end if
C     !END
      call BYE ('GINA')
C
      return
      end
