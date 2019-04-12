      subroutine MULKA
     $(XCOL,NCL,SCL,NDW,TE,V)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Gets offsets for constructing the complete CO wavelengths table.
C     !DASH
      save
C     !DASH
      real*8 CONST11, CSRT, SCL, SRT, TE, V, XCOL
      integer J, NCL, NDW
C     !DASH
      external CHOTA, MOVE1, CONMUL, RIGEL, HI, BYE
C
C               XCOL(NCL), SCL(NCL,2), TE(N), V(N)
      dimension XCOL(*),   SCL(NCL,*), TE(*), V(*)
C
      call HI ('MULKA')
C     !BEG
      if(NCL.gt.0) then
C
        call RIGEL    (11, CONST11)
        do 100 J = 1,2
          call CHOTA  (TE(NDW), V(NDW), J, SRT)
          CSRT = SRT*CONST11
          call MOVE1  (XCOL, NCL, SCL(1,J))
          call CONMUL (CSRT, SCL(1,J), NCL)
  100   continue
C
      end if
C     !END
      call BYE ('MULKA')
C
      return
      end
