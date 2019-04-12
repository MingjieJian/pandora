      subroutine BINGE
     $(DRAW,CRAW,M,DELTA,DCON,CCON,N)
C
C     Rudolf Loeser, 1992 Jan 24
C---- Eliminates duplicate lines, for TABOR.
C     (This is version 4 of BINGE.)
C     !DASH
      save
C     !DASH
      real*8 CCON, CRAW, DCON, DELTA, DRAW
      integer FLAG, I, M, N
C     !DASH
      external COMPD, HI, BYE
C
C               DRAW(M), CRAW(M), DCON(N), CCON(N)
      dimension DRAW(*), CRAW(*), DCON(*), CCON(*)
C
      call HI ('BINGE')
C     !BEG
      N = 1
      DCON(N) = DRAW(1)
      CCON(N) = CRAW(1)
      if(M.gt.1) then
C
        do 100 I = 2,M
          call COMPD (DRAW(I),DCON(N),DELTA,FLAG)
          if(FLAG.eq.0) then
            CCON(N) = CCON(N)+CRAW(I)
          else
            N = N+1
            DCON(N) = DRAW(I)
            CCON(N) = CRAW(I)
          end if
  100   continue
C
      end if
C     !END
      call BYE ('BINGE')
C
      return
      end
