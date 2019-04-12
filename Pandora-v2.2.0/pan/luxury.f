      subroutine LUXURY
     $(II,N,XJNUR,XJNU)
C
C     Rudolf Loeser, 1978 Apr 09
C---- Extends a reduced set of Continuous Intensity values.
C     !DASH
      save
C     !DASH
      real*8 XJNU, XJNUR
      integer II, IIM, N, NII
C     !DASH
      external SET1, MOVE1, HI, BYE
C
C               XJNUR(N), XJNU(N)
      dimension XJNUR(*), XJNU(*)
C
      call HI ('LUXURY')
C     !BEG
      if(II.gt.0) then
        IIM = II-1
        call SET1  (XJNU    ,IIM,XJNUR(1))
        NII = N-IIM
        call MOVE1 (XJNUR(2),NII,XJNU(II))
      else
        call MOVE1 (XJNUR   ,N  ,XJNU    )
      end if
C     !END
      call BYE ('LUXURY')
C
      return
      end
