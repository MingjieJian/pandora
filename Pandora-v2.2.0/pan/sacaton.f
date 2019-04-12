      subroutine SACATON
     $(N,NL,NSL,GMI,RLI,RKI,SPKL,SLT)
C
C     Rudolf Loeser, 1978 Jun 27
C---- Provides for supplementary levels, for SETPIJ.
C     !DASH
      save
C     !DASH
      real*8 GMI, RKI, RLI, SLT, SPKL
      integer I, KOLEV, N, NL, NSL
C     !DASH
      external ZERO1, DEER, HI, BYE
C
C               SLT(N), GMI(N,NSL), RLI(N,NSL), RKI(N,NSL), SPKL(N)
      dimension SLT(*), GMI(N,*),   RLI(*),     RKI(*),     SPKL(*)
C
      data KOLEV /1/
C
      call HI ('SACATON')
C     !BEG
      call ZERO1  (SLT,N)
      if(NSL.gt.NL) then
C----   Compute SPKL
        call DEER (N,NL,NSL,KOLEV,GMI,RLI,RKI,SPKL)
C----   Now, form SLT = SPKL * GM(level KOLEV)
        do 100 I = 1,N
          SLT(I) = SPKL(I)*GMI(I,KOLEV)
  100   continue
      end if
C     !END
      call BYE ('SACATON')
C
      return
      end
