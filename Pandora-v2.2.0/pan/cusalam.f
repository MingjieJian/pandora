      subroutine CUSALAM
     $(I,XLM,TE,HN,N,EMA,DMPI)
C
C     Rudolf Loeser, 2003 Jan 15
C---- Computes H(15/1) Ly line background source function, for SAIGA.
C     !DASH
      save
C     !DASH
      real*8 EMA, EX, HN, PFAC, TE, WLIN, XDEN, XLM, XNUM
      integer I, N
      logical DMPI
C     !DASH
      external HILLY, AQUAMA, HI, BYE
C
C               TE(N), HN(N,Limdat(1))
      dimension TE(*), HN(N,*)
C
      call HI ('CUSALAM')
C     !BEG
      call HILLY    (15, XLM, TE(I), HN(I,1), HN(I,15), EMA, WLIN, PFAC,
     $               EX, XNUM, XDEN)
      if(DMPI) then
        call AQUAMA (15, HN(I,15), XNUM, XDEN, EMA)
      end if
C     !END
      call BYE ('CUSALAM')
C
      return
      end
