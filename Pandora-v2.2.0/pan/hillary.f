      subroutine HILLARY
     $(N,XLM,TE,H1,H2,OPAC,SHL,ITAU,DMPI)
C
C     Rudolf Loeser, 2002 Sep 24
C---- Computes a value of the H Lyman alpha background source function.
C     (This is version 3 of HILLARY.)
C     !DASH
      save
C     !DASH
      real*8 EX, H1, H2, OPAC, PFAC, SHL, TE, WLIN, XDEN, XLM, XNUM
      integer ITAU, N
      logical DMPI
C     !DASH
      external HILLY, RALLY, HI, BYE
C
      call HI ('HILLARY')
C     !BEG
      call HILLY   (N, XLM, TE, H1, H2, SHL, WLIN, PFAC, EX, XNUM, XDEN)
      if(DMPI) then
        call RALLY (ITAU, XLM, WLIN, TE, H1, H2, PFAC, EX, XNUM, XDEN,
     $              OPAC, SHL)
      end if
C     !END
      call BYE ('HILLARY')
C
      return
      end
