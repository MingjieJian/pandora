      subroutine QUACK
     $(LU,N,ITMX,S,VEC)
C
C     Rudolf Loeser, 2005 May 17
C---- Iterative analysis of S (c.f. STOCK).
C     (This is version 7 of QUACK.)
C     !DASH
      save
C     !DASH
      real*8 S, VEC
      integer INP, ITMX, LU, MOD, N
      character TITLE*15
C     !DASH
      external SUGAR, SLENDER, SPICY, IDATH, HI, BYE
C
C               S(N,ITMX), VEC(N)
      dimension S(*),      VEC(*)
C
      data INP,MOD,TITLE /0, 0, 'S (Perseus)     '/
C
      call HI ('QUACK')
C     !BEG
      if((LU.gt.0).and.(ITMX.gt.1)) then
        call SUGAR   (N, ITMX, S)
        call SLENDER (LU, TITLE)
        call SPICY   (LU, N, ITMX, INP, S)
        call IDATH   (LU, N, S, VEC, ITMX, INP, TITLE, MOD)
      end if
C     !END
      call BYE ('QUACK')
C
      return
      end
