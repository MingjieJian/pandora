      subroutine NORI
     $(IMAGE,JQ,JX,Z,A,SYM,KODE)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Enters a data-set into a diffusion plot.
C     !DASH
      save
C     !DASH
      real*8 A, X, Z
      integer I, JQ, JX, KODE, LINC
      character IMAGE*(*), PLUS*1, SYM*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(39),PLUS  )
C     !DASH
      external LINK, KPLOTC, HI, BYE
C
C               Z(N), A(N)
      dimension Z(*), A(*)
C
      call HI ('NORI')
C     !BEG
      LINC = 1
      if(KODE.eq.1) then
        do 100 I = JQ,JX
          call LINK   (IMAGE,A(I),Z(I),SYM,LINC)
  100   continue
        do 101 I = JQ,JX
          call KPLOTC (IMAGE,A(I),Z(I),PLUS)
  101   continue
      else
        do 102 I = JQ,JX
          X = I
          call LINK   (IMAGE,X,A(I),SYM,LINC)
  102   continue
        do 103 I = JQ,JX
          X = I
          call KPLOTC (IMAGE,X,A(I),PLUS)
  103   continue
      end if
C     !END
      call BYE ('NORI')
C
      return
      end
