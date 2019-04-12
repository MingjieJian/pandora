      subroutine ZANCLE
     $(N,NL,BATA,BDI,SET)
C
C     Rudolf Loeser, 2004 May 24
C---- Computes SET: Stimulated Emission Term, for every transition.
C     (See also PYRGOS.)
C     (This is version 2 of ZANCLE.)
C     !DASH
      save
C     !DASH
      real*8 BATA, BDI, BRAT, SET
      integer I, IL, IU, IUL, N, NL
C     !DASH
      external INDXUL, DIVIDE, HI, BYE
C
C               BATA(N,MUL), BDI(N,NL), SET(N,MUL)
      dimension BATA(N,*),   BDI(N,*),  SET(N,*)
C
      call HI ('ZANCLE')
C     !BEG
      do 102 IU = 2,NL
        do 101 IL = 1,(IU-1)
          call INDXUL   (IU, IL, IUL)
C
          do 100 I = 1,N
            call DIVIDE (BDI(I,IU), BDI(I,IL), BRAT)
            SET(I,IUL) = BRAT*BATA(I,IUL)
  100     continue
C
  101   continue
  102 continue
C     !END
      call BYE ('ZANCLE')
C
      return
      end
