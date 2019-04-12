      subroutine ESTOC
     $(N,IU,IL,XNU,SET,B,S,W,IW)
C
C     Rudolf Loeser, 1990 Jul 27
C---- Drives direct calculation of S.
C     !DASH
      save
C     !DASH
      real*8 B, S, SET, W, XNU
      integer IL, IN, IS, ISOB, ISR, IU, IW, IXVAL, MOX, N
C     !DASH
      external NINTI, SADIE, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               SET(N,MUL), B(N), XNU(NSL), S(N)
      dimension SET(*),     B(*), XNU(*),   S(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXVAL ),(IN( 2),ISOB  ),(IN( 3),ISR   )
C
      call HI ('ESTOC')
C     !BEG
C     (Get, and allocate, W allotment)
      call NINTI (IN, IS, MOX, 'ESTOC')
C
      call SADIE (N, IU, IL, XNU, SET, W(ISR), B, W(ISOB), W(IXVAL),
     $            W, IW, S)
C
C     (Give back W allotment)
      call WGIVE (W, 'ESTOC')
C     !END
      call BYE ('ESTOC')
C
      return
      end
