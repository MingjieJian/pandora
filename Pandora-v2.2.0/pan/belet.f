      subroutine BELET
     $(N,NL,XND,XNK,PND)
C
C     Rudolf Loeser, 1982 Jan 11
C---- Sets up PND, for CRATON.
C     !DASH
      save
C     !DASH
      real*8 PND, XND, XNK
      integer N, NL
C     !DASH
      external MOVE1, HI, BYE
C
C               PND(N,(NL+1)), XND(N,NL), XNK(N)
      dimension PND(N,*),      XND(N,*),  XNK(*)
C
      call HI ('BELET')
C     !BEG
      call MOVE1 (XND,(N*NL),PND(1,     1))
      call MOVE1 (XNK, N    ,PND(1,(NL+1)))
C     !END
      call BYE ('BELET')
C
      return
      end
