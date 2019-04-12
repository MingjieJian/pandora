      subroutine DOGWOOD
     $(XND,N,NL,RND,ZN,NN,IPNT,IWRK,IFLG)
C
C     Rudolf Loeser, 1999 Nov 05
C---- Establishes ranks of level populations.
C     (This is version 4 of DOGWOOD.)
C     !DASH
      save
C     !DASH
      real*8 RND, XND, ZN
      integer I, IFLG, IPNT, IWRK, J, N, NL, NN
C     !DASH
      external MOVED, INDARRI, SINGD, ORDERI, HI, BYE
C
C               XND(N,NL), RND(N,NL), ZN(NL), NN(NL), IPNT(NL), IWRK(NL)
      dimension XND(N,*),  RND(N,*),  ZN(*),  NN(*),  IPNT(*),  IWRK(*)
C
      call HI ('DOGWOOD')
C     !BEG
      do 101 I = 1,N
C
        call MOVED   (XND(I,1), N, NL, ZN, 1, NL)
        call SINGD   (ZN, NL, IFLG, IPNT)
        if(IFLG.le.0) then
          goto 102
        end if
C
        call INDARRI (NN, 1, 1, NL)
        call ORDERI  (NN, IPNT, NL, IWRK)
C
        do 100 J = 1,NL
          RND(I,NN(J)) = J
  100   continue
C
  101 continue
C
  102 continue
C     !END
      call BYE ('DOGWOOD')
C
      return
      end
