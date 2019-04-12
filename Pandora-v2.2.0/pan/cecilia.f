      subroutine CECILIA
     $(N,X,R,BB,S,CQ,ITS,KODE,CSFCRIT)
C
C     Rudolf Loeser, 1981 Jul 21
C---- Computes S for SASKIA.
C     (This is version 5 of CECILIA.)
C     !DASH
      save
C     !DASH
      real*8 BB, CQ, CSFCRIT, HALF, R, S, X, ZERO
      integer ITS, KEQ, KNT, KODE, MXKNT, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external NEARLY, GARNET, HI, BYE
C
C               X(N,N), R(N), BB(N), S(N), CQ(N)
      dimension X(*),   R(*), BB(*), S(*), CQ(*)
C
      data MXKNT /25/
C
      call HI ('CECILIA')
C     !BEG
      call NEARLY   (R,N, ZERO,HALF, KEQ)
      if(KEQ.ne.N) then
        KODE = 0
        ITS  = -1
      else
        call GARNET (N,X,R,BB,S,CQ,MXKNT,KNT,CSFCRIT)
        if(KNT.lt.MXKNT) then
          KODE = 1
          ITS  = KNT
        else
          KODE = 0
          ITS  = 99
        end if
      end if
C     !END
      call BYE ('CECILIA')
C
      return
      end
