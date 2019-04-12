      subroutine ARBUTUS
     $(N,WN,XA,RR,XJNU,XJNUO,ITS,KODE,CSFCRIT)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes Jnu, for CRANE.
C     !DASH
      save
C     !DASH
      real*8 CSFCRIT, HALF, RR, WN, XA, XJNU, XJNUO, ZERO
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
      external NEARLY, ROSIN, HI, BYE
C
C               WN(N,N), XA(N), RR(N), XJNU(N), XJNUO(N)
      dimension WN(*),   XA(*), RR(*), XJNU(*), XJNUO(*)
C
      data MXKNT /25/
C
      call HI ('ARBUTUS')
C     !BEG
      call NEARLY  (XA,N, ZERO,HALF, KEQ)
      if(KEQ.ne.N) then
        KODE = 0
        ITS  = -1
      else
        call ROSIN (N,WN,XA,RR,XJNU,XJNUO,MXKNT,KNT,CSFCRIT)
        if(KNT.lt.MXKNT) then
          KODE = 1
          ITS  = KNT
        else
          KODE = 0
          ITS  = 99
        end if
      end if
C     !END
      call BYE ('ARBUTUS')
C
      return
      end
