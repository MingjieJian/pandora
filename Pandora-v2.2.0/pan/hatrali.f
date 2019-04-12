      subroutine HATRALI
     $(TE,TCO,HND,CON,CHN,OHN,N,ABDC,ABDO,NO)
C
C     Rudolf Loeser, 1981 May 19
C---- Computes and prints molecular number densities, for HODRE.
C     !DASH
      save
C     !DASH
      real*8 ABDC, ABDO, CHN, CND, CON, ECH, ECO, EOH, HND, OHN, OND,
     $       TCO, TE
      integer I, N, NO
      logical PRNT
C     !DASH
      external CONK, PUP, HI, BYE
C
C               TE(N), HND(N), CHN(N), CON(N), TCO(N), OHN(N)
      dimension TE(*), HND(*), CHN(*), CON(*), TCO(*), OHN(*)
C
      call HI ('HATRALI')
C     !BEG
      PRNT = NO.gt.0
C
      do 100 I = 1,N
        call CONK  (ABDC, ABDO, TE(I), TCO(I), HND(I), ECO, ECH, EOH,
     $              CND, OND, CON(I), CHN(I), OHN(I))
C
        if(PRNT) then
          call PUP (NO, I, ABDC, ABDO, HND(I), TE(I), TCO(I),
     $              ECO, ECH, EOH, CND, OND, CON(I), CHN(I), OHN(I))
        end if
  100 continue
C     !END
      call BYE ('HATRALI')
C
      return
      end
