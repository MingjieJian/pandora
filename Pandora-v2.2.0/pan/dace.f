      subroutine DACE
     $(DCON,CCON,N,CRIT,DH,CH,DDR,CDR,K)
C
C     Rudolf Loeser, 1992 Apr 09
C---- Coalesces "unresolvable" components for TABOR.
C     (This is version 3 of DACE.)
C     !DASH
      save
C     !DASH
      real*8 CCON, CDR, CH, CRIT, CSUM, DCON, DDR, DH, ONE, S, ZERO
      integer I, J, K, N, NH
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external CLAUDE, ARRSUM, WESLEY, HI, BYE
C
C               DCON(N), CCON(N), DH(N/2), CH(N/2), DDR(K), CDR(K)
      dimension DCON(*), CCON(*), DH(*),   CH(*),   DDR(*), CDR(*)
C
      call HI ('DACE')
C     !BEG
      if(DCON(N).le.CRIT) then
        I = 0
        goto 102
      end if
C---- Extract half sets
      call CLAUDE   (DCON,CCON,N,DH,CH,NH)
C     !EJECT
C---- Treat core
      J = 0
  100 continue
        if(DH(J+1).lt.CRIT) then
          J = J+1
          go to 100
        end if
      if(J.gt.0) then
        DH(1) = ZERO
        call ARRSUM (CH,J,CSUM)
        CH(1) = CSUM
C
C----   Initialize outward scan
        J = J+1
        I = 2
      else
        J = 1
        I = 1
      end if
      DH(I) = DH(J)
      CH(I) = CH(J)
C
  101 continue
C----   Step along to next given component
        J = J+1
        if(J.le.NH) then
          if((DH(J)-DH(I)).lt.CRIT) then
C----       Coalesce it
            S = CH(I)+CH(J)
            DH(I) = DH(I)*CH(I)/S+DH(J)*CH(J)/S
            CH(I) = S
          else
C----       Acccept it as is
            I = I+1
            DH(I) = DH(J)
            CH(I) = CH(J)
          end if
          go to 101
        end if
C
  102 continue
      if(I.le.1) then
C----   Minimal default
        K = 1
        DDR(1) = ZERO
        CDR(1) = ONE
      else
C----   Restore full sets
        call WESLEY (DH,CH,I,DDR,CDR,K)
      end if
C     !END
      call BYE ('DACE')
C
      return
      end
