      subroutine ELMO
     $(NO,EMU,NW,WAVES,WTAB,EMINT,BRIGHT,MXKOUNT,CRIT,KSTAR)
C
C     Rudolf Loeser, 1980 Oct 01
C---- Computes and prints Color Temperatures,
C     for a given look-angle (mu, =EMU), for SHIVA.
C     !DASH
      save
C     !DASH
      real*8 A, BRIGHT, CRIT, DELTA, EI, EIP, EMINT, EMU, TB, TCA, TCB,
     $       W, WAVES, WP, WTAB, WV, ZERO
      integer I, IFLG, KOUNTA, KOUNTB, KSTAR, MXKOUNT, NO, NW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external COMPD, ELMER, MATAI, BRAID, HI, BYE
C
C               WAVES(Nmkuse), EMINT(Nmkuse), BRIGHT(Nmkuse),
      dimension WAVES(*),      EMINT(*),      BRIGHT(*),
C
C               WTAB(Nmkuse)
     $          WTAB(*)
C
      data DELTA /1.D-8/
C
      call HI ('ELMO')
C     !BEG
C---- Loop over all wavelength intervals
      do 100 I = 1,(NW-1)
        W  = WAVES(I)
        WP = WAVES(I+1)
        call COMPD     (W, WP, DELTA, IFLG)
        if(IFLG.eq.0) then
C----     Skip if interval of zero length
          TCA = ZERO
        else
          EI  = EMINT(I)
          EIP = EMINT(I+1)
          TB  = BRIGHT(I)
          WV  = WTAB(I)
          if((EI.gt.ZERO).and.(EIP.gt.ZERO).and.(TB.gt.ZERO)) then
C----       Good data - compute
            call ELMER (TB, W, WP, EI, EIP, TCA, KOUNTA, TCB, KOUNTB,
     $                  MXKOUNT, CRIT, A)
          else
C----       Bad data - print error message and skip
            call MATAI (NO, WV)
            goto 100
          end if
        end if
C----   Print for this interval
        call BRAID     (NO, EMU, W, WV, TB, EI, A, TCA, KOUNTA, TCB,
     $                  KOUNTB, MXKOUNT, KSTAR)
  100 continue
C     !END
      call BYE ('ELMO')
C
      return
      end
