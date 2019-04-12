      subroutine LEDGER
     $(NO,N,Z,TE,NW,MUX,Y,MYX,XLAM,BRIGHT,YNT,XLTIT,ISTAR,MODE,LFB,
     $ LFBV)
C
C     Rudolf Loeser, 1980 Nov 17
C---- Prints and plots a Spectrum Summary.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, TE, X, XLAM, XLTIT, Y, YNT, Z, dummy
      integer I, IJECT, ISTAR, J, KSYM, LFB, LFBV, LINES, LINIT, MODE,
     $        MUX, MUXJ, MYX, N, NO, NW, jummy
      character LINE*127
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external  BLUEJAY, FOUND, ETHEL, ELENA, KPLOTC, RHEA, HOUND,
     $          HI, BYE
      intrinsic mod
C
C               MUX(NW), Y(NW), XLAM(NW), BRIGHT(NW), XLTIT(NW), TE(N),
      dimension MUX(*),  Y(*),  XLAM(*),  BRIGHT(*),  XLTIT(*),  TE(*),
C
C               YNT(NW), ISTAR(NW), MODE(NW), MYX(NW), Z(N)
     $          YNT(*),  ISTAR(*),  MODE(*),  MYX(*),  Z(*)
C     !EJECT
C
      call HI ('LEDGER')
C     !BEG
C---- Initialize
      call BLUEJAY     (NO, LINES, LINIT, IJECT)
      call FOUND       (N, TE, NW, BRIGHT, IMAGE, MUX)
C---- Loop over all depths
      J = 1
      MUXJ = MUX(J)
      do 101 I = 1,N
        KSYM = 1
  100   continue
        if(MUXJ.eq.I) then
C----     Encode print line, with spectrum data
          if(KSYM.eq.1) then
            call ETHEL (3, LINE, I, KSYM, Z(I), TE(I), Y(J), XLAM(J),
     $                  BRIGHT(J), YNT(J), XLTIT(J), ISTAR(J), MODE(J),
     $                  MYX(J))
          else
            call ETHEL (2, LINE, I, KSYM, dummy, dummy, Y(J), XLAM(J),
     $                  BRIGHT(J), YNT(J), XLTIT(J), ISTAR(J), MODE(J),
     $                  MYX(J))
          end if
C----     Print
          call ELENA   (NO, LINE, LINES, LINIT, IJECT, LFB, LFBV)
C----     Enter point in plot
          X = I
          call KPLOTC  (IMAGE, X, BRIGHT(J), SYMBS(KSYM))
          KSYM = mod(KSYM,46)+1
C----
          J = J+1
          if(J.le.NW) then
            MUXJ = MUX(J)
            goto 100
          end if
          MUXJ = N+1
        else
C----     Encode and print line, without spectrum data
          if(KSYM.eq.1) then
            call ETHEL (1, LINE, I, KSYM, Z(I), TE(I), dummy, dummy,
     $                  dummy, dummy, dummy, jummy, jummy, jummy)
            call ELENA (NO, LINE, LINES, LINIT, IJECT, LFB, LFBV)
          end if
        end if
  101 continue
C---- Print explanations
      call RHEA        (NO, -1)
C---- Plot graph
      call HOUND       (NO, IMAGE, LFB)
C     !END
      call BYE ('LEDGER')
C
      return
      end
