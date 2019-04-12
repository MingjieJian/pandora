      subroutine ADANEXT
     $(SUB,VEC,PD,KMAX,GOOD)
C     Rudolf Loeser, 1986 Jul 28
C---- Generates and sums a sequence of component subintegrals,
C     for ADAIR.
C     !DASH
      save
C     !DASH
      real*8 DMAX, DMIN, FL, FLIM, FLM, FM, FR, FRM, GG, GS, PD, VEC,
     $       XL, XLM, XM, XR, XRM
      integer KD, KMAX, NF, NOUT
      logical FULL, GOOD, USE
C     !COM
      common /ADACOMM/ XL,FL,XLM,FLM,XM,FM,XRM,FRM,XR,FR,
     $                 FLIM,DMAX,DMIN,GG,NOUT,KD,NF
C     !DASH
      external SUB, ADAPOP, ADAPUSH, ADACOMP
C
      dimension PD(6,KMAX), VEC(*)
C     !EJECT
C
C     !BEG
  100 continue
      if(KD.gt.0) then
C----   Stack is not empty: get next interval to work on
        call ADAPOP      (PD)
  101   continue
C----   Compute subintegral, and test its acceptability
        call ADACOMP     (GS,SUB,VEC,USE)
        if(USE) then
C----     The subintegral is acceptable:
C         update total integral, and do next interval from stack
          GG = GG+GS
          goto 100
        else
C----     The subintegral is not acceptable:
C         save right half of interval, and refine left half
          FULL = KD.ge.KMAX
          if(FULL) then
C----       There is not enough room in PD for deferring the right
C           half to permit refining the left half:
C           must accept subintegral willy-nilly, but set error flag
            GG = GG+GS
            GOOD = .false.
            goto 100
          else
C----       Push right half-interval on stack
            call ADAPUSH (PD)
C----       Set up left half-interval as the next interval to work on
            XR = XM
            FR = FM
            XM = XLM
            FM = FLM
            goto 101
          end if
        end if
      end if
C     !END
C
      return
      end
