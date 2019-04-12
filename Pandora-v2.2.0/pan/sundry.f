      subroutine SUNDRY
     $(W,IW,NO,N,Z,TE,MUX,MYX,Y,XLAM,BRIGHT,YNT,XLTIT,ISTAR,MODE,LFB,
     $ LFBV)
C
C     Rudolf Loeser, 1973 Apr 04
C---- Generates a Spectrum Summary.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, TE, W, XLAM, XLTIT, Y, YNT, Z
      integer ISIG, ISTAR, IW, LFB, LFBV, MODE, MUX, MYX, N, NO, NW
C     !DASH
      external REAP, ARRAS, LEDGER, LINER, HI, BYE
C
      dimension W(*), IW(*)
C
C               XLTIT(NW), MUX(NW), Y(NW), XLAM(NW), BRIGHT(NW), TE(N),
      dimension XLTIT(*),  MUX(*),  Y(*),  XLAM(*),  BRIGHT(*),  TE(*),
C
C               YNT(NW), ISTAR(NW), MODE(NW), MYX(NW), Z(N)
     $          YNT(*),  ISTAR(*),  MODE(*),  MYX(*),  Z(*)
C
      call HI ('SUNDRY')
C     !BEG
C---- Get data
      call REAP       (NW,MUX,MYX,XLAM,BRIGHT,Y,YNT,XLTIT,ISTAR,MODE,
     $                 LFB,N)
      if(NW.gt.0) then
C----   Sort them . . .
        call ARRAS    (W,IW,N,NW,MUX,BRIGHT,Y,XLAM,YNT,XLTIT,ISTAR,
     $                 MODE,MYX,ISIG)
        if(ISIG.gt.0) then
C----     . . . and print and plot
          call LEDGER (NO,N,Z,TE,NW,MUX,Y,MYX,XLAM,BRIGHT,YNT,XLTIT,
     $                 ISTAR,MODE,LFB,LFBV)
        else
          call LINER  (3,NO)
          write (NO,100)
  100     format(' ','Trouble setting up SPECTRUM SUMMARY (sorting ',
     $               'failed): skipped.')
        end if
      end if
C     !END
      call BYE ('SUNDRY')
C
      return
      end
