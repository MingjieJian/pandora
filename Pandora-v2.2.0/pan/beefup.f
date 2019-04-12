      subroutine BEEFUP
     $(RNUP,RCPP,MRPP,RNU,RCP,MRP,JLEV,XNU,XNUC,WT,NWT)
C
C     Rudolf Loeser, 1982 Dec 13
C---- Makes augmented RNU and RCP tables for level JLEV.
C     (This is version 2 of BEEFUP.)
C     !DASH
      save
C     !DASH
      real*8 RCP, RCPP, RNU, RNUP, SIG, WT, XNU, XNUC
      integer JLEV, MRP, MRPP, MRX, NWT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(30),MRX)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external MANUEL, THEODOR, HALT, HI, BYE
C
C               RCPP(MRX+1), RNU(MRX+1), RCP(MRX+1), XNU(NSL), WT(NWT),
      dimension RCPP(*),     RNU(*),     RCP(*),     XNU(*),   WT(*),
C
C               RNUP(MRX+1), XNUC(NSL)
     $          RNUP(*),     XNUC(*)
C
      data SIG /-1.D0/
C
      call HI ('BEEFUP')
C     !BEG
C---- Check whether augmentation possible
      if(MRPP.le.1) then
C
C----   No - make straight copies
        MRP = 1
        RNU(1) = RNUP(1)
        RCP(1) = RCPP(1)
      else
C
C----   Yes - make raw augmented table
        call MANUEL    (NWT, MRP, MRPP, JLEV, XNU, XNUC, WT, RNUP,
     $                  RCPP, RNU, RCP, SIG)
        if(MRP.le.MRX) then
C----     Interpolate to get missing RCP values
          call THEODOR (MRP, JLEV, XNU, XNUC, RNU, RCP, SIG)
C
        else
C----     Error -- bail out
          write (MSSLIN(1),100) JLEV,MRP,MRX
  100     format('Building RNUP table for level ',I5,': MRP =',I12,
     $           ' is greater than MRX =',I12)
          call HALT    ('BEEFUP', 1)
        end if
C
      end if
C     !END
      call BYE ('BEEFUP')
C
      return
      end
