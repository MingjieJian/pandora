      subroutine COHOSH
     $(X,LDL,DDL,WVL,CDW,DNU,KF,XIF,KXLIM,KX,XIX,OFF,IPNT,W,IW,GOODY)
C
C     Rudolf Loeser, 1989 Feb 08
C---- Makes an XI table for a blended line.
C     OFF and IPNT are working storage.
C     (This is version 2 of COHOSH.)
C     !DASH
      save
C     !DASH
      real*8 CDW, DDL, DELTA, DNU, OFF, W, WVL, X, XIF, XIX
      integer IPNT, IW, KF, KX, KXLIM, L, LDL
      logical GOODY, YES
C     !DASH
      external BOLA, MOVE1, CONADD, GOURD, PALLE, HARE, ABRAXAS, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               DDL(LDL), XIF(KM), XIX(KM), OFF(LDL), IPNT(LDL)
      dimension DDL(*),   XIF(*),  XIX(*),  OFF(*),   IPNT(*)
C
      data DELTA /1.D-5/
C
      call HI ('COHOSH')
C     !BEG
C---- Make sorted table of distinct offsets
      call BOLA     (LDL, DDL, CDW, L, OFF, DELTA, IPNT)
C
C---- Set up final table, XIX, of length KX
      if(L.le.1) then
        call MOVE1  (XIF, KF, XIX)
        KX = KF
        call CONADD (OFF(1), XIX, KX)
      else
        call GOURD  (L, OFF, KF, XIF, KX, XIX, DELTA, IPNT)
      end if
C---- Make sure that neither "end value" is zero
      call ABRAXAS  (XIX, KX)
C---- Augment this table for possible coincident absorption edges and
C     background lines
      call PALLE    (X, W, IW, WVL, CDW, DNU, XIX, KX, YES)
C
C---- Check size of KX (with error stop if needed)
      call HARE     (KX, GOODY)
C     !END
      call BYE ('COHOSH')
C
      return
      end
