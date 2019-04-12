      subroutine MANUEL
     $(NWT,MRP,MRPP,JLEV,XNU,XNUC,WT,RNUP,RCPP,RNU,RCP,SIG)
C
C     Rudolf Loeser, 1982 Dec 13
C---- Sets up raw augmented tables, for level JLEV, for BEEFUP.
C     (This is version 2 of MANUEL.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, RCP, RCPP, RN, RNU, RNUP, SIG, WT, XNU, XNUC
      integer I, J, JLEV, KAR, KIN, KNS, LOOK, MRP, MRPP, NOTE, NWT
      logical DUMP
      character LINE*120
C     !DASH
      external LANTERN, RENTAL, RANTER, DAMON, LOOKSD, HI, BYE
C
C               RCP(MRX+1), RCPP(MRX+1), RNU(MRX+1), WT(NWT), XNU(NSL),
      dimension RCP(*),     RCPP(*),     RNU(*),     WT(*),   XNU(*),
C
C               RNUP(MRX+1), XNUC(NSL)
     $          RNUP(*),     XNUC(*)
C
      data DELTA /1.D-8/
C     !EJECT
C
      call HI ('MANUEL')
C     !BEG
C---- Set up dump if needed
      call LANTERN    (DUMP, 'MANUEL', LINE, KAR, JLEV, RNUP, MRPP)
C---- Initialize
      MRP = 0
      KNS = 0
C
C---- Look at each wavelength value (in descending order)
      do 105 I = NWT,1,-1
C----   Convert to ratio of frequencies
        call DAMON    (WT(I), XNU, XNUC, JLEV, RN)
C
C       Look only for RN values falling  BETWEEN  existing RNUP values
        call LOOKSD   (RNUP, MRPP, DELTA, RN, KIN, NOTE, LOOK)
        if((LOOK.eq.1).and.(NOTE.eq.2)) then
C         (Dump ?)
          call RENTAL (DUMP, LINE, KAR, KNS, KIN, MRP)
C----     Copy any values that have not yet been entered into RNU, RCP
          if(KIN.gt.KNS) then
            do 104 J = (KNS+1),KIN
              MRP = MRP+1
              RNU(MRP) = RNUP(J)
              RCP(MRP) = RCPP(J)
  104       continue
          end if
C----     Insert RN value into RNU, and put interpolation signal in RCP
          MRP = MRP+1
          RNU(MRP) = RN
          RCP(MRP) = SIG
C
          KNS = KIN
        end if
  105 continue
C
      if(MRPP.gt.KNS) then
C----   Copy final values
        do 106 J = (KNS+1),MRPP
          MRP = MRP+1
          RNU(MRP) = RNUP(J)
          RCP(MRP) = RCPP(J)
  106   continue
      end if
C
C     (Final dump ?)
      call RANTER     (DUMP, 'MANUEL', LINE, KAR, JLEV, RNU, MRP)
C     !END
      call BYE ('MANUEL')
C
      return
      end
