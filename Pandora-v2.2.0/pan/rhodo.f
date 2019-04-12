      subroutine RHODO
     $(NO,NVY,IMAGE,WTAB,KF,DFLX,MRR,SFLX,TFLX)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Plots Flux vs. Delta-Lambda, for TOTILA.
C     !DASH
      save
C     !DASH
      real*8 DFLX, SFLX, TFLX, WTAB, XLL, XRL, YLL, YUL
      integer KF, KODE, MRR, NO, NVY
      character IMAGE*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external PONTUS, MELIC, ONION, EMESA, HI, BYE
C
C               WTAB(KM), DFLX(KM), SFLX(KM), TFLX(KM)
      dimension WTAB(*),  DFLX(*),  SFLX(*),  TFLX(*)
C
      call HI ('RHODO')
C     !BEG
C---- Get graph limits
      call PONTUS    (WTAB,KF,DFLX,MRR,SFLX,TFLX,YUL,YLL,XLL,XRL,KODE)
C
      if(KODE.eq.1) then
C----   Initialize graph image
        call MELIC   (IMAGE,XLL,XRL,YLL,YUL)
C----   Enter points
        if(MRR.gt.0) then
          call ONION (KF,1,WTAB,DFLX,IMAGE,ALPHS( 4),2)
        end if
        call ONION   (KF,1,WTAB,SFLX,IMAGE,ALPHS(19),2)
        call ONION   (KF,1,WTAB,TFLX,IMAGE,ALPHS(20),2)
C----   Print graph
        call EMESA   (NO,IMAGE,NVY)
      end if
C     !END
      call BYE ('RHODO')
C
      return
      end
