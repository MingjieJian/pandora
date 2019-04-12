      subroutine ZEPELIN
     $(NO,N,NL,LRJ,IRKCOMP,TAUJ,AKJ,TKRJ,RKCJ,RKI)
C
C     Rudolf Loeser, 1975 Sep 10
C---- Prints details of Additional Photoionization calculation.
C     (This is version 2 of ZEPELIN.)
C     !DASH
      save
C     !DASH
      real*8 AKJ, RKCJ, RKI, TAUJ, TKRJ
      integer IN, IRKCOMP, J, LRJ, N, NL, NO, jummy
C     !DASH
      external ABJECT, MINK, EUREI, HI, BYE
C
C               LRJ(NL), TKRJ(MLS), TAUJ(N,MLS), RKCJ(MLS), RKI(N,NSL),
      dimension LRJ(*),  TKRJ(*),   TAUJ(N,*),   RKCJ(*),   RKI(N,*),
C
C               IRKCOMP(NSL), AKJ(N,MLS)
     $          IRKCOMP(*),   AKJ(N,*)
C
      call HI ('ZEPELIN')
C     !BEG
      if(NO.gt.0) then
        call ABJECT    (NO)
        write (NO,100)
  100   format(' ','Details of Additional Photoionization calculation')
C
        do 101 J = 1,NL
          if((IRKCOMP(J).gt.0).and.(LRJ(J).gt.0)) then
            call MINK  (J,LRJ,IN,jummy)
            call EUREI (NO,N,LRJ(J),J,TKRJ(IN),RKCJ(IN),TAUJ(1,IN),
     $                  AKJ(1,IN),RKI(1,J))
          end if
  101   continue
      end if
C     !END
      call BYE ('ZEPELIN')
C
      return
      end
