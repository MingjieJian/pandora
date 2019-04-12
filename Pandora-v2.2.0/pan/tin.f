      subroutine TIN
     $(N,NL,RKI,LRJ,TKRJ,RKCJ,TAUJ,AKJ,IQRK,ANY,XCBL,TAUK,HE304,M304,
     $ J304I)
C
C     Rudolf Loeser, 1975 Sep 10
C---- Computes additional photoionization, for all levels.
C     (This is version 2 of TIN.)
C     !DASH
      save
C     !DASH
      real*8 AKJ, HE304, RKCJ, RKI, TAUJ, TAUK, TKRJ, XCBL
      integer IN, IQRK, J, J304I, LRJ, M304, N, NL, jummy
      logical ANY
C     !DASH
      external MINK, DINGHY, HI, BYE
C
C               LRJ(NL), AKJ(N,MLS), IQRK(NSL), XCBL(Miklen), HE304(N),
      dimension LRJ(*),  AKJ(N,*),   IQRK(*),   XCBL(*),      HE304(*),
C
C               RKCJ(MLS), TAUJ(N,MLS), RKI(N,NSL), TKRJ(MLS), TAUK(N)
     $          RKCJ(*),   TAUJ(N,*),   RKI(N,*),   TKRJ(*),   TAUK(*)
C
      call HI ('TIN')
C     !BEG
      do 100 J = 1,NL
C
        if(LRJ(J).gt.0) then
          if(IQRK(J).gt.0) then
            ANY = .true.
C
            call MINK   (J,LRJ,IN,jummy)
            call DINGHY (LRJ(J),TKRJ(IN),RKCJ(IN),TAUJ(1,IN),AKJ(1,IN),
     $                   N,RKI(1,J),XCBL,TAUK,HE304,M304,J304I)
          end if
        end if
C
  100 continue
C     !END
      call BYE ('TIN')
C
      return
      end
