      subroutine CRUX
     $(EP1,EP2,METEP,EPCBR,WEP,IQEPS,CQTAIL,N,MQT,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C---- Prints Lyman Epsilons data, for LINTEL.
C     (This is version 2 of CRUX.)
C     !DASH
      save
C     !DASH
      real*8 CQTAIL, EP1, EP2, EPCBR, WEP
      integer IQEPS, METEP, MQT, N, NO
      character METHEP*10
C     !DASH
      external VECOUT, LINER, HI, BYE
C
C               EP1(N), EP2(N), CQTAIL(MQT)
      dimension EP1(*), EP2(*), CQTAIL(*)
C
      dimension METHEP(4)
C
      data METHEP /'      Nova', ' Complex-U',
     $             ' Complex-L', '     Chain'/
C
      call HI ('CRUX')
C     !BEG
      call VECOUT   (NO,EP1,N,'Input values of EP1')
C
      if((METEP.eq.3).and.(IQEPS.gt.0).and.(MQT.gt.0)) then
        call VECOUT (NO,CQTAIL,MQT,'QTAIL, for eliminating EP1 < 0')
      end if
C
      call VECOUT   (NO,EP2,N,'Input values of EP2')
C
      call LINER    (1,NO)
      write (NO,100) METEP,METHEP(METEP+1),WEP,EPCBR
  100 format(' ','METEP   ',I3,' =',A10,11X,
     $                      'Lyman Epsilons method selection parameter'/
     $       ' ','WEP     ',1PE12.4,14X,'Epsilon-1 weighting parameter'/
     $       ' ','EPCBR   ',E12.4,14X,'Branching ratio')
C     !END
      call BYE ('CRUX')
C
      return
      end
