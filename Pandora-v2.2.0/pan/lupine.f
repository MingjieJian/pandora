      subroutine LUPINE
     $(NO,MRR,FRR,DRP,DR,A,EMINT,DSK)
C
C     Rudolf Loeser, 1981 Jul 01
C---- Prints, for RAJA.
C     !DASH
      save
C     !DASH
      real*8 A, DR, DRP, DSK, EMINT, FRR
      integer I, MRR, NO
C     !DASH
      external LINER, HI, BYE
C
C               FRR(MRR), DRP(MRR), DR(MRR), A(MRR), EMINT(MRR)
      dimension FRR(*),   DRP(*),   DR(*),   A(*),   EMINT(*)
C
      call HI ('LUPINE')
C     !BEG
      if(NO.gt.0) then
        call LINER (2,NO)
        write (NO,100)
  100   format(' ',11X,'FRR',12X,'DRP',12X,'DR',14X,'A',14X,'I')
        call LINER (1,NO)
C
        write (NO,101) (I,FRR(I),DRP(I),DR(I),A(I),EMINT(I),I=1,MRR)
  101   format(5(' ',I3,1P5E15.4/))
C
        call LINER (1,NO)
        write (NO,102) DSK
  102   format(' ',77X,'DSK =',1PE11.4)
      end if
C     !END
      call BYE ('LUPINE')
C
      return
      end
