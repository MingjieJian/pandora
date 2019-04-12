      subroutine COOT
     $(N,NL,NO,FCK,CKI)
C
C     Rudolf Loeser, 1984 Jul 20
C---- Combines CKI and FCK, and prints comparisons.
C     !DASH
      save
C     !DASH
      real*8 CKI, FCK
      integer I, N, NL, NO
      logical KILROY, PRNT
C     !DASH
      external ABJECT, LINER, HI, BYE
C
C               FCK(N), CKI(N,NL)
      dimension FCK(*), CKI(N,*)
C
      data KILROY /.true./
C
      call HI ('COOT')
C     !BEG
      PRNT = NO.gt.0
C
      do 102 I = 1,N
        if(FCK(I).gt.CKI(I,1)) then
C
          if(PRNT) then
C
            if(KILROY) then
              KILROY = .false.
              call ABJECT (NO)
              write (NO,100)
  100         format(' ','Fast electrons: the following regular ',
     $                   'values of CK1 were replaced by FCK1:')
              call LINER  (1, NO)
            end if
C
            write (NO,101) I,CKI(I,1),FCK(I)
  101       format(' ','Depth =',I3,5X,'CK1 =',1PE16.8,5X,'FCK1 =',
     $                 E16.8)
          end if
C
          CKI(I,1) = FCK(I)
        end if
  102 continue
C     !END
      call BYE ('COOT')
C
      return
      end
