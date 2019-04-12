      subroutine ENORM
     $(NO,N,NL,XN1O,XN1,XND,RN1)
C
C     Rudolf Loeser, 1998 May 26
C---- Prints, for RABAM.
C     !DASH
      save
C     !DASH
      real*8 RN1, XN1, XN1O, XND
      integer N, NL, NO
      logical PRNTZ
C     !DASH
      external  LINER, VECOUT, ROMA, HI, BYE
C
C               XND(N,NL), RN1(N), XN1O(N), XN1(N)
      dimension XND(*),    RN1(*), XN1O(*), XN1(*)
C
      data PRNTZ /.true./
C
      call HI ('ENORM')
C     !BEG
      if(NO.gt.0) then
        call VECOUT (NO,XN1O,N,'N1(old)'                )
        call VECOUT (NO,XN1 ,N,'N1(new)'                )
        call VECOUT (NO,RN1 ,N,'RN1 = N1(new) / N1(old)')
C
        call LINER  (2,NO)
        write (NO,100)
  100   format(' ','ND (upper levels)')
        call ROMA   (NO,N,NL, 2,NL, XND, 'Level ',PRNTZ)
      end if
C     !END
      call BYE ('ENORM')
C
      return
      end
