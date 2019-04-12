      subroutine MALAGA
     $(NO,N,PFT)
C
C     Rudolf Loeser, 1982 Jul 08
C---- Prints Partition Function ratios.
C     !DASH
      save
C     !DASH
      real*8 PFT
      integer I, IE, IS, N, NO
C     !DASH
      external  LINER, GUZMAN, HI, BYE
      intrinsic min
C
C               PFT(N,NMT)
      dimension PFT(*)
C
C
      call HI ('MALAGA')
C     !BEG
      if(NO.gt.0) then
        call LINER    (3,NO)
        write (NO,100)
  100   format(' ','CPR - Partition Function ratios, 2*UII/UI.')
C
        IE = 0
  101   continue
          IS = IE+1
          IE = min(IE+10,N)
          call LINER  (2,NO)
          write (NO,102) (I,I=IS,IE)
  102     format(' ',6X,'Depth',10I11)
          call LINER  (1,NO)
C
          call GUZMAN (PFT,N,NO,IS,IE)
C
        if(IE.lt.N) goto 101
C
      end if
C     !END
      call BYE ('MALAGA')
C
      return
      end
