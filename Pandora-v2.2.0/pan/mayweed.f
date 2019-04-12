      subroutine MAYWEED
     $(JLIM,KLIM,JDIM,KDIM,LABEL,W,WNA,WNB,N,DUMP)
C
C     Rudolf Loeser, 1992 Nov 03
C---- Counts the number of cases of WNA .le. W(j,k) .le. WNB.
C     (This is version 2 of MAYWEED.)
C     !DASH
      save
C     !DASH
      real*8 W, WNA, WNB
      integer J, JDIM, JLIM, K, KDIM, KLIM, LUEO, N
      logical DUMP, YES
      character LABEL*5
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external WITHIN, HI, BYE
C
C               W(JDIM,KDIM)
      dimension W(JDIM,*)
C
      call HI ('MAYWEED')
C     !BEG
      do 102 K = 1,KLIM
        do 101 J = 1,JLIM
          call WITHIN (WNA, W(J,K), WNB, 0, YES)
C
          if(YES) then
            N = N+1
            if(DUMP) then
              write (LUEO,100) LABEL,K,J,WNA,W(J,K),WNB,N
  100         format(' ','  MAYWEED',3X,A5,5X,'K=',I4,3X,'J=',I4,3X,
     $                   'WNA=',1PE15.8,3X,'W=',E15.8,3X,'WNB=',E15.8,
     $                   3X,'N=',I8)
            end if
          end if
C
  101   continue
  102 continue
C     !END
      call BYE ('MAYWEED')
C
      return
      end
