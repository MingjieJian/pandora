      subroutine NEPEAN
     $(DL,K,BLIM,RLIM,LBLU,LRED)
C
C     Rudolf Loeser, 1991 Jun 21
C---- Finds approximate Delta-Lambda bracketing indices for a blended
C     line component.
C     !DASH
      save
C     !DASH
      real*8 BLIM, DL, RLIM
      integer K, LBLU, LRED
C     !DASH
      external NOTMORE, NOTLESS, HI, BYE
C
C               DL(KM)
      dimension DL(*)
C
      call HI ('NEPEAN')
C     !BEG
      call NOTMORE (DL,K,BLIM,LBLU)
      if(LBLU.lt.1) then
        LBLU = 1
      end if
C
      call NOTLESS (DL,K,RLIM,LRED)
      if(LRED.lt.1) then
        LRED = K
      end if
C     !END
      call BYE ('NEPEAN')
C
      return
      end
