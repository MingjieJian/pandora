      subroutine YANG
     $(NO,N,K,XI,DL,CDW,WVL,SLF,FSAV,EDITED)
C
C     Rudolf Loeser, 1992 Mar 20
C---- Prints, for PLOVER.
C     (This is version 3 of YANG.)
C     !DASH
      save
C     !DASH
      real*8 CDW, DL, FSAV, SLF, WVL, XI
      integer K, N, NO
      logical EDITED
C     !DASH
      external LINER, VECOUT, ARROUT, HI, BYE
C
C               XI(K), DL(K), SLF(N,K), FSAV(N,K)
      dimension XI(*), DL(*), SLF(*),   FSAV(*)
C
      call HI ('YANG')
C     !BEG
      if(NO.gt.0) then
        call LINER    (3,NO)
        write (NO,100) WVL,CDW
  100   format(' ','Frequency-dependent Line Source Function, SLF'//
     $         ' ','Line center wavelength  =',1PE20.12/
     $         ' ','Reference doppler width =',  E20.8)
C
        call VECOUT   (NO,DL,K,'DL')
        call VECOUT   (NO,XI,K,'XI')
        if(EDITED) then
          call ARROUT (NO,FSAV,N,K,'F, before editing')
        end if
        call ARROUT   (NO,SLF ,N,K,'SLF'              )
      end if
C     !END
      call BYE ('YANG')
C
      return
      end
