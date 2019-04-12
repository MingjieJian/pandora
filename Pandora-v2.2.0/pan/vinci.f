      subroutine VINCI
     $(NO,SHL,DSK,R1N,T)
C
C     Rudolf Loeser, 1981 Aug 26
C---- Computes and prints total flux, for RAJA.
C     (This is version 4 of VINCI.)
C     !DASH
      save
C     !DASH
      real*8 CMPKM, DSK, R1N, SHL, T, TN
      integer NO
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('VINCI')
C     !BEG
      T  = SHL+DSK
      TN = T/((R1N*CMPKM)**2)
C
      if(NO.gt.0) then
        call LINER (1,NO)
        write (NO,100) T,TN
  100   format(' ',75X,     'Total =',1PE11.4//
     $         ' ',70X,'Total/R**2 =',  E11.4)
      end if
C     !END
      call BYE ('VINCI')
C
      return
      end
