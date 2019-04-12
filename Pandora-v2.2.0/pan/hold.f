      subroutine HOLD
     $(ITAU,SOME,RK,TRI,RKR,RKI,KNT,IA,IB,TRMN,TRMX)
C
C     Rudolf Loeser, 1991 Mar 22
C---- Dumps for ADDER.
C     !DASH
      save
C     !DASH
      real*8 RK, RKI, RKR, TRI, TRMN, TRMX
      integer I, IA, IB, ITAU, KNT, LUEO
      logical SOME
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               TRI(KNT), RKI(KNT), RKR(KNT)
      dimension TRI(*),   RKI(*),   RKR(*)
C
      call HI ('HOLD')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) ITAU,RK,IA,IB,KNT,SOME
  100 format(' ','TREFF interval search.',10X,'ITAU =',I4,5X,'RK =',
     $           1PE16.8/
     $       ' ','IA =',I5,5X,'IB =',I5,5X,'KNT =',I5,5X,'SOME =',L5//
     $       ' ',18X,'TRI',13X,'RKR',13X,'RKI')
C
      if(KNT.gt.0) then
        write (LUEO,101) (I,TRI(I),RKR(I),RKI(I),I=1,KNT)
  101   format(' ',I5,1P3E16.8)
      end if
C
      call LINER (1, LUEO)
      write (LUEO,102) TRMN,TRMX
  102 format(' ',5X,'TRMN =',1PE16.8,10X,'TRMX =',E16.8)
C     !END
      call BYE ('HOLD')
C
      return
      end
