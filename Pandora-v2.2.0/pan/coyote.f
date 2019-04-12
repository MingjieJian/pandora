      subroutine COYOTE
     $(LAB,N,NRP,TAU,WNR,WHR,WRK,ILFLX)
C
C     Rudolf Loeser, 1981 Nov 03
C---- Prints, for FLAVIUS.
C     !DASH
      save
C     !DASH
      real*8 TAU, WHR, WNR, WRK
      integer ILFLX, LUEO, N, NP3, NRP
      character LAB*14
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, COMPACT, ARROUT, HI, BYE
C
C               TAU(NRP), WNR(NRP,NRP), WHR(NRP,NRP), WRK(NRP,NRP)
      dimension TAU(*),   WNR(*),       WHR(*),       WRK(*)
C
      call HI ('COYOTE')
C     !BEG
      call LINER     (3, LUEO)
      write (LUEO,100) LAB
  100 format(' ',5('----------'),'for Ray: ',A14,5('----------'))
      NP3 = N+3
C
      call VECOUT    (LUEO, TAU, NRP, 'TAU')
C
      call COMPACT   (WNR, NP3, NRP, NRP, WRK)
      call ARROUT    (LUEO, WRK, NP3, NRP, 'WN/Shell, full')
C
      if(ILFLX.gt.0) then
        call COMPACT (WHR, NP3, NRP, NRP, WRK)
        call ARROUT  (LUEO, WRK, NP3, NRP, 'WH/Shell, full')
      end if
C     !END
      call BYE ('COYOTE')
C
      return
      end
